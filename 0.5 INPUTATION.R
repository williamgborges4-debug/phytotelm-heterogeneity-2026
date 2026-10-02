# Clear environment 
rm(list = ls())

# Set working directory to script location ----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load required libraries ----
library(dplyr)
library(Rphylopars)
library(rotl)

# Load and filter botanic abundance data ---- 
df_botanic = read.csv("data_abun_botanic_d02_m05_y25.csv", header = TRUE, sep = ";")

# Exclude species lacking continuous trait data
especies = c(
  "Allophylus.edulis", "Annona.rugulosa", "Banara.tomentosa",
  "Balfourodendron.riedelianum", "Chrysophyllum.gonocarpum",
  "Cordia.trichotoma", "Eugenia.involucrata", "Eugenia.uniflora",
  "Guarea.macrophylla", "Helietta.apiculata", "Inga.vera",
  "Lonchocarpus.campestris", "Roupala.montana", "Sorocea.bonplandii",
  "Trichilia.elegans"
)
df_botanic_filt = df_botanic[, !names(df_botanic) %in% especies]

# Load trait data ----
traits_botanic = read.csv2("data_botanic_traits_d02_m05_y25.csv", header = TRUE)

# Replace string "<NA>" with true NA values
traits_botanic[traits_botanic == "<NA>"] = NA

# Convert numeric columns from comma to period decimal separator
traits_botanic = traits_botanic %>%
  mutate(across(c(LMA, N, P, NPratio, FA), ~ as.numeric(gsub(",", ".", .x))))

# Confirm column classes
sapply(traits_botanic, class)

# Standardize species names for Open Tree query (underscore -> dot)
traits_botanic$species = gsub("_", ".", traits_botanic$species)

# Retrieve phylogenetic tree from Open Tree of Life ----
species_list = traits_botanic$species
taxa = tnrs_match_names(species_list)

# Remove unmatched species prior to tree inference
taxa_clean = taxa[!is.na(taxa$ott_id), ]
unmatched  = taxa$search_string[is.na(taxa$ott_id)]
if (length(unmatched) > 0) message("Unmatched species removed: ", paste(unmatched, collapse = ", "))

tree = tol_induced_subtree(ott_ids = taxa_clean$ott_id, label_format = "name")
tree = compute.brlen(tree, method = "Grafen")

# Standardize species names for phylopars compatibility (dot -> underscore)
traits_botanic$species = gsub("\\.", "_", traits_botanic$species)

# Select trait columns and retain only species present in the phylogeny ----
trait_columns = traits_botanic %>%
  select(species, LMA, N, P, NPratio) %>%
  filter(species %in% tree$tip.label)

# Confirm all trait columns are numeric
sapply(trait_columns, class)

# Fit phylogenetic imputation models ----
fit    = phylopars(trait_data = trait_columns, tree = tree, model = "BM")
fit_ou = phylopars(trait_data = trait_columns, tree = tree, model = "OU")
fit_l  = phylopars(trait_data = trait_columns, tree = tree, model = "lambda")
fit_eb = phylopars(trait_data = trait_columns, tree = tree, model = "EB")

# Compare models by AIC
AIC(fit, fit_ou, fit_l, fit_eb)

# Extract ancestral reconstructions and retain tip estimates only ----
imputed_traits = fit_ou$anc_recon
tip_labels     = tree$tip.label
imputed_tips   = imputed_traits[tip_labels, ]

imputed_tips_df = data.frame(
  species   = rownames(imputed_tips),
  imputed_tips,
  row.names = NULL
)

# Merge imputed values with original trait data
traits_imputed = cbind(traits_botanic, imputed_tips_df)

# Restore original species name format and remove redundant columns ----
colnames(traits_imputed)[colnames(traits_imputed) == "species"] = "Species"
traits_imputed$Species   = gsub("_", " ", traits_imputed$Species)
traits_imputed$species.1 = NULL
traits_imputed$LMA.1     = NULL
traits_imputed$N.1       = NULL
traits_imputed$P.1       = NULL
traits_imputed$NPratio.1 = NULL

# Export imputed trait dataset ----
write.csv2(traits_imputed, "data_botanic_traits_imput_d02_m05_y25.csv", row.names = FALSE)

