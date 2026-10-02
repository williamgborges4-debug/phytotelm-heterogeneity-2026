# Set working directory to script location ----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load and clean data ----

# Botanic abundance data ---- 

df_botanic = read.csv("data_abun_botanic_d02_m05_y25.csv", header = T, sep = ";")

sps_botanic = cut_commu_bot(df_botanic)
df_botanic_filt = df_botanic[,sps_botanic]
#vegan::specnumber(df_botanic_filt)

# Botanic traits data ----

traits_botanic = read.csv2("data_botanic_traits_imput_d02_m05_y25.csv", header = TRUE)

colnames(traits_botanic)[colnames(traits_botanic) == "species"] = "Species"
traits_botanic$Species = gsub(" ", ".", traits_botanic$Species)

traits_botanic_filt = traits_botanic |> 
  dplyr::filter(Species %in% sps_botanic)

#traits_botanic_filt$FA = NULL
#traits_botanic_filt$LMA = NULL
#traits_botanic_filt$N = NULL
#traits_botanic_filt$P = NULL
#traits_botanic_filt$NPratio = NULL

#remove(df_botanic, traits_botanic, sps_botanic)

# Leaf litter breakdown data ----

df_leaf = read.csv("data_decomp_02_m05_y25.csv", header = T, sep = ";")

df_leaf_inga = subset(df_leaf, Species == "Inga")
df_leaf_euc = subset(df_leaf, Species == "Euca")
df_leaf_pinus = subset(df_leaf, Species == "Pinus")
df_leaf_arau = subset(df_leaf, Species == "Arau")


# Invertebrates abundance data ----

df_benthos = read.csv("data_benthos_abun_d25_m04_y25.csv", 
                      header = T, sep = ";")

df_benthos_inga = subset(df_benthos, Species == "Inga")
df_benthos_euc= subset(df_benthos, Species == "Euca")
df_benthos_pinus = subset(df_benthos, Species == "Pinus")
df_benthos_arau= subset(df_benthos, Species == "Arau")

# Cut species from each community with abundance higher than 0
sps_inga = cut_commu(df_benthos_inga)
sps_euc = cut_commu(df_benthos_euc)
sps_pinus = cut_commu(df_benthos_pinus)
sps_arau = cut_commu(df_benthos_arau)

df_benthos_inga_filt = df_benthos_inga[, sps_inga]
df_benthos_euc_filt = df_benthos_euc[, sps_euc]
df_benthos_pinus_filt = df_benthos_pinus[, sps_pinus]
df_benthos_arau_filt = df_benthos_arau[, sps_arau]

# Predators

predators = c("Aranae", "Ceratopogonidae_1", "Ceratopogonidae_2", "Culicoides_1", "Formicidae", "Hebridae_1", "Hebridae_2", "Hydracarina_1", "Hydracarina_2", "Hydracarina_3", "Mecistogaster_1", "Mesovelidae_1", "Sciomyzidae_1", "Sciomyzidae_2", "Tabanidae_1", "Toxorhynchites_1")

df_predadores = cbind(df_benthos[,1:6], df_benthos[,predators])

df_predadores_ing = subset(df_predadores, Species == "Inga")
df_predadores_euc = subset(df_predadores, Species == "Euca")
df_predadores_pinus = subset(df_predadores, Species == "Pinus")
df_predadores_arau = subset(df_predadores, Species == "Arau")

# All morphospecies columns
all_species = colnames(df_benthos)[7:ncol(df_benthos)]

# Filter out morphospecies that are NOT predators.
non_predators = all_species[!all_species %in% predators]

# Create a new data frame with metadata + morphospecies that are not predators.
df_nao_predadores = cbind(df_benthos[, 1:6], df_benthos[, non_predators])

df_nao_predadores_inga = subset(df_nao_predadores, Species == "Inga")
df_nao_predadores_euc = subset(df_nao_predadores, Species == "Euca")
df_nao_predadores_pinus = subset(df_nao_predadores, Species == "Pinus")
df_nao_predadores_arau = subset(df_nao_predadores, Species == "Arau")


# Invertebrates traits ----

trait_benthos = read.csv("data_benthos_traits_d25_m04_y25.csv", 
                         header = T, sep = ";")


# Cut trait data for each specific community 

trait_benthos_inga = trait_benthos |> 
  dplyr::filter(Species %in% sps_inga)

trait_benthos_euc = trait_benthos |> 
  dplyr::filter(Species %in% sps_euc)

trait_benthos_pinus = trait_benthos |> 
  dplyr::filter(Species %in% sps_pinus)

trait_benthos_arau = trait_benthos |> 
  dplyr::filter(Species %in% sps_arau)

