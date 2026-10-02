library(ade4)
library(ape)
library(FD)
library(picante)
library(adiv)
library(ggplot2)
library(dplyr)

# Analysis of vegetation community and the environmental heterogeneity ----

# 1. Functional diversity 

# Functional distance matrix

trait1 = as.data.frame(traits_botanic_filt[,5])
rownames(trait1) = traits_botanic_filt$Species

trait2 = as.data.frame(traits_botanic_filt[,6])
rownames(trait2) = traits_botanic_filt$Species

trait3 = as.data.frame(traits_botanic_filt[,7])
rownames(trait3) =traits_botanic_filt$Species

trait4 = as.data.frame(traits_botanic_filt[,8])
rownames(trait4) =traits_botanic_filt$Species

trait5 = as.data.frame(traits_botanic_filt[,9])
rownames(trait5) =traits_botanic_filt$Species

trait6 = as.data.frame(traits_botanic_filt[,10])
rownames(trait6) =traits_botanic_filt$Species

trait7 = as.data.frame(traits_botanic_filt[,11])
rownames(trait7) =traits_botanic_filt$Species

trait8 = as.data.frame(traits_botanic_filt[,12])
rownames(trait8) =traits_botanic_filt$Species

# Creating a ktab.list object to use in the analysis further 
ktab_list = ktab.list.df(list(trait1, trait2, trait3, trait4,trait5,trait6,trait7,trait8))

# Calculating Gower's distances 
mat_dist_botanic = dist.ktab(ktab_list, type = c("Q", "Q", "Q","Q","N","N","N","Q"))

#X = FD::dbFD(mat_dist_botanic, df_botanic_filt, corr="cailliez")

# Create a dendogram 
dend = hclust(mat_dist_botanic, "average")
plot(dend)

# Create a tree
tree_dend = as.phylo(dend)
plot(tree_dend)

# Calculate functional diversity 
FD = pd(df_botanic_filt, tree_dend)

df_botanic_filt2 = df_botanic_filt
df_botanic_filt2$FD = FD$PD
df_botanic_filt2$Richeness = FD$SR
df_botanic_filt2$Abundance = rowSums(df_botanic_filt)

# RAO
RAO = QE(df_botanic_filt, mat_dist_botanic, formula = "QE")
RAO = data.frame(RAO= RAO$diversity)
df_botanic_filt2$RAO = RAO


# 2. Environmental heterogeneity

# 2. Environmental heterogeneity
# Preapare the dataset to create habitat heterogneity 

df_canopy = cbind(df_leaf[,2:4], Canopy_opening = df_leaf[,8])
df_canopy_summary = df_canopy |>
  dplyr::group_by(Area, Transect, Parcel) |>
  dplyr::summarise(mean_canopy = mean(Canopy_opening, na.rm = TRUE), .groups = "drop")

df_botanic_summary = cbind(df_botanic[,2:4], df_botanic_filt2[,42:45])
df_botanic_summary2 = dplyr::left_join(df_botanic_summary, df_canopy_summary,
                                       by = c("Area", "Transect", "Parcel"))

mat_dist_botanic_2 = vegdist(vegan::decostand(df_botanic_summary2[,4:8],
                                              method = "standardize"), method = "euc")
pcoa_traits_mist = pcoa(mat_dist_botanic_2, correction = "cailliez")
eixos_mist = as.data.frame(pcoa_traits_mist$vectors[,1:2])

# Anchor Axis.1 to correlate positively with FD
cor1 <- suppressWarnings(cor(eixos_mist$Axis.1, df_botanic_summary2$FD, use = "complete.obs"))
s1 <- ifelse(is.na(cor1) | cor1 == 0, 1, sign(cor1))
eixos_mist$Axis.1 <- eixos_mist$Axis.1 * s1

df_botanic_summary = cbind(df_botanic_summary2, eixos_mist)
df_botanic_summary$Area[df_botanic_summary$Area == "Pinnus"] <- "Pinus"

hull_cwm = df_botanic_summary |>
  dplyr::group_by(Area) |>
  dplyr::filter(dplyr::n() >= 3) |>
  dplyr::slice(chull(Axis.1, Axis.2)) |>
  dplyr::ungroup()


ggplot(df_botanic_summary, aes(x = Axis.1, y = Axis.2, color = Area, shape = Area)) + 
  geom_point(size = 4, alpha = 0.7) + 
  scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +
  scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A"))


df_botanic_summary_pc1 = cbind(df_botanic_summary2[,1:3], PC1 = eixos_mist$Axis.1)


#Invertebrates ---- 

# Functional distance matrices

mat_dist_inga = create_mat_dist(trait_benthos_inga)
mat_dist_euc = create_mat_dist(trait_benthos_euc)
mat_dist_pinus = create_mat_dist(trait_benthos_pinus)
mat_dist_arau = create_mat_dist(trait_benthos_arau)

fd_benthos_inga = FD::dbFD(mat_dist_inga, df_benthos_inga_filt, corr="cailliez")
df_leaf_inga$FRic = fd_benthos_inga$FRic
df_leaf_inga$FEve = fd_benthos_inga$FEve
df_leaf_inga$FDis = fd_benthos_inga$FDis

df_leaf_inga$Basal_richness = specnumber(df_nao_predadores_inga[,7:length(df_nao_predadores_inga)])
df_leaf_inga$Predators_richness = specnumber(df_predadores_ing[,7:length(df_predadores_ing)])

fd_benthos_euc = FD::dbFD(mat_dist_euc, df_benthos_euc_filt, corr="cailliez")
df_leaf_euc$FRic = fd_benthos_euc$FRic
df_leaf_euc$FEve = fd_benthos_euc$FEve
df_leaf_euc$FDis = fd_benthos_euc$FDis
df_leaf_euc$Basal_richness = specnumber(df_nao_predadores_euc[,7:length(df_nao_predadores_euc)])
df_leaf_euc$Predators_richness = specnumber(df_predadores_euc[,7:length(df_predadores_euc)])




fd_benthos_pinus = FD::dbFD(mat_dist_pinus, df_benthos_pinus_filt, corr="cailliez")
df_leaf_pinus$FRic = fd_benthos_pinus$FRic
df_leaf_pinus$FEve = fd_benthos_pinus$FEve
df_leaf_pinus$FDis = fd_benthos_pinus$FDis
df_leaf_pinus$Basal_richness = specnumber(df_nao_predadores_pinus[,7:length(df_nao_predadores_pinus)])
df_leaf_pinus$Predators_richness = specnumber(df_predadores_pinus[,7:length(df_predadores_pinus)])

fd_benthos_arau = FD::dbFD(mat_dist_arau, df_benthos_arau_filt, corr="cailliez")
df_leaf_arau$FRic = fd_benthos_arau$FRic
df_leaf_arau$FEve = fd_benthos_arau$FEve
df_leaf_arau$FDis = fd_benthos_arau$FDis
df_leaf_arau$Basal_richness = specnumber(df_nao_predadores_arau[,7:length(df_nao_predadores_arau)])
df_leaf_arau$Predators_richness = specnumber(df_predadores_arau[,7:length(df_predadores_arau)])

# Join data-sets

X_inga = left_join(df_leaf_inga, df_botanic_summary_pc1, by=c("Area", "Transect","Parcel"))

X_inga_pad = vegan::decostand(X_inga[,7:16], method = "standardize")

X_inga = cbind(X_inga[,1:6], X_inga_pad)

X_euc = left_join(df_leaf_euc, df_botanic_summary_pc1, by=c("Area", "Transect","Parcel"))

X_euc_pad = vegan::decostand(X_euc[,7:16], method = "standardize")

X_euc = cbind(X_euc[,1:6], X_euc_pad)

X_pinus = left_join(df_leaf_pinus, df_botanic_summary_pc1, by=c("Area", "Transect","Parcel"))

X_pinus_pad = vegan::decostand(X_pinus[,7:16], method = "standardize")

X_pinus = cbind(X_pinus[,1:6], X_pinus_pad)

X_arau = left_join(df_leaf_arau, df_botanic_summary_pc1, by=c("Area", "Transect","Parcel"))

X_arau_pad = vegan::decostand(X_arau[,7:16], method = "standardize")

X_arau = cbind(X_arau[,1:6], X_arau_pad)

