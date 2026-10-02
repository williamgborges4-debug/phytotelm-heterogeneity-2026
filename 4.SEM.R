#SEM

#ID  Area Transect Parcel Replica Species Mass_loss Canopy_opening Clorofila Desvpad_temp
#1 E1I1 Ative        1      1       1    Inga     21.80          17.12    268.37         0.33
#2 E1I2 Ative        1      2       2    Inga     14.83          32.80    268.37         0.33
#3 E1I3 Ative        1      3       3    Inga     26.19          23.97    268.37         0.33
#4 E1I4 Ative        1      4       4    Inga     31.88          38.58    268.37         0.33
#5 E2I1 Ative        2      1       1    Inga     21.81          23.84    104.78         0.33
#6 E2I2 Ative        2      2       2    Inga     18.17          20.22    104.78         0.33
#FRic       FEve      FDis Basal_richness Predators_richness       PC1
#1 24.927050 0.48531195 0.8045106              4                  0 -30.03111
#2 14.289418 0.05747693 5.7156533              3                  2 -23.42613
#3  0.269797 0.35366176 1.5415702              5                  0 -34.48956
#4 24.760433 0.56260220 4.0798475              5                  0 -21.52724
#5 23.034490 0.33676447 2.1118636              4                  1 -31.08242
#6 40.890907 0.38826978 1.9686949              8                  3 -20.04782

library(piecewiseSEM)
library(lme4)


estimate_sem = function(data){
  mod = psem(
    
    lmer(Clorofila ~ PC1+Desvpad_temp +(1|Transect)+(1|Replica),data = data),
    lmer(Desvpad_temp ~ PC1 +(1|Transect) + (1|Replica), data = data),
    
    lmer(Basal_richness~ Clorofila+ Predators_richness+(1|Transect)+(1|Replica),data =data),
    
    lmer(FEve ~ Basal_richness+(1|Transect)+(1|Replica),data = data),
    lmer(FRic ~ Basal_richness+(1|Transect)+(1|Replica),data = data),
    lmer(FDis ~ Basal_richness+(1|Transect)+(1|Replica), data = data),
    
    lmer(Mass_loss ~ Desvpad_temp+FRic+FDis+FEve+(1|Transect) + (1|Replica), data = data)
  )
  
  return(summary(mod))
}

estimate_sem(X_inga)
estimate_sem(X_euc)
estimate_sem(X_arau)
estimate_sem(X_pinus)

######################
# your standardized original variables
vars_hv <- c("FD", "Richeness", "Abundance", "RAO", "mean_canopy")
X <- vegan::decostand(df_botanic_summary2[, vars_hv], method = "standardize")

# PCoA scores (axis 1)
axis1 <- pcoa_traits_mist$vectors[,1]

# correlation of each variable with Axis.1
cor_axis1 <- apply(X, 2, function(v) cor(v, axis1, use = "pairwise.complete.obs"))

# relative contribution in percentage
contri_axis1 <- abs(cor_axis1) / sum(abs(cor_axis1)) * 100

# final result
data.frame(Variavel = vars_hv,
           Correlacao = round(cor_axis1, 3),
           Contribuicao_percent = round(contri_axis1, 2))


library(openxlsx)

df_botanic_summary$RAO = df_botanic_summary$RAO$RAO

openxlsx::write.xlsx(df_botanic_summary, "df_botanic_summary.xlsx", asTable = TRUE)

class(df_botanic_summary$RAO)
head(df_botanic_summary$RAO)
sapply(df_botanic_summary, class)

library(dplyr)
library(tidyr)

df_abund <- df_predadores |>
  summarise(across(6:21,
                   list(media = ~mean(.x, na.rm = TRUE),
                        dp    = ~sd(.x,   na.rm = TRUE),
                        n     = ~sum(!is.na(.x))),
                   .names = "{.col}.{.fn}")) |>
  pivot_longer(everything(),
               names_to = c("variavel","estat"),
               names_sep = "\\.",
               values_to = "valor") |>
  pivot_wider(names_from = estat, values_from = valor) |>
  arrange(variavel)

