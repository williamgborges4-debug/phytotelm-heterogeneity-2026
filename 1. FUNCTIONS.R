rm(list = ls())

# Function 1 ----

cut_commu_bot = function(comunidade){
  species_abun = data.frame(Specie = character(), Abun = numeric(), stringsAsFactors = FALSE)
  
  for (i in 5:length(comunidade)) {
    nome_coluna = colnames(comunidade)[i]
    abundancia = sum(comunidade[[nome_coluna]], na.rm = TRUE)
    species_abun = rbind(species_abun, data.frame(Specie = nome_coluna, Abun = abundancia))
  }
  species_abun_high_0 = subset(species_abun, Abun > 5)
  species = species_abun_high_0$Specie
  return(species)
}

# Function 2 ----

# Use to subset species in the communities with abundance higher than 0

cut_commu = function(comunidade){
  species_abun = data.frame(Specie = character(), Abun = numeric(), stringsAsFactors = FALSE)
  
  for (i in 7:length(comunidade)) {
    nome_coluna = colnames(comunidade)[i]
    abundancia = sum(comunidade[[nome_coluna]], na.rm = TRUE)
    species_abun = rbind(species_abun, data.frame(Specie = nome_coluna, Abun = abundancia))
  }
  species_abun_high_0 = subset(species_abun, Abun > 0)
  species = species_abun_high_0$Specie
  return(species)
}

# Function 3 ----

# Use to create a matrix of distance for traits for each leaf species

create_mat_dist = function(traits){
  trait1 = as.data.frame(traits[,2])
  rownames(trait1) = traits$Species
  
  trait2 = as.data.frame(traits[,3])
  rownames(trait2) = traits$Species
  
  trait3 = as.data.frame(traits[,4])
  rownames(trait3) = traits$Species
  
  trait4 = as.data.frame(traits[,5:12])
  trait4 = prep.binary(trait4, col.blocks = 8)
  rownames(trait4) = traits$Species
  
  trait5 = as.data.frame(traits[,13])
  rownames(trait5) = traits$Species
  
  # Creating a ktab.list object to use in the analysis further 
  ktab_list = ktab.list.df(list(trait1, trait2, trait3, trait4, trait5))
  
  # Calculating Gower's distances 
  
  mat_dist = dist.ktab(ktab_list, type = c("N", "N", "N", "B", "Q"))
  mat_dist = as.matrix(mat_dist)
  return(mat_dist)
}

