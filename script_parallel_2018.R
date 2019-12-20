###################################################
#Full 2018 BBS run





remove(list = ls())
n_saved_steps = 2000
n_thin = 20
n_burnin = 10000
n_chains = 3
n_adapt = NULL

dir.create("output", showWarnings = F)

# Install Adam's version from Github (comment these three lines out if already installed:
# install.packages("devtools")
# library(devtools)
# devtools::install_github("AdamCSmithCWS/bbsBayes")


library(bbsBayes)
library(foreach)
library(doParallel)
library(dplyr)
library(tidyverse)

#fetch_bbs_data()

stratified_data <- stratify(by = "bbs_cws")

allspecies.eng = stratified_data$species_strat$english
allspecies.fre = stratified_data$species_strat$french
allspecies.num = stratified_data$species_strat$sp.bbs

allspecies.file = str_replace_all(str_replace_all(allspecies.eng,"[:punct:]",replacement = ""),
                             "\\s",replacement = "_")

###################################################
# Analysis by Species X Model Combination
###################################################


# Set up parallel stuff
n_cores <- 20
cluster <- makeCluster(n_cores, type = "PSOCK")
registerDoParallel(cluster)


model = "gamye"
nspecies = length(allspecies.eng)

nrecs_sp = (table(stratified_data$bird_strat$AOU))

sp.order = sample(size = nspecies,x = c(1:nspecies),replace = F)

foreach(i = sp.order,
        .packages = 'bbsBayes',
        .inorder = FALSE,
        .errorhandling = "pass") %dopar%
  {
    
    species = allspecies.file[i]
    species.eng = allspecies.eng[i]
    species.num = allspecies.num[i]
    
  sp.dir = paste0("output/", species)
  dir.create(sp.dir)
  
  nrecs = nrecs_sp[paste(species.num)]
  
  if(nrecs > 100){
  #### identifying the K folds for cross-validation
    ## selecting stratified samples that remove 10% of data within each stratum
  jags_data <- prepare_jags_data(strat_data = stratified_data,
                                 species_to_run = species.eng,
                                 min_max_route_years = 5,
                                 model = model,
                                 heavy_tailed = T)
  
  


  

    jags_mod <- run_model(jags_data = jags_data,
                               n_saved_steps = n_saved_steps,
                               n_burnin = n_burnin,
                               n_chains = n_chains,
                               n_thin = n_thin,
                               parallel = T,
                          parameters_to_save = c("n","n3","nu","B.X","beta.X","strata","sdbeta","sdX"),
                          modules = NULL)
     save(jags_mod, file = paste0(sp.dir, "/jags_mod_full.RData"))
     save(jags_data, file = paste0(sp.dir, "/jags_data.RData"))
     

  }#end if nrecs > 100
  
    }#end of full model parallel loop
    
stopCluster(cl = cluster)








