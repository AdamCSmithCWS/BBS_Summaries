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

library(dplyr)
library(tidyverse)

library(bbsBayes)
library(foreach)
library(doParallel)

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


model = "gamye"
nspecies = length(allspecies.eng)

nrecs_sp = (table(stratified_data$bird_strat$AOU))

sp.order = sample(size = nspecies,x = c(1:nspecies),replace = F)

j = 0
sp.rerun = 0
for(i in sp.order){
  species = allspecies.file[i]
  species.eng = allspecies.eng[i]
  species.num = allspecies.num[i]
  
  sp.dir = paste0("output/", species)
  nrecs = nrecs_sp[paste(species.num)]
  
  if(!is.na(nrecs) & nrecs > 100){
    
if(file.exists(paste0(sp.dir, "/jags_mod_full.RData")) == F){
  j = j+1
  sp.rerun[j] = i 
}
  }
}

splitters = c("Clark's Grebe","Western Grebe","Alder Flycatcher","Willow Flycatcher")
split_miny = c(1990,1990,1978,1978)
names(split_miny) <- splitters

to_rerun <- which(allspecies.eng %in% splitters)

sp.rerun <- to_rerun



# Set up parallel stuff
n_cores <- 15
cluster <- makeCluster(n_cores, type = "PSOCK")
registerDoParallel(cluster)



fullrun <- foreach(i = sp.rerun,
        .packages = 'bbsBayes',
        .inorder = FALSE,
        .errorhandling = "pass") %dopar%
  {
    
    species = allspecies.file[i]
    species.eng = allspecies.eng[i]
    species.num = allspecies.num[i]
     
    miny = NULL
    
    if(species.eng %in% splitters){
      miny <- split_miny[species.eng] 
     }
  sp.dir = paste0("output/", species)
  
  #if(file.exists(paste0(sp.dir, "/jags_mod_full.RData")) == F){
    
  dir.create(sp.dir, showWarnings = F)
  
  nrecs = nrecs_sp[paste(species.num)]
  
 if(nrecs > 100){
  #### identifying the K folds for cross-validation
    ## selecting stratified samples that remove 10% of data within each stratum
  jags_data <- prepare_jags_data(strat_data = stratified_data,
                                 species_to_run = species.eng,
                                 min_max_route_years = 5,
                                 min_year = miny,
                                 model = model,
                                 heavy_tailed = T)
  
  
  save(jags_data, file = paste0(sp.dir, "/jags_data.RData"))
  

  

    jags_mod <- run_model(jags_data = jags_data,
                               n_saved_steps = n_saved_steps,
                               n_burnin = n_burnin,
                               n_chains = n_chains,
                               n_thin = n_thin,
                               parallel = F,
                          parameters_to_save = c("n","n3","nu","B.X","beta.X","strata","sdbeta","sdX"),
                          modules = NULL)
     save(jags_mod, file = paste0(sp.dir, "/jags_mod_full.RData"))
     
     rm(list = c("jags_mod","jags_data"))

  }#end if nrecs > 100
  #}# end if results don't yet exist
  
    }#end of full model parallel loop
    
stopCluster(cl = cluster)


 #save(list = c("sp.rerun"),file = "sp.rerun.RData")





