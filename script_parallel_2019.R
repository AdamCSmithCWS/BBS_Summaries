###################################################
#Full 2019 BBS run





    inits <- NULL
    n_burnin <- 10000
    n_saved_steps = 1200
    n_thin = 20
    n_iter = ((n_saved_steps*n_thin))
n_chains = 3
n_adapt = NULL

dir.create("output", showWarnings = F)

library(dplyr)
library(tidyverse)

library(bbsBayes) #CRAN version
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

set.seed(2019)
sp.order = sample(size = nspecies,x = c(1:nspecies),replace = F)

#j = 0
#sp.rerun = 0
for(i in sp.order){
  species = allspecies.file[i]
  species.eng = allspecies.eng[i]
  species.num = allspecies.num[i]
  
  sp.dir = paste0("output/", species)
  nrecs = nrecs_sp[paste(species.num)]
  
  if(!is.na(nrecs) & nrecs > 100){
    #### alterntive approach to re running analysis without overwriting
# if(file.exists(paste0(sp.dir, "/jags_mod_full.RData")) == F){
#   j = j+1
#   sp.rerun[j] = i 
# }
  }

}

# split species groups that can't be separated in the early years ---------
splitters = c("Clark's Grebe","Western Grebe","Alder Flycatcher","Willow Flycatcher")
split_miny = c(1990,1990,1978,1978)
names(split_miny) <- splitters




# Set up parallel stuff
n_cores <- 12 #each core will run a jags model in parallel, so total requirement = n_cores*3
cluster <- makeCluster(n_cores, type = "PSOCK")
registerDoParallel(cluster)



fullrun <- foreach(i = sp.order,
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
    ##### one-off because this species was introduced in North America in the mid-1980s - meaningless to include pre 1990
    if(species.eng == "Eurasian Collared-Dove"){
      miny <- 1990 
    }
    
  sp.dir = paste0("output/", species)
  dir.create(sp.dir, showWarnings = F)
  ### this if file.exists line can be uncommented if re-running using previous versions as initial values
  ### or if re-starting analysis, in which case it only runs the model for species with no saved results
   if(!file.exists(paste0(sp.dir, "/jags_mod_full.RData"))){
  #if(!file.exists(paste0(sp.dir, "/jags_data.RData"))){
    
  ### optional approach to using previous runs as initial values
  # load(paste0(sp.dir, "/jags_mod_full.RData"))
  #   inits <- get_final_values(jags_mod)
  #   n_burnin <- 0
  #   n_saved_steps = 1200
  #   n_thin = 20
  #   n_iter = ((n_saved_steps*n_thin))
  # 
  # }else{
  #     inits <- NULL
  #     n_burnin <- 10000
  #     n_saved_steps = 1200
  #     n_thin = 20
  #     n_iter = ((n_saved_steps*n_thin))
  #   } #end if file.exists loop for setting initial values

  rm(list = c("jags_data","jags_mod"))
  nrecs = nrecs_sp[paste(species.num)]
  
 if(nrecs > 100){
  #### identifying the K folds for cross-validation
    ## selecting stratified samples that remove 10% of data within each stratum
  jags_data <- prepare_jags_data(strat_data = stratified_data,
                                 species_to_run = species.eng,
                                 min_max_route_years = 2,
                                 min_year = miny,
                                 model = model,
                                 heavy_tailed = T)
  
  
  save(jags_data, file = paste0(sp.dir, "/jags_data.RData"))
  

  

    jags_mod <- run_model(jags_data = jags_data,
                               n_iter = n_iter,
                               n_burnin = n_burnin,
                               n_chains = n_chains,
                               n_thin = n_thin,
                               parallel = T,
                          model_file_path = "model/GAMYE_Alt_prior.R",
                          inits = inits,
                          #modules = NULL,
                          parameters_to_save = c("n","n3","nu","B.X","beta.X","strata","sdbeta","sdX","alpha"))
     save(list = c("jags_mod","jags_data"), file = paste0(sp.dir, "/jags_mod_full.RData"))
     
     rm(list = c("jags_mod","jags_data"))

  }#end if nrecs > 100
  
  ### uncomment to restart analysis without overwriting any previous results
  }# end if results don't yet exist
  
    }#end of full model parallel loop
    
stopCluster(cl = cluster)


 #save(list = c("sp.rerun"),file = "sp.rerun.RData")





