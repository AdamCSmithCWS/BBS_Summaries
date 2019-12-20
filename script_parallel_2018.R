###################################################
# Adam C. Smith & Brandon P.M. Edwards
# GAM Paper Script
# gam-kfold-bbsBayes.R
# Created July 2019
# Last Updated July 2019
###################################################

###################################################
# Initial Setup + Setting Constants
###################################################
setwd("C:/BBS19/Cosewic2019")

remove(list = ls())
# These are just the defaults for bbsBayes run_model. Modify as needed
n_saved_steps = 2000
n_thin = 20
n_burnin = 10000
n_chains = 3
n_adapt = 1000

dir.create("output", showWarnings = F)

# Install v1.1.2 from Github (comment these three lines out if already installed:
# install.packages("devtools")
# library(devtools)
# devtools::install_github("BrandonEdwards/bbsBayes")#, ref = "v1.1.2")

library(bbsBayes)
library(foreach)
library(doParallel)
library(dplyr)
library(tidyr)

# Only need to run this if you don't have BBS data saved
# in directory on your computer
# yes immediately following to agree to terms and conditions
# fetch_bbs_data()
# yes

stratified_data <- stratify(by = "bbs_cws")

species_to_run <- c("Pacific Wren",
                    "Bewick's Wren",
                    "LeConte's Sparrow",
                    "Blackpoll Warbler",
                    "Bobolink",
                    "Canada Warbler",
                    "Western Wood-Pewee",
                    "McCown's Longspur",
                    "Horned Lark",
                    "Killdeer",
                    "Barn Swallow",
                    "Bank Swallow")


###################################################
# Analysis by Species X Model Combination
###################################################


# Set up parallel stuff
n_cores <- 7
cluster <- makeCluster(n_cores, type = "PSOCK")
registerDoParallel(cluster)


model = "gamye"
nspecies = length(species_to_run)


foreach(i = 1:nspecies,
        .packages = 'bbsBayes',
        .inorder = FALSE,
        .errorhandling = "pass") %dopar%
  {
    
    species = species_to_run[i]
    
  sp.dir = paste0("output/", species)
  dir.create(sp.dir)
  
  
  #### identifying the K folds for cross-validation
    ## selecting stratified samples that remove 10% of data within each stratum
  jags_data <- prepare_jags_data(strat_data = stratified_data,
                                 species_to_run = species,
                                 min_max_route_years = 5,
                                 model = model,
                                 heavy_tailed = T)
  
  


  
#inits = function()
    #model_file <- paste0("C:/Users/RA/Documents/R/win-library/3.6/bbsBayes/models/gam-ye.t.jags")
    # 
    jags_mod <- run_model(jags_data = jags_data,
                              # model_file_path = model_file,
                               n_saved_steps = n_saved_steps,
                               #n_adapt = n_adapt,
                               n_burnin = n_burnin,
                               n_chains = n_chains,
                               n_thin = n_thin,
                               parallel = F,
                          parameters_to_save = c("n","n3","nu","sdnoise","B.X","beta.X"),
                          modules = NULL)
     save(jags_mod, file = paste0(sp.dir, "/jags_mod_full.RData"))
     save(jags_data, file = paste0(sp.dir, "/jags_data.RData"))
     
    }#end of full model parallel loop
    
stopCluster(cl = cluster)








