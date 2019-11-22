######### script to compile bbsBayes output into CWS web content
### author: Adam Smith
##########################
library(pacman)


p_load(char = c("bbsBayes","ggplot2","ggmcmc","tidyverse"),character.only = T)


dat_strat = stratify(by = "bbs_cws")


species_to_run = unique(dat_strat$species_strat$english)


speciestemp = c("Bobolink","McCown's Longspur")


qs = c(0.025,0.05,0.95,0.975)


for(ss in speciestemp){
  
  rm(list = c("jags_mod_full.RData","jags_data.RData"))
  
  load(paste0("model_results/",ss,"/jags_mod_full.RData"))
  load(paste0("model_results/",ss,"/jags_data.RData"))
  
  
  
  
  ###############################################################
  ###############################################################
  ###############################################################
  #### loop for short-term and long-term indices and trends
  
  
  for(fy in c(1970,2008)){
  inds = generate_regional_indices(jags_mod = jags_mod,
                                   jags_data = jags_data,
                                   quantiles = qs,
                                   regions = c("continental","stratum","national", "prov_state","bcr"),
                                   startyear = fy,
                                   max_backcast = 5)
  
  trs = generate_regional_trends(indices = inds,
                                 Min_year = fy,
                                 quantiles = qs,
                                 slope = T,
                                 prob_decrease = c(0,25,30,50),
                                 prob_increase = c(0,33,100))
  
  
  ###############################################################
  ###############################################################
  ###############################################################
  ### geofacet plots
  pdf(paste0("output/geofacets_strata/",ss,"_geofacet_strata.pdf"),
      width = 11,
      height = 8.5)
  gf = geofacet_plot(indices_list = inds,
                     select = T,
                     stratify_by = "bbs_cws",
                     multiple = F,
                     trends = trs,
                     slope = T,
                     species = ss)
  print(gf)
  dev.off()
  
  
  ###############################################################
  ###############################################################
  ###############################################################
  ### index plots, all single pdf for a species and time-series
  
  
  
  
  ###############################################################
  ###############################################################
  ###############################################################
  ### trend maps
  
  
  
 
  
  ###############################################################
  ###############################################################
  ###############################################################
  ### bring in last year's coverage calculation
  
  
  
  ###############################################################
  ###############################################################
  ###############################################################
  ### COSEWIC output
  

  ###############################################################
  ###############################################################
  ###############################################################
  ### grab the list of strata included for each trend,
  ## generate the relevant map
  ## add the map title to the trend file
  
  
  ###############################################################
  ###############################################################
  ###############################################################
  ## add the forweb column
  
  
  
  
  ### append results to previous output
  
}


### read in the full files
### sort and re-name relevant columns to match webcontent exactly
### consider the strata names issue - must match last year's web content











