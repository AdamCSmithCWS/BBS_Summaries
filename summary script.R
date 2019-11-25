######### script to compile bbsBayes output into CWS web content
### author: Adam Smith
##########################
library(pacman)


p_load(char = c("bbsBayes","ggplot2","ggrepel","RColorBrewer"),character.only = T)


dat_strat = stratify(by = "bbs_cws")


species_to_run = unique(dat_strat$species_strat$english)


speciestemp = c("Bobolink","McCown's Longspur")


# qs = c(0.025,0.05,0.95,0.975)
YYYY = 2018

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
                                   #quantiles = qs,
                                   regions = c("continental","stratum","national", "prov_state","bcr"),
                                   startyear = fy,
                                   max_backcast = 5)
  # inds2 = generate_regional_indices(jags_mod = jags_mod,
  #                                  jags_data = jags_data,
  #                                  quantiles = qs,
  #                                  regions = c("prov_state"),
  #                                  startyear = fy,
  #                                  max_backcast = 5)
  
  trs = generate_regional_trends(indices = inds,
                                 Min_year = fy,
                                 #quantiles = qs,
                                 slope = T,
                                 prob_decrease = c(0,25,30,50),
                                 prob_increase = c(0,33,100))
  
  
  ###############################################################
  ###############################################################
  ###############################################################
  ### geofacet plots
  pdf(paste0("output/geofacets_strata/",ss,"_",fy,"_geofacet_strata.pdf"),
      width = 11,
      height = 8.5)
  gf = geofacet_plot(indices_list = inds,
                     select = T,
                     stratify_by = "bbs_cws",
                     multiple = T,
                     trends = trs,
                     slope = T,
                     species = ss)
  print(gf)
  dev.off()
  
  
  ###############################################################
  ###############################################################
  ###############################################################
  ### index plots, all single pdf for a species and time-series
  
  ipp = plot_strata_indices(indices_list = inds,
                            min_year = fy,
                            add_observed_means = F,
                            species = ss)
  
  pdf(paste0("output/Indices/",ss,"_",fy,"_Indices.pdf"),
      width = 8.5,
      height = 6)
  print(ipp)
  dev.off()
  
  
  
  ###### generate the require ggplot components to add to each ipp within a loop
  
  
  pdf(paste0("output/Indices_comparison/",ss,"_",fy,"_Indices_comparison.pdf"),
      width = 8.5,
      height = 6)
  
  indsdf = inds$data_summary
  for(i in 1:length(ipp)){
    
    treg = names(ipp)[i]
    treg = gsub(treg,pattern = "_", replacement = "-")
    ttind = indsdf[which(indsdf$Region == treg),]
    ttp = ipp[[i]]
    ## add mean counts, number of routes, trend, nnzero
    # transform count variables to mirror existing axis
    
    trtmp = trs[which(trs$Region == treg),]
    trlab = paste(trtmp$Region_alt,round(signif(trtmp$Slope_Trend,2),1),"%/yr")
    
    ulim = max(ttind$Index_q_0.975)
    ttind$prts.sc = (ttind$nrts/mean(ttind$nrts_total))*(ulim*0.5)
    ttmax = ttind[which(ttind$Year == YYYY),]
    ttmax$lbl = paste(ttmax$nrts,"routes in",YYYY,ttmax$nrts_total,"total")
    
    ttmin = ttind[which(ttind$Year == fy),]
    ttmin$lbl = "Observed means"
    
    np <- ttp + 
      geom_point(data = ttind,aes(x = Year, y = obs_mean),colour = brewer.pal(5,"Set1")[2],alpha = 0.3)+
      geom_col(data = ttind,aes(x = Year, y = prts.sc),width = 0.5,fill = brewer.pal(5,"Set1")[5],alpha = 0.1)+
      geom_text_repel(data = ttmax,aes(x = Year, y = prts.sc,label = lbl),nudge_y = 0.1*ulim,colour = brewer.pal(5,"Set1")[5],alpha = 0.5,size = 3)+
    geom_text_repel(data = ttmin,aes(x = Year, y = obs_mean,label = lbl),nudge_y = 0.1*ulim,colour = brewer.pal(5,"Set1")[2],alpha = 0.5,size = 3)+
    annotate("text",x = mean(c(fy,YYYY)),y = ulim*0.9,label = trlab)
  print(np)
    
  }
  dev.off()
  
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











