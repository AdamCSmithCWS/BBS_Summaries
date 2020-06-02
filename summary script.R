######### script to compile bbsBayes output into CWS web content
### author: Adam Smith
##########################
library(pacman)

pkgs = c("bbsBayes","ggplot2","ggrepel","RColorBrewer","tidyverse",
         "doParallel","foreach","dplyr","gganimate","magick","transformr","gifski")

source("functions/generate-map-abundance.R")

p_load(char = pkgs,character.only = T)

sapply(list.files(pattern="[.]R$", path="functions/", full.names=TRUE), source);


 dat_strat = stratify(by = "bbs_cws")

 
c_orng = brewer.pal(9,"Set1")[5]
c_red = brewer.pal(9,"Set1")[1]
c_blue = brewer.pal(9,"Set1")[2]
c_purp = brewer.pal(9,"Set1")[4]
c_green = brewer.pal(9,"Set1")[3]


# speciestemp = c("Blackpoll Warbler","American Kestrel","Pacific Wren",
#                 "Bewick's Wren","LeConte's Sparrow","Horned Lark","Killdeer",
#                 "Bobolink","McCown's Longspur","Canada Warbler","Western Wood-Pewee",
#                 "Barn Swallow","Bank Swallow")
# speciestemp = c("Tree Swallow","Chimney Swift","Common Nighthawk",
#                 "Eastern Whip-poor-will","Purple Martin","Black Swift")
# 
# # speciestemp2 = c("Red Crossbill",
# #                  "Clark's Nutcracker",
# #                  "Varied Thrush",
# #                  "Purple Finch",
# #                  "Bohemian Waxwing",
# #                  "White-winged Crossbill",
# #                  "Cassin's Finch",
# #                  "Common Redpoll",
# #                  "Pine Grosbeak",
# #                  "Pine Siskin",
# #                  "Band-tailed Pigeon",
# #                  "Evening Grosbeak")
# 
# 
# 
# 
# speciestemp2 = c("Chestnut-collared Longspur",
#                  "Black-throated Green Warbler",
#                  "Bay-breasted Warbler",
#                  "Cape May Warbler")
# 
# speciestemp2 = c("Canada Warbler",
#                  "Black-throated Green Warbler",
#                  "Bay-breasted Warbler",
#                  "Cape May Warbler")
# speciestemp2 = c("Tree Swallow")

allspecies.eng = dat_strat$species_strat$english
allspecies.fre = dat_strat$species_strat$french
allspecies.num = dat_strat$species_strat$sp.bbs

allspecies.file = str_replace_all(str_replace_all(allspecies.eng,"[:punct:]",replacement = ""),
                                  "\\s",replacement = "_")

# speciestemp2 = c("Ferruginous Hawk",
#                  "Band-tailed Pigeon",
#                  "Lesser Yellowlegs",
#                  "Great Blue Heron",
#                  "Short-eared Owl",
#                  "Golden-winged Warbler")
COSEWIC = T
if(COSEWIC){
rollTrend = "Trend"
}

GEN_time = F
if(GEN_time){
gen_time_3 = data.frame(species.eng = speciestemp2,
                        gen_time = c(21,13,12,22,12,10))
}

# qs = c(0.025,0.05,0.95,0.975)
YYYY = 2018 ## BBS data version year
short_time = 10 #length of time for short-term trend


lastyear = read.csv("2017estimates/All BBS trends 2017 w reliab.csv",stringsAsFactors = F)
covs = lastyear[,c("sp","species","geo.area","trendtype","trendtime","startyear","reliab.cov")]

covs <- covs[which((covs$trendtime == "full")),]

oldregs = read.csv("old region names.csv",stringsAsFactors = F)

covs = merge(covs,oldregs,by = "geo.area")



# reliability category definitions ----------------------------------------

prec_cuts = c(abs(2*((0.7^(1/20))-1)),
              abs(2*((0.5^(1/20))-1)))*100 
names(prec_cuts) <- c("High","Medium")

cov_cuts = c(0.5,0.25)
names(cov_cuts) <- c("High","Medium")

pool_cuts = c(0.33,0.1)
names(pool_cuts) <- c("High","Medium")

backcast_cuts = c(0.90,0.75)
names(backcast_cuts) <- c("High","Medium")


# load the maps for CWS webmaps ---------------------------------------

WEBMAP = T

if(WEBMAP){
canmap <- rgdal::readOGR(dsn = system.file("maps",
                                                 package = "bbsBayes"),
                      layer = "BBS_CWS_strata",
                      verbose = FALSE)

canmap@data$country <- substr(canmap@data$ST_12,1,2)

canmap <- canmap[canmap$country == "CA",]


stratmap <- sf::read_sf(dsn = system.file("maps",
                                           package = "bbsBayes"),
                         layer = "BBS_CWS_strata",
                         quiet = TRUE)


basmap = rgdal::readOGR(dsn = system.file("maps",
                                          package = "bbsBayes"),
                        layer = "BBS_USGS_strata",
                        verbose = FALSE)
basmap@data$country <- substr(basmap@data$ST_12,1,2)

basmap <- basmap[basmap$country == "CA",]
}

# Species loop ------------------------------------------------------------

external_drive <- FALSE
if(external_drive){
  in_file <- paste0("f:/BBS_Summaries/output/")
}else{
  in_file <- paste0("output/")
  
}



mx_back = 5 # set maximum extrapolation of estimates in regions with no data
exclude_backcast = FALSE # set to TRUE to exclude strata flagged in mx_back, if FALSE, strata are just flagged
short_start = 1995 #start year for the short-term trend annual indices, may be moved back but will affect the included strata depending on the value of mx_back


# parallel setup ----------------------------------------------------------

 splitters = c("Clark's Grebe","Western Grebe","Alder Flycatcher","Willow Flycatcher")
#to_rerun <- which(allspecies.eng %in% splitters)

####
n_cores <- 40
cluster <- makeCluster(n_cores,type = "PSOCK")
registerDoParallel(cluster)

nspecies <- length(allspecies.eng)



allsum <- foreach(ssi = 1:nspecies,
                  .packages = pkgs,
                  .inorder = FALSE,
                  .errorhandling = "pass") %dopar% {

# for(ssi in which(allspecies.eng %in% speciestemp2)){
 
  ss = allspecies.eng[ssi]
  ss.f = allspecies.fre[ssi]
  ss.n = allspecies.num[ssi]
  ss.file = allspecies.file[ssi]
  if(ss == "Eurasian Collared-Dove"){next}
  
  if(GEN_time){
    short_time = gen_time_3[which(gen_time_3$species.eng == ss),"gen_time"]
  }
  
  rm(list = c("jags_mod","jags_data"))
  
  if(file.exists(paste0(in_file,ss.file,"/jags_mod_full.RData"))){
  
    load(paste0(in_file,ss.file,"/jags_mod_full.RData"))
  load(paste0(in_file,ss.file,"/jags_data.RData"))
  
  strat = jags_mod$stratify_by
  


  # Loop for short and long-term trends and indices -------------------------
  
  for(trend_time in c("Long-term","Short-term")){
    if(trend_time == "Long-term"){ 
      fy <- 1970 
      if(ss %in% c("Alder Flycatcher","Willow Flycatcher")){
        fy <- 1978 #5 years after the split 
      }
      if(ss %in% c("Clark's Grebe","Western Grebe","Eurasian Collared-Dove")){
        fy <- 1990 #5 years after the split and first year EUCD observed on > 3 BBS routes
      }
    }else{
      fy <- YYYY-short_time
    }

 
    plot_header = paste(ss.file,trend_time,sep = "_")
    
    ### indices to visualise the trajectories (with year-effects)
  inds_vis = generate_indices(jags_mod = jags_mod,
                                   jags_data = jags_data,
                                   #quantiles = qs,
                                   regions = c("continental","national", "prov_state","bcr","stratum","bcr_by_country"),
                                   #max_backcast = 5,
                                   max_backcast = mx_back,
                                   startyear = min(short_start,fy))
 
    ### indices to calculate trends (without year-effects)
  inds_tr = generate_indices(jags_mod = jags_mod,
                                   jags_data = jags_data,
                                   #quantiles = qs,
                                   regions = c("continental","national", "prov_state","bcr","stratum","bcr_by_country"),
                                   startyear = min(short_start,fy),
                                   max_backcast = mx_back,
                                   #max_backcast = 5,
                                   alternate_n = "n3")
  
  inds_vist = inds_vis$data_summary
  inds_vist$Trend_Time = trend_time
  
  inds_trt = inds_tr$data_summary
  inds_trt$Trend_Time = trend_time
  
  inds_vist$For_Web <- for_web_func(inds_vist)
  inds_trt$For_Web <- for_web_func(inds_trt)

  

  # Write the species index files -------------------------------------------
  
  
  # append species names ----------------------------------------------------
  
  inds_vist$species = ss
  inds_vist$espece = ss.f
  inds_vist$bbs_num = ss.n
  
  inds_trt$species = ss
  inds_trt$espece = ss.f
  inds_trt$bbs_num = ss.n
  
  
 
  if(trend_time == "Long-term"){

    inds_visout = inds_vist
    rm("inds_vist")
    

    inds_trout = inds_trt
    rm("inds_trt")
    
  }else{
 
    inds_visout = rbind(inds_visout,inds_vist)
    rm("inds_vist")
    

    inds_trout = rbind(inds_trout,inds_trt)
    rm("inds_trt")
    if(fy == YYYY-short_time){
    write.csv(inds_visout,paste0("estimates/trends_indices/",paste(ss.file,sep = "_")," annual indices.csv"),row.names = F)
  
    
    write.csv(inds_trout,paste0("estimates/alternate_trends_indices/",paste(ss.file,sep = "_"),"smooth annual indices.csv"),row.names = F)
    }
  }
    
  
  
  rm("trs_web")
  rm("trs_alt")
  trs_web = generate_trends(indices = inds_tr,
                                 Min_year = fy,
                                 #quantiles = qs,
                                 slope = T,
                                 prob_decrease = c(0,25,30,50),
                                 prob_increase = c(0,33,100))
  
  trs_alt = generate_trends(indices = inds_vis,
                                 Min_year = fy,
                                 #quantiles = qs,
                                 slope = T,
                                 prob_decrease = c(0,25,30,50),
                                 prob_increase = c(0,33,100))
  
  
  
  
  # Pooling Factor Calculations ---------------------------------------------
  
  stratlist = unique(data.frame(strat_name = jags_data$strat_name,
                                strat = jags_data$strat))
  stratlist = stratlist[order(stratlist$strat),]
  stratnames = stratlist$strat_name

  if(length(stratnames) > 2){
  sdbeta.mat = jags_mod$sims.list[["sdbeta"]]
  BXmat = jags_mod$sims.list[["B.X"]]
  beta.x.mat = jags_mod$sims.list[["beta.X"]]
  
  poola = NA
  pool = matrix(NA,nrow = jags_data$nstrata,ncol = jags_data$nknots)
  for (k in 1:jags_data$nknots){
    bdif = beta.x.mat[,,k]-BXmat[,k]
    pm_var_strat_bdif = mean (apply (bdif, 1, var))
    
    var_pmean_bdifa = var(apply(bdif,2,mean))
    
    #poola[k] <- min(var_pmean_bdifa/pm_var_strat_bdif  ,1)
    
    for(s in 1:jags_data$nstrata){
      pvar_mean_bdif = var(bdif[,s])
      
      pool[s,k] <- min(pvar_mean_bdif/pm_var_strat_bdif  ,1)
      
      
    }
    
    
  }
  
  lstc = c((ncol(pool)-2):ncol(pool))
  fstc = c(1:3)
  if(fy < 1990){
    pool_by_strat = rowMeans(pool[,c(fstc,lstc)]) #summarizing the pooling for the outer knots
  }else{
    
    pool_by_strat = rowMeans(pool[,lstc]) #summarizing the pooling for the last 3 knots
  }
  
  }else{
    pool_by_strat <- rep(NA,nrow(trs_web))
    
  }
  

  
  for(j in 1:nrow(trs_web)){
    ww = which(stratlist$strat_name %in% unlist(str_split(trs_web[j,"Strata_included"],pattern = " ; ")))
    trs_web[j,"pool"] = 1-mean(pool_by_strat[ww])
    trs_alt[j,"pool"] = 1-mean(pool_by_strat[ww])
    
  }
  
  
  # END pooling factor ------------------------------------------------------
  
  trs_web$Trend_Time = trend_time
  #trst$reliab.prec = trst$Trend_Q0.975-trst$Trend_Q0.025
  
  
  trs_alt$Trend_Time = trend_time
  #trst2$reliab.prec = trst2$Trend_Q0.975-trst2$Trend_Q0.025
  
  

# insert last year's coverage estimates -----------------------------------

  covsp = covs[which(covs$sp == ss.n & covs$trendtype == tolower(trend_time)),]
  covsp = unique(covsp[,c("new.area","reliab.cov")])
  if(any(duplicated(covsp$new.area))){
    dd = which(duplicated(covsp$new.area))
    covsp = covsp[-dd,]
  }
  trs_web = merge(trs_web,
             covsp,
             by.x = "Region_alt",
             by.y = "new.area",
             all.x = T,sort = F)  

  trs_alt = merge(trs_alt,
             covsp,
             by.x = "Region_alt",
             by.y = "new.area",
             all.x = T,sort = F)

  
 
  

# insert reliability categories---------------------------------------------------


  trs_web$precision = reliab_func_prec(trs_web$Width_of_95_percent_Credible_Interval)
  trs_web$coverage = reliab_func_cov(trs_web$reliab.cov)
  trs_web$local_data = reliab_func_pool(trs_web$pool)
  trs_web$backcast_reliab = reliab_func_backcast(trs_web$backcast_flag)
  
  trs_alt$precision = reliab_func_prec(trs_alt$Width_of_95_percent_Credible_Interval)
  trs_alt$coverage = reliab_func_cov(trs_alt$reliab.cov)
  trs_alt$local_data = reliab_func_pool(trs_alt$pool)
  trs_alt$backcast_reliab = reliab_func_backcast(trs_alt$backcast_flag)
  
  trs_web$Reliability = factor(rep("Low",nrow(trs_web)),levels = c("Low","Medium","High"),ordered = T)
  for(j in 1:nrow(trs_web)){
  trs_web[j,"Reliability"] = levels(trs_web$Reliability)[min(as.integer(trs_web[j,c("precision","coverage","backcast_reliab")]),na.rm = T)]
  }
  trs_alt$Reliability = factor(rep("Low",nrow(trs_alt)),levels = c("Low","Medium","High"),ordered = T)
  for(j in 1:nrow(trs_alt)){
    trs_alt[j,"Reliability"] = levels(trs_alt$Reliability)[min(as.integer(trs_alt[j,c("precision","coverage","backcast_reliab")]),na.rm = T)]
  }
# Identify trends to publish on CWS website -------------------------------



  trs_web$For_Web <- for_web_func(trs_web)
  trs_alt$For_Web <- for_web_func(trs_alt)

# Generate the web-maps --------------------------------------------------------
if(WEBMAP){
trs_web = generate_web_maps(trs_web)
}



# Write the species trend files -------------------------------------------


# append species names ----------------------------------------------------

  trs_web$species = ss
  trs_web$espece = ss.f
  trs_web$bbs_num = ss.n
  

  trs_alt$species = ss
  trs_alt$espece = ss.f
  trs_alt$bbs_num = ss.n
  

if(trend_time == "Long-term"){
  trstout = trs_web
  #rm("trs_web")
  
  trstout2 = trs_alt
  #rm("trs_alt")
  
}else{
  trstout = rbind(trstout,trs_web)
  #rm("trs_web")
  write.csv(trstout,paste0("estimates/trends_indices/",paste(ss.file,sep = "_")," trends.csv"),row.names = F)
  
  trstout2 = rbind(trstout2,trs_alt)
  #rm("trs_alt")
  write.csv(trstout2,paste0("estimates/alternate_trends_indices/",paste(ss.file,sep = "_")," trends incl yeareffects.csv"),row.names = F)
}



# Abundance maps and animated gifs ----------------------------------------

  if(trend_time == "Long-term"){
    
    # mp = generate_map_abundance(abundances = inds_vis,annual = T,select = T,stratify_by = "latlong",species = paste0("BBS ",ss),map = stratmap)
    # 
    # panim = animate(mp, nframes = 100, fps = 10, end_pause = 15, rewind = FALSE,height = 800,width = 800)#,renderer = magick_renderer())
    # 
    # anim_save(filename = paste0(plot_header,"_animated_YE_abundance_map.gif"),animation = panim,path = "estimates/Abundance_maps")
    # 
    
    mp = generate_map_abundance(abundances = inds_tr,annual = T,select = T,stratify_by = "latlong",species = paste0("BBS ",ss),map = stratmap)
    
    panim = animate(mp, nframes = 100, fps = 7, end_pause = 15, rewind = FALSE,height = 800,width = 800)#,renderer = magick_renderer())
    
    anim_save(filename = paste0(plot_header,"_animated_abundance_map.gif"),animation = panim,path = "estimates/Abundance_maps")
  }
    
    mp = generate_map_abundance(abundances = trs_alt,annual = F,select = T,stratify_by = "latlong",species = paste0("BBS ",ss),map = stratmap)
    
    
    pdf(paste0("estimates/Abundance_maps/",plot_header,"_abundance_map.pdf"),
        width = 8.5,
        height = 6)
    print(mp)
    dev.off()
    
  
  
  
  

# geofacet plots ----------------------------------------------------------



  pdf(paste0("estimates/geofacets_strata/",plot_header,"_geofacet_strata.pdf"),
      width = 11,
      height = 8.5)
  gf = geofacet_plot(indices_list = inds_vis,
                     select = T,
                     stratify_by = "bbs_cws",
                     multiple = T,
                     trends = trs_web,
                     slope = F,
                     species = ss)
  print(gf)
  dev.off()
  
  pdf(paste0("estimates/geofacets_prov/",plot_header,"_geofacet_prov.pdf"),
      width = 11,
      height = 8.5)
  gf = geofacet_plot(indices_list = inds_vis,
                     select = T,
                     stratify_by = "bbs_cws",
                     multiple = F,
                     trends = trs_web,
                     slope = F,
                     species = ss)
  print(gf)
  dev.off()
 

# Basic index plots, all single pdf for a species and time-series ---------------

  

  ipp = plot_indices(indices_list = inds_vis,
                            min_year = min(short_start,fy),
                            add_observed_means = F,
                            species = ss)
  
  pdf(paste0("estimates/Indices/",plot_header,"_Indices.pdf"),
      width = 8.5,
      height = 6)
  print(ipp)
  dev.off()
  
  
  

  # Complex index plots, all single pdf for a species and time-series ---------------
  
  
  pdf(paste0("estimates/Indices_comparison/",plot_header,"_Indices_comparison.pdf"),
      width = 8.5,
      height = 6)
  
  indsdf = inds_vis$data_summary
  indsdf2 = inds_tr$data_summary
  
  for(i in 1:length(ipp)){
    
    treg = names(ipp)[i]
    treg = gsub(treg,pattern = "_", replacement = "-")
    ttind = indsdf[which(indsdf$Region_alt == treg),]
    ttind2 = indsdf2[which(indsdf2$Region_alt == treg),]
    
    if(nrow(ttind) == 0){
      treg = gsub(treg,pattern = "-", replacement = " ")
      ttind = indsdf[which(indsdf$Region_alt == treg),]
      ttind2 = indsdf2[which(indsdf2$Region_alt == treg),]
    }
    
    if(nrow(ttind) == 0){
      treg = gsub(treg,pattern = " ", replacement = "_")
      ttind = indsdf[which(indsdf$Region_alt == treg),]
      ttind2 = indsdf2[which(indsdf2$Region_alt == treg),]
    }
    
    if(nrow(ttind) == 0 & grepl(pattern = "_BCR_",treg,fixed = T)){
      treg = gsub(treg,pattern = "_", replacement = " ")
      treg = gsub(treg,pattern = " BCR ", replacement = "-BCR_")
      ttind = indsdf[which(indsdf$Region_alt == treg),]
      ttind2 = indsdf2[which(indsdf2$Region_alt == treg),]
    }
    
    ttp = ipp[[i]]
    ## add mean counts, number of routes, trend, nnzero
    # transform count variables to mirror existing axis
    
    trtmp = trs_web[which(trs_web$Region_alt == treg),]
    
    trtmp2 = trs_alt[which(trs_alt$Region_alt == treg),]
    
    st_exc = unique(trtmp$Strata_excluded)
    if(st_exc != "" & exclude_backcast){
      stx2 = unlist(strsplit(st_exc,split = " ; "))
      
      if(length(stx2) > 2){
        st_exc <- paste("Excluding",length(stx2),"strata")
      }else{
        st_exc <- paste("Excluding",st_exc)
      }
    }else{
      st_exc = ""
    }
    
    
     # trlab = paste("Slope_Trend",round(signif(trtmp2$Slope_Trend,2),1),"%/yr","since",trtmp2$Start_year,st_exc,
     #               round(signif(trtmp2$Slope_Trend_Q0.025,2),1),":",round(signif(trtmp2$Slope_Trend_Q0.975,2),1))
     trlab2 = paste("Trend",round(signif(trtmp$Trend,2),1),"%/yr","since",trtmp$Start_year,st_exc,
                    round(signif(trtmp$Trend_Q0.025,2),1),":",round(signif(trtmp$Trend_Q0.975,2),1))
     
    ulim = max(ttind$Index_q_0.975)
    ttind$prts.sc = (ttind$nrts/mean(ttind$nrts_total))*(ulim*0.5)
    ttmax = ttind[which(ttind$Year == YYYY),]
    ttmax$lbl = paste(ttmax$nrts,"routes in",YYYY,ttmax$nrts_total,"total")
    
    ttmin = ttind[which(ttind$Year == min(ttind$Year)),]
    ttmin$lbl = "Observed means"
    
    np <- ttp + 
      geom_ribbon(data = ttind2,aes(x = Year, ymin = Index_q_0.025,ymax = Index_q_0.975),fill = c_orng,alpha = 0.1)+
      geom_line(data = ttind2,aes(x = Year, y = Index),colour = c_orng,alpha = 0.4)+
      geom_point(data = ttind,aes(x = Year, y = obs_mean),colour = c_blue,alpha = 0.3)+
      geom_col(data = ttind,aes(x = Year, y = prts.sc),width = 0.5,fill = c_green,alpha = 0.1)+
      geom_text_repel(data = ttmax,aes(x = Year, y = prts.sc,label = lbl),nudge_y = 0.1*ulim,colour = c_green,alpha = 0.5,size = 3)+
    geom_text_repel(data = ttmin,aes(x = Year, y = obs_mean,label = lbl),nudge_y = 0.1*ulim,colour = c_blue,alpha = 0.5,size = 3)+
    #annotate("text",x = mean(c(fy,YYYY)),y = ulim*0.9,label = trlab)+
    annotate("text",x = mean(c(fy,YYYY)),y = ulim*0.8,label = trlab2, colour = c_orng)
    
    print(np)
    
  }
  dev.off()
  

# range wide trend maps --------------------------------------------------------------


  
  pdf(paste0("estimates/Trend_maps/",plot_header,"_trend_map.pdf"),
      width = 8.5,
      height = 6)
  pp = generate_map(trend = trs_web,select = T,stratify_by = strat,slope = F)
  print(pp)
  dev.off()
  
  
 
  


# COSEWIC output ----------------------------------------------------------
if(COSEWIC & trend_time == "Long-term"){
 
  
  fy2 = max(c(min(short_start,fy),min(jags_data$r_year)))
  indscos = generate_indices(jags_mod = jags_mod,
                                   jags_data = jags_data,
                                   #quantiles = qs,
                                   regions = c("continental","national","prov_state","bcr","stratum"),
                                   startyear = fy2,
                                   max_backcast = mx_back,
                                   alternate_n = "n3")

  
  for(ly2 in c((fy2+short_time):YYYY)){
  trst = generate_trends(indices = indscos,
                                 Min_year = ly2-short_time,
                                 Max_year = ly2,
                                 #quantiles = qs,
                                 slope = T,
                                 prob_decrease = c(0,25,30,50),
                                 prob_increase = c(0,33,100))
  if(ly2 == fy2+short_time){
    tcos = trst
  }else{
    tcos = rbind(tcos,trst)
  }
  
  }

  tcos$rolt = tcos[,rollTrend]
  tcos$roltlci = tcos[,paste0(rollTrend,"_Q0.025")]
  tcos$roltlci2 = tcos[,paste0(rollTrend,"_Q0.25")]
  tcos$roltuci = tcos[,paste0(rollTrend,"_Q0.975")]
  tcos$roltuci2 = tcos[,paste0(rollTrend,"_Q0.75")]
  
  thresh30 = (0.7^(1/short_time)-1)*100
  thresh50 = (0.5^(1/short_time)-1)*100
  
  threshs = data.frame(thresh = c(thresh30,thresh50),
                       p_thresh = c(paste("-30% over",short_time,"years"),
                                          paste("-50% over",short_time,"years")),
                       Year = rep(min(tcos$End_year),2))
  
  pdf(paste0("estimates/Rolling_Trends/",plot_header,"_Rolling_Trends.pdf"),
      width = 8.5,
      height = 6)
  for(rg in unique(tcos$Region_alt)){
    
    tmp = tcos[which(tcos$Region_alt == rg),]
    
    st_exc = unique(trtmp$Strata_excluded)
    if(st_exc != "" & exclude_backcast){
      stx2 = unlist(strsplit(st_exc,split = " ; "))
      
      if(length(stx2) > 2){
        st_exc <- paste("Excluding",length(stx2),"strata")
      }else{
        st_exc <- paste("Excluding",st_exc)
      }
    }else{
      st_exc = ""
    }
    
    
    tmpend4 = tmp[nrow(tmp)-4,]
    tmpend4$lab50 = "50% CI"
    tmpend4$lab95 = "95% CI"
    
    tmpend = tmp[nrow(tmp),]
    
    pth_30_labs = paste0(signif(100*tmpend[,"prob_decrease_30_percent"],2),"% probability of 30% decrease") 
    pth_50_labs = paste0(signif(100*tmpend[,"prob_decrease_50_percent"],2),"% probability of 50% decrease") 
    tmpend$pdec = paste(signif(tmpend[,"Percent_Change"],2),"% Change over",short_time,"years") 
    
      
    cpt = ggplot(data = tmp,aes(x = End_year,y = rolt))+
      theme_minimal()+
      labs(title = paste(ss,"rolling",short_time,"year trends",rg,st_exc),
           subtitle = paste("Based on",rollTrend,"in",YYYY,":",pth_30_labs,"and",pth_50_labs))+
           xlab(paste("Ending year of",short_time,"trend"))+
           ylab(paste(short_time,"year trends"))+
      geom_hline(yintercept = thresh30,colour = c_orng)+
      geom_hline(yintercept = thresh50,colour = c_red)+
      geom_hline(yintercept = 0,colour = grey(0.5))+
      geom_label_repel(data = threshs,aes(x = Year,y = thresh,label = p_thresh),position = "nudge")+
      geom_linerange(aes(x = End_year,ymin = roltlci,ymax = roltuci),colour = c_blue,alpha = 0.6,size = 0.9)+
      geom_pointrange(aes(x = End_year,y = rolt,ymin = roltlci2,ymax = roltuci2),colour = c_blue,size = 1.3,fatten = 1)+
      geom_text_repel(data = tmpend,aes(x = End_year,y = rolt,label = pdec),min.segment.length = 0.1,nudge_y = -0.5)+
      geom_text_repel(data = tmpend4,aes(x = End_year,y = roltuci,label = lab95),colour = c_blue,alpha = 0.6,min.segment.length = 0.1,nudge_y = 0.5,nudge_x = 0.5)+
      geom_text_repel(data = tmpend4,aes(x = End_year,y = roltlci2,label = lab50),colour = c_blue,min.segment.length = 0.1,nudge_y = -0.5,nudge_x = -0.5)
      
      
      

    ## update the theme?
  print(cpt)
    
    }
  dev.off()
  
}#end if COSEWIC


  ### append results to previous output
  
}#short and long-term

  
  }#if output exists
  
}#species loop


stopCluster(cl = cluster)
### read in the full files
### sort and re-name relevant columns to match webcontent exactly
### consider the strata names issue - must match last year's web content











