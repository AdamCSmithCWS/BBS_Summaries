######### script to compile bbsBayes output into CWS web content
### author: Adam Smith
##########################
library(pacman)


p_load(char = c("bbsBayes","ggplot2","ggrepel","RColorBrewer","tidyverse"),character.only = T)


sapply(list.files(pattern="[.]R$", path="functions/", full.names=TRUE), source);


 dat_strat = stratify(by = "bbs_cws")

 
c_orng = brewer.pal(9,"Set1")[5]
c_red = brewer.pal(9,"Set1")[1]
c_blue = brewer.pal(9,"Set1")[2]
c_purp = brewer.pal(9,"Set1")[4]
c_green = brewer.pal(9,"Set1")[3]


speciestemp = c("Blackpoll Warbler","American Kestrel","Pacific Wren",
                "Bewick's Wren","LeConte's Sparrow","Horned Lark","Killdeer",
                "Bobolink","McCown's Longspur","Canada Warbler","Western Wood-Pewee",
                "Barn Swallow","Bank Swallow")

speciestemp2 = c("LeConte's Sparrow","Horned Lark",
                "Barn Swallow","Bank Swallow")

allspecies.eng = dat_strat$species_strat$english
allspecies.fre = dat_strat$species_strat$french
allspecies.num = dat_strat$species_strat$sp.bbs

allspecies.file = str_replace_all(str_replace_all(allspecies.eng,"[:punct:]",replacement = ""),
                                  "\\s",replacement = "_")


COSEWIC = T
if(COSEWIC){
rollTrend = "Trend"
}



# qs = c(0.025,0.05,0.95,0.975)
YYYY = 2018 ## BBS data version year
short_time = 10 #length of time for short-term trend


lastyear = read.csv("2017estimates/All BBS trends 2017 w reliab.csv",stringsAsFactors = F)
covs = lastyear[,c("sp","species","geo.area","trendtype","trendtime","startyear","reliab.cov")]

covs <- covs[which((covs$trendtime == "full") |
                     (covs$trendtype == "short-term" & covs$trendtime == "reduc")),]

oldregs = read.csv("old region names.csv",stringsAsFactors = F)

covs = merge(covs,oldregs,by = "geo.area")



# reliability category definitions ----------------------------------------

prec_cuts = c(abs(2*((0.7^(1/20))-1)),
              abs(2*((0.5^(1/20))-1)))*100 
names(prec_cuts) <- c("High","Medium")

cov_cuts = c(0.5,0.25)
names(cov_cuts) <- c("High","Medium")




# load the maps for CWS webmaps ---------------------------------------

WEBMAP = T

if(WEBMAP){
canmap <- rgdal::readOGR(dsn = system.file("maps",
                                                 package = "bbsBayes"),
                      layer = "BBS_CWS_strata",
                      verbose = FALSE)

canmap@data$country <- substr(canmap@data$ST_12,1,2)

canmap <- canmap[canmap$country == "CA",]


basmap = rgdal::readOGR(dsn = system.file("maps",
                                          package = "bbsBayes"),
                        layer = "BBS_USGS_strata",
                        verbose = FALSE)
basmap@data$country <- substr(basmap@data$ST_12,1,2)

basmap <- basmap[basmap$country == "CA",]
}

# Species loop ------------------------------------------------------------



for(ssi in which(allspecies.eng %in% speciestemp2)){
 
  ss = allspecies.eng[ssi]
  ss.f = allspecies.fre[ssi]
  ss.n = allspecies.num[ssi]
  ss.file = allspecies.file[ssi]
  
  noise = "heavy_tailed"
  rm(list = c("jags_mod","jags_data"))
  
  if(file.exists(paste0("model_results/",noise,"/",ss.file,"/jags_mod_full.RData"))){
  
    load(paste0("model_results/",noise,"/",ss.file,"/jags_mod_full.RData"))
  load(paste0("model_results/",noise,"/",ss.file,"/jags_data.RData"))
  
  strat = jags_mod$stratify_by
  
  

###
  #testing alternate regions
  # tmp = get_composite_regions(strata_type = jags_mod$stratify_by)
  # tmp$boreal = "Eastern"
  # tmp[which(tmp$Province_State %in% c("British Columbia",
  #                                     "Alberta",
  #                                     "Northwest Territories",
  #                                     "Saskatchewan",
  #                                     "Yukon",
  #                                     "Alaska")),"boreal"] = "Western"
  # indstmp = generate_regional_indices(jags_mod = jags_mod,
  #                           jags_data = jags_data,
  #                           #quantiles = qs,
  #                           alt_region_names = tmp,
  #                           regions = c("boreal","continental","national","stratum", "prov_state","bcr","bcr_by_country"),
  #                           startyear = min(1995,fy),
  #                           max_backcast = 5)
  # 
  # trs = generate_regional_trends(indices = indstmp,
  #                                Min_year = fy,
  #                                #quantiles = qs,
  #                                slope = T,
  #                                prob_decrease = c(0,25,30,50),
  #                                prob_increase = c(0,33,100))
  
  
  
  
  # Loop for short and long-term trends and indices -------------------------
  
  for(fy in c(1970,YYYY-short_time)){
    if(fy == 1970){trend_time = "Long-term"}else{trend_time = "Short-term"}
    plot_header = paste(ss,noise,trend_time,sep = "_")
    
  inds = generate_regional_indices(jags_mod = jags_mod,
                                   jags_data = jags_data,
                                   #quantiles = qs,
                                   regions = c("continental","national", "prov_state","bcr","stratum","bcr_by_country"),
                                   startyear = min(1995,fy),
                                   max_backcast = 5)
 
  
  inds2 = generate_regional_indices(jags_mod = jags_mod,
                                   jags_data = jags_data,
                                   #quantiles = qs,
                                   regions = c("continental","national", "prov_state","bcr","stratum","bcr_by_country"),
                                   startyear = min(1995,fy),
                                   max_backcast = 5,
                                   alternate_n = "n3")
  
  indst = inds$data_summary
  indst$Trend_Time = trend_time
  

  
  indst2 = inds2$data_summary
  indst2$Trend_Time = trend_time
  
  if(fy == 1970){

    indsout = indst
    rm("indst")
    

    indsout2 = indst2
    rm("indst2")
    
  }else{
 
    indsout = rbind(indsout,indst)
    rm("indst")
    

    indsout2 = rbind(indsout2,indst2)
    rm("indst2")
    
    write.csv(indsout,paste0("output/trends_indices/",paste(ss.file,noise,sep = "_")," annual indices.csv"),row.names = F)
  
    
    write.csv(indsout2,paste0("output/alternate_trends_indices/",paste(ss.file,noise,sep = "_"),"smooth annual indices.csv"),row.names = F)
  }
  
  
  
  trs = generate_regional_trends(indices = inds2,
                                 Min_year = fy,
                                 #quantiles = qs,
                                 slope = T,
                                 prob_decrease = c(0,25,30,50),
                                 prob_increase = c(0,33,100))
  
  trs2 = generate_regional_trends(indices = inds,
                                 Min_year = fy,
                                 #quantiles = qs,
                                 slope = T,
                                 prob_decrease = c(0,25,30,50),
                                 prob_increase = c(0,33,100))
  
  
  
  trst = trs
  trst$Trend_Time = trend_time
  trst$reliab.prec = trst$Trend_Q0.975-trst$Trend_Q0.025
  
  trst2 = trs2
  trst2$Trend_Time = trend_time
  trst2$reliab.prec = trst2$Trend_Q0.975-trst2$Trend_Q0.025
  
  

# insert last year's coverage estimates -----------------------------------

  covsp = covs[which(covs$sp == ss.n & covs$trendtype == tolower(trend_time)),]
trst = merge(trst,
             covsp[,c("new.area","reliab.cov")],
             by.x = "Region_alt",
             by.y = "new.area",
             all.x = T,sort = F)  

trst2 = merge(trst2,
             covsp[,c("new.area","reliab.cov")],
             by.x = "Region_alt",
             by.y = "new.area",
             all.x = T,sort = F)



# insert reliability categories---------------------------------------------------

trst$precision = reliab_func_prec(trst$reliab.prec)
trst$coverage = reliab_func_cov(trst$reliab.cov)

trst2$precision = reliab_func_prec(trst2$reliab.prec)
trst2$coverage = reliab_func_cov(trst2$reliab.cov)


# Identify trends to publish on CWS website -------------------------------



trst2$For_Web <- for_web_func(trst2)
trst$For_Web <- for_web_func(trst)

# Generate the web-maps --------------------------------------------------------
if(WEBMAP){
trst = generate_web_maps(trst)
}

###############################################################
###############################################################
###############################################################
## add the forweb column


# Write the species trend files -------------------------------------------



if(fy == 1970){
  trstout = trst
  rm("trst")
  
  trstout2 = trst2
  rm("trst2")
  
}else{
  trstout = rbind(trstout,trst)
  rm("trst")
  write.csv(trstout,paste0("output/trends_indices/",paste(ss.file,noise,sep = "_")," trends.csv"),row.names = F)
  
  trstout2 = rbind(trstout2,trst2)
  rm("trst2")
  write.csv(trstout2,paste0("output/alternate_trends_indices/",paste(ss.file,noise,sep = "_")," trends incl yeareffects.csv"),row.names = F)
}



# geofacet plots ----------------------------------------------------------



  pdf(paste0("output/geofacets_strata/",plot_header,"_geofacet_strata.pdf"),
      width = 11,
      height = 8.5)
  gf = geofacet_plot(indices_list = inds,
                     select = T,
                     stratify_by = "bbs_cws",
                     multiple = T,
                     trends = trs,
                     slope = F,
                     species = ss)
  print(gf)
  dev.off()
  
  pdf(paste0("output/geofacets_prov/",plot_header,"_geofacet_prov.pdf"),
      width = 11,
      height = 8.5)
  gf = geofacet_plot(indices_list = inds,
                     select = T,
                     stratify_by = "bbs_cws",
                     multiple = F,
                     trends = trs,
                     slope = F,
                     species = ss)
  print(gf)
  dev.off()
 

# index plots, all single pdf for a species and time-series ---------------

  

  ipp = plot_strata_indices(indices_list = inds,
                            min_year = min(1995,fy),
                            add_observed_means = F,
                            species = ss)
  
  pdf(paste0("output/Indices/",plot_header,"_Indices.pdf"),
      width = 8.5,
      height = 6)
  print(ipp)
  dev.off()
  
  
  
  ###### generate the require ggplot components to add to each ipp within a loop
  
  
  pdf(paste0("output/Indices_comparison/",plot_header,"_Indices_comparison.pdf"),
      width = 8.5,
      height = 6)
  
  indsdf = inds$data_summary
  indsdf2 = inds2$data_summary
  
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
    
    
    ttp = ipp[[i]]
    ## add mean counts, number of routes, trend, nnzero
    # transform count variables to mirror existing axis
    
    trtmp = trs[which(trs$Region_alt == treg),]
    
    trtmp2 = trs2[which(trs2$Region_alt == treg),]
    
    st_exc = unique(trtmp$Strata_excluded)
    if(st_exc != ""){
      if(nchar(st_exc) > 20){
        stx2 = unlist(strsplit(st_exc,split = " ; "))
        st_exc <- paste("Excluding",length(stx2),"strata")
      }else{
        st_exc <- paste("Excluding",st_exc)
      }}
    
    
     trlab = paste("Slope_Trend",round(signif(trtmp2$Slope_Trend,2),1),"%/yr","since",trtmp2$Start_year,st_exc,
                   round(signif(trtmp2$Slope_Trend_Q0.025,2),1),":",round(signif(trtmp2$Slope_Trend_Q0.975,2),1))
     trlab2 = paste("GAM_Trend",round(signif(trtmp$Trend,2),1),"%/yr","since",trtmp$Start_year,st_exc,
                    round(signif(trtmp$Trend_Q0.025,2),1),":",round(signif(trtmp$Trend_Q0.975,2),1))
     
    ulim = max(ttind$Index_q_0.975)
    ttind$prts.sc = (ttind$nrts/mean(ttind$nrts_total))*(ulim*0.5)
    ttmax = ttind[which(ttind$Year == YYYY),]
    ttmax$lbl = paste(ttmax$nrts,"routes in",YYYY,ttmax$nrts_total,"total")
    
    ttmin = ttind[which(ttind$Year == fy),]
    ttmin$lbl = "Observed means"
    
    np <- ttp + 
      geom_ribbon(data = ttind2,aes(x = Year, ymin = Index_q_0.025,ymax = Index_q_0.975),fill = c_orng,alpha = 0.1)+
      geom_line(data = ttind2,aes(x = Year, y = Index),colour = c_orng,alpha = 0.4)+
      geom_point(data = ttind,aes(x = Year, y = obs_mean),colour = c_blue,alpha = 0.3)+
      geom_col(data = ttind,aes(x = Year, y = prts.sc),width = 0.5,fill = c_green,alpha = 0.1)+
      geom_text_repel(data = ttmax,aes(x = Year, y = prts.sc,label = lbl),nudge_y = 0.1*ulim,colour = c_green,alpha = 0.5,size = 3)+
    geom_text_repel(data = ttmin,aes(x = Year, y = obs_mean,label = lbl),nudge_y = 0.1*ulim,colour = c_blue,alpha = 0.5,size = 3)+
    annotate("text",x = mean(c(fy,YYYY)),y = ulim*0.9,label = trlab)+
    annotate("text",x = mean(c(fy,YYYY)),y = ulim*0.8,label = trlab2, colour = c_orng)
    
    print(np)
    
  }
  dev.off()
  

# range wide trend maps --------------------------------------------------------------


  
  pdf(paste0("output/Trend_maps/",plot_header,"_trend_map.pdf"),
      width = 8.5,
      height = 6)
  pp = generate_map(trend = trs,select = T,stratify_by = strat,slope = F)
  print(pp)
  dev.off()
  
  
 
  


# COSEWIC output ----------------------------------------------------------
if(COSEWIC){
 
  
  fy2 = min(1995,fy)
  indscos = generate_regional_indices(jags_mod = jags_mod,
                                   jags_data = jags_data,
                                   #quantiles = qs,
                                   regions = c("continental","national","prov_state","bcr","stratum"),
                                   startyear = fy2,
                                   max_backcast = 5,
                                   alternate_n = "n3")

  
  for(ly2 in c((fy2+short_time):YYYY)){
  trst = generate_regional_trends(indices = indscos,
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
  
  pdf(paste0("output/Rolling_Trends/",plot_header,"_Rolling_Trends.pdf"),
      width = 8.5,
      height = 6)
  for(rg in unique(tcos$Region_alt)){
    
    tmp = tcos[which(tcos$Region_alt == rg),]
    
    st_exc = unique(tmp$Strata_excluded)
    if(st_exc != ""){
      if(nchar(st_exc) > 20){
        stx2 = unlist(strsplit(st_exc,split = " ; "))
        st_exc <- paste("Excluding",length(stx2),"strata")
      }else{
      st_exc <- paste("Excluding",st_exc)
    }}
    
    
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
 #} #noise
  
  }#if output exists
  
}#species loop

### read in the full files
### sort and re-name relevant columns to match webcontent exactly
### consider the strata names issue - must match last year's web content











