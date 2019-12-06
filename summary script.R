######### script to compile bbsBayes output into CWS web content
### author: Adam Smith
##########################
library(pacman)


p_load(char = c("bbsBayes","ggplot2","ggrepel","RColorBrewer"),character.only = T)


 dat_strat = stratify(by = "bbs_cws")
# 
# 
# species_to_run = unique(dat_strat$species_strat$english)

c_orng = brewer.pal(9,"Set1")[5]
c_red = brewer.pal(9,"Set1")[1]
c_blue = brewer.pal(9,"Set1")[2]
c_purp = brewer.pal(9,"Set1")[4]
c_green = brewer.pal(9,"Set1")[3]


speciestemp = c("Blackpoll Warbler","American Kestrel","Pacific Wren","Bewick's Wren","LeConte's Sparrow","Horned Lark","Killdeer","Bobolink","McCown's Longspur","Canada Warbler","Western Wood-Pewee")
#speciestemp = c("Horned Lark","Killdeer","Pacific Wren","Bewick's Wren","LeConte's Sparrow")
rollTrend = "Trend"

# qs = c(0.025,0.05,0.95,0.975)
YYYY = 2018
short_time = 10 #length of time for short-term trend


for(ss in speciestemp){
  #if(ss %in% speciestemp[1:2]){noise = "normal_tailed"}else{noise = "heavy_tailed"}
 #for(noise in "normal_tailed"){#c("heavy_tailed","normal_tailed")){ 
  noise = "heavy_tailed"
  rm(list = c("jags_mod","jags_data"))
  
  load(paste0("model_results/",noise,"/",ss,"/jags_mod_full.RData"))
  load(paste0("model_results/",noise,"/",ss,"/jags_data.RData"))
  
  strat = jags_mod$stratify_by
  
  
  ###############################################################
  ###############################################################
  ###############################################################
  #### loop for short-term and long-term indices and trends
  
  
  for(fy in c(1970,YYYY-short_time)){
    if(fy == 1970){trend_time = "Long-term"}else{trend_time = "Short-term"}
    plot_header = paste(ss,noise,trend_time,sep = "_")
    
  inds = generate_regional_indices(jags_mod = jags_mod,
                                   jags_data = jags_data,
                                   #quantiles = qs,
                                   regions = c("continental","national", "prov_state","bcr","stratum"),
                                   startyear = min(1995,fy),
                                   max_backcast = 5)
 
  
  inds2 = generate_regional_indices(jags_mod = jags_mod,
                                   jags_data = jags_data,
                                   #quantiles = qs,
                                   regions = c("continental","national", "prov_state","bcr","stratum"),
                                   startyear = min(1995,fy),
                                   max_backcast = 5,
                                   alternate_n = "n3")
  

  if(fy == 1970){
    indst = inds$data_summary
    indst$Trend_Time = "Long-term"
    indsout = indst
    rm("indst")
    
    indst2 = inds2$data_summary
    indst2$Trend_Time = "Long-term"
    indsout2 = indst2
    rm("indst2")
    
  }else{
    indst = inds$data_summary
    indst$Trend_Time = "Short-term"
    indsout = rbind(indsout,indst)
    rm("indst")
    
    indst2 = inds2$data_summary
    indst2$Trend_Time = "Short-term"
    indsout2 = rbind(indsout2,indst2)
    rm("indst2")
    
    write.csv(indsout,paste0("output/trends_indices/",paste(ss,noise,sep = "_")," annual indices.csv"),row.names = F)
  
    
    write.csv(indsout,paste0("output/trends_indices_smooth/",paste(ss,noise,sep = "_"),"smooth annual indices.csv"),row.names = F)
  }
  
  
  
  trs = generate_regional_trends(indices = inds2,
                                 Min_year = fy,
                                 #quantiles = qs,
                                 slope = T,
                                 prob_decrease = c(0,25,30,50),
                                 prob_increase = c(0,33,100))
  
  if(fy == 1970){
    trst = trs
    trst$Trend_Time = "Long-term"
    trstout = trst
    rm("trst")
  }else{
    trst = trs
    trst$Trend_Time = "Short-term"
    trstout = rbind(trstout,trst)
    rm("trst")
    write.csv(trstout,paste0("output/trends_indices/",paste(ss,noise,sep = "_")," trends.csv"),row.names = F)
  }
  
  ###############################################################
  ###############################################################
  ###############################################################
  ### geofacet plots
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
  
  pdf(paste0("output/geofacets_prov/",plot_header,"_geofacet_strata.pdf"),
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
 
  ###############################################################
  ###############################################################
  ###############################################################
  ### index plots, all single pdf for a species and time-series
  
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
    st_exc = unique(trtmp$Strata_excluded)
    if(st_exc != ""){
      if(nchar(st_exc) > 20){
        stx2 = unlist(strsplit(st_exc,split = " ; "))
        st_exc <- paste("Excluding",length(stx2),"strata")
      }else{
        st_exc <- paste("Excluding",st_exc)
      }}
    
    
     trlab = paste("Slope_Trend",round(signif(trtmp$Slope_Trend,2),1),"%/yr","since",trtmp$Start_year,st_exc)
     trlab2 = paste("GAM_Trend",round(signif(trtmp$Trend,2),1),"%/yr","since",trtmp$Start_year,st_exc)
     
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
  
  ###############################################################
  ###############################################################
  ###############################################################
  ### trend maps
  
  pdf(paste0("output/Trend_maps/",plot_header,"_trend_map.pdf"),
      width = 8.5,
      height = 6)
  pp = generate_map(trend = trs,select = T,stratify_by = strat,slope = F)
  print(pp)
  dev.off()
  
  
 
  
  ###############################################################
  ###############################################################
  ###############################################################
  ### bring in last year's coverage calculation
  
  
  
  ###############################################################
  ###############################################################
  ###############################################################
  ### COSEWIC output
  
  fy2 = min(1995,fy)
  indscos = generate_regional_indices(jags_mod = jags_mod,
                                   jags_data = jags_data,
                                   #quantiles = qs,
                                   regions = c("continental","national","prov_state"),
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
  if(ly2 == fy2+10){
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
  
  
  
  ###############################################################
  ###############################################################
  ###############################################################
  ## add the reliability categories
  
  
  ### append results to previous output
  
}#short and long-term
 #} #noise
}

### read in the full files
### sort and re-name relevant columns to match webcontent exactly
### consider the strata names issue - must match last year's web content











