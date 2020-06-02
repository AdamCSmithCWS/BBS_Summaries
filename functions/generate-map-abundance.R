generate_map_abundance <- function(abundances = NULL,
                                   annual = FALSE,
                                   select = FALSE,
                                   stratify_by = NULL,
                                   observed = FALSE,
                                   species = "",
                                   map = map)
{
  # Silly things to remove "visible binding" note in check
  Trend <- NULL
  rm(Trend)
  Aplot <- NULL
  rm(Aplot)
  

  
  if (is.null(stratify_by))
  {
    stop("Argument stratify_by is empty."); return(NULL)
  }
  
  if (is.null(abundances))
  {
    stop("Argument abundances is empty"); return(NULL)
  }
  
  if(annual)
  {
    abundances <- abundances$data_summary
  }
  
  if(select)
  {
    abundances = abundances[which(abundances$Region_type == "stratum"),]
  }
  
  
  
  
  
  
  
  # map <- sf::read_sf(dsn = system.file("maps",
  #                                      package = "bbsBayes"),
  #                    layer = maps[[stratify_by]],
  #                    quiet = TRUE)
  
  
  if(observed){
    if(annual){
      fyr = min(abundances$Year)
      lyr = max(abundances$Year)
      abundances$Aplot <- as.numeric(as.character(abundances$obs_mean))
    }else{
      fyr = min(abundances$Start_year)
      lyr = min(abundances$End_year)
      abundances$Aplot <- as.numeric(as.character(abundances$Observed_Relative_Abundance))
    }
    leg_head <- "Observed Mean Counts"
  }else{
    if(annual){
      fyr = min(abundances$Year)
      lyr = max(abundances$Year)
      abundances$Aplot <- as.numeric(as.character(abundances$Index))
    }else{
      fyr = min(abundances$Start_year)
      lyr = min(abundances$End_year)
      abundances$Aplot <- as.numeric(as.character(abundances$Relative_Abundance))
    }
    leg_head <- "Modeled Mean Counts"
  }
  
  qqs <- c(0,stats::quantile(abundances$Aplot,probs = seq(0,1,length.out = 10)[-1]))
  breaks <- qqs
  
  breaks[length(breaks)] <- ceiling(breaks[length(breaks)])
  breaks <- signif(breaks,2)
  breaks <- unique(breaks)
  
  labls <- c(paste0(breaks[-c(length(breaks))],":", breaks[-c(1)]))
  labls <- paste0(labls)
  
  map_palette <- RColorBrewer::brewer.pal(length(labls),"YlOrRd")#c("#EDF8E9","#C7E9C0","#A1D99B","#74C476","#31A354","#006D2C")[1:length(labls)] #this is RColorBrewer::brewer.pal(6,"Greens")
  names(map_palette) <- labls
  
  abundances$Aplot <- cut(abundances$Aplot,breaks = c(breaks),labels = labls)
  
  
  abundances$ST_12 = abundances$Region
  
 if(annual){ 
  expmap = expand.grid(ST_12 = map$ST_12,
                       Year = fyr:lyr)
  map2 = dplyr::left_join(x = map,y = expmap,by = "ST_12")
  map2 = dplyr::left_join(x = map2,y = abundances,by = c("ST_12","Year"))
 }else{
   map2 = dplyr::left_join(x = map,y = abundances,by = "ST_12")
   
 }  
  if(species != ""){
    ptit = paste(species,leg_head)
  }else{
    ptit = leg_head
  }
  
  if(annual){  
  mp.plot = ggplot2::ggplot()+
    ggplot2::geom_sf(data = map2,ggplot2::aes(fill = Aplot,group = Year),colour = grey(0.7),size = 0.1)+
    ggplot2::theme_minimal()+
    ggplot2::ylab("")+
    ggplot2::xlab("")+
    #ggplot2::labs(title = ptit)+
    ggplot2::theme(legend.position = "right", line = ggplot2::element_line(size = 0.4),
                   rect = ggplot2::element_rect(size = 0.1),
                   axis.text = ggplot2::element_blank(),
                   axis.line = ggplot2::element_blank())+
    ggplot2::scale_colour_manual(values = map_palette, aesthetics = c("fill"),
                                 guide = ggplot2::guide_legend(reverse=TRUE),
                                 name = paste0(leg_head,"\n",fyr,"-",lyr))+
    gganimate::transition_time(Year)+
      ggplot2::labs(title = paste(ptit," : {frame_time}"))
  }else{
    mp.plot = ggplot2::ggplot()+
      ggplot2::geom_sf(data = map2,ggplot2::aes(fill = Aplot),colour = grey(0.7),size = 0.1)+
      ggplot2::theme_minimal()+
      ggplot2::ylab("")+
      ggplot2::xlab("")+
      ggplot2::labs(title = paste0(ptit,":",fyr,"-",lyr))+
      ggplot2::theme(legend.position = "right", line = ggplot2::element_line(size = 0.4),
                     rect = ggplot2::element_rect(size = 0.1),
                     axis.text = ggplot2::element_blank(),
                     axis.line = ggplot2::element_blank())+
      ggplot2::scale_colour_manual(values = map_palette, aesthetics = c("fill"),
                                   guide = ggplot2::guide_legend(reverse=TRUE),
                                   name = paste0(leg_head,"\n",fyr,"-",lyr))
  }
  
  return(mp.plot)
}