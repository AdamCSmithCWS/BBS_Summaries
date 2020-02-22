
generate_web_maps <- function(df,
                              sp = ss.n){
  
  dfstrata = df[which(df$Region_type == "stratum"),]
  for(j in 1:nrow(df)){
    if(df[j,"For_Web"]){
      mapname = paste(sp,df[j,"Region_alt"],df[j,"Trend_Time"],"map.png")
      df[j,"mapfile"] <- mapname
      
      sts = unlist(strsplit(df[j,"Strata_included"],split = " ; "))
     
      mapo = map_f(st = dfstrata,
                   stinc = sts,
                   map = canmap)
      png(filename = paste0("WebMaps/",mapname),
          bg = "white",width = 480, height = 320)
      print(mapo)
      dev.off()
      
      
      
    }
  }
  
  
  
  return(df)
  
}

map_f <- function(st, #dataframe of trends
                  map = canmap,
                  slope = F,
                  stinc = sts){
  
  
  stplot = st[which(st$Region %in% c(stinc)),]

  breaks <- c(-7, -4, -2, -1, -0.5, 0.5, 1, 2, 4, 7,10000)
  labls = c(paste0("< ",breaks[1]),paste0(breaks[-c(length(breaks)-1,length(breaks))],":", breaks[-c(1,length(breaks))]),paste0("> ",breaks[length(breaks)-1]))
  labls = paste0(labls, " %")
  labls[length(labls)+1] <- "X"
  map@data$row_num <- 1:nrow(map@data)
  map@data <- merge(map@data, stplot, by.x = "ST_12", by.y = "Region", all.x = T,sort = F)
  map@data <- map@data[order(map@data$row_num), ]
  if(slope){
    map@data$Trend <- as.numeric(as.character(map@data$Slope_Trend))
  }else{
    map@data$Trend <- as.numeric(as.character(map@data$Trend))
  }
  
  map@data$Trend <- cut(map@data$Trend, breaks = c(-Inf, breaks,Inf),
                        labels = labls,
                        ordered_result = T)
  
  map@data <- subset(map@data, select = c(Trend))
  
  map_palette <- c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffbf",
                   "#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695",grey(0.8))
  names(map_palette) <- labls
  
  return(
    sp::spplot(map, col.regions = map_palette,
               edge.col = grey(0.5),
               sp.layout = list(basmap, edge.col = grey(0.5),
                                fill="transparent", first=FALSE),
               par.settings = list(axis.line = list(col = 'transparent')))
  )
}
