### demo script to estimate trends for customized composite regions

library(bbsBayes)
library(tidyverse)


species = "Golden-winged Warbler"

## species string used to make folders (removes punctuation and spaces)
sp = str_replace_all(str_replace_all(species,"[:punct:]",replacement = ""),
                "\\s",replacement = "_")
## load modeling output from CWS analysis
load(paste0("output/",sp,"/","jags_mod_full.RData"))


st_comp_regions <- get_composite_regions(strata_type = "bbs_cws")


# overlay spatial object on strata ----------------------------------------
library(sf)
library(sp)
library(ggplot2)

laea = st_crs("+proj=laea +lat_0=40 +lon_0=-95") # Lambert equal area coord reference system

# strat_maps <- c("BBS_CWS_strata",
#                 "BBS_USGS_strata",
#                 "BBS_BCR_strata",
#                 "BBS_LatLong_strata",
#                 "BBS_ProvState_strata")
# 
# names(strat_maps) <- c("bbs_cws","bbs_usgs","bcr","latlong","state")


locat = system.file("maps",
                    package = "bbsBayes")

map.file = c("BBS_CWS_strata")
st_nm = c("bbs_cws")
strata_map = read_sf(dsn = locat,
                     layer = map.file)
strata_map = st_transform(strata_map,crs = laea)

custom_map = read_sf(dsn = paste0("output/",sp),
                     layer = "gwwa_new_range_28JUL11_final")
st_crs(custom_map)
custom_map = st_transform(custom_map,crs = laea)

custom_regs = st_join(x = strata_map,
                      y = custom_map,
                      join = st_intersects)

Appalach = custom_regs$ST_12[which(custom_regs$Id == 2)]
# pre-defined regions -----------------------------------------------------

st_comp_regions$GWWA_regions <- ifelse(st_comp_regions$region %in% Appalach,"Appalachian","Northern")
custom_indices <- generate_indices(jags_mod = jags_mod,
                                   jags_data = jags_data,
                                   alt_region_names = st_comp_regions,
                                   regions = "GWWA_regions")

custom_indices_smooth <- generate_indices(jags_mod = jags_mod,
                                   jags_data = jags_data,
                                   alt_region_names = st_comp_regions,
                                   regions = "GWWA_regions",
                                   alternate_n = "n3")

cont_indices <- generate_indices(jags_mod = jags_mod,
                                   jags_data = jags_data,
                                   regions = "continental")

cont_indices_smooth <- generate_indices(jags_mod = jags_mod,
                                          jags_data = jags_data,
                                        regions = "continental",
                                          alternate_n = "n3")

strat_indices_smooth <- generate_indices(jags_mod = jags_mod,
                                        jags_data = jags_data,
                                        regions = "stratum",
                                        alternate_n = "n3")
strat_trends = generate_trends(strat_indices_smooth)

y1s = c(1966,1966,1989,1999,2009)
y2s = c(2019,1989,2019,2019,2019)

for(i in 1:length(y1s)){
  y1 = y1s[i]
  y2 = y2s[i]
  
 tmp = generate_trends(indices = custom_indices_smooth,
                                Min_year = y1,
                                Max_year = y2)
  
  if(i == 1){
    custom_trends = tmp
  }else{
    custom_trends = bind_rows(custom_trends,tmp)
  }
  rm(list = "tmp")
 tmp = generate_trends(indices = cont_indices_smooth,
                       Min_year = y1,
                       Max_year = y2)
 
 custom_trends = bind_rows(custom_trends,tmp)
 
}


tp = plot_indices(indices = custom_indices,
                  #species = species,
                  add_observed_means = FALSE,
                  add_number_routes = FALSE,
                  axis_title_size = 12,
                  axis_text_size = 14)
tpc = plot_indices(indices = cont_indices,
                   #species = species,
                   add_observed_means = FALSE,
                   add_number_routes = FALSE,
                   axis_title_size = 12,
                   axis_text_size = 14)


# plot each with smooth overlay and proportional area ---------------------
wts = custom_indices_smooth$area_weights
wts$regs_custom <- ifelse(wts$region %in% Appalach,"Appalachian","Northern")
wt = round(tapply(wts$area_sq_km,wts$regs_custom,sum)/sum(wts$area_sq_km),2)*100
## wt is the percent of the range in each region - area weights for combining the mean counts

map_palette <- c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffbf",
                 "#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695")

wc = 1#which colour in map_palette to use for the trajectory overplot

tmap = generate_map(strat_trends,stratify_by = "bbs_cws",species = "Golden-winged Warbler")

library(patchwork)


  rr = "Continental"
  wtt = paste0(abs(round(custom_trends[which(custom_trends$Start_year == 1966 &
                             custom_trends$End_year == 2019 &
                             custom_trends$Region == rr),"Percent_Change"],0)),
             "% Decline since 1966")
  inds = cont_indices_smooth$data_summary
  nnz = data.frame(Year = rep(inds$Year, times = floor(inds$nnzero/5)))
  
  tmp = tpc[[1]]+
    #geom_line(data = inds,aes(x = Year,y = Index), colour = map_palette[wc]) + 
    annotate(geom = "text",x = 2019,y = 1.35,
             label = wtt,
             size = 5,hjust = "inward")+
  geom_dotplot(data = nnz,mapping = aes(x = Year),drop = TRUE,
               binaxis = "x", stackdir = "up",method = "histodot",
               binwidth = 1,width = 0.2,inherit.aes = FALSE,fill = grey(0.6),
               colour = grey(0.4),alpha = 0.2,dotsize = 0.4)
  
  
  rr = "Appalachian"

    wtt = paste0(abs(round(custom_trends[which(custom_trends$Start_year == 1966 &
                                                        custom_trends$End_year == 2019 &
                                                        custom_trends$Region == rr),"Percent_Change"],0)),
                        "% Decline : ",wt[rr],"% of range")
    inds = filter(custom_indices_smooth$data_summary,Region == rr)
    nnz = data.frame(Year = rep(inds$Year, times = floor(inds$nnzero/5)))
    
    tmp1 = tp[[2]]+
      #geom_line(data = inds,aes(x = Year,y = Index), colour = map_palette[wc]) + 
      annotate(geom = "text",x = 2019,y = 1.35,
               label = wtt,
               size = 5,hjust = "inward") +
      geom_dotplot(data = nnz,mapping = aes(x = Year),drop = TRUE,
                   binaxis = "x", stackdir = "up",method = "histodot",
                   binwidth = 1,width = 0.2,inherit.aes = FALSE,fill = grey(0.6),
                   colour = grey(0.4),alpha = 0.2,dotsize = 0.4)
    
    #print(tmp)
    
    rr = "Northern"
    
    wtt = paste0(abs(round(custom_trends[which(custom_trends$Start_year == 1966 &
                                                 custom_trends$End_year == 2019 &
                                                 custom_trends$Region == rr),"Percent_Change"],0)),
                 "% Decline : ",wt[rr],"% of range")
    
    inds = filter(custom_indices_smooth$data_summary,Region == rr)
    nnz = data.frame(Year = rep(inds$Year, times = floor(inds$nnzero/5)))
    
    tmp2 = tp[[1]]+
      #geom_line(data = inds,aes(x = Year,y = Index), colour = map_palette[wc]) + 
      annotate(geom = "text",x = 2019,y = 1.35,
               label = wtt,
               size = 5,hjust = "inward") +
      geom_dotplot(data = nnz,mapping = aes(x = Year),drop = TRUE,
                   binaxis = "x", stackdir = "up",method = "histodot",
                   binwidth = 1,width = 0.2,inherit.aes = FALSE,fill = grey(0.6),
                   colour = grey(0.4),alpha = 0.2,dotsize = 0.4)
    
 
    pdf(paste0("additional_output/",species," trajectories.pdf"),
        height = 8.5,
        width = 11)
    print((tmap+tmp)/(tmp2+tmp1))
    
dev.off()
