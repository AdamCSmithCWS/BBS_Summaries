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

st_comp_regions$GWWA_regions <- ifelse(st_comp_regions$region %in% Appalach,"Appalachian","Other")
custom_indices <- generate_indices(jags_mod = jags_mod,
                                   jags_data = jags_data,
                                   alt_region_names = st_comp_regions,
                                   regions = "GWWA_regions")


tp = plot_indices(indices = custom_indices,
                  species = species,
                  add_observed_means = TRUE,
                  add_number_routes = TRUE)









