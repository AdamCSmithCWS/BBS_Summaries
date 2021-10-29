
### custom simple trend estimates for specific time-periods and species

library(tidyverse)
library(bbsBayes)



strat_data = stratify(by = "bbs_cws")



allspecies.eng = strat_data$species_strat$english #database english names
allspecies.fre = strat_data$species_strat$french #database french names
allspecies.num = strat_data$species_strat$sp.bbs #databaset species numbers

# these are species file names with no special characters
allspecies.file = str_replace_all(str_replace_all(allspecies.eng,"[:punct:]",replacement = ""),
                                  "\\s",replacement = "_")



# start and end years 
Y_start = c(1970)
Y_end = c(2012) #population goal definitions based on trends up to 2012

region_types = c("national") #types of regions to calculate
      # options include "continental","stratum", "national", "prov_state", "bcr", and "bcr_by_country" 

regions_keep = c("Canada")
      # selection of specific regions of the region_types above

stored_files = "C:/BBS_Summaries/output/" #modify to identify folder that holds species specific output folders



trends = NULL
species_inc = allspecies.eng


for(sp in species_inc[36:length(species_inc)]){
  

  
  y1 = Y_start
  y2 = Y_end
  
    if(sp == ""){next}
    ws = which(allspecies.eng == sp)

# following loop just corrects species names for some common miss-matches ---------------------------------------
if(length(ws) == 0){
  sp_fl_2 = str_replace_all(str_replace_all(sp,"[:punct:]",replacement = ""),
                  "\\s",replacement = "_")
  
  ws = which(allspecies.file == sp_fl_2)
  
  if(length(ws) == 0){
   sp_fl_2 = paste0(sp_fl_2,"_all_forms") # relevant for YEWA, RTHA, DEJU etc.
   ws = which(allspecies.file == sp_fl_2)
   
  }
}
    if(length(ws) == 0){    stop("ERROR species name not recognized")}
    
  sp_fl = allspecies.file[ws] 
  spo = allspecies.eng[ws]
    

# Loads saved model output ------------------------------------------------
  if(file.exists(paste0(stored_files,sp_fl,"/jags_mod_full.RData"))){
   load(paste0(stored_files,sp_fl,"/jags_mod_full.RData"))
    
    inds = generate_indices(jags_mod = jags_mod,
                     jags_data = jags_data,
                     regions = region_types,
                     quantiles = c(0.025,0.5,0.975),
                     alternate_n = "n3",
                     startyear = y1)
    
    trs = generate_trends(indices = inds,
                          quantiles = c(0.025,0.5,0.975),
                          Min_year = y1,
                          Max_year = y2)
    trs$species <- spo # adds species name to file
    
    trends = bind_rows(trends,trs)
    # similar thing could be done to save annual indices
    
  }
  
  }



trends <- trends %>% filter(Region_alt %in% regions_keep)


write.csv(trends,paste("output/trends",Sys.Date(),".csv"))


