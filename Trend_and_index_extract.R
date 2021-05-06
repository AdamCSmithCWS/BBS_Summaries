###script to extract the CWS web content and regional estimates
## examples below to extract all prov_state estimates, all bcr estimates, etc.


library(tidyverse)

YYYY = 2019


# allspecies.eng = dat_strat$species_strat$english
# allspecies.fre = dat_strat$species_strat$french
# allspecies.num = dat_strat$species_strat$sp.bbs

pathstrats = system.file("composite-regions",
                         package = "bbsBayes")
strats = read.csv(paste0(pathstrats,"/stratcan.csv"))


all = read.csv(paste0("All ",YYYY," BBS trends.csv"))

 all$Strata_included <- gsub(x = all$Strata_included,pattern = " ; ",fixed = T,replacement = " - ")
 all$Strata_excluded <- gsub(x = all$Strata_excluded,pattern = " ; ",fixed = T,replacement = " - ")
 # 
 # write.csv(all,paste0("All ",YYYY," BBS trends.csv"),row.names = F)


web <- filter(all,For_Web == TRUE)

web$prob_decrease_0_25_percent = web$prob_decrease_0_percent - web$prob_decrease_25_percent 
web$prob_decrease_25_50_percent = web$prob_decrease_0_percent - (web$prob_decrease_0_25_percent + web$prob_decrease_50_percent) 
 # (web$prob_decrease_0_percent != (web$prob_decrease_0_25_percent + web$prob_decrease_25_50_percent + web$prob_decrease_50_percent ))

web$prob_increase_0_33_percent = web$prob_increase_0_percent - web$prob_increase_33_percent 
web$prob_increase_33_100_percent = web$prob_increase_0_percent - (web$prob_increase_0_33_percent + web$prob_increase_100_percent) 

web = web[which(web$Region_type != "bcr"),]
hdrsT = read.csv("2017estimates/web_headers_xwalk.csv")



clout = hdrsT$original
clnms = hdrsT$upload
web = web[,clout]


# create and save lists of web content strings ----------------------------

# trendload = read.csv(c("2017estimates/Trend_Load_Table_2019.csv"))

### region names and types
regionx = read.csv("2017estimates/regional_Xwalk.csv")

web = inner_join(web,regionx,by = c("Region","Region_type")) #drops the BCR-7 province rows
web$Region_type <- web$region.type
web$Region <- web$Geographic.area

stratax = unique(regionx[which(regionx$Region_type == "stratum"),c("Geographic.area","Region")])


stincmat_ex = web$Strata_excluded %>% str_split_fixed(pattern = " - ",n = nrow(stratax))
stincmat_in = web$Strata_included %>% str_split_fixed(pattern = " - ",n = nrow(stratax))

for(i in 1:nrow(stratax)){
        ii = stratax[i,"Region"]
        jj = stratax[i,"Geographic.area"]
        
stincmat_ex = gsub(x = stincmat_ex,pattern = ii,replacement = jj)
stincmat_in = gsub(x = stincmat_in,pattern = ii,replacement = jj)

}


for(j in 1:nrow(web)){
        tmp = stincmat_ex[j,]
        tmp = tmp[which(tmp != "")]
        web[j,"Strata_excluded"] <- paste(tmp,collapse = " - ")
        
        tmp2 = stincmat_in[j,]
        tmp2 = tmp2[which(tmp2 != "")]
        web[j,"Strata_included"] <- paste(tmp2,collapse = " - ")
        rm(list = c("tmp","tmp2"))
}

# fix significant digits --------------------------------------------------
numcols = which(unlist(lapply(web,class)) == "numeric")
for(j in numcols){
        web[,j] <- round(signif(web[,j],3),3)
}



# fix species names table -------------------------------------------------

spload = read.csv("2017estimates/Bird_Load_Table_constant.csv")



s1 = c("Dark-eyed Junco (all forms)",
       "Northern Flicker (all forms)",
       "Redpoll (Common/Hoary)",
       "Red-tailed Hawk (all forms)",
       "Sapsuckers (Yellow-bellied/Red-naped/Red-breasted/Williamson's)",
       "Yellow-rumped Warbler (all forms)",
       "McCown's Longspur")
s2 = c("Dark-eyed Junco",
       "Northern Flicker",
       "Unidentified Common/Hoary Redpoll",
       "Red-tailed Hawk",
       "Sapsuckers (Yellow-bellied/Red-naped/Red-breasted)",
       "Yellow-rumped Warbler",
       "Thick-billed Longspur")

# web = web[-which(web$species == "Western Grebe" &
#                          web$bbs_num == 10),]

for(i in 1:length(s1)){
web[which(web$species == s1[i]),"species"] <- s2[i]
}

splist = unique(web[,c("species","bbs_num")])

splist = left_join(splist,spload,by = c("species" = "English_Name",
                                        "bbs_num" = "BBS_Number"))

print(paste("merge will drop",splist[which(is.na(splist$Sort_Order)),"species"]))

splist = unique(web[,c("species","bbs_num")])

spload2 = inner_join(spload,splist,c("English_Name" = "species",
                                    "BBS_Number" = "bbs_num"))
miss_sp = spload$English_Name[-which(spload$English_Name %in% spload2$English_Name)]
 print(paste("no trend data for",miss_sp))
 

# retain only species in the web species list -----------------------------

web = inner_join(web,spload[,c("English_Name","BBS_Number")],by = c("species" = "English_Name",
                                                                    "bbs_num" = "BBS_Number")) 
 
 # fix trend_time names ----------------------------------------------------
 
 web$Trend_Time <- tolower(web$Trend_Time)
 
 
 
#replace names of web file
web = web[,clout]
names(web) = clnms


write.csv(web, paste0("Trend_Load_Table_",YYYY+1,".csv"),row.names = F)


# Annual Indices ----------------------------------------------------------
hdrsI = read.csv("2017estimates/web_headers_xwalk_indices.csv")
clouti = hdrsI$original
clnmsi = hdrsI$upload





alli = read.csv(paste0("All ",YYYY," BBS indices.csv"))



# alli$Strata_included <- gsub(x = alli$Strata_included,pattern = " ; ",fixed = T,replacement = " - ")
# alli$Strata_excluded <- gsub(x = alli$Strata_excluded,pattern = " ; ",fixed = T,replacement = " - ")
# 
# write.csv(alli,paste0("All ",YYYY," BBS indices.csv"),row.names = F)

webi <- filter(alli,For_Web == TRUE)

# webi = webi[-which(webi$species == "Western Grebe (Clark's/Western)" &
#                            webi$bbs_num == 12),]

for(i in 1:length(s1)){
        webi[which(webi$species == s1[i]),"species"] <- s2[i]
}



webi = inner_join(webi,spload[,c("English_Name","BBS_Number")],by = c("species" = "English_Name",
                                                                    "bbs_num" = "BBS_Number")) 




# fixing region names -----------------------------------------------------

webi = inner_join(webi,regionx,by = c("Region","Region_type")) #drops the BCR-7 province rows
webi$Region_type <- webi$region.type
webi$Region <- webi$Geographic.area


# fix significant digits --------------------------------------------------
numcols = which(unlist(lapply(webi,class)) == "numeric")
for(j in numcols){
        webi[,j] <- signif(webi[,j],3)
}


# fix trend_time names ----------------------------------------------------

webi$Trend_Time <- tolower(webi$Trend_Time)

webi = webi[,clouti]
names(webi) <- clnmsi


write.csv(webi, paste0("Annual_Load_Table_",YYYY+1,".csv"),row.names = F)



### checks

n_ind <- webi %>% distinct(species,`Geographic Area`,trendtime)
n_t <- web %>% distinct(`bbs number`,`Geographic area`,`Time Period Type`)

if(nrow(n_t) != nrow(n_ind)){stop("Something is missing from index or trend file")}



# END WEBSITE UPLOAD SUMMARY ----------------------------------------------


















# Export regional trends and indices ----------------------


tmp = filter(alli,Region_type %in% c("continental","national"))
write.csv(tmp,paste0(YYYY,"All BBS indices continent and national.csv"),row.names = F)

tmp = filter(alli,Region_type %in% c("prov_state"))
write.csv(tmp,paste0(YYYY,"All BBS indices prov_state.csv"),row.names = F)

tmp = filter(alli,Region_type %in% c("bcr_by_country","bcr"))
write.csv(tmp,paste0(YYYY,"All BBS indices full_bcr and bcr_by_country.csv"),row.names = F)

tmp = filter(alli,Region_type %in% c("stratum"),str_detect(Region,pattern = "^US"),
             Trend_Time == "Long-term")
write.csv(tmp,paste0(YYYY,"All BBS indices strata in USA.csv"),row.names = F)
tmp = filter(alli,Region_type %in% c("stratum"),str_detect(Region,pattern = "^CA"),
             Trend_Time == "Long-term")
write.csv(tmp,paste0(YYYY,"All BBS indices strata in Canada.csv"),row.names = F)


tmp = filter(all,Region_type %in% c("continental","national"))
write.csv(tmp,paste0(YYYY,"All BBS trends continent and national.csv"),row.names = F)

tmp = filter(all,Region_type %in% c("prov_state"))
write.csv(tmp,paste0(YYYY,"All BBS trends prov_state.csv"),row.names = F)

tmp = filter(all,Region_type %in% c("bcr_by_country","bcr"))
write.csv(tmp,paste0(YYYY,"All BBS trends full_bcr and bcr_by_country.csv"),row.names = F)

tmp = filter(all,Region_type %in% c("stratum"))
write.csv(tmp,paste0(YYYY,"All BBS trends stratum.csv"),row.names = F)

