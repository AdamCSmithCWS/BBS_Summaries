###script to extract the CWS web content and regional estimates
## examples below to extract all prov_state estimates, all bcr estimates, etc.


library(tidyverse)

YYYY = 2019


allspecies.eng = dat_strat$species_strat$english
allspecies.fre = dat_strat$species_strat$french
allspecies.num = dat_strat$species_strat$sp.bbs



all = read.csv(paste0("All ",YYYY," BBS trends.csv"))

 # all$Strata_included <- gsub(x = all$Strata_included,pattern = " ; ",fixed = T,replacement = " ")
 # all$Strata_excluded <- gsub(x = all$Strata_excluded,pattern = " ; ",fixed = T,replacement = " ")
 # 
 # write.csv(all,paste0("All ",YYYY," BBS trends.csv"),row.names = F)


web <- filter(all,For_Web == TRUE)

web$prob_decrease_0_25_percent = web$prob_decrease_0_percent - web$prob_decrease_25_percent 
web$prob_decrease_25_50_percent = web$prob_decrease_0_percent - (web$prob_decrease_0_25_percent + web$prob_decrease_50_percent) 
 # (web$prob_decrease_0_percent != (web$prob_decrease_0_25_percent + web$prob_decrease_25_50_percent + web$prob_decrease_50_percent ))

web$prob_increase_0_33_percent = web$prob_increase_0_percent - web$prob_increase_33_percent 
web$prob_increase_33_100_percent = web$prob_increase_0_percent - (web$prob_increase_0_33_percent + web$prob_increase_100_percent) 

clout = c("bbs_num",
          "species",
          "espece",
          "Region_alt",
          "Trend_Time",
          "Start_year",
          "End_year",
          "Trend",
          "Trend_Q0.025",
          "Trend_Q0.975",
          "Reliability",
          "Width_of_95_percent_Credible_Interval",
          "reliab.cov",
          "backcast_flag",
          "prob_decrease_0_percent",
          "prob_increase_0_percent",
          "prob_decrease_50_percent",
          "prob_decrease_25_50_percent",
          "prob_decrease_0_25_percent",
          "prob_increase_0_33_percent",
          "prob_increase_33_100_percent",
          "prob_increase_100_percent",
          "Percent_Change",
          "Percent_Change_Q0.025",
          "Percent_Change_Q0.975",
          "Number_of_Routes",
          "Strata_included",
          "Strata_excluded",
          "mapfile")

clnms = c("sp","species","espece","geo.area","trendtype",
          "startyear","endyear","trend",
          "llimit","ulimit","reliab.over",
          "reliab.prec","reliab.cov","reliab.pool",
          "p.decrease","p.increase","p.d50","pd50.25",
          "pd25.0","pi0.33","pi33.100","pi100",
          "percent.change","percent.change.llimit",
          "percent.change.ulimit","nroutesduringtrend",
          "strata.inc","st.excl.long","mapfile")

web = web[,clout]
names(web) = clnms




write.csv(web, paste0(YYYY," BBS trends for CWS website.csv"),row.names = F)


# Annual Indices ----------------------------------------------------------





alli = read.csv(paste0("All ",YYYY," BBS indices.csv"))

# alli$Strata_included <- gsub(x = alli$Strata_included,pattern = " ; ",fixed = T,replacement = " ")
# alli$Strata_excluded <- gsub(x = alli$Strata_excluded,pattern = " ; ",fixed = T,replacement = " ")
# 
# write.csv(alli,paste0("All ",YYYY," BBS indices.csv"),row.names = F)

webi <- filter(alli,For_Web == TRUE)

clouti =  c("bbs_num",
                   "species",
                   "espece",
                   "Region_alt",
                   "Trend_Time",
                   "Year",
                   "Index",
                   "Index_q_0.025",
                   "Index_q_0.975")
clnmsi = c("sp","species","espece","geo.area","trendtype",
           "year","an.index",
           "llimit","ulimit")

webi = webi[,clouti]
names(webi) <- clnmsi


write.csv(webi, paste0(YYYY," BBS indices for CWS website.csv"),row.names = F)



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

