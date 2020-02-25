###script to compile the web content and the full species info.


library(tidyverse)
library(bbsBayes)

dat_strat = stratify(by = "bbs_cws")



allspecies.eng = dat_strat$species_strat$english
allspecies.fre = dat_strat$species_strat$french
allspecies.num = dat_strat$species_strat$sp.bbs

allspecies.file = str_replace_all(str_replace_all(allspecies.eng,"[:punct:]",replacement = ""),
                                  "\\s",replacement = "_")



external_drive <- TRUE
if(external_drive){
  in_file <- paste0("f:/BBS_Summaries/estimates/trends_indices/")
}else{
  in_file <- paste0("estimates/trends_indices/")
  
}

tfiles = list.files(pattern="trends[.]csv$", path=in_file, full.names=TRUE)

       
all = NULL
for(ff in tfiles){
  tmp = read.csv(ff,stringsAsFactors = F)
  
  all = bind_rows(all,tmp)
  
}

write.csv(all, paste0(in_file,"All 2018 BBS trends.csv"),row.names = F)


ifiles = list.files(pattern="indices[.]csv$", path=in_file, full.names=TRUE)


alli = NULL
for(ff in ifiles){
  tmp = read.csv(ff,stringsAsFactors = F)
  
  alli = bind_rows(alli,tmp)
  
}


write.csv(alli, paste0(in_file,"All 2018 BBS indices.csv"),row.names = F)






# exploring the backcast reliability values -------------------------------



# exporting hte aerial insectivore, and raptor annual indices for the AI meeting.

splist = read.csv("C:/Estimating_Change_in_NorthAmerican_Birds/Rosenberg et al species list.csv")


spai = splist[which(splist$AI == "AI"),"species"]

indai = NULL
for(ssp in which(allspecies.eng %in% spai)){
  sp.e = allspecies.eng[ssp]
  sp.f = allspecies.fre[ssp]
  sp.n = allspecies.num[ssp]
  spf = allspecies.file[ssp]
  

  tmp = read.csv(paste0(in_file,spf," annual indices.csv"),stringsAsFactors = F)
  tmp$species = sp.e
  tmp$espece = sp.f
  tmp$bbs_num = sp.n
  
  
  indai = bind_rows(indai,tmp)
  
}
write.csv(indai,"AI annual indices BBS 2018.csv",row.names = F)


