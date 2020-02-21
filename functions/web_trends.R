for_web_func <- function(df){
  y = rep(FALSE,nrow(df))
  for(j in 1:nrow(df)){
    if(df[j,"Region_type"] == "continental"){next}
    if(df[j,"Trend_Time"] == "alternate"){next}
    sts = unlist(strsplit(df[j,"Strata_included"],split = " ; "))
    if(any(stringr::str_starts(sts,pattern = "CA-"))){
      y[j] <- TRUE
    }
  }
  return(y)
}
