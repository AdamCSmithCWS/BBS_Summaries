reliab_func_prec <- function(x){
  y = rep("Low",length(x))
  y[which(x <= prec_cuts["Medium"])] <- "Medium"
  y[which(x < prec_cuts["High"])] <- "High"
  return(y)
}

reliab_func_cov <- function(x){
  y = rep("Low",length(x))
  y[which(x >= cov_cuts["Medium"])] <- "Medium"
  y[which(x > cov_cuts["High"])] <- "High"
  return(y)
}

