i_startv = function(x){
  set_values = as.data.frame(apply(x[,c(6,7,8)],2,function(x){as.numeric(x)}))
  values = c(0)
  v_i002 = which(!(set_values[,1] %in% values) == TRUE & 
    !(set_values[,2] %in% values) == FALSE &
    !(set_values[,3] %in% values) == FALSE)
  v_i010 = which(!(set_values[,1] %in% values) == FALSE & 
    !(set_values[,2] %in% values) == TRUE &
    !(set_values[,3] %in% values) == FALSE)
  v_i020 = which(!(set_values[,1] %in% values) == FALSE & 
    !(set_values[,2] %in% values) == FALSE &
    !(set_values[,3] %in% values) == TRUE)
  v_i102 = which(!(set_values[,1] %in% values) == TRUE & 
    !(set_values[,2] %in% values) == TRUE &
    !(set_values[,3] %in% values) == FALSE)
  v_i120 = which(!(set_values[,1] %in% values) == FALSE & 
    !(set_values[,2] %in% values) == TRUE &
    !(set_values[,3] %in% values) == TRUE)
  v <<- c(v1 = ifelse(length(v_i002) == 0,length(v_i010),length(v_i002)),
  v2 = ifelse(length(v_i102) == 0,length(v_i120),length(v_i102)),
  v3 = ifelse(length(v_i020) == 0,length(v_i010),length(v_i020)))
  v_i002
  v_i010
  v_i020
  v_i102
  v_i120
}
