isced_counter = function(x){
  set_values = as.data.frame(apply(x[,c(6,7,8)],2,function(x){as.numeric(x)}))
  values = c(0)
  # ISCED counter --------------------------------
  i_counter = unlist(apply(set_values,2,function(x){sum(x) != 0}))
}