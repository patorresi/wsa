#' Check warnings 
#'
#' This function check for possible errors in the data.frame.
#' @param df The data frame gathered will utilized the uploaded data frame.
#' @keywords warnings
#' @export
#' @examples
#' check_df()

check_df = function(df){
################################################################################
# Warnings.
################################################################################

# ISCED level 02 values
i02v = sum(as.numeric(df[,6])) > 0
# ISCED level 1 values
i10v = sum(as.numeric(df[,7])) > 0
# ISCED level 2 values 
i20v = sum(as.numeric(df[,8])) > 0


w0 = length(which(df[,1] == 0, df[,4] == 0 | df[,5] == 0)) == 0 


# only one leader
w1 = if(i02v == TRUE){
  sum(as.numeric(df[,9])) == 1
}else{
  sum(as.numeric(df[,9])) == 0
}

# staff role 
w2 = if(i02v == TRUE){
  sum(df[,10] %in% c(1,2,3,4,5,6,7,8,9,11,12)) == nrow(df)
}else{
  sum(df[,10] %in% c(1,2,3,4,5,6,7,8,9,11,12)) == 0
}


fun1 = function(x,y){
  if(x == 1){
     y != 0
  }else{
     y == 0
  }
}

# domain isced level 1
w3 = sum(mapply(fun1,df[,7],df[,11])) == nrow(df)

#if(i02v == TRUE){
#  sum(df[,11] %in% c(1:4,9)) == nrow(df)
#}else{
#  sum(df[,11] %in% c(1:4,9)) == 0
#}

# domain isced level 2
w4 = sum(mapply(fun1,df[,8],df[,12])) == nrow(df)

# all teacher listed need to have at least one participation in one isced level
w5 = sum(apply(df[,c(6,7,8)],1,FUN=function(x){sum(as.numeric(x))}) >= 1) == nrow(df)

w6 = (i02v + i10v + i20v)<=2


if(sum(c(w0,w1,w2,w3,w4,w5,w6))==length(c(w0,w1,w2,w3,w4,w5,w6))){
  print("The table uploaded is ready to be processed")
}else{print("There is a problem with the presented table, please review and start again.")}

# print("You should revise the list form. Some of the rows may not contain all the necesary fields completed.")
# print("Ready to go!")
}
