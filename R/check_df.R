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

# Rule 0
# All the inscribed members need to have at least their name, birthday and gender.
w0 = length(which(df[,1] == 0 & df[,4] == 0 & df[,5] == 0)) == 0 

# Rule 1
# Check if there is only one lider in ISCED level 02.
w1 = if(i02v == TRUE){
  sum(as.numeric(df[,9])) == 1
}else{
  sum(as.numeric(df[,9])) == 0
}

# Function to check valid values.
# if x is a valid teacher (1) their domain need to be in a specific range of valid
# responses.

i02_valid_r = c(1,2,3,4,5,6,7,8,9,10,11,12)
fun_i02 = function(x,y){
  if(x == 1){
     y %in% i02_valid_r
  }else{
     y == 0
  }
}

# Rule 2
# Check if the staff as valid Staff Role
w2 = sum(mapply(fun_i02,df[,6],df[,10])) == nrow(df)
  
fun1 = function(x,y){
  if(x == 1){
     y %in% c(1,2,3,4,9)
  }else{
     y == 0
  }
}

# Rule 3
# Check if the teacher as valid Main Subject domain
w3 = sum(mapply(fun1,df[,7],df[,11])) == nrow(df)

# Rule 4
# Check if the teacher as valid Main Subject domain
w4 = sum(mapply(fun1,df[,8],df[,12])) == nrow(df)

# Rule 5
# All teachers or staff needed be list it at least one ISCED level
# and no more than 2. 
w5 = sum(apply(df[,c(6,7,8)],1,FUN=function(x){sum(as.numeric(x))}) > 0 &
apply(df[,c(6,7,8)],1,FUN=function(x){sum(as.numeric(x))}) < 3) == nrow(df)
 
value = (sum(c(w0,w1,w2,w3,w4,w5)) == length(c(w0,w1,w2,w3,w4,w5)))

# print("You should revise the list form. Some of the rows may not contain all the necesary fields completed.")
# print("Ready to go!")
}

