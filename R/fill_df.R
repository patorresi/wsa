#' Fill data frame function
#'
#' This function remove and replace external values with 0.
#' @param df The data frame gathered will utilized the uploaded data frame.
#' @keywords cleaning
#' @export
#' @examples
#' fill_df()


fill_df = function(x){
# now I have only 0 values.
clean_df = as.data.frame(apply(x,2,FUN=function(x){ifelse(is.na(x)==TRUE,0,x)}))

clean_df[,4]  = ifelse(nchar(clean_df[,4]) == 4,clean_df[,4],0)
clean_df[,5]  = ifelse(clean_df[,5] %in% c(1,2,3,9),clean_df[,5],0)
clean_df[,6]  = ifelse(clean_df[,6] == 1,clean_df[,6],0)
clean_df[,7]  = ifelse(clean_df[,7] == 1,clean_df[,7],0)
clean_df[,8]  = ifelse(clean_df[,8] == 1,clean_df[,8],0)
# if there is 02 values at least one of them has to be leader
clean_df[,9]  = ifelse(clean_df[,9] == 1,clean_df[,9],0)
clean_df[,10] = ifelse(clean_df[,10] %in% c(1,2,3,4,5,6,7,8,9,11,12),clean_df[,10],0)
clean_df[,11] = ifelse(clean_df[,11] %in% c(1:4,9),clean_df[,11],0)
clean_df[,12] = ifelse(clean_df[,12] %in% c(1:4,9),clean_df[,12],0)
clean_df
}

