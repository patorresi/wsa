#' Prepare dataframe function
#'
#' This function extract the relevant values from the dataframe.
#' @param df The data frame gathered will utilized the uploaded data frame.
#' @keywords values
#' @export
#' @examples
#' prepare_df()


prepare_df = function(x){
	x = as.data.frame(x)
	db_pi = which(x[,1] == '<_fld_teacher_name_>' | x[,12] == "<_list_end_>" | x[,5] == "X" )
	db_co = nrow(x)
	db_er = which(rowSums(is.na(x)) == ncol(x))
	row_rm = c(db_pi,db_co,db_er)
	df = x[!1:dim(x)[1] %in% row_rm,]
	df
}
