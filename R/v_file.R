#' Check valid file 
#'
#' This function check for possible errors in the data.frame.
#' @param file Check if the value uploaded is valid..
#' @keywords valitation
#' @export
#' @examples
#' v_file()


v_file = function(file){
	if(dim(file)[2] != 12){
		fs = FALSE
	}else if(is.na(file[which((file[,12] == '<_list_end_>') == TRUE),12])){
		fs = FALSE
	}else{
		fs = TRUE
	}
}




