#' Function to manipulate data 
#'
#'
#' @title
#' @param data
#' @param group_yn
get_data_trans <- function(data,group_yn) {
	
	if(group_yn=="N"){

	trans_data <- data %>% 
		mutate(birth=1986-Age,
					 avg_mf=(Male+Female)/2)
	return(trans_data)

	}else if (group_yn=="Y"){
	
	trans_data <- data %>% 
		select(Age,Male,Female) %>% 
		gather(key=gender,value=value,-Age) 	
		
	return(trans_data)
}
}