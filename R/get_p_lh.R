#' .. Probability of being left handed ..
#'
#' .. content for \details{} ..
#'
#' @get_p_lh
#' @param lh,dd
get_p_lh <- function(lh,dd) {
	
		dd <- dd %>% slice(2:nrow(dd)) %>% 
		mutate(Age=as.numeric(Age))
	
		p_lh_given_a <- get_p_lh_given_A(lh,dd)
		p_list <- p_lh_given_a%>% 
			summarise(p=sum(avg_mf*Both.Sexes))
		
		both_sexes_sum <- dd %>% 
			na.omit() %>% 
			summarise(bs_sum <- sum(Both.Sexes)) %>% 
			pull()
		
		
		
		return(p_list/both_sexes_sum)
		

}
