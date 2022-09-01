#' Probabilty of Age of death given he is right handed
#'
#' @get_p_A_given_rh
#' @param lh
#' @param dd
#' 
#' 
get_p_A_given_rh <- function(lh, dd) {
	
	dd <- dd %>% slice(2:nrow(dd)) %>% 
		mutate(Age=as.numeric(Age))
	
	both_sexes_sum <- dd %>% 
		na.omit() %>% 
		summarise(bs_sum <- sum(Both.Sexes)) %>% 
		pull()
	
	p_A <- dd %>% 
		na.omit() %>% 
		mutate(bs=Both.Sexes/both_sexes_sum) %>% 
		pull()
	
	p_right <-(1- get_p_lh(lh,dd) %>% pull())
	
	p_rh_A =1- get_p_lh_given_A(lh,dd)
	
	numerator <- (p_rh_A*p_A) %>% 
		select(avg_mf) 
	
	return(numerator/p_right)
	
}
