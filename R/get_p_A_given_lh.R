#' Probabilty of Age of death given he is left handed
#'
#' @get_p_A_given_lh
#' @param lh
#' @param dd
#' 
#' 
get_p_A_given_lh <- function(lh, dd) {
	
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
	
	p_left <- get_p_lh(lh,dd) %>% pull()
	
	p_lh_A = get_p_lh_given_A(lh,dd)
	
	numerator <- (p_lh_A*p_A) %>% 
		select(avg_mf) 
	
	return(numerator/p_left)

}
