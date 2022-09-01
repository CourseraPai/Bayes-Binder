#' .. Probability of being lefthanded given age of death ..
#'
#' @get_p_lh_given_A
#' @param lh,dd
get_p_lh_given_A <- function(lh,dd) {
	
	dd <- dd %>% slice(2:nrow(dd)) %>% 
		mutate(Age=as.numeric(Age))

	early_1900s_rate  <- lh %>% 
		mutate(birth=Age,
		avg_mf=(Male+Female)/2) %>% 
		head(10) %>% 
		summarise(head_avg=mean(avg_mf)/100) %>% 
		pull()

	
	late_1900s_rate  <- lh %>%
		mutate(birth=Age,
		avg_mf=(Male+Female)/2) %>% 
		tail(10) %>% 
		summarise(tail_avg=mean(avg_mf)/100) %>% 
		pull()
	
	middle_rates <- lh %>% 
		mutate(birth=Age,
					 avg_mf=(Male+Female)/200,
					 Age=as.numeric(Age))
	
	dd_middle <- dd %>% 
		left_join(middle_rates,by="Age")
	
	
	youngest_age <- lh %>% 
		select(Age) %>% 
		summarise(Age=min(Age)) %>% 
		pull()
	
	oldest_age <- lh %>% 
		select(Age) %>%
		summarise(Age=max(Age)) %>% 
		pull()
	
	p_lh_given_a <- dd_middle %>% 
		mutate(avg_mf=ifelse((Age<=youngest_age),early_1900s_rate,avg_mf),
					 avg_mf=ifelse((Age>oldest_age),late_1900s_rate,avg_mf),
					 avg_mf=as.numeric(avg_mf)) %>% 
		select(Age,avg_mf,Both.Sexes) %>% 
		na.omit()
	
	return(as.data.frame(p_lh_given_a))
	
}
