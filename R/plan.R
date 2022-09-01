bayes_plan <-
  drake_plan(

   url = "https://gist.githubusercontent.com/mbonsma/8da0990b71ba9a09f7de395574e54df1/raw/aec88b30af87fad8d45da7e774223f91dad09e88/lh_data.csv",
   lefthanded_data = datamanip::get_data(url),
   url2 = "https://gist.githubusercontent.com/mbonsma/2f4076aab6820ca1807f4e29f75f18ec/raw/62f3ec07514c7e31f5979beeca86f19991540796/cdc_vs00199_table310.tsv",
   death_distribution_data=read.delim(url2,sep="\t",header=TRUE)[-1,] %>% na.omit() %>% mutate(Age=as.numeric(Age)), 
   p_lh_given_A=get_p_lh_given_A(lefthanded_data,death_distribution_data),
   p_lh=get_p_lh(lefthanded_data,death_distribution_data),
   p_A_given_lh=get_p_A_given_lh(lefthanded_data,death_distribution_data),
   p_A_given_rh=get_p_A_given_rh(lefthanded_data,death_distribution_data),
   
   lefthanded_people_plot=get_line_point_plot(readd(lefthanded_data),"Age","value","Y","Y","gender"),
   lefthanded_rate_plot=get_line_point_plot(readd(lefthanded_data),"birth","avg_mf","Y","N"),
   people_die_plot= get_line_point_plot(readd(death_distribution_data),"Age","Both.Sexes","N","N"),
   
   
   report = target(
   	command = {
   		rmarkdown::render(knitr_in("doc/Bayes.Rmd"))
   		file_out("doc/Bayes.html")
   	}
   )
   
   
   
   
)
