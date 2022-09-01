install.packages('dflow', dependencies=TRUE, repos='http://cran.rstudio.com/')
devtools::load_all()

url <- "https://gist.githubusercontent.com/mbonsma/8da0990b71ba9a09f7de395574e54df1/raw/aec88b30af87fad8d45da7e774223f91dad09e88/lh_data.csv"
lefthanded_data <- get_data(url) %>% mutate(category="Blue")
datamanip::get_EDA(bayes,2)

url2 = "https://gist.githubusercontent.com/mbonsma/2f4076aab6820ca1807f4e29f75f18ec/raw/62f3ec07514c7e31f5979beeca86f19991540796/cdc_vs00199_table310.tsv"
death_distribution_data <- read.delim(url2,sep="\t",header=TRUE) 
death_distribution_data <- death_distribution_data %>% slice(2:nrow(death_distribution_data)) %>% 
	mutate(Age=as.numeric(Age))


death_distribution_databayes %>% 
	select(Age,Male,Female) %>% 
	gather(key=gender,value=value,-Age) %>% 
	ggplot(aes(x=Age,y=value,group=gender,colour=gender,fill=gender))+
	geom_point(aes(shape=gender),size=2)+
	geom_line()+
	scale_shape_manual(values=c(4, 19)) +
	scale_color_manual(values=c("orange","blue"))+
	theme(panel.background = element_blank())


bayes %>% 
	mutate(birth=1986-Age,
				 avg_mf=(Male+Female)/2) %>% 
	ggplot(aes(x=birth,y=avg_mf))+
	geom_line(colour="seagreen",size=0.75)+
	coord_flip()+
	theme(panel.background = element_blank(),
				axis.line = element_line())


#*********************************************************************************************************


head_avg <- bayes %>% 
	mutate(birth=1986-Age,
				 avg_mf=(Male+Female)/2) %>%
mutate(birth=1986-Age,
avg_mf=(Male+Female)/2) %>% head(10) %>% summarise(head_avg=mean(avg_mf)/100) %>% 
	pull()

tail_avg <- bayes %>%
mutate(birth=1986-Age,
avg_mf=(Male+Female)/2) %>% tail(10) %>% summarise(tail_avg=mean(avg_mf)/100) %>% 
	pull()


ages_mid <- ages %>% 
	left_join(try,by="Age")

young <- 1990-1986+10
old <- 1990-1986+86

try <- bayes %>% 
	mutate(birth=1986-Age,
				 avg_mf=(Male+Female)/200,
				 Age=as.numeric(Age))


p_lh_given_a <- ages_mid %>% 
	mutate(avg_mf=ifelse((Age<=young),head_avg,avg_mf),
	       avg_mf=ifelse((Age>old),tail_avg,avg_mf),
				 avg_mf=as.numeric(avg_mf)) %>% 
	select(Age,avg_mf,Both.Sexes) %>% 
	na.omit()


#*******************************************************************************************************
ages %>% 
	select(Age,Both.Sexes) %>% 
	na.omit() %>% 
	ggplot(aes(x=Age,y=Both.Sexes))+
	geom_point(colour="blue",size=3)+
	geom_line(colour="blue",size=1.5)+
	theme(panel.background = element_blank(),
				axis.line = element_line(),
				axis.text.x=element_text(angle=45))

#*******************************************************************************************************

p <- p_lh_given_a %>% na.omit(avg_mf) %>% mutate(p=as.numeric(avg_mf)*Both.Sexes) %>% 
	summarise(p=sum(p))
d <- ages_mid %>% na.omit(avg_mf) %>% summarise(d=sum(Both.Sexes))

bs_sum <- ages %>% 
	na.omit() %>% 
	summarise(bs_sum <- sum(Both.Sexes)) %>% 
	pull()

crack <- (ages %>% 
	na.omit() %>% 
	mutate(bs=Both.Sexes/bs_sum)) %>% 
	summarise(bs=sum(bs)) %>% 
	pull()

p_lh_given_a <- slice(p_lh_given_a,1:115)

f <- data.frame(cbind(age=seq(1:115),(p_lh_given_a$avg_mf*crack)/0.09))

options(scipen = 999)

f %>% ggplot(aes(x=age,y=V2))+
	geom_line()


p_rh_given_a=1-p_lh_given_a
p_rh=1-0.09

gf <- data.frame(cbind(age=seq(1:115),(p_rh_given_a$avg_mf*crack)/p_rh))

gf %>% ggplot(aes(x=age,y=V2))+
	geom_line()



url = "https://gist.githubusercontent.com/mbonsma/8da0990b71ba9a09f7de395574e54df1/raw/aec88b30af87fad8d45da7e774223f91dad09e88/lh_data.csv"
lefthanded_data = datamanip::get_data(url)
url2 = "https://gist.githubusercontent.com/mbonsma/2f4076aab6820ca1807f4e29f75f18ec/raw/62f3ec07514c7e31f5979beeca86f19991540796/cdc_vs00199_table310.tsv"
death_distribution_data <- read.delim(url2,sep="\t",header=TRUE)
p_lh_given_A=get_p_lh_given_A(lefthanded_data,death_distribution_data)
p_lh=get_p_lh(lefthanded_data,death_distribution_data)
p_A_given_lh=get_p_A_given_lh(lefthanded_data,death_distribution_data)

ages=seq(0,118,by=1)
cbind(ages,readd(p_A_given_lh),readd(p_A_given_rh))

readd(p_A_given_lh) %>% 
	ggplot(aes(x=ages,y=avg_mf))+
	geom_line(colour="blue",size=1.5)

readd(p_A_given_rh) %>% 
	ggplot(aes(x=ages,y=avg_mf))+
	geom_line(colour="orange",size=1.5)

final <- cbind(seq(0,112,by=1),readd(p_A_given_lh),readd(p_A_given_rh) ) 
names(final) <- c("Age","Left","Right")

final %>% 
	filter(Age %in% c(6:115)) %>% 
	mutate(left_sum=(Age*Left),
	       right_sum=(Age*Right)) %>% 
	select(left_sum,right_sum) %>% 
	summarise(left_final=sum(left_sum),
						right_final=sum(right_sum)) %>% 
	mutate(left_handed_die_early_by_years=right_final-left_final) %>% 
	pull()

final %>% 
	gather(key=left_right,value=value,-Age) %>% 
	ggplot(aes(x=Age,y=value,group=left_right))+
	geom_line(aes(colour=left_right),size=1.5)+
	scale_color_manual(values=c("blue","orange"))


get_line_point_plot(data,x_variable,y_variable,group_yn,group_variable)
get_data_trans(data)


readd(death_distribution_data) %>% 
	select(Age,Both.Sexes) %>% 
	na.omit() %>% 
	ggplot(aes(x=Age,y=Both.Sexes))+
	geom_point(colour="blue",size=3)+
	geom_line(colour="blue",size=1.5)+
	theme(panel.background = element_blank(),
				axis.line = element_line())
