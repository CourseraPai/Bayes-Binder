#' Function to plot a basic line_point plot with or without a grouping variable
#'
#'
#' @title
#' @param data
#' @param x_variable
#' @param y_variable
#' @param trans_reqd
#' @param group_yn
#' @param group_variable
#' 
get_line_point_plot <- function(data, x_variable, y_variable,trans_reqd, group_yn,
                                group_variable) {
	
	if(group_yn=="N"){
		if(trans_reqd=="N"){
			lh=data %>% na.omit()
		}else if(trans_reqd=="Y"){
			lh <- get_data_trans(data,"N")
		}
		
	}else if (group_yn=="Y"){
		lh <- get_data_trans(data,"Y")
	}
	
	
	
	
	if(group_yn=="N"){

  data_phrase <- eval(parse(text=paste0("lh %>% ggplot(aes(x=",x_variable,",y=",y_variable,"))")))
  
  line_point_plot <- data_phrase +
  	geom_line(colour="seagreen",size=0.75)+
  	theme(panel.background = element_blank(),
  				axis.line = element_line())
  
  return(line_point_plot)
	}else if(group_yn=="Y"){
		data_phrase <- eval(parse(text=paste0("lh %>% ggplot(aes(x=",x_variable,",y=",y_variable,",group=",group_variable,",colour=",
																					group_variable,",fill=",group_variable,"))")))
		
		line_point_plot <- data_phrase+ 
			geom_line(size=0.75)+
			geom_point(aes(shape=gender),size=2)+
			theme(panel.background = element_blank(),
						axis.line = element_line())+
			scale_color_manual(values=c("orange","blue"))+
			scale_shape_manual(values=c(4, 19)) 
			
		
		return(line_point_plot)
			
	}
  
}
