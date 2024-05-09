#code to generate repeatability comparison plot for assays and axys
#original code by A. R. Martinig
#last edited May 6, 2024 by A. R. Martinig

# run start up code, focal data subsets 
Start-up code.R

rep<-read.csv("axy_comparison_repeatability_data.csv", header=T) %>%
	mutate(subset=as.factor(subset),
		behaviour=as.factor(Behaviour), 
		age=as.factor(age),
		rep_type =as.factor(rep_type))

summary(rep)
head(rep)
str(rep)

#colour blind friendly palatte #c("#000000", "#E69F00" orangish, "#56B4E9" light blue, "#009E73" #green, "#F0E442" yellow, "#0072B2" blue, "#D55E00" orange, "#CC79A7" pink)

pd <- position_dodge(0.5)

ggplot(rep, aes(x = age, y = Repeatability, shape = behaviour, colour = subset, linetype = rep_type)) + 
  	geom_point(position = position_dodge(width = 0.5), size = 5) + 
   	geom_errorbar(aes(ymin=LCI, ymax=UCI), position=pd, width=0.1, size=0.5)+ 
  	scale_x_discrete(
  		limits=c("Y", "A"),
  		labels=c("Y" = "Yearling", "A" = "Adult")) +
  	scale_linetype_manual(
  		values=c("solid", "dashed"), 
  		labels=c("non_adjusted" = "Non-adjusted", 
  			"adjusted" = "Adjusted"), 
  		name = "Repeatability type") +
  	scale_colour_manual(
   		breaks=c("all","consecutive", "random"),
  		values = c("black","#D55E00", "#0072B2"), 
  		labels=c("all" = "Complete dataset", 
  			"consecutive" = "Consecutive 7 minute subset", 
  			"random"="Random 7 minute subset"),
  		name = "Subset") + 
	scale_shape_manual(
      	values=c(16, 17), 
      	labels=c("PC1" = "Axis 1", 
      		"PC2" = "Axis 2"), 
      	name = "Accelerometry behaviour") +	
  	coord_cartesian(ylim=c(0, 0.35), clip="off")+ #set the range
  	guides(
  	#keyheight changes space btw rows, order changes order of multiple legend items
  		linetype = guide_legend(keyheight=2, order=2), 
  		colour = guide_legend(keyheight=2, order=3), 
  		shape = guide_legend(keyheight=1.2, order=1)) +    
  	theme_squirrel +
  	theme(legend.text.align = 0, #also needed to help left-justify legend
        legend.key.size = unit(3, 'lines'),
        plot.caption = element_text(hjust = 0, vjust = 2.12)) +
  	labs(y = "Repeatability ± 95 % CrI", x = "Life stage")
