#code to generate repeatability comparison plot for assays and axys
#last edited March 24, 2024 by J. I. Sanders

# run start up code, focal data subsets 
Start-up code.R

rep<-read.csv("assayaxyrepeatabilitydata.csv", header=T) %>%
	mutate(Method=as.factor(Method),
		Behaviour=as.factor(Behaviour), 
		age=as.factor(age),
		rep_type =as.factor(rep_type))

summary(rep)
head(rep)
str(rep)

#colour blind friendly palatte #c("#000000", "#E69F00" orangish, "#56B4E9" light blue, "#009E73" #green, "#F0E442" yellow, "#0072B2" blue, "#D55E00" orange, "#CC79A7" pink)

pd <- position_dodge(0.3)

my_colors <- 
   tibble(color = c("#D55E00", "#0072B2", "black"), 
   subset = c("all","random", "consecutive"))  

ggplot(rep, aes(x = age, y = Repeatability, shape = Behaviour, colour=Method, linetype=rep_type)) + #group = behaviour seems to override the position_dodge for the points
  	geom_point(position = position_dodge(width = 0.3), size = 5) + 
  	scale_x_discrete(labels=c("A" = "Adult", "J" = "Juvenile", "Y" = "Yearling")) +
  	scale_linetype_manual(values=c("solid", "dashed"), labels=c("non_adjusted" = "Non-adjusted", "adjusted" = "Adjusted"), name = "Repeatability type") +
  	scale_colour_manual(values = c("#D55E00", "#0072B2", "black"), name = "Method", 
  			breaks=c("Assay","Accelerometer"),
  			labels=c("Assay" = "Assay", "Accelerometer" = "Accelerometer"))+ 
    scale_shape_manual(name="Accelerometer behaviour", values=c(19,17,15,18), labels=c("sPC1" = "Assay Behavioural axis 1", "sPC2" = "Assay Behavioural axis 2", "xPC1" = "Accelerometer Behavioural axis 1", "xPC2" = "Accelerometer Behavioural axis 2")) +			
  coord_cartesian(ylim=c(0, 0.6), clip="off")+ #set the range
  guides(linetype = guide_legend(keyheight=2, order=2), colour = guide_legend(keyheight=2, order=3), shape = guide_legend(keyheight=1.2, order=1)) + #keyheight changes space btw rows, order changes order of multiple legend items #linetype=FALSE #removes the legend showing line type
  geom_errorbar(aes(ymin=LCI, ymax=UCI), position=pd, width=0.1, size=0.5)+ 
  theme_bw() +
  theme(legend.text.align = 0, #also needed to help left-justify legend
  		axis.line = element_line(colour = "black"),
        legend.key.size = unit(3, 'lines'),
        plot.caption = element_text(hjust = 0, vjust = 2.12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 15)) +
  labs(y = "Repeatability ± 95 % CrI", x = "Life stage")
