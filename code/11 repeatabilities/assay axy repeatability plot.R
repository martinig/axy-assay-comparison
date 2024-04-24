#code to generate repeatability comparison plot for assays and axys
#original code by A. R. Martinig
#last edited April 18, 2024 by A. R. Martinig

# run start up code, focal data subsets 
Start-up code.R

rep<-read.csv("assayaxyrepeatabilitydata.csv", header=T) %>%
	mutate(Method=as.factor(Method),
		Behaviour=as.factor(Behaviour), 
		age=as.factor(age),
		rep_type =as.factor(rep_type),
		age = fct_relevel (age, "J", "Y", "A"))

summary(rep)
head(rep)
str(rep)

#colour blind friendly palatte #c("#000000", "#E69F00" orangish, "#56B4E9" light blue, "#009E73" #green, "#F0E442" yellow, "#0072B2" blue, "#D55E00" orange, "#CC79A7" pink)

pd <- position_dodge(0.5)

my_colors <- 
   tibble(color = c("#D55E00", "#0072B2", "black"))  

ggplot(rep, aes(x = age, y = Repeatability, shape = Behaviour, colour = rep_type, linetype = FALSE)) + #group = behaviour seems to override the position_dodge for the points
  	geom_point(position = position_dodge(width = 0.5), size = 5) + 
  	scale_x_discrete(labels=c("J" = "Juvenile", "Y" = "Yearling", "A" = "Adult")) +
#  	scale_linetype_manual(values=c("solid", "dashed"), labels=c("non_adjusted" = "Non-adjusted", "adjusted" = "Adjusted"), name = "Repeatability type") +
  	scale_colour_manual(values = c("#D55E00", "#0072B2", "black"), name = "Repeatability type", 
  			breaks=c("non_adjusted","adjusted"),
  			labels=c("non_adjusted" = "Non-adjusted", "adjusted" = "Adjusted")) + 
    scale_shape_manual(name="Behavioural Axis", values=c(16,1,15,0), labels=c("sPC1" = "Activity", "sPC2" = "Exploration", "xPC1" = "Foraging", "xPC2" = "Movement")) +			
  coord_cartesian(ylim=c(0, 0.6), clip="off")+ #set the range
  guides(linetype = FALSE, colour = guide_legend(keyheight=2, order=3), shape = guide_legend(keyheight=1.2, order=1)) + #keyheight changes space btw rows, order changes order of multiple legend items #linetype=FALSE #removes the legend showing line type
  geom_errorbar(aes(ymin=LCI, ymax=UCI), position=pd, width=0.1, size=0.5)+ 
  theme_squirrel +
  theme(legend.text.align = 0, #also needed to help left-justify legend
        legend.key.size = unit(3, 'lines'),
        plot.caption = element_text(hjust = 0, vjust = 2.12)) +
  labs(y = "Repeatability ± 95 % CrI", x = "Life stage")
