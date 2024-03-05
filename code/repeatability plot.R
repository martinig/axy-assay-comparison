rm(list=ls(all=T))
rep<-read.csv("/Users/april-martinig/Documents/Files/Manuscripts/Martinig et al. 2020/Consistency/Prashanna's plots/repeatdata.csv")

library(ggplot2)
library(dplyr)

#colour blind friendly palatte #c("#000000", "#E69F00" orangish, "#56B4E9" light blue, "#009E73" #green, "#F0E442" yellow, "#0072B2" blue, "#D55E00" orange, "#CC79A7" pink)

#Boon 2007 data didn't have upper&lower CI so they are NA for now
rep[rep == "na"] <- NA

Age<-as.factor(rep$Age)
stage<-as.factor(rep$stage)
Behaviour<-as.factor(rep$Behaviour)
rep[,3] <- as.numeric(as.character(rep[,3]))
rep[,4] <- as.numeric(as.character(rep[,4]))

pd <- position_dodge(0.3)

my_colors <- 
   tibble(color = c("#D55E00", "#0072B2", "black", "black", "#CC79A7", "black", "black"), 
   Age = c("A","A2", "E", "E2", "YK", "Y", "Y2"))  

ggplot(rep, aes(x = stage, y = Repeatability, shape = Behaviour, colour=Age, linetype=Status)) + #group = behaviour seems to override the position_doddge for the points
  	geom_point(position = position_dodge(width = 0.3), size = 5) + 
  	scale_x_discrete(labels=c("A" = "Emergence\nto weaning", "B" = "Weaning\nto yearling", "C" = "Adults")) +
  	scale_linetype_manual(values=c("solid", "dashed"), labels=c("all" = "All individuals", "survivors" = "Surviving\nindividuals"), name = "Subset") +
  	scale_colour_manual(values = c("#D55E00", "#0072B2", "black", "black", "black", "black", "#CC79A7"), name = "Study", breaks=c("A","A2", "E", "E"=="E2", "YK"=="Y1", "YK"=="Y2", "YK"),
  	labels=c( #atop centers the text; added spaces to left justify text
  	expression(atop(NA, atop(textstyle('Taylor et al. 2012'), 
          textstyle(italic('(366 adults)'))~phantom (1000000)))),
  	expression(atop(NA, atop(textstyle('Boon et al. 2008')~phantom (1000000),
          textstyle(italic('(71 adult females)'))~phantom (100)))),
    expression(atop(NA, atop(textstyle('This study')~phantom (100000),
          textstyle(italic('(102 juveniles)'))~phantom (1)))),
    "thisstudy",
    "thisstudy", 
    "thisstudy", 
    expression(atop(NA, atop(textstyle('*Kelley et al. 2015'),
          	textstyle(italic('(16 yearlings)'))~phantom (1000000))))))+ #~phantom (1000000) is to force it to left justify
  coord_cartesian(ylim=c(0,0.8), clip="off")+ #set the range
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
  labs(y = "Repeatability ± 95 % CrI", x = "Life stage") +
  #annotate("text", x =1.7, y = 0.535, label = "*", size=10, color="#CC79A7") +
   # annotate("text", x =1.7, y = 0.54, label = "*", size=6) +
	#annotate("text", x =2.2, y = 0.36, label = "*", size=10, color="#CC79A7") +
	#annotate("text", x =2.2, y = 0.365, label = "*", size=6) + 
	annotate("text", x =3.3, y = -0.1, label = expression(paste("*Recomputed values")), size=3, parse=TRUE)	
	
	
	
	annotate("text", x = 2.2, y = -0.5, label = expression(paste("*Individuals from Kelley ", italic("et al."), " 2015 study")), size=3.3, parse=TRUE)
