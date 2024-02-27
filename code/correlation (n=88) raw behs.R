#robust correlation and pearson's correlation between raw behaviours for focal and assay data
#last updated Frebruary 27, 2024 by J. I. Sanders

# run start up code, focal data subsets 
Start-up code.R
focal data subsets.R

library(robustHD)
library(blme)
library(lme4)
library(glmmTMB)

#standardize variables to test for differences between methods (Pearson and robust)

#generate combined dataset
combine<-dplyr::inner_join(assays, axy, by=("squirrel_id"="squirrel_id")) %>% #extract only the PCA scores for the n=88 subset
		ungroup() 

#There may be repeated values in some squirrels due to discrepancies in the number of values for assays and axys. If there are more assay values, the axy values will be repeated and vice versa. Right now, we are keeping repeated values.

summary(combine)
head(combine)
nrow(combine)
combine %>% as_tibble() %>% count(squirrel_id) %>% nrow() #67 individuals

#Pearson's correlation
attach(combine);tt=cbind(walk, jump, hole, hang, chew, groom, still, feed, forage, nestmove, nestnotmove, notmoving, travel)
Hmisc::rcorr(tt, type="pearson")

#correlations are the same even if I standardize the variables



#correlations for unfilterd data to pull out correlations within assays and within focals

p1<-ggstatsplot::ggcorrmat(
  assays,
  cor.vars = c(walk, jump, hole, hang, chew, groom, still),
  cor.vars.names = c("Walk", "Jump", "Hole", "Hang", "Chew/Scratch", "Self-grooming", "Still"),
  output = "plot",
  matrix.type = "lower",
  type = "pearson",
  beta = 0.1,
  k = 2,
  sig.level = 0.05,
  conf.level = 0.95,
  nboot = 1000,
  pch= "",
  colors = c("black", "white", "black"),
    ggcorrplot.args = list(
    lab_size = 2) #makes the numbers in the boxes smaller
)+ 
  scale_y_discrete(position = "right")+
  theme_bw() +
  labs(y="", x="") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text=element_text(size = 11), #changes size of axes #s
        axis.text.x=element_text(angle = 90),
        axis.title=element_text(size= 0), #changes size of axes labels
        text = element_text(size = 15),
        legend.position = c(0.15,0.8), #move legend into plot
        legend.title=element_blank())
        

p2<-ggstatsplot::ggcorrmat(
  axy,
  cor.vars = c(feed, forage, nestmove, nestnotmove, notmoving, travel),
  cor.vars.names = c("Feed", "Forage", "Nest Moving", "Nest Not Moving", "Not Moving", "Travel"),
  output = "plot",
  matrix.type = "lower",
  type = "pearson",
  beta = 0.1,
  k = 2,
  sig.level = 0.05,
  conf.level = 0.95,
  nboot = 1000,
  pch= "",
  colors = c("black", "white", "black"),
  ggcorrplot.args = list(
    lab_size = 2) #makes the numbers in the boxes smaller
)+ 
  scale_y_discrete(position = "right")+
  theme_bw() +
  labs(y="", x="") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text=element_text(size = 11), #changes size of axes #s
        axis.text.x=element_text(angle = 90),
        axis.title=element_text(size= 0), #changes size of axes labels
        text = element_text(size = 15),
        legend.position = c(0.15,0.8), #move legend into plot
        legend.title=element_blank())        
        

#Combine robust plots
	 prow <- cowplot::plot_grid( 
	 				   p1 + theme(legend.position="none"),
	                   p2 + theme(legend.position="none"),
	                   align = 'vh',
	                   labels = c("(a)", "(b)"), label_size=10,
	                   hjust = -0.5,
	                   vjust=5,
	                   nrow = 1,
	                   ncol=2)
	
	legend_b <- cowplot::get_legend(p1 + theme(legend.position="bottom")) #extract the legend from one of the plots 
	
cowplot::plot_grid(prow, NULL, legend_b, ncol = 1,nrow=3, rel_heights = c(1,-0.2,0.5))