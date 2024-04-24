#pearson's correlations within and between raw behaviours for axys and assay data
#original code by A. R. Martinig
#last edited April 16, 2024 by A. R. Martinig

# run
Start-up code.R

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



#correlations between raw behaviours across methods

########################################
# merged axy and personality dataset
# n=61
########################################

combine <- axy %>%
	group_by(squirrel_id, axy_date) %>%
	mutate(feed=sum(feed),
		forage =sum(forage),
		nestmove =sum(nestmove),
		nestnotmove =sum(nestnotmove),
		notmoving =sum(notmoving),
		travel =sum(travel),
		total=feed+forage+nestmove+nestnotmove+notmoving+travel) %>%
  filter(row_number()==1) %>%
  droplevels()%>%	
  mutate(prop_feeding=(feed/total),
         prop_foraging=(forage/total),
         prop_nestmoving =(nestmove/total),
         prop_nestnotmoving =(nestnotmove/total),
         prop_notmoving =(notmoving/total),
         prop_travel=(travel/total)) %>%
 ungroup() %>%        
 inner_join(assays, by=c("squirrel_id")) %>%
 select(-c(tod, feed, forage, nestmove, nestnotmove, notmoving, travel, axy_id, observer, ageclass, sex, age, grid, videodate, trialtime, total))

head(combine)

########################################
######  extracting summary stats  ######
########################################

(combine) %>% as_tibble() %>% count(squirrel_id) %>% nrow() #61 individuals
nrow(combine) #2409

length(unique(combine $axy_date)) #741
length(unique(combine $trialdate)) #50


########################################
# Pearson's correlations
########################################

attach(combine);tt=cbind(walk,   jump,   hole,  hang, chew,  groom,  still, prop_feeding, prop_foraging, prop_nestmoving, prop_nestnotmoving, prop_notmoving, prop_travel)
cor(tt)  
Hmisc::rcorr(tt, type="pearson")