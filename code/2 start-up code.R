#all the data cleaning is here 
#original code by A. R. Martinig
#last edited on May 1, 2024 by A. R. Martinig 

options(scipen=999, dplyr.width = Inf, tibble.print_min = 50, repos='http://cran.rstudio.com/') #scipen forces outputs to not be in scientific notation #dplyr.width will show all columns for head() function and tibble.print_min sets how many rows are printed and repos sets the cran mirror

#load libraries
pacman::p_load(
				ggplot2, 
				ggstatsplot,
               dplyr, 
               lubridate, 
               tidyverse,   
               broom,  
               FSA,      
               glmmTMB,  
               lme4,   
               tidyr,      
               DescTools,
               scales,
               ggpubr,
               grid,
               lattice,
               sjPlot,
               sjlabelled,
               sjmisc,
               cowplot, 
               broom.mixed,
               ggforce, 
               gridGraphics,
               ggeffects,
               magrittr,
               MCMCglmm,
               data.table,
               lattice,
               stats,
               cowplot,
               krsp
)


select<-dplyr::select
filter<-dplyr::filter


##############################
#  importing  data and formatting it  #
##############################


########################################
#raw behavioural assays
########################################

#cleaning the assay datast we will be using

assays<-read.csv("raw assays.csv", header=T) %>%
	group_by(sq_id) %>%
	mutate(trialdate=ymd(trialdate),
	assay_month=month(trialdate),
	assay_season=case_when(
		 	assay_month %in% c(3, 4, 5)  ~ "spring",
		 	assay_month %in% c(6, 7, 8) ~ "summer",
		 	assay_month %in% c(9, 10)  ~ "autumn",
      	.default = "winter")) %>%
	arrange(trialdate) %>%
	mutate(trialnumber=row_number())%>%
	ungroup() %>%
#converting the raw scores
	mutate(
		ageclass=ifelse(age==0, "J", 
			ifelse(age==1, "Y", 
			ifelse(age>1, "A",  ageclass))),
		squirrel_id=sq_id,
		age=as.numeric(age),
		walk=(walk/450), 
		jump=(jump/450), 
		hole=(hole/450), 
		hang=(hang/450), 
		chew=(chew/450), 
		groom=(groom/450), 
		still=(still/450),
		front=(front/300), 
		back=(back/300), 
		attack=(attack/300), 
		attacklatency=(attacklatency/300), 
		approachlatency=(approachlatency/300)) %>%
	filter(
		!squirrel_id== 23686, #missing sex ID
		!ageclass=="J", #excluding because we don't have this for axys
		!is.na(squirrel_id), 
		!observer %in% c("SWK"), 
		is.na(hang) | hang<=1, 
		is.na(chew) | chew<=1, 
		is.na(still) | still<=1, 
		is.na(front) | front<=1, 
		is.na(back) | back<=1, 
		is.na(attack) | attack<=0.96, #attack is set to 0.96 because numerous squirrels have 288-294 attacks, which are impossible to get in 300 seconds 
		#only excludes 2 squirrels - the first (10265) had 294 attacks and a jump rate that was an outlier AND had decimals (which is impossible for a count behaviour!) and the second (10342) had 288 attacks
		#this leaves squirrels with <=252 attacks (which is still high)
		is.na(attacklatency) | attacklatency<=1, 
		is.na(approachlatency) | approachlatency<=1,
		!is.na(walk)) %>% 
	select(-c(X, sq_id, observer.software,  collar, Exclude_unless_video_reanalyzed, Exclude_reason, Proceed_with_caution, Proceed_with_caution_reason, Last_Edited, Comments, oft_duration, mis_duration, na.rm, colours, midden, taglft, tagrt, front, back, attack, attacklatency, approachlatency)) %>%
	droplevels()
	
summary(assays)
head(assays)

(assays) %>% as_tibble() %>% count(squirrel_id) %>% nrow() #543 individuals
nrow(assays) #741

table(assays$sex, assays$ageclass)
table(assays$observer)

########################################
#bare minimum needed for axy data subsets
########################################

birth<-read.csv ("fitness.csv") %>%
	select(c(squirrel_id, sex, byear, dyear, litter_id)) %>%
	mutate(sex=ifelse(squirrel_id %in% c(21128, 21348), "F",
		ifelse(squirrel_id %in% c(19890, 23326, 23210), "M", as.character(sex)))) %>%
	group_by(squirrel_id) %>%
	filter(row_number()==1)

summary(birth)
head(birth)





#conserved theme across plots for plots
#general theme
theme_squirrel <-
	theme_bw() +
	theme(axis.line = element_line(colour = "black"),
        axis.text=element_text(size=15), #changes size of axes #s
        axis.title=element_text(size=15), #changes size of axes labels
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 15)) 
 
#dot-whisker plot theme       
theme_squirrel_dot <- 
	theme_bw() +
 	theme(plot.margin = margin(0, 0.5, 0, 0, "cm"),
		axis.line=element_line(),
		axis.line.y=element_blank(),
		axis.ticks.length=unit(0.4, "cm"),
		axis.ticks.y=element_blank(),
    	axis.text=element_text(size=10), #changes size of axes #s
        axis.title=element_text(size=15), #changes size of axes labels
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 10))      
        