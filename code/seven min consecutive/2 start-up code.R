#consecutive seven minute sampling

#this is the first R script that always needs to be run
#all the data cleaning is here 
#if a mistake is found message April before making changes
#Last edited on March 5, 2024 by A. R. Martinig 

#axy-assay analysis for Jonas

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
               krsp
)


select<-dplyr::select
filter<-dplyr::filter


##############################
#  importing  data and formatting it  #
##############################


########################################
#raw behavioural assays
#impossible values removed
########################################

#creating the assay datast we will be using here

assays<-read.csv("raw assays.csv", header=T) %>%
	group_by(sq_id) %>%
	mutate(trialdate=ymd(trialdate)) %>%
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
	
	filter(!squirrel_id== 23686, #IT DOES NOT HAVE A SEX LISTED ANYWHERE!
	
	!is.na(squirrel_id), !observer %in% c("SWK"), is.na(hang) |hang<=1, is.na(chew) |chew<=1, is.na(still) |still<=1, is.na(front) | front<=1, is.na(back) |back<=1, is.na(attack) |attack<=0.96, is.na(attacklatency) |attacklatency<=1, is.na(approachlatency) |approachlatency<=1) %>% 
	#attack is set to 0.96 because numerous squirrels have 288-294 attacks, which are impossible to get in 300 seconds 
	#only excludes 2 squirrels from our n=88 dataset, the first (10265) had 294 attacks and a jump rate that was an outlier AND had decimals (which is impossible for a count behaviour!) and the second (10342) had 288 attacks
	#this leaves squirrels with <=252 attacks (which also should be investigated)
	select(-c(sq_id, observer.software,  collar, Exclude_unless_video_reanalyzed, Exclude_reason, Proceed_with_caution, Proceed_with_caution_reason, Last_Edited, Comments, oft_duration, mis_duration, colours, midden, taglft, tagrt, front, back, attack, attacklatency, approachlatency)) %>%
	droplevels()
	
summary(assays)
head(assays)

(assays) %>% as_tibble() %>% count(squirrel_id) %>% nrow() #822 individuals
nrow(assays) #1184


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

########################################
#raw axy data before cleaning
########################################

#for now I am just using Emily's data as I am waiting for Matt to provide me complete records
  
axy<-read.csv("allaxy_consecutive_7minute_sample.csv", header=T) %>%
	mutate(
		axy_id=paste(id, date, tod, sep = "-"), 
		axy_date=ymd(date),
		axy_yr=year(date),
		axy_month=month(date)) %>%
	filter(!is.na(id)) %>% #remove the rows with NA for squirrel_id
  	group_by(id, date, datetime) %>%
  	mutate(row_num = row_number()) %>%
  	pivot_wider(names_from = All, values_from = row_num, values_fn = length, values_fill = 0) %>%
  ungroup() %>%
  select(squirrel_id= id, axy_date, axy_yr, axy_month, tod, feed=Feed, forage=Forage, nestmove=NestMove, nestnotmove=NestNotMove, notmoving=NotMoving, travel=Travel, axy_id)

head(axy)
summary(axy)

axy %>% filter(squirrel_id== 23286 & axy_id=="23286-2019-08-22-day")


(axy) %>% as_tibble() %>% count(squirrel_id) %>% nrow() #241 individuals
nrow(axy) #87990