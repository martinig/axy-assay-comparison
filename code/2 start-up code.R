#this is the first R script that always needs to be run
#all the data cleaning is here 
#if a mistake is found message April before making changes
#Last edited on Feb 13, 2024 by A. R. Martinig 

#axy-assay analysis for Jonas

options(scipen=999, dplyr.width = Inf, tibble.print_min = 50, repos='http://cran.rstudio.com/') #scipen forces outputs to not be in scientific notation #dplyr.width will show all columns for head() function and tibble.print_min sets how many rows are printed and repos sets the cran mirror

#load libraries
pacman::p_load(ggplot2, 
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
               ggeffects
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

assays%>%filter(is.na(trialnumber))
assays %>% filter(squirrel_id== 13305)

assays<-read.csv("Trials.csv", header=T) %>%
	bind_rows(read.csv("Personality KRSP Master File (Dec 12, 2023).csv", header=T)%>%select(-X) %>%mutate_at(vars(walk, jump, hole, hang, chew, groom, still, front, back, attack, attacklatency, approachlatency), as.numeric)) %>% #Ben's master dataset has a lot of problems, so I have to fix them below
	group_by(sq_id, trialdate) %>%
	#filter out duplicates (not just across datasets, for example sq_id 19729 has multiple records on the same day in Ben's data)
	filter(row_number()==1) %>%
	ungroup() %>%
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
	
	!is.na(squirrel_id), !observer %in% c("SWK"), is.na(hang) |hang<=1, is.na(chew) |chew<=1, is.na(still) |still<=1, is.na(front) |front<=1, is.na(back) |back<=1, is.na(attack) |attack<=0.96, is.na(attacklatency) |attacklatency<=1, is.na(approachlatency) |approachlatency<=1) %>% 
	#attack is set to 0.96 because numerous squirrels have 288-294 attacks, which are impossible to get in 300 seconds 
	#only excludes 2 squirrels from our n=88 dataset, the first (10265) had 294 attacks and a jump rate that was an outlier AND had decimals (which is impossible for a count behaviour!) and the second (10342) had 288 attacks
	#this leaves squirrels with <=252 attacks (which also should be investigated)
	select(-c(sq_id, observer.software,  collar, Exclude_unless_video_reanalyzed, Exclude_reason, Proceed_with_caution, Proceed_with_caution_reason, Last_Edited, Comments, oft_duration, mis_duration, colours, midden, taglft, tagrt)) %>%
	droplevels()
	
summary(assays)
head(assays)

(assays) %>% as_tibble() %>% count(squirrel_id) %>% nrow() #822 individuals
nrow(assays) #1184

table(assays$sex, assays$ageclass)
table(assays$observer)


########################################
######  extracting summary stats  ######
########################################

#total number of inds and sex stats
other_stats<-assays%>%
	group_by(squirrel_id)%>%
	filter(row_number()==1)

(other_stats) %>% as_tibble() %>% count(squirrel_id) %>% nrow() #810 individuals
table(other_stats$sex) #sex number

#ageclass stats
age_class_stats<-assays%>%
	group_by(squirrel_id, ageclass)%>%
	filter(row_number()==1)

table(age_class_stats$ageclass) #age class number (remember: some individuals will have multiple records across age classes!)
table(age_class_stats$ageclass, age_class_stats$sex) 


#trial number by age class stats
adults<-assays %>% filter(ageclass=="A") %>% group_by(squirrel_id) %>% mutate(sum=n()) %>% filter(row_number()==1)

yearl<-assays%>%filter(ageclass=="Y") %>% group_by(squirrel_id) %>% mutate(sum=n()) %>% filter(row_number()==1)

ju<-assays%>%filter(ageclass=="J") %>% group_by(squirrel_id) %>% mutate(sum=n()) %>% filter(row_number()==1)

#note: trial number is not reliable for adults or yearlings BECAUSE the count starts with the first trial - which may be during earlier phases (like an adult with 5 trials, could have had 3 of them done as a juvenile) - to get around this, I calculated sums after subsetting

nrow(adults)
table(adults$sum, adults$sex)

nrow(yearl)
table(yearl$sum, yearl$sex)

nrow(ju)
table(ju$sum, ju$sex)

########################################
#bare minimum needed for focal data subsets
########################################

birth<-read.csv ("fitness.csv") %>%
	select(c(squirrel_id, sex, byear, dyear, litter_id)) %>%
	mutate(sex=ifelse(squirrel_id %in% c(21128, 21348), "F",
		ifelse(squirrel_id %in% c(19890, 23326, 23210), "M", as.character(sex)))) 

summary(birth)
head(birth)

########################################
#raw axy data before cleaning
########################################

#for now I am just using Emily's data as I am waiting for Matt to provide me complete records
  
axy<-read.csv("SquirrelAxyData_Emily.csv", header=T) %>%
	mutate(
	axy_id=paste(Squirrel_ID, date, tod, sep = "-"), 
	axy_date=ymd(date),
	axy_yr=year(date),
	axy_month=month(date)) %>%
	select(squirrel_id= Squirrel_ID, axy_date, axy_yr, axy_month, tod, feed=Feed, forage=Forage, nestmove=NestMove, nestnotmove=NestNotMove, notmoving, travel=Travel, total, out, act, grid=Grid, treatment=Treatment, axy_id)

head(axy)

(axy) %>% as_tibble() %>% count(squirrel_id) %>% nrow() #251 individuals
nrow(axy) #25446



#conserved theme across plots
squirrel_theme <- theme_bw() +
    theme(#legend.position = "top",
      axis.line = element_line(colour = "black"),
      panel.grid.major = element_blank(), #eliminates background grid
      panel.grid.minor = element_blank(), #eliminates background grid
      panel.border = element_blank(), #eliminates plot border
      panel.background = element_blank(),
      axis.title.x = element_text(size = 11), 
      axis.title.y = element_text(size = 11),
      axis.text.x = element_text(size = 11, colour = "black"), 
      axis.text.y = element_text(size = 11, colour = "black"), 
      legend.title = element_text(size = 11, colour = "black"), 
      legend.text = element_text(size = 10, colour = "black"))