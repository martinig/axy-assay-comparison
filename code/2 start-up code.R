#all the data cleaning is here 
#original code by A. R. Martinig
#last edited on April 17, 2024 by A. R. Martinig 

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

########################################
#raw axy data before cleaning
########################################
  
axy<-read.csv("KRSP_sqr_axy_all_2014_2022_dailybyTOD.csv", header=T) %>%
	mutate(
		axy_id=paste(id, date, tod, sep = "-"), 
		axy_date=ymd(date),
		axy_yr=year(date),
		axy_month=month(date)) %>%
	filter(!is.na(id)) %>% #remove the rows with NA for squirrel_id
	select(squirrel_id= id, axy_date, axy_yr, axy_month, tod, feed=Feed, forage=Forage, nestmove=NestMove, nestnotmove=NestNotMove, notmoving=NotMoving, travel=Travel, total=Total, axy_id)

head(axy)
summary(axy)

(axy) %>% as_tibble() %>% count(squirrel_id) %>% nrow() #340 individuals
nrow(axy) #38284



########################################
#complete axy dataset, n=340 inds
########################################

axy1<- dplyr::left_join(axy, birth, by=c("squirrel_id")) %>%
  group_by(axy_id, squirrel_id) %>%
  mutate(
    axy_age = axy_yr-byear, #calc age
    axy_ageclass = ifelse(axy_age==1, "Y", 
                          ifelse(axy_age >1, "A",
                                 ifelse(axy_age < 1, "J", "")))) %>% #creating age class  
  ungroup() %>%   	      	
  ##group by squirrel id and axy id (treats each axy as behavior trial) 
  group_by(squirrel_id, axy_id) %>%
  mutate(total_obs=sum(total), 
         #get the totals for each behaviour
         total_feeding=sum(feed),
         total_foraging=sum(forage),
         total_nestmoving=sum(nestmove),
         total_nestnotmoving=sum(nestnotmove),
         total_notmoving =sum(notmoving),
         total_travel=sum(travel),
         #calc the proportions
         prop_feeding=(total_feeding/total_obs),
         prop_foraging=(total_foraging/total_obs),
         prop_nestmoving =(total_nestmoving/total_obs),
         prop_nestnotmoving =(total_nestnotmoving/total_obs),
         prop_notmoving =(total_notmoving/total_obs),
         prop_travel=(total_travel/total_obs)) %>%
  filter(row_number()==1, #keep only one row
         !axy_ageclass=="J") %>% #remove the 1 male juvenile
  ungroup() %>%
  droplevels() %>%
  select(-c(total, total_obs, total_feeding, total_foraging, total_nestmoving, total_nestnotmoving, total_notmoving, total_travel))

summary(axy1) 
head(axy1)


########################################
######  extracting summary stats  ######
########################################

(axy1) %>% as_tibble() %>% count(squirrel_id) %>% nrow() #340 individuals
nrow(axy1) #38280 records

#deployment dates needed to calculate the exact number of sessions
(axy1) %>% as_tibble() %>% count(squirrel_id, axy_date) %>% nrow() #approximately 9627 deployment days 

#year range
table(axy1$axy_yr)

#sex stats
stats3<-axy1%>%group_by(squirrel_id)%>%filter(row_number()==1)
table(stats3$sex)

#ageclass stats
stats4<-axy1%>%group_by(squirrel_id, axy_ageclass)%>%filter(row_number()==1)
table(stats4$axy_ageclass)
table(stats4$axy_ageclass, stats4$sex) 

#ageclass stats
stats5<-axy1%>%group_by(squirrel_id, axy_ageclass)
table(stats5$axy_ageclass) 

#observers
#obs<-axy1%>%group_by(axy_id)%>%filter(row_number()==1) %>% group_by(f_observer) %>% mutate(sum=n())
#table(obs$f_observer)
#summary(obs$sum)

#trial number by age class stats
ads<-axy1 %>% filter(axy_ageclass =="A") %>% group_by(squirrel_id) %>% mutate(sum=n()) %>% filter(row_number()==1)

yrs<-axy1 %>% filter(axy_ageclass =="Y") %>% group_by(squirrel_id) %>% mutate(sum=n()) %>% filter(row_number()==1)

#note: trial number is not reliable for adults or yearlings BECAUSE the count starts with the first trial - which may be during earlier phases (like an adult with 5 trials, could have had 3 of them done as a juvenile) - to get around this, I calculated sums after subsetting

nrow(ads)
table(ads$sum, ads$sex)

nrow(yrs)
table(yrs$sum, yrs$sex)



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