#random seven minute sampling

#all the data cleaning is here 
#original code by A. R. Martinig
#Last edited on April 24, 2024 by A. R. Martinig 


########################################
#random seven minute sampling axy data
########################################
  
axy<-read.csv("allaxy_random_7minute_sample.csv", header=T) %>%
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

########################################
#complete axy dataset, n=241 inds
#before merging with personality data
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
    mutate(total_obs=14, 
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
	select(-c(total_obs, total_feeding, total_foraging, total_nestmoving, total_nestnotmoving, total_notmoving, total_travel))

summary(axy1) 
head(axy1)

table(axy1$sex)


########################################
######  extracting summary stats  ######
########################################

(axy1) %>% as_tibble() %>% count(squirrel_id) %>% nrow() #241 individuals
nrow(axy1) #6271

#deployment dates needed to calculate the exact number of sessions
(axy1) %>% as_tibble() %>% count(squirrel_id, axy_yr, axy_month) %>% nrow() #approximately 465 sessions 

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

juvs<-axy1 %>% filter(axy_ageclass =="J") %>% group_by(squirrel_id) %>% mutate(sum=n()) %>% filter(row_number()==1)


#note: trial number is not reliable for adults or yearlings BECAUSE the count starts with the first trial - which may be during earlier phases (like an adult with 5 trials, could have had 3 of them done as a juvenile) - to get around this, I calculated sums after subsetting

nrow(ads)
table(ads$sum, ads$sex)

nrow(yrs)
table(yrs$sum, yrs$sex)

nrow(juvs)
table(juvs$sum, juvs$sex)


########################################
#raw axy data before cleaning
########################################

#for now I am just using Emily's data as I am waiting for Matt to provide me complete records
  
axy<-read.csv("allaxy_random_7minute_sample.csv", header=T) %>%
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