#axy data for the complete dataset
#last edited Nov 10, 2023 by A. R. Martinig

#run the following prior to running script:
start-up code.R

########################################
#complete axy dataset, n=250 inds
#before merging with personality data
########################################

axy1<- dplyr::inner_join(axy, birth, by=c("squirrel_id")) %>%
	group_by(axy_id, squirrel_id) %>%
  	mutate(
  		axy_age = axy_yr-byear, #calc age
  		axy_ageclass = ifelse(axy_age==1, "Y", 
  			ifelse(axy_age >1, "A",
        	ifelse(axy_age < 1, "J", "")))) %>% #creating age class  
     ungroup() %>%   	      	
     ##group by squirrel id (all axy are one behavioral trial per individual)
    #group_by(squirrel_id) %>%
    #mutate(total_obs=sum(total), 
         #get the totals for each behaviour
         #overall_total_feeding=sum(feed),
         #overall_total_foraging=sum(forage),
         #overall_total_nestmoveing=sum(nestmove),
         #overall_total_nestnotmoving=sum(nestnotmove),
         #overall_total_notmoving =sum(notmoving),
         #overall_total_travel=sum(travel),
         #calc the proportions
         #overall_prop_feeding=(overall_total_feeding/total_obs),
         #overall_prop_foraging=(overall_total_foraging/total_obs),
         #overall_prop_nestmoveing =(overall_total_nestmoveing/total_obs),
         #overall_prop_nestnotmoving =(overall_total_nestnotmoving/total_obs),
         #overall_prop_notmoving =(overall_total_notmoving/total_obs),
         #overall_prop_travel=(overall_total_travel/total_obs)) %>%
	#ungroup() %>%
##group by squirrel id and axy id (treats each axy as behavior trial) 
	group_by(squirrel_id, axy_id) %>%
    mutate(total_obs=sum(total), 
         #get the totals for each behaviour
         total_feeding=sum(feed),
         total_foraging=sum(forage),
         total_nestmoveing=sum(nestmove),
         total_nestnotmoving=sum(nestnotmove),
         total_notmoving =sum(notmoving),
         total_travel=sum(travel),
         #calc the proportions
         prop_feeding=(total_feeding/total_obs),
         prop_foraging=(total_foraging/total_obs),
         prop_nestmoveing =(total_nestmoveing/total_obs),
         prop_nestnotmoving =(total_nestnotmoving/total_obs),
         prop_notmoving =(total_notmoving/total_obs),
         prop_travel=(total_travel/total_obs)) %>%
	filter(row_number()==1) %>%  #selects first row!
  ungroup() %>%
  droplevels() %>%
  select(-c(total_obs, total_feeding, total_foraging, total_nestmoveing, total_nestnotmoving, total_notmoving, total_travel, out, act))

summary(axy1) 
head(axy1)

table(axy1$sex)


########################################
######  extracting summary stats  ######
########################################

(axy1) %>% as_tibble() %>% count(squirrel_id) %>% nrow() #250 individuals
nrow(axy1) #25199

#deployment dates needed to calculate the exact number of sessions
(axy1) %>% as_tibble() %>% count(squirrel_id, axy_yr, axy_month) %>% nrow() #approximately 557 sessions 

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
# merged axy and personality dataset
# n=60
########################################

axy2<- dplyr::inner_join(assays, axy1, by=c("squirrel_id", "sex")) %>%
  group_by(axy_id)%>%
  filter(row_number()==1) %>%
  droplevels()%>%
  select(-c(videodate, trialtime)) %>%
  ungroup()

########################################
######  extracting summary stats  ######
########################################

(axy2) %>% as_tibble() %>% count(squirrel_id) %>% nrow() #60 individuals
nrow(axy2)

table(axy2$axy_yr)

stats5<-axy2%>%group_by(squirrel_id)%>%filter(row_number()==1)
table(stats5$sex)

#assay ageclass
stats6a<-axy2%>%group_by(squirrel_id, ageclass)%>%filter(row_number()==1)
table(stats6a$ageclass)

#axy ageclass
stats6b<-axy2%>%group_by(squirrel_id, axy_ageclass)%>%filter(row_number()==1)
table(stats6b$axy_ageclass)


########################################
# merged axy and personality dataset
# filtered for matching ageclass, n=46
########################################

#run filtered by ageclass
axy_ageclass <- dplyr::inner_join(assays, axy1, by=c("ageclass"="axy_ageclass", "squirrel_id"="squirrel_id", "sex"="sex")) %>%
  droplevels()%>%
  group_by(axy_id)%>%
  filter(row_number()==1) %>%
  select(-c(videodate, trialtime)) %>%
  ungroup()

########################################
######  extracting summary stats  ######
########################################

(axy_ageclass) %>% as_tibble() %>% count(squirrel_id) %>% nrow() #46 individuals
nrow(axy_ageclass)

table(axy_ageclass $axy_yr)

stats7<-axy_ageclass%>%group_by(squirrel_id)%>%filter(row_number()==1)
table(stats7$sex)

stats8<-axy_ageclass%>%group_by(squirrel_id, ageclass)%>%filter(row_number()==1)
table(stats8$ageclass) #assay ageclass = axy ageclass



########################################
# merged axy and personality dataset
# filtered for matching AGE, n=10
########################################

#Run filtered by age 
axy_age <- dplyr::inner_join(assays, axy1, by=c("ageclass"="axy_ageclass", "age"="axy_age", "squirrel_id"="squirrel_id", "sex"="sex")) %>%
  droplevels()%>%
  group_by(axy_id)%>%
  filter(row_number()==1) %>%
  select(-c(videodate, trialtime)) %>%
  ungroup()
  
########################################
######  extracting summary stats  ######
########################################

(axy_age) %>% as_tibble() %>% count(squirrel_id) %>% nrow() #10 individuals
nrow(axy_age)

table(axy_age $axy_yr)

stats9<-axy_age%>%group_by(squirrel_id)%>%filter(row_number()==1)
table(stats9$sex)

stats10<-axy_age%>%group_by(squirrel_id, ageclass)%>%filter(row_number()==1)
table(stats10$ageclass) #assay ageclass = axy ageclass