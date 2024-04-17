
########################################
# merged axy and personality dataset
# n=67
########################################

axy2<- dplyr::inner_join(assays, axy1, by=c("squirrel_id", "sex")) %>%
  group_by(axy_id)%>%
  filter(row_number()==1) %>%
  droplevels()%>%
  select(-c(videodate, trialtime)) %>%
  ungroup()

head(axy2)

########################################
######  extracting summary stats  ######
########################################

(axy2) %>% as_tibble() %>% count(squirrel_id) %>% nrow() #67 individuals
nrow(axy2) #8678

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
# filtered for matching ageclass, n=52
########################################

#run filtered by ageclass
axy_ageclass <- dplyr::inner_join(assays, axy1, by=c("ageclass"="axy_ageclass", "squirrel_id"="squirrel_id", "sex"="sex")) %>%
  droplevels()%>%
  group_by(axy_id)%>%
  filter(row_number()==1) %>%
  select(-c(videodate, trialtime)) %>%
  ungroup()

head(axy_ageclass)

########################################
######  extracting summary stats  ######
########################################

(axy_ageclass) %>% as_tibble() %>% count(squirrel_id) %>% nrow() #52 individuals
nrow(axy_ageclass) #6722

table(axy_ageclass $axy_yr)

stats7<-axy_ageclass%>%group_by(squirrel_id)%>%filter(row_number()==1)
table(stats7$sex)

stats8<-axy_ageclass%>%group_by(squirrel_id, ageclass)%>%filter(row_number()==1)
table(stats8$ageclass) #assay ageclass = axy ageclass



########################################
# merged axy and personality dataset
# filtered for matching AGE, n=17
########################################

#Run filtered by age 
axy_age <- dplyr::inner_join(assays, axy1, by=c("ageclass"="axy_ageclass", "age"="axy_age", "squirrel_id"="squirrel_id", "sex"="sex")) %>%
  droplevels()%>%
  group_by(axy_id)%>%
  filter(row_number()==1) %>%
  select(-c(videodate, trialtime)) %>%
  ungroup()
  
head(axy_age)  
  
########################################
######  extracting summary stats  ######
########################################

(axy_age) %>% as_tibble() %>% count(squirrel_id) %>% nrow() #17 individuals
nrow(axy_age) #1860

table(axy_age $axy_yr)

stats9<-axy_age%>%group_by(squirrel_id)%>%filter(row_number()==1)
table(stats9$sex)

stats10<-axy_age%>%group_by(squirrel_id, ageclass)%>%filter(row_number()==1)
table(stats10$ageclass) #assay ageclass = axy ageclass