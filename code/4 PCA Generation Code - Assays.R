#PCA generation for OFT and MIS scores
#using all assays; OFT n = 809; MIS n = 791, and then subsetting for the final dataset (n=67)
#last edited Feb 13, 2024 by A. R. Martinig

#run the following prior to running script:
start-up code.R
axy data subsets.R

########################################
#Run assay PCA
########################################

OFT<-assays%>%filter(!is.na(walk))

names(OFT)
summary(OFT)
(OFT) %>% as_tibble() %>% count(squirrel_id) %>% nrow() #809 individuals
nrow(OFT)

### script for running PCA on select columns from data set
###need to check that you use the right set of behaviours, and that they are formatted as was done previously
##e.g., proportion of time, counts, yes/no, etc.
OFT_j<-prcomp(OFT[c("walk", "jump", "hole", "hang", "chew", "groom", "still")], center=TRUE, scale =TRUE)
summary(OFT_j)
OFT_j

#using correlation matrix doesn't change anything :)
#OFT_j<-princomp(OFT[c("walk", "jump", "hole", "hang", "chew", "groom", "still")], cor =TRUE)
#summary(OFT_j)
#OFT_j
#print(loadings(OFT_j), cutoff=0)


###script for adding PC1 and PC2 column of OFT to data table
OFT$OFT1<-prcomp(~walk +jump+ hole + hang +chew +groom + still, data=OFT, center=TRUE, scale =TRUE)$x[,1]
OFT$OFT2<-prcomp(~walk +jump+ hole + hang +chew +groom + still, data=OFT, center=TRUE, scale =TRUE)$x[,2]

OFT_only<-OFT%>%select(squirrel_id, OFT1, OFT2, trialnumber)			

personality_all<-assays%>%
	group_by(squirrel_id, trialnumber)%>%
	filter(row_number()==1) %>%
	left_join(OFT_only, by=c("squirrel_id", "trialnumber")) %>% 
	select(squirrel_id, sex, OFT1, OFT2, observer, ageclass, cohort, year, age, grid, trialnumber, trialdate)

summary(personality_all)
head(personality_all)

personality_all%>%filter(is.na(age))
	
personality_all %>% as_tibble() %>% count(squirrel_id) %>% nrow() #822 individuals
personality_all %>% as_tibble() %>% count(sex) %>% nrow() #2 sexes
nrow(personality_all) #1184

########################################
# merged axy and personality dataset
# n=67
########################################

personality_67<-assays%>%
	left_join(OFT_only, by=c("squirrel_id", "trialnumber")) %>% 
	inner_join(axy1, by=c("squirrel_id"="squirrel_id",  "sex"="sex")) %>%
	group_by(squirrel_id, trialnumber)%>%
	filter(row_number()==1) %>%
	ungroup() %>%
	select(squirrel_id, OFT1, OFT2, observer, ageclass, cohort, year, age, grid, trialnumber, trialdate)
	
summary(personality_67)	
head(personality_67)	
personality_67 %>% as_tibble() %>% count(squirrel_id) %>% nrow() #67 individuals


########################################
# merged axy and personality dataset
# filtered for matching ageclass, n=52
########################################

personality_52<-assays%>%
	left_join(OFT_only, by=c("squirrel_id", "trialnumber")) %>% 
	inner_join(axy1, by=c("ageclass"="axy_ageclass", "squirrel_id"="squirrel_id", "sex"="sex")) %>%
	group_by(squirrel_id, trialnumber)%>%
	filter(row_number()==1) %>%
	ungroup() %>%
	select(squirrel_id, OFT1, OFT2, observer, ageclass, sex, cohort, year, age, grid, trialnumber, trialdate)
	
summary(personality_52)	
head(personality_52)	
personality_52 %>% as_tibble() %>% count(squirrel_id) %>% nrow() #52 individuals	