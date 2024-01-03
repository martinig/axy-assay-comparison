#PCA generation for OFT and MIS scores
#using all assays; OFT n = 809; MIS n = 791, and then subsetting for the final dataset (n=60)
#last edited Nov 3, 2023 by A. R. Martinig

#run the following prior to running script:
start-up code.R
axy data subsets.R

########################################
#Run assay PCA
########################################

OFT<-assays%>%filter(!is.na(walk))
#MIS<-assays%>%filter(!is.na(front), !is.na(attacklatency))

names(OFT)
summary(OFT)
(OFT) %>% as_tibble() %>% count(squirrel_id) %>% nrow() #809 individuals
nrow(OFT)

#names(MIS) 
#summary(MIS)
#(MIS) %>% as_tibble() %>% count(squirrel_id) %>% nrow() #791 individuals
#nrow(MIS)

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

#aggression trials
#MIS_max_j<-prcomp(MIS[c("front", "back", "attack", "attacklatency", "approachlatency")], center=TRUE, scale =TRUE)
#summary(MIS_max_j)
#MIS_max_j

###script for adding PC1 column of MIS to data table
#MIS$MIS1<-prcomp(~front + back + attack + attacklatency + approachlatency, data=MIS, center=TRUE, scale =TRUE)$x[,1]

OFT_only<-OFT%>%select(squirrel_id, OFT1, OFT2, trialnumber)			
#MIS_only<-MIS%>%select(squirrel_id, MIS1, trialnumber)			

#personality<-full_join(OFT_only, MIS_only, by=c("squirrel_id", "trialnumber"))

personality_all<-assays%>%
	group_by(squirrel_id, trialnumber)%>%
	filter(row_number()==1) %>%
	left_join(OFT_only, by=c("squirrel_id", "trialnumber")) %>% 
	select(-videodate,-trialtime)
	
personality_all %>% as_tibble() %>% count(squirrel_id) %>% nrow() #810 individuals
personality_all %>% as_tibble() %>% count(sex) %>% nrow() #2 sexes
nrow(personality_all)

########################################
# merged axy and personality dataset
# n=60
########################################

personality_60<-assays%>%
	left_join(OFT_only, by=c("squirrel_id", "trialnumber")) %>% 
	inner_join(axy1, by=c("squirrel_id"="squirrel_id",  "sex"="sex")) %>%
	group_by(squirrel_id, trialnumber)%>%
	filter(row_number()==1) %>%
	ungroup() %>%
	select(squirrel_id, OFT1, OFT2, observer, ageclass, cohort, year, age, grid=grid.x, trialnumber, trialdate, walk, jump, hole, hang, chew, groom, still, front, back, attack, attacklatency, approachlatency)
	
summary(personality_60)	
head(personality_60)	
personality_60 %>% as_tibble() %>% count(squirrel_id) %>% nrow() #60 individuals


########################################
# merged axy and personality dataset
# filtered for matching ageclass, n=46
########################################

personality_46<-assays%>%
	left_join(OFT_only, by=c("squirrel_id", "trialnumber")) %>% 
	inner_join(axy1, by=c("ageclass"="axy_ageclass", "squirrel_id"="squirrel_id", "sex"="sex")) %>%
	group_by(squirrel_id, trialnumber)%>%
	filter(row_number()==1) %>%
	ungroup() %>%
	select(squirrel_id, OFT1, OFT2, observer, ageclass, sex, cohort, year, age, grid=grid.x, trialnumber, trialdate, walk, jump, hole, hang, chew, groom, still, front, back, attack, attacklatency, approachlatency)
	
summary(personality_46)	
head(personality_46)	
personality_46 %>% as_tibble() %>% count(squirrel_id) %>% nrow() #46 individuals	