#PCA generation for OFT scores
#original code by A. R. Martinig
#last edited April 18, 2024 by A. R. Martinig

#run the following prior to running script:
#start-up code.R
#axy data subsets.R

########################################
#Run assay PCA
########################################

names(assays)
summary(assays)
(assays) %>% as_tibble() %>% count(squirrel_id) %>% nrow() #543 individuals
nrow(assays)

### script for running PCA on select columns from data set
###need to check that you use the right set of behaviours, and that they are formatted as was done previously
##e.g., proportion of time, counts, yes/no, etc.
OFT<-prcomp(assays[c("walk", "jump", "hole", "hang", "chew", "groom", "still")], center=TRUE, scale =TRUE)
summary(OFT)
OFT

#using correlation matrix doesn't change anything :)
#OFT_j<-princomp(assays[c("walk", "jump", "hole", "hang", "chew", "groom", "still")], cor =TRUE)
#summary(OFT_j)
#OFT_j
#print(loadings(OFT_j), cutoff=0)


###script for adding PC1 and PC2 column of OFT to data table
assays$OFT1<-prcomp(~walk +jump+ hole + hang +chew +groom + still, data= assays, center=TRUE, scale =TRUE)$x[,1]
assays$OFT2<-(-1)*prcomp(~walk +jump+ hole + hang +chew +groom + still, data= assays, center=TRUE, scale =TRUE)$x[,2]
############# note I flip the sigh for PC2 so that postive PC2 values mean more exploration #################

OFT_only<-assays%>%select(squirrel_id, OFT1, OFT2, trialnumber)			

personality_all<-assays%>% 
	select(squirrel_id, sex, OFT1, OFT2, observer, ageclass, cohort, year, age, grid, trialnumber, trialdate)

summary(personality_all)

personality_all %>% as_tibble() %>% count(squirrel_id) %>% nrow() #543 individuals
personality_all %>% as_tibble() %>% count(sex) %>% nrow() #2 sexes
nrow(personality_all) #741



########################################
######  extracting summary stats  ######
########################################

#total number of inds and sex stats
other_stats<-personality_all%>%
	group_by(squirrel_id)%>%
	filter(row_number()==1)

(other_stats) %>% as_tibble() %>% count(squirrel_id) %>% nrow() #543 individuals
table(other_stats$sex) #sex number

#ageclass stats
age_class_stats<-personality_all%>%
	group_by(squirrel_id, ageclass)%>%
	filter(row_number()==1)

table(age_class_stats$ageclass) #age class number (remember: some individuals will have multiple records across age classes!)
table(age_class_stats$ageclass, age_class_stats$sex) 


#trial number by age class stats
adults<-personality_all %>% filter(ageclass=="A") %>% group_by(squirrel_id) %>% mutate(sum=n()) %>% filter(row_number()==1)

yearl<-personality_all%>%filter(ageclass=="Y") %>% group_by(squirrel_id) %>% mutate(sum=n()) %>% filter(row_number()==1)

ju<-personality_all%>%filter(ageclass=="J") %>% group_by(squirrel_id) %>% mutate(sum=n()) %>% filter(row_number()==1)

#note: trial number is not reliable for adults or yearlings BECAUSE the count starts with the first trial - which may be during earlier phases (like an adult with 5 trials, could have had 3 of them done as a juvenile) - to get around this, I calculated sums after subsetting

nrow(adults)
table(adults$sum, adults$sex)

nrow(yearl)
table(yearl$sum, yearl$sex)
