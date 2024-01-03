#repeatability estimates for adult squirrels sampled more than once for the assay complete dataset (n = 664)
#last edited November 4, 2021 by A. R. Martinig

#run the following prior to running script:
start-up code.R
focal data subsets.R
PCA Generation Code - Focals.R
PCA Generation Code - Assays.R
local density (global datasets).R
familiarity assays (global datasets).R
familiarity focals (global datasets).R

library(lme4)

adult_assay664<-left_join(personality_664, clean_assay, by=c("squirrel_id"="squirrel_id", "year"="year")) %>%
	filter(ageclass=="A") %>%
	group_by(squirrel_id) %>%
	filter(n()>1) %>% 
	select(-c(trialtime, walk, jump, hole, hang, chew, groom, still, front, back, attack, attacklatency, approachlatency)) %>%
	mutate(trialnumber=as.numeric(trialnumber)) %>%
	group_by(cohort, grid)%>%
	mutate(year=ifelse(year==2005, 0,
	            ifelse(year==2008, 3,
	            ifelse(year==2009, 4,
	            ifelse(year==2010, 5,
	            ifelse(year==2012, 7,
	            ifelse(year==2013, 8,
	            ifelse(year==2014, 9,
	            ifelse(year==2015, 10,
	            ifelse(year==2017, 12,
	            ifelse(year==2018, 13,
	            ifelse(year==2019, 14, year)))))))))))) %>%
	ungroup() %>%	
	group_by(squirrel_id) %>% #convert these variables to among-ind effects 
	mutate(b.assay.local.density= mean(assay.local.density),		b.assay_avg_fam= mean(assay_avg_fam, na.rm=T)) %>%
	ungroup()
  	  
summary(adult_assay664)

(adult_assay664) %>% as_tibble() %>% dplyr::count(squirrel_id) #103 individuals
summary(adult_assay664$trialnumber)
nrow(adult_assay664) #217
	
#################################################
#############       Adults         ##############
#############      n = 103         ##############
#################################################

#############################
######## OFT1 models ########
#############################

#############################
#non-adjusted repeatability
#############################

m1a<-lmer(OFT1 ~ (1|squirrel_id) + (1| year), data=adult_assay664)
summary(m1a)

plot(m1a) 
hist(resid(m1a))

#for OFT PC1 (i.e. OFT1)
sm1<-arm::sim(m1a,1000)
smfixef=sm1@fixef
smranef=sm1@ranef
smfixef=coda::as.mcmc(smfixef)
MCMCglmm::posterior.mode(smfixef)
coda::HPDinterval(smfixef)

##among-individual variance
bID<-sm1@ranef$squirrel_id
bvar<-as.vector(apply(bID, 1, var)) ##between individual variance posterior distribution
bvar<-coda::as.mcmc(bvar)
MCMCglmm::posterior.mode(bvar) ## mode of the distribution
coda::HPDinterval(bvar)

##residual variance
rvar<-sm1@sigma^2
rvar<-coda::as.mcmc(rvar)
MCMCglmm::posterior.mode(rvar)
coda::HPDinterval(rvar)

##repeatability
rID<-bvar/(bvar+rvar)
MCMCglmm::posterior.mode(rID)
coda::HPDinterval(rID)
	
#mean-standardization (divide the amongindividual variation in a trait by the square of its mean)	
pop_mean<-mean(adult_assay664$OFT1) 
MCMCglmm::posterior.mode(bvar)/(pop_mean)^2



#############################
#adjusted repeatability
#############################

m1b<-lmer(OFT1 ~ trialnumber + grid + sex + b.assay.local.density + b.assay_avg_fam + (1|squirrel_id) + (1| year), data=adult_assay664)
summary(m1b)

plot(m1b) 
hist(resid(m1b))
fligner.test(OFT1~interaction(trialnumber,grid,sex,b.assay.local.density,b.assay_avg_fam), data= adult_assay664)


#for OFT PC1 (i.e. OFT1)
sm1<-arm::sim(m1b,1000)
smfixef=sm1@fixef
smranef=sm1@ranef
smfixef=coda::as.mcmc(smfixef)
MCMCglmm::posterior.mode(smfixef)
coda::HPDinterval(smfixef)

##among-individual variance
bID<-sm1@ranef$squirrel_id
bvar<-as.vector(apply(bID, 1, var)) ##between individual variance posterior distribution
bvar<-coda::as.mcmc(bvar)
MCMCglmm::posterior.mode(bvar) ## mode of the distribution
coda::HPDinterval(bvar)

##residual variance
rvar<-sm1@sigma^2
rvar<-coda::as.mcmc(rvar)
MCMCglmm::posterior.mode(rvar)
coda::HPDinterval(rvar)

##repeatability
rID<-bvar/(bvar+rvar)
MCMCglmm::posterior.mode(rID)
coda::HPDinterval(rID)
	
#mean-standardization (divide the amongindividual variation in a trait by the square of its mean)	
pop_mean<-mean(adult_assay664$OFT1) 
MCMCglmm::posterior.mode(bvar)/(pop_mean)^2


#############################
######## MIS1 models ########
#############################

#############################
#non-adjusted repeatability
#############################

m2a<-lmer(MIS1 ~ (1|squirrel_id) + (1| year), data= adult_assay664)
summary(m2a)

plot(m2a) 
hist(resid(m2a))

#for MIS PC1 (i.e. MIS1)
sm2<-arm::sim(m2a,1000)
smfixef2=sm2@fixef
smranef2=sm2@ranef
smfixef2=coda::as.mcmc(smfixef2)
MCMCglmm::posterior.mode(smfixef2)
coda::HPDinterval(smfixef2) #potential issues with trialnumber; gridJO

##among-individual variance
bID2<-sm2@ranef$squirrel_id
bvar2<-as.vector(apply(bID2, 1, var)) ##between individual variance posterior distribution
bvar2<-coda::as.mcmc(bvar2)
MCMCglmm::posterior.mode(bvar2) ## mode of the distribution
coda::HPDinterval(bvar2)

##residual variance
rvar2<-sm2@sigma^2
rvar2<-coda::as.mcmc(rvar2)
MCMCglmm::posterior.mode(rvar2)
coda::HPDinterval(rvar2)

##repeatability
rID2<-bvar2/(bvar2+rvar2)
MCMCglmm::posterior.mode(rID2)
coda::HPDinterval(rID2)

#mean-standardization (divide the amongindividual variation in a trait by the square of its mean)	
pop_mean<-mean(adult_assay664$MIS1) 
MCMCglmm::posterior.mode(bvar)/(pop_mean)^2


#############################
#adjusted repeatability
#############################

m2b<-lmer(MIS1 ~ trialnumber + grid + sex + b.assay.local.density + b.assay_avg_fam + (1|squirrel_id) + (1| year), data=adult_assay664)
summary(m2b)

plot(m2b) 
hist(resid(m2b))
fligner.test(MIS1~interaction(trialnumber,grid,sex,b.assay.local.density,b.assay_avg_fam), data= adult_assay664)

#for MIS PC1 (i.e. MIS1)
sm2<-arm::sim(m2b,1000)
smfixef2=sm2@fixef
smranef2=sm2@ranef
smfixef2=coda::as.mcmc(smfixef2)
MCMCglmm::posterior.mode(smfixef2)
coda::HPDinterval(smfixef2) #potential issues with trialnumber; gridJO

##among-individual variance
bID2<-sm2@ranef$squirrel_id
bvar2<-as.vector(apply(bID2, 1, var)) ##between individual variance posterior distribution
bvar2<-coda::as.mcmc(bvar2)
MCMCglmm::posterior.mode(bvar2) ## mode of the distribution
coda::HPDinterval(bvar2)

##residual variance
rvar2<-sm2@sigma^2
rvar2<-coda::as.mcmc(rvar2)
MCMCglmm::posterior.mode(rvar2)
coda::HPDinterval(rvar2)

##repeatability
rID2<-bvar2/(bvar2+rvar2)
MCMCglmm::posterior.mode(rID2)
coda::HPDinterval(rID2)

#mean-standardization (divide the amongindividual variation in a trait by the square of its mean)	
pop_mean<-mean(adult_assay664$MIS1) 
MCMCglmm::posterior.mode(bvar)/(pop_mean)^2