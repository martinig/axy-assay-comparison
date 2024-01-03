#repeatability estimates for juvenile squirrels sampled more than once for the assay complete dataset (n = 664)
#last edited August 7, 2020 by A. R. Martinig

#run the following prior to running script:
start-up code.R
focal data subsets.R
OFT MIS score generation (n=664).R

juv_assay664<-left_join(personality_664, clean_assay, by=c("squirrel_id"="squirrel_id", "year"="year")) %>%
	filter(ageclass=="J") %>%
	group_by(squirrel_id) %>%
	filter(n()>1) %>%
	select(-c(trialtime, walk, jump, hole, hang, chew, groom, still, front, back, attack, attacklatency, approachlatency)) %>%
	mutate(trialnumber=as.numeric(trialnumber)) %>%
	group_by(cohort, grid)%>%
	mutate(year=ifelse(year==2005, 0,
	            ifelse(year==2009, 4,
	            ifelse(year==2012, 7,
	            ifelse(year==2017, 12,
	            ifelse(year==2018, 13, year))))))%>%
	ungroup() %>%
	group_by(squirrel_id) %>% #convert these variables to among-ind effects 
	mutate(b.assay.local.density= mean(assay.local.density)) %>%
	ungroup()

summary(juv_assay664)

(juv_assay664) %>% as_tibble() %>% dplyr::count(squirrel_id) #62 individuals
summary(juv_assay664$trialnumber)
nrow(juv_assay664) #142

#################################################
#############     Juveniles        ##############
#############       n = 62         ##############
#################################################

#############################
######## OFT1 models ########
#############################

#############################
#non-adjusted repeatability
#############################

m5a<-lmer(OFT1 ~ (1|squirrel_id) + (1| year), data= juv_assay664)
summary(m5a)

plot(m5a)
hist(resid(m5a)) 

sm1<-arm::sim(m5a,1000)
smfixef=sm1@fixef
smranef=sm1@ranef
smfixef=coda::as.mcmc(smfixef)
MCMCglmm::posterior.mode(smfixef)
coda::HPDinterval(smfixef) #potential issue w/trialnumber, grid SU, sexM

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
pop_mean<-mean(juv_assay664$OFT1) 
MCMCglmm::posterior.mode(bvar)/(pop_mean)^2


#############################
#adjusted repeatability
#############################

m5b<-lmer(OFT1 ~ trialnumber + grid + sex + b.assay.local.density + (1|squirrel_id) + (1| year), data= juv_assay664)
summary(m5b)

plot(m5b)
hist(resid(m5b)) 
fligner.test(OFT1~interaction(trialnumber,grid,sex, b.assay.local.density), data= juv_assay664)

sm1<-arm::sim(m5b,1000)
smfixef=sm1@fixef
smranef=sm1@ranef
smfixef=coda::as.mcmc(smfixef)
MCMCglmm::posterior.mode(smfixef)
coda::HPDinterval(smfixef) #potential issue w/trialnumber, grid SU, sexM

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
pop_mean<-mean(juv_assay664$OFT1) 
MCMCglmm::posterior.mode(bvar)/(pop_mean)^2


#############################
######## MIS1 models ########
#############################

#############################
#non-adjusted repeatability
#############################

m6a<-lmer(MIS1 ~ (1|squirrel_id) + (1| year), data= juv_assay664)
summary(m6a)

plot(m6a)
hist(resid(m6a)) 

#for MIS PC1 (i.e. MIS1)
sm2<-arm::sim(m6a,1000)
smfixef2=sm2@fixef
smranef2=sm2@ranef
smfixef2=coda::as.mcmc(smfixef2)
MCMCglmm::posterior.mode(smfixef2)
coda::HPDinterval(smfixef2) 

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
pop_mean<-mean(juv_assay664$MIS1) 
MCMCglmm::posterior.mode(bvar)/(pop_mean)^2


#############################
#adjusted repeatability
#############################

m6b<-lmer(MIS1 ~ trialnumber + grid + sex + b.assay.local.density + (1|squirrel_id) + (1| year), data= juv_assay664)
summary(m6b)

plot(m6b)
hist(resid(m6b)) 
fligner.test(MIS1~interaction(trialnumber,grid,sex, b.assay.local.density), data= juv_assay664)


#for MIS PC1 (i.e. MIS1)
sm2<-arm::sim(m6b,1000)
smfixef2=sm2@fixef
smranef2=sm2@ranef
smfixef2=coda::as.mcmc(smfixef2)
MCMCglmm::posterior.mode(smfixef2)
coda::HPDinterval(smfixef2) 

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
pop_mean<-mean(juv_assay664$MIS1) 
MCMCglmm::posterior.mode(bvar)/(pop_mean)^2