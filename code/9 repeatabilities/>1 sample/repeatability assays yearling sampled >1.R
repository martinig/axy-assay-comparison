#repeatability estimates for yearling squirrels sampled more than once for the assay complete dataset (n = 664)
#last edited November 4, 2021 by A. R. Martinig

#run the following prior to running script:
start-up code.R
focal data subsets.R
PCA Generation Code - Assays.R

yearling_assay664<-left_join(personality_664, clean_assay, by=c("squirrel_id"="squirrel_id", "year"="year")) %>%
	filter(ageclass=="Y") %>%
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
	            ifelse(year==2018, 13,
	            ifelse(year==2019, 14, year)))))))))))%>%
	ungroup() %>%
	group_by(squirrel_id) %>% #convert these variables to among-ind effects 
	mutate(b.assay.local.density= mean(assay.local.density),		b.assay_avg_fam= mean(assay_avg_fam, na.rm=T)) %>%
	ungroup()

summary(yearling_assay664)

(yearling_assay664) %>% as_tibble() %>% dplyr::count(squirrel_id) #47 individuals
summary(yearling_assay664$trialnumber)
nrow(yearling_assay664) #139


#################################################
#############     Yearlings         #############
#############       n = 47         ##############
#################################################

#############################
######## OFT1 models ########
#############################

#############################
#non-adjusted repeatability
#############################

m3a<-lmer(OFT1 ~ (1|squirrel_id) + (1| year), data= yearling_assay664)
summary(m3a)

plot(m3a) 
hist(resid(m3a))

#for OFT PC1 (i.e. OFT1)
sm1<-arm::sim(m3a,1000)
smfixef=sm1@fixef
smranef=sm1@ranef
smfixef=coda::as.mcmc(smfixef)
MCMCglmm::posterior.mode(smfixef)
coda::HPDinterval(smfixef) #potential issue w/grid RR, SU, SUX

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
pop_mean<-mean(yearling_assay664$OFT1) 
MCMCglmm::posterior.mode(bvar)/(pop_mean)^2


#############################
#adjusted repeatability
#############################

m3b<-lmer(OFT1 ~ trialnumber + grid + sex + b.assay.local.density + b.assay_avg_fam + (1|squirrel_id) + (1|year), data= yearling_assay664)
summary(m3b)

plot(m3b)
hist(resid(m3b))
fligner.test(OFT1~interaction(trialnumber,grid,sex,b.assay.local.density,b.assay_avg_fam), data= yearling_assay664) 

#for OFT PC1 (i.e. OFT1)
sm1<-arm::sim(m3b,1000)
smfixef=sm1@fixef
smranef=sm1@ranef
smfixef=coda::as.mcmc(smfixef)
MCMCglmm::posterior.mode(smfixef)
coda::HPDinterval(smfixef) #potential issue w/grid RR, SU, SUX

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
pop_mean<-mean(yearling_assay664$OFT1) 
MCMCglmm::posterior.mode(bvar)/(pop_mean)^2


#############################
######## MIS1 models ########
#############################

#############################
#non-adjusted repeatability
#############################

m4a<-lmer(MIS1 ~ (1|squirrel_id) + (1| year), data= yearling_assay664)
summary(m4a)

plot(m4a) 
hist(resid(m4a))

#for MIS PC1 (i.e. MIS1)
sm2<-arm::sim(m4a,1000)
smfixef2=sm2@fixef
smranef2=sm2@ranef
smfixef2=coda::as.mcmc(smfixef2)
MCMCglmm::posterior.mode(smfixef2)
coda::HPDinterval(smfixef2) #potential issues w/ grid RR

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
pop_mean<-mean(yearling_assay664$MIS1) 
MCMCglmm::posterior.mode(bvar)/(pop_mean)^2

#############################
#adjusted repeatability
#############################

m4b<-lmer(MIS1 ~ trialnumber + grid + sex + b.assay.local.density + b.assay_avg_fam + (1|squirrel_id) + (1| year), data= yearling_assay664)
summary(m4b)

plot(m4b) 
hist(resid(m4b))
fligner.test(MIS1~interaction(trialnumber,grid,sex,b.assay.local.density,b.assay_avg_fam), data= yearling_assay664) 

#for MIS PC1 (i.e. MIS1)
sm2<-arm::sim(m4b,1000)
smfixef2=sm2@fixef
smranef2=sm2@ranef
smfixef2=coda::as.mcmc(smfixef2)
MCMCglmm::posterior.mode(smfixef2)
coda::HPDinterval(smfixef2) #potential issues w/ grid RR

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
pop_mean<-mean(yearling_assay664$MIS1) 
MCMCglmm::posterior.mode(bvar)/(pop_mean)^2