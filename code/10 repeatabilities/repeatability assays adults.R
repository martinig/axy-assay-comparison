#repeatability estimates for adult squirrels for the assay complete dataset
#original code by A. R. Martinig
#last edited April 21, 2024 by A. R. Martinig

#run the following prior to running script:
#start-up code.R
#PCA Generation Code - Assays.R
#local density (global datasets).R
#familiarity assays (global datasets).R

adult_assay_all<-left_join(personality_all, clean_assay, by=c("squirrel_id"="squirrel_id", "year"="year")) %>%
	filter(ageclass=="A") %>% 
	mutate(trialnumber=as.numeric(trialnumber),
		grid_yr=paste(grid, year, sep=""),
		year=year-2005) %>%
	ungroup() %>%	
	group_by(squirrel_id) %>% #convert these variables to among-ind effects 
	mutate(b.assay.local.density= mean(assay.local.density),		
		b.assay_avg_fam= mean(assay_avg_fam, na.rm=T)) %>%
	ungroup()
  	  
summary(adult_assay_all)

(adult_assay_all) %>% as_tibble() %>% dplyr::count(squirrel_id) %>% nrow() #367 individuals
summary(adult_assay_all$trialnumber)
nrow(adult_assay_all) #484
	
	

	
#########################################
#############       Adults         ##############
#############      n = 367         ##############
#########################################

#############################
######## OFT1 models ########
#############################

#############################
#non-adjusted repeatability
#############################

m1a<-lmer(OFT1 ~ (1|squirrel_id) + (1| grid_yr), data=adult_assay_all)
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
	



#############################
#adjusted repeatability
#############################

m1b<-lmer(OFT1 ~ trialnumber + sex + b.assay.local.density + b.assay_avg_fam + (1|squirrel_id) + (1|grid_yr), data=adult_assay_all)
summary(m1b)

plot(m1b) 
hist(resid(m1b))

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
	


#############################
######## OFT2 models ########
#############################

#############################
#non-adjusted repeatability
#############################

m2a<-lmer(OFT2 ~ (1|squirrel_id) + (1|grid_yr), data= adult_assay_all)
summary(m2a)

plot(m2a) 
hist(resid(m2a))

#for OFT PC2 (i.e. OFT2)
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



#############################
#adjusted repeatability
#############################

m2b<-lmer(OFT2 ~ trialnumber + sex + b.assay.local.density + b.assay_avg_fam + (1|squirrel_id) + (1| grid_yr), data=adult_assay_all)
summary(m2b)

plot(m2b) 
hist(resid(m2b))

#for OFT PC2 (i.e. OFT2)
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