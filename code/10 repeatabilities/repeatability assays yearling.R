#repeatability estimates for yearling squirrels for the assay complete dataset
#original code by A. R. Martinig
#last edited April 21, 2024 by A. R. Martinig


#run the following prior to running script:
#start-up code.R
#PCA Generation Code - Assays.R
#local density (global datasets).R
#familiarity assays (global datasets).R

yearling_assay_all<-left_join(personality_all, clean_assay, by=c("squirrel_id"="squirrel_id", "year"="year")) %>%
	filter(ageclass=="Y") %>% 
	mutate(trialnumber=as.numeric(trialnumber),
		grid_yr=paste(grid, year, sep=""),
		year=year-2005) %>%
	group_by(squirrel_id) %>% #convert these variables to among-ind effects 
	mutate(b.assay.local.density= mean(assay.local.density),		
		b.assay_avg_fam= mean(assay_avg_fam, na.rm=T)) %>%
	ungroup()

summary(yearling_assay_all)

(yearling_assay_all) %>% as_tibble() %>% dplyr::count(squirrel_id) %>% nrow() #209 individuals
summary(yearling_assay_all$trialnumber)
nrow(yearling_assay_all) #257


#########################################
#############     Yearlings         #############
#############      n = 209         ##############
#########################################

#############################
######## OFT1 models ########
#############################

#############################
#non-adjusted repeatability
#############################

m3a<-lmer(OFT1 ~ (1|squirrel_id) + (1| grid_yr), data= yearling_assay_all)
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


#############################
#adjusted repeatability
#############################

m3b<-lmer(OFT1 ~ trialnumber +  sex + b.assay.local.density + b.assay_avg_fam + (1|squirrel_id) + (1| grid_yr), data= yearling_assay_all)
summary(m3b)

plot(m3b)
hist(resid(m3b))

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


#############################
######## OFT2 models ########
#############################

#############################
#non-adjusted repeatability
#############################

m4a<-lmer(OFT2 ~ (1|squirrel_id) + (1| grid_yr), data= yearling_assay_all)
summary(m4a)

plot(m4a) 
hist(resid(m4a))

#for OFT PC2 (i.e. OFT2)
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


#############################
#adjusted repeatability
#############################

m4b<-lmer(OFT2 ~ trialnumber +  sex + b.assay.local.density + b.assay_avg_fam + (1|squirrel_id) + (1| grid_yr), data= yearling_assay_all)
summary(m4b)

plot(m4b) 
hist(resid(m4b))

#for OFT PC2 (i.e. OFT2)
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