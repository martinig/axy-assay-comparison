#consecutive seven minute sampling

#repeatabilities for axy1 file, yearlings only
##KEEP IN MIND: Some squirrels had axy conducted when they were in different ageclasses (A and Y)
###these squirrels cause imbalances when looking at ageclass summaries
#original code by A. R. Martinig
#last edited April 24, 2024 by A. R. Martinig

#run the following prior to running script:
start-up code.R
axy data subsets.R
PCA generation code - axy.R
local density (global datasets).R
familiarity axy (global datasets).R

#create working dataframe
yearling_axy_all<-left_join(axy1, clean_axy, by=c("squirrel_id"="squirrel_id", "axy_yr"="axy_yr"))%>%
  left_join((tbl(con, "flastall2") %>% select(squirrel_id, grid=gr) %>% collect()), by="squirrel_id") %>% #to bring in the grid information
  filter(axy_ageclass=="Y") %>% 
  mutate(
  		 grid=ifelse(grid=="SUX", "SU", grid),
		grid_yr=paste(grid, axy_yr, sep=""),
		axy_yr=axy_yr-2014) %>%
	group_by(squirrel_id) %>% #convert these variables to among-ind effects    
	mutate(b.axy.local.density=mean(axy.local.density),
		b.axy_avg_fam=mean(axy_avg_fam, na.rm=T)) %>%
	ungroup()

summary(yearling_axy_all)

(yearling_axy_all) %>% as_tibble() %>% dplyr::count(squirrel_id) %>% nrow() #86 individuals
(yearling_axy_all) %>% as_tibble() %>% dplyr::count(squirrel_id, axy_yr, axy_date) %>% nrow() #1764 deployment days
nrow(yearling_axy_all) #1792 records

########################################
#############     Yearlings        #############
#############      n = 86         ##############
########################################

#############################
######## PC1 models ########
#############################

#############################
#non-adjusted repeatability
#############################

m1a<-lmer(PC1 ~ (1|squirrel_id) + (1|grid_yr), data=yearling_axy_all)
summary(m1a)

plot(m1a)
hist(resid(m1a))

#for axy PC1
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

m1b<-lmer(PC1 ~ sex + b.axy.local.density + b.axy_avg_fam + (1|squirrel_id) + (1|grid_yr), data=yearling_axy_all)
summary(m1b)

plot(m1b)
hist(resid(m1b))

#for axy PC1
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
######## PC2 models ########
#############################

#############################
#non-adjusted repeatability
#############################

m2a<-lmer(PC2 ~ (1|squirrel_id) + (1|grid_yr), data=yearling_axy_all)
summary(m2a)

plot(m2a)
hist(resid(m2a))

#for axy PC2
sm2<-arm::sim(m2a,1000)
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


#############################
#adjusted repeatability
#############################

m2b<-lmer(PC2 ~ sex + b.axy.local.density + b.axy_avg_fam + (1|squirrel_id) + (1|grid_yr), data=yearling_axy_all)
summary(m2b)

plot(m2b)
hist(resid(m2b))

#for axy PC2
sm2<-arm::sim(m2b,1000)
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