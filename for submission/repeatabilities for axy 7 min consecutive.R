#code to obtain the repeatabilities for the accelerometry data after subsetting full dataset to 7 minutes of consecutive samples per day per squirrel
#original code by A. R. Martinig
#last edited on May 7, 2024 by A. R. Martinig 


#########################################################
#############                           Axy                            #############
#############      7 minutes consecutive subset         #############
#########################################################

options(scipen=999, dplyr.width = Inf, tibble.print_min = 50, repos='http://cran.rstudio.com/') #scipen forces outputs to not be in scientific notation #dplyr.width will show all columns for head() function and tibble.print_min sets how many rows are printed and repos sets the cran mirror

#load libraries
pacman::p_load(
				ggplot2, 
				ggstatsplot,
               dplyr, 
               lubridate, 
               tidyverse,   
               broom,  
               FSA,      
               glmmTMB,  
               lme4,   
               tidyr,      
               DescTools,
               scales,
               ggpubr,
               grid,
               lattice,
               sjPlot,
               sjlabelled,
               sjmisc,
               cowplot, 
               broom.mixed,
               ggforce, 
               gridGraphics,
               ggeffects,
               magrittr,
               MCMCglmm,
               data.table,
               lattice,
               stats,
               cowplot,
               krsp
)


select<-dplyr::select
filter<-dplyr::filter


##############################
#  calculating repeatabilities  #
##############################
	
#########################################
#############       Adults         ##############
#############      n = 177         ##############
#########################################	
	
#create working dataframe
adult_axy_all<-read.csv("dataset_consecutive.csv", header=T) %>% 
  filter(axy_ageclass =="A") %>% 
  mutate(
  		grid=ifelse(grid=="SUX", "SU", grid),
		grid_yr=paste(grid, axy_yr, sep=""),
		axy_yr = axy_yr-2014) %>%
	group_by(squirrel_id) %>% #convert these variables to among-ind effects    
	mutate(b.axy.local.density=mean(axy.local.density),
		b.axy_avg_fam=mean(axy_avg_fam, na.rm=T)) %>%
	ungroup()
	
summary(adult_axy_all)

(adult_axy_all) %>% as_tibble() %>% dplyr::count(squirrel_id) %>% nrow() #177 adults
(adult_axy_all) %>% as_tibble() %>% dplyr::count(squirrel_id, axy_yr, axy_date) %>% nrow() #4507 deployment days
nrow(adult_axy_all) #4507 records


#############################
######## PC1 models ########
#############################

#############################
#non-adjusted repeatability
#############################

m1a<-lmer(PC1 ~ (1|squirrel_id) + (1|grid_yr), data=adult_axy_all)
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

m1b<-lmer(PC1 ~ sex + b.axy.local.density + b.axy_avg_fam + (1|squirrel_id) + (1|grid_yr), data=adult_axy_all)
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

m2a<-lmer(PC2 ~ (1|squirrel_id) + (1|grid_yr), data=adult_axy_all)
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

m2b<-lmer(PC2 ~ sex + b.axy.local.density + b.axy_avg_fam + (1|squirrel_id) + (1|grid_yr), data=adult_axy_all)
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



########################################
#############     Yearlings        #############
#############      n = 86         ##############
########################################

#create working dataframe
yearling_axy_all<-read.csv("dataset_consecutive.csv", header=T) %>% 
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