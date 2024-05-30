#code to obtain the repeatabilities for the assay and accelerometry data 
#original code by A. R. Martinig
#last edited on May 7, 2024 by A. R. Martinig 


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


###########################################
#############           Assay             #############
#############      Full dataset         #############
###########################################

##############################
#  calculating repeatabilities  #
##############################

#########################################
#############       Adults         ##############
#############      n = 367         ##############
#########################################

adult_assay_all<-read.csv("dataset_complete_assay.csv", header=T)  %>%
	filter(ageclass=="A") %>% 
	mutate(trialnumber=as.numeric(trialnumber),
  		grid=ifelse(grid=="SUX", "SU", grid),
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

#############################
######## OFT1 models ########
#############################

#############################
#non-adjusted repeatability
#############################

m1a<-lmer(OFT1 ~ (1|squirrel_id) + (1|grid_yr), data=adult_assay_all)
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

m2b<-lmer(OFT2 ~ trialnumber + sex + b.assay.local.density + b.assay_avg_fam + (1|squirrel_id) + (1|grid_yr), data=adult_assay_all)
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


#########################################
#############     Yearlings         #############
#############      n = 209         ##############
#########################################


yearling_assay_all<-read.csv("dataset_complete_assay.csv", header=T)  %>%	filter(ageclass=="Y") %>% 
	mutate(trialnumber=as.numeric(trialnumber),
  		grid=ifelse(grid=="SUX", "SU", grid),
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


#############################
######## OFT1 models ########
#############################

#############################
#non-adjusted repeatability
#############################

m3a<-lmer(OFT1 ~ (1|squirrel_id) + (1|grid_yr), data= yearling_assay_all)
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

m3b<-lmer(OFT1 ~  trialnumber +  sex + b.assay.local.density + b.assay_avg_fam + (1|squirrel_id) + (1|grid_yr), data= yearling_assay_all)
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

m4a<-lmer(OFT2 ~ (1|squirrel_id) + (1|year), data= yearling_assay_all) #note that fit is singular when I use grid_yr, so using year for this model only
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

m4b<-lmer(OFT2 ~  trialnumber +  sex + b.assay.local.density + b.assay_avg_fam + (1|squirrel_id) + (1|grid_yr), data= yearling_assay_all)
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


###########################################
#############           Axy                #############
#############      Full dataset         #############
###########################################

##############################
#  calculating repeatabilities  #
##############################


#########################################
#############       Adults         ##############
#############      n = 259         ##############
#########################################

#create working dataframe
adult_axy_all<-read.csv("dataset_complete_axy.csv", header=T)  %>%
  filter(axy_ageclass=="A") %>% 
  mutate(
  		grid=ifelse(grid=="SUX", "SU", grid),
		grid_yr=paste(grid, axy_yr, sep=""),
		axy_yr=axy_yr-2014) %>%
	group_by(squirrel_id) %>% #convert these variables to among-ind effects    
	mutate(b.axy.local.density=mean(axy.local.density),
		b.axy_avg_fam=mean(axy_avg_fam, na.rm=T)) %>%
	ungroup()
	
	
summary(adult_axy_all)

(adult_axy_all) %>% as_tibble() %>% dplyr::count(squirrel_id) %>% nrow() #259 adults
(adult_axy_all) %>% as_tibble() %>% dplyr::count(squirrel_id, axy_yr, axy_date) %>% nrow() #7334 deployment days
nrow(adult_axy_all) #29144 records

#############################
######## PC1 models ########
#############################

#############################
#non-adjusted repeatability
#############################

m1a<-lmer(PC1 ~ (1|squirrel_id) + (1|grid_yr) + (1|tod), data=adult_axy_all, control=lmerControl(optimizer="Nelder_Mead"))
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

m1b<-lmer(PC1 ~  sex + b.axy.local.density + b.axy_avg_fam + (1|squirrel_id) + (1|grid_yr) + (1|tod), data=adult_axy_all)
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

m2a<-lmer(PC2 ~ (1|squirrel_id) + (1|grid_yr) + (1|tod), data=adult_axy_all)
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

m2b<-lmer(PC2 ~ sex + b.axy.local.density + b.axy_avg_fam + (1|squirrel_id) + (1|grid_yr) + (1|tod), data=adult_axy_all)
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
#############      n = 119         ##############
########################################


#create working dataframe
yearling_axy_all<-read.csv("dataset_complete_axy.csv", header=T)  %>%  filter(axy_ageclass=="Y") %>% 
  mutate(
  		 grid=ifelse(grid=="SUX", "SU", grid),
		grid_yr=paste(grid, axy_yr, sep=""),
		axy_yr=axy_yr-2014) %>%
	group_by(squirrel_id) %>% #convert these variables to among-ind effects    
	mutate(b.axy.local.density=mean(axy.local.density),
		b.axy_avg_fam=mean(axy_avg_fam, na.rm=T)) %>%
	ungroup()

summary(yearling_axy_all)

(yearling_axy_all) %>% as_tibble() %>% dplyr::count(squirrel_id) %>% nrow() #119 individuals
(yearling_axy_all) %>% as_tibble() %>% dplyr::count(squirrel_id, axy_yr, axy_date) %>% nrow() #2293 deployment days
nrow(yearling_axy_all) #9136 records


#############################
######## PC1 models ########
#############################

#############################
#non-adjusted repeatability
#############################

m1a<-lmer(PC1 ~ (1|squirrel_id) + (1|grid_yr) + (1|tod), data=yearling_axy_all)
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

m1b<-lmer(PC1 ~  sex + b.axy.local.density + b.axy_avg_fam + (1|squirrel_id) + (1|grid_yr) + (1|tod), data=yearling_axy_all)
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

m2a<-lmer(PC2 ~ (1|squirrel_id) + (1|grid_yr) + (1|tod), data=yearling_axy_all)
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

m2b<-lmer(PC2 ~   sex + b.axy.local.density + b.axy_avg_fam + (1|squirrel_id) + (1|grid_yr) + (1|tod), data=yearling_axy_all, control=lmerControl(optimizer="Nelder_Mead"))
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