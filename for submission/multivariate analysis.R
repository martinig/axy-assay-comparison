#multivariate analysis for the complete assay and axy dataset
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


###################################
#  preparing data for multivarite analysis  #
###################################

final_MCMC<-read.csv("final_dataset_merged.csv", header=T)  %>% 
  	mutate(
  		grid=ifelse(grid=="SUX", "SU", grid),
  		grid_yr=paste(grid, year, sep=""),
  		date=yday(trialdate), #converts dates to days since Jan 1st!
         age2=age^2,
         local.density=assay.local.density,
         avg_fam =assay_avg_fam) %>% #make a quadratic age variable
  mutate(year=year-2005) %>% 
   group_by(grid)%>%
   #new NAs that come up because of the way standardization works are replaced with 0
   mutate(age=((age-mean(age))/(1*(sd(age)))),
 		age = replace(age, is.na(age), 0),
   		age2 =((age2-mean(age2))/(1*(sd(age2)))),
   		age2 = replace(age2, is.na(age2), 0),
		local.density =((local.density-mean(local.density))/(1*(sd(local.density)))),
 		local.density = replace(local.density, is.na(local.density), 0),
		avg_fam =((avg_fam-mean(avg_fam))/(1*(sd(avg_fam)))),
 		avg_fam = replace(avg_fam, is.na(avg_fam), 0),
		date =((date-mean(date))/(1*(sd(date)))),
		date = replace(date, is.na(date), 0)) %>%
	ungroup()

(final_MCMC) %>% as_tibble() %>% count(squirrel_id) %>% nrow() #822 individuals
nrow(final_MCMC) #39021


#bayesian multivariate generalized linear model analysis 

###modelling priors
#R is residual structure, G is random effects structure, B is fixed effects
#numbers inside diag() indicate the number of response variables in my model
#number of zeros in alpha.mu=c() need to match the number of response variables in model

#this is a parameter expanded prior
prior.iw<-list(
	R=list(V=diag(4), nu=1), 
	G=list(
		G1=list(V=diag(4), nu= 1, 	
			alpha.mu=c(0,0,0,0), 
			alpha.V=diag(4)*1000), #when alpha.V is non-zero, parameter expanded algorithms are used #for this the last number should be something large (e.g. 1000, depending on the scale of the data)

#need to repeat to deal with second random effect
		G2=list(V=diag(4), nu= 1, 	
			alpha.mu=c(0,0,0,0), 
			alpha.V=diag(4)*1000)))
#the use of parameter expansion means the priors are no longer inverse-Wishart but scaled-F
#parameter expanded models can be used, which enable prior specifications from the scaled non-central F-distribution								
				
final_MCMC<-as.data.frame(final_MCMC)

###model structure...double check that variables are written correctly (e.g., capitalization, underscoring, etc.)
mod.1 <- MCMCglmm(
	cbind(OFT1, OFT2, PC1, PC2) ~ 
	trait-1 +
	trait:sex +
	trait:age +
	trait:age2 +
	trait:date +
	trait:local.density +
	trait:avg_fam,
	random = ~us(trait):squirrel_id + us(trait):year,
	rcov = ~idh(trait):units, #"us" allows for trait to have different residuals #to force residuals cov to be zero just replace "us" with "idh" ---> you might have to change your prior?! #you use rcov = 0 because you know this is so, but if you allow the model to run as is, you can see if this does meet the expectation that it would be zero
	family = c("gaussian", "gaussian", "gaussian", "gaussian"), #state response variables distributions (start with Gaussian priors for all parameters, including categorical. This just means that your uncertainty around the initial guess is governed by a normal distribution with a specific variance (indicating the degree of belief))
	data= final_MCMC, 
	prior = prior.iw, 
	verbose = FALSE,
	pr=TRUE, #this saves the BLUPs 
	nitt=303000, #number of iterations
	thin=300, #interval at which the Markov chain is stored
	burnin=3000) #number of iterations before samples are stored

summary(mod.1) 
#plot(mod.1)
 
# Posterior distribution of location effects
round(apply(mod.1$Sol,2,mean),3)
round(apply(mod.1$Sol,2, quantile, c(0.025, 0.975)),3)

round(apply(mod.1$VCV,2,mean),3)
round(apply(mod.1$VCV,2, quantile, c(0.025, 0.975)),3)


#4 RESPONSE VARS

###posterior correlation matrix - 1 through 16 is among individual, 17-32 is within-individual
c1 <- posterior.cor(mod.1$VCV[,1:16])
round(apply(c1,2,mean),3)
round(apply(c1,2, quantile, c(0.025, 0.975)),3)

c2 <- posterior.cor(mod.1$VCV[,17:32])
round(apply(c2,2,mean),3)
round(apply(c2,2, quantile, c(0.025, 0.975)),3)

#get the posterior mode values instead of posterior mean for the MCMC output for the random effects
posterior.mode(mod.1$VCV[,1:16]) #squirrel id
HPDinterval(mod.1$VCV[,1:16])

posterior.mode(mod.1$VCV[,17:32]) #year
HPDinterval(mod.1$VCV[,17:32])  

posterior.mode(mod.1$VCV[,33:36]) #residuals
HPDinterval(mod.1$VCV[,33:36])  #use 33:48 when rcov=~us(trait):units   


#to get the gelman-rubin stat:
chains <- as.mcmc.list(lapply(1:2, function(i)
	MCMCglmm(cbind(OFT1, MIS1, PC1, PC2) ~ trait-1 + trait:sex + trait:age + trait:age2 + trait:date + trait:local.density + trait:avg_fam, random = ~us(trait):squirrel_id + us(trait):year, rcov = ~us(trait):units, family = c("gaussian", "gaussian", "gaussian", "gaussian"), data= final_MCMC, prior = prior.iw, verbose=FALSE, nitt= 303000, thin=300, burnin=3000)$Sol ))

gelman.diag(chains) #if it has converged, the scale reduction should be 1 (i.e., none)
gelman.plot(chains)