#random seven minute sampling

#PCA calculcations for axy behaviours 
#last updated March 5, 2024 by A. R. Martinig

#run the following prior to running script:
start-up code.R
axy data subsets.R


########################################
#complete axy dataset, n=241 inds
#before merging with personality data
########################################

colnames(axy1)

PCaxys1<-prcomp(axy1[c("prop_feeding", "prop_foraging", "prop_nestmoving", "prop_nestnotmoving", "prop_notmoving", "prop_travel")], center=TRUE, scale. =TRUE)
summary(PCaxys1) 
PCaxys1
##PC1: -feeding, -foraging, -travel
##PC2: nestmoving, nestnotmoving

#add PCs to dataset
axy1$PC1<-prcomp(~prop_feeding + prop_foraging + prop_nestmoving + prop_nestnotmoving +  prop_notmoving + prop_travel, data= axy1, center=TRUE, scale =TRUE)$x[,1]
axy1$PC2<-(-1)*prcomp(~prop_feeding + prop_foraging + prop_nestmoving + prop_nestnotmoving +  prop_notmoving + prop_travel, data= axy1, center=TRUE, scale =TRUE)$x[,2]