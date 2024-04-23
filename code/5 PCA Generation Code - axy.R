#PCA calculations for axy behaviours 
#original code by A. R. Martinig
#last edited April 16, 2024 by A. R. Martinig

#run the following prior to running script:
#start-up code.R
#axy data subsets.R


########################################
#complete axy dataset, n=340 inds
#before merging with personality data
########################################

colnames(axy1)

PCaxys1<-prcomp(axy1[c("prop_feeding", "prop_foraging", "prop_nestmoving", "prop_nestnotmoving", "prop_notmoving", "prop_travel")], center=TRUE, scale. =TRUE)
summary(PCaxys1) 
PCaxys1
##PC1: feeding, foraging, -nestnotmoving, travel
##PC2: -nestnotmoving, notmoving
##PC3: nestmoveing

#add PCs to dataset
axy1$PC1<-prcomp(~prop_feeding + prop_foraging + prop_nestmoving + prop_nestnotmoving +  prop_notmoving + prop_travel, data= axy1, center=TRUE, scale =TRUE)$x[,1]
axy1$PC2<-(-1)*prcomp(~prop_feeding + prop_foraging + prop_nestmoving + prop_nestnotmoving +  prop_notmoving + prop_travel, data= axy1, center=TRUE, scale =TRUE)$x[,2] 
#############note I flip the sign for PC2 so that postive PC2 values mean more movement#################