#PCA calculcations for axy behaviours 
#last updated Feb 13, 2024 by A. R. Martinig

#run the following prior to running script:
start-up code.R
axy data subsets.R


########################################
#complete axy dataset, n=250 inds
#before merging with personality data
########################################

colnames(axy1)

PCaxys1<-prcomp(axy1[c("prop_feeding", "prop_foraging", "prop_nestmoving", "prop_nestnotmoving", "prop_notmoving", "prop_travel")], center=TRUE, scale. =TRUE)
summary(PCaxys1) 
PCaxys1
##PC1: feeding, foraging, -nestnotmoving, travel
##PC2: nestnotmoving, -notmoving
##PC3: nestmoveing, notmoving

#add PCs to dataset
axy1$PC1<-prcomp(~prop_feeding + prop_foraging + prop_nestmoving + prop_nestnotmoving +  prop_notmoving + prop_travel, data= axy1, center=TRUE, scale =TRUE)$x[,1]
axy1$PC2<-(-1)*prcomp(~prop_feeding + prop_foraging + prop_nestmoving + prop_nestnotmoving +  prop_notmoving + prop_travel, data= axy1, center=TRUE, scale =TRUE)$x[,2]



####testing ageclass issues
#test_axys<-axys1%>%group_by(squirrel_id)%>%filter(row_number()==1)
#test_raxys<-r_axys1%>%group_by(squirrel_id)%>%filter(row_number()==1)
#test_adults327<-adult_axys327%>%group_by(squirrel_id)%>%filter(row_number()==1)

#check ageclass summaries
#table(test_axys$axy_ageclass) #212 A, 4 J, 111 Y
#table(test_raxys$axy_ageclass) #195 A, 4 J, 128 Y (loss of 17A/gain 17Y)
#table(test_adults327$axy_ageclass) #219 A (gained 7)

#where are the extra individuals coming from?
#axys1%>%filter(squirrel_id==19583) #36 records, 6 when sq is A
#r_axys1%>%filter(squirrel_id==19583) #36 records, 6 when sq is A
#adult_axys327%>%filter(squirrel_id==19583) #6 records total

#test_axys%>%filter(squirrel_id==19583) #1 record, sq is Y
#test_raxys%>%filter(squirrel_id==19583) #1 record, sq is Y, different trialdate than in axys(b/c of ordering by date)
#test_adults327%>%filter(squirrel_id==19583) #1 record, sq now shows up as adult & different trialdate

#suggested that this squirrel (and the others listed) get ignored when we use the filter(row_number()==1) code bc the adult records are NOT the first row (at least in the r_axys1 file)
#but overall the actual nrow() counts are consistent between r_axys1 and axys1
#and actual records of A squirrels are consistent across the three datasets