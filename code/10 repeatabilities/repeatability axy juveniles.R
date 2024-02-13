#repeatabilities for axy1 file, juveniles only
##KEEP IN MIND: Some squirrels had axy conducted when they were in different ageclasses (A and Y)
###these squirrels cause imbalances when looking at ageclass summaries
#last edited Feb 13, 2024 by A. R. Martinig

#run the following prior to running script:
start-up code.R
axy data subsets.R
PCA generation code - axy.R
local density (global datasets).R
familiarity axy (global datasets).R

#create working dataframe
juvenile_axy_all<-left_join(axy1, clean_axy, by=c("squirrel_id"="squirrel_id", "axy_yr"="axy_yr"))%>%
  left_join((tbl(con, "flastall2") %>% select(squirrel_id, grid=gr) %>% collect()), by="squirrel_id") %>% #to bring in the grid information
  filter(axy_ageclass=="J") #%>% 
#  mutate(axy_yr=axy_yr-2014)%>%
#  group_by(squirrel_id) %>% #convert these variables to among-ind effects    
#  mutate(b.axy.local.density=mean(axy.local.density),
#		b.axy_avg_fam=mean(axy_avg_fam, na.rm=T)) %>%
#	ungroup()

summary(juvenile_axy_all)

(juvenile_axy_all) %>% as_tibble() %>% dplyr::count(squirrel_id) %>% nrow() #1 individual
(juvenile_axy_all) %>% as_tibble() %>% dplyr::count(squirrel_id, axy_yr, axy_month) %>% nrow() #1 sessions
nrow(juvenile_axy_all) #4

#insufficient sample size, so did not calculate repeatability