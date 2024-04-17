
########################################
# merged axy and personality dataset
# n=67
########################################

personality_67<-assays%>%
	left_join(OFT_only, by=c("squirrel_id", "trialnumber")) %>% 
	inner_join(axy1, by=c("squirrel_id"="squirrel_id",  "sex"="sex")) %>%
	group_by(squirrel_id, trialnumber)%>%
	filter(row_number()==1) %>%
	ungroup() %>%
	select(squirrel_id, OFT1, OFT2, observer, ageclass, cohort, year, age, grid, trialnumber, trialdate)
	
summary(personality_67)	
head(personality_67)	
personality_67 %>% as_tibble() %>% count(squirrel_id) %>% nrow() #67 individuals


########################################
# merged axy and personality dataset
# filtered for matching ageclass, n=52
########################################

personality_52<-assays%>%
	left_join(OFT_only, by=c("squirrel_id", "trialnumber")) %>% 
	inner_join(axy1, by=c("ageclass"="axy_ageclass", "squirrel_id"="squirrel_id", "sex"="sex")) %>%
	group_by(squirrel_id, trialnumber)%>%
	filter(row_number()==1) %>%
	ungroup() %>%
	select(squirrel_id, OFT1, OFT2, observer, ageclass, sex, cohort, year, age, grid, trialnumber, trialdate)
	
summary(personality_52)	
head(personality_52)	
personality_52 %>% as_tibble() %>% count(squirrel_id) %>% nrow() #52 individuals	