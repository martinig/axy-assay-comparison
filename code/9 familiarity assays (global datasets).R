#code to calculate familiarity scores (assays dataset)
#original code by E. R. Siracusa
#last updated on Feb 13, 2024 by A. R. Martinig

#run the following prior to running script:
start-up code.R
focal data subsets.R
local density.R

assay.neighbours.all <- data.frame() #create an empty data frame to store the iterations of your loop in


##################################################
##generate combined dataset for assays ------
##################################################

assay_fam <- left_join(min_info_assay, census_forcombining, by=c("squirrel_id"="squirrel_id", "year"="year", "trial_month"="month")) %>% 
	mutate(assay.local.density = as.numeric(0), #this creates a new column in which to store your calculated local density
		date = as.character(date),
		grid=grid.x,
		locX = 
			ifelse(squirrel_id==10135 & year == "2005", 14,
			ifelse(squirrel_id==12705 & reflo=="G10.", 7.0,
			ifelse(squirrel_id==10192 & year=="2009", 9.2,	
			locX))),	
		locY=
			ifelse(squirrel_id==10135 & year=="2005", 9,
			ifelse(squirrel_id==12705 & reflo=="G10.", 10.5,
			ifelse(squirrel_id==10192 & year =="2009", 4.9,
			locY))),
	    reflo=
	    	ifelse(squirrel_id==10135 & year=="2005","N9", 				
	    	ifelse(squirrel_id==12210 & year=="2013", "-49",
	    	ifelse(squirrel_id==10192 & 	year=="2009", "I5",
	    reflo)))) %>%
	mutate(date=
		ifelse(squirrel_id==10135 & year=="2005", "2005-08-15", 
		ifelse(squirrel_id==10192 & year =="2009","2009-05-15",
		date))) %>%
	mutate(date = ymd(date)) %>%
	select (-c(grid.x, grid.y, sex)) %>%
#squirrels with two spring OR two fall census records, keep only census record closest to august or may in the season (e.g., if there was a census in august and september, we kept the august record)	
	filter(!(squirrel_id == 6500 & date == "2005-06-15"),
		!(squirrel_id == 6569 & date == "2005-06-26"),
		!(squirrel_id == 8468 & date == "2008-06-26"),
		!(squirrel_id == 10343 & date == "2009-08-15"),
		!(squirrel_id == 10370 & date == "2008-05-15"),
		!(squirrel_id == 10376 & date == "2009-08-15"),
		!(squirrel_id == 10405 & date == "2009-05-15"),
		!(squirrel_id == 10467 & date == "2008-08-15"),
		!(squirrel_id == 10661 & date == "2009-08-15"),
		!(squirrel_id == 11081 & date == "2009-08-15"),
		!(squirrel_id == 11201 & date == "2010-06-14"),
		!(squirrel_id == 11254 & date == "2009-08-15"),
		!(squirrel_id == 11266 & date == "2010-05-15"),
		!(squirrel_id == 11282 & date == "2010-06-14"),
		!(squirrel_id == 11882 & date == "2010-05-15"),
		!(squirrel_id == 13664 & date == "2009-08-15"),
		!(squirrel_id == 21944 & date == "2019-10-01")) 

missings<-assay_fam %>%
	filter(is.na(reflo)) %>%
	left_join(mothers, by=c("squirrel_id"="juv_id", "year"="cohort", "trial_month"="month")) %>%
	select(squirrel_id, year, trialdate, trial_month, date=date.y, reflo=reflo.y, locX=locX.y, locY=locY.y, Ft, assay.local.density, grid)
	
summary(missings)
nrow(missings) 
head(missings)

assay_fam %<>% 
	filter(!is.na(reflo))
	
assay_fam<-rbind(assay_fam, missings)	
		
summary(assay_fam)
(assay_fam) %>% as_tibble() %>% dplyr::count(squirrel_id) %>% nrow() #822 inds
nrow(assay_fam) #944

###########################
#familiarity for assays ------
###########################

n_a <- length(assay_fam$squirrel_id) #This is the length of your data (i.e. the squirrels that you want to create neighbourhoods and calculate density for). You can substitute squirrel_id for any column. This is the data the code below will loop through. 

#for assays
for (j in 1:n_a) {
	print(j)
	#this selects neighbours in the same grid, year, month and within 130 m of your focal individual.
	neighbours <- subset(census, 
		census$grid==assay_fam$grid[j] & 
		census$year==assay_fam$year[j] & 
		census$month==assay_fam$trial_month[j] & (30*assay_fam$locX[j]-30*census$locX)^2+(30*assay_fam$locY[j]-30*census$locY)^2<=(distance)^2)
	
	#this makes sure your focal squirrel is not included in your neighbours.
	neighbours <- subset(neighbours, !neighbours$squirrel_id==assay_fam$squirrel_id[j])
	
	#Add an if/else clause to deal with squirrels that were off grid and therefore have 0 neighbours… as this causes the loop to fail. We are inputting NAs for the average familiarity for these individuals	
	
	if(length(neighbours$squirrel_id > 0)){
		#this creates new columns in your 'neighbours' dataframe so that you can identify which focal individual you just created a neighbourhood for and adds back some of the information about the focal individual
		neighbours$Focal.ID <- assay_fam$squirrel_id[j]
		neighbours$Focal.locX <- assay_fam$locX[j]
		neighbours$Focal.locY <- assay_fam$locY[j]
		neighbours$Focal.reflo <- assay_fam$reflo[j]
		neighbours$Focal.Date <- assay_fam$date[j]
		#make sure neighbours aren't duplicated (i.e. because an individual has more than one primary midden) - this shouldn't happen because of how we subsetted the census data above 
		neighbours %<>% filter(!duplicated(squirrel_id))
		
		#Calculate Distance			
		n <- length(neighbours$Focal.ID)
		for(k in 1:n){
			dis <- sqrt((30*(neighbours$Focal.locX[k])-30*(neighbours$locX[k]))^2+(30*(neighbours$Focal.locY[k])-30*(neighbours$locY[k]))^2)
			neighbours[k,"Nbor.dis"] <- dis
		}	
		
		#this serves as a double check and helps catch any mistakes in the database where two squirrels were assigned the same primary midden				
		neighbours %<>% filter(Nbor.dis > 0)
		
		#Calculate Familiarity - here we are going to cycle through each neighbour and see how long the focal squirrel and neighbouring squirrel have occupied their current territories next to each other
		n <- length(neighbours$Focal.ID)
		for(i in 1:n){
			#this selects only census information that is on or before the date that you want to calculate familiarity from	
			fam1 <- subset(census, census$date<=neighbours$Focal.Date[i])
			
			#select all instances where the focal squirrel lived at this reflo
			fam2 <- subset(fam1, fam1$squirrel_id==neighbours$Focal.ID[i] & fam1$reflo==neighbours$Focal.reflo[i])
			#because we have juveniles for whom we are using mom's midden as their "home" there will be instances where the focal squirrel is not found in the census before the date of interest. This will cause a -Inf in the data. These should be zeros in the data
			if(nrow(fam2)==0){
				own <- neighbours$Focal.Date[i]
			} else {
				#select the earliest date that that the focal squirrel lived at this reflo
				own <- min(fam2$date)
			}
			
			#select all instances where the neighbouring squirrel lived at this reflo
			fam3 <- subset(fam1, fam1$squirrel_id==neighbours$squirrel_id[i] & fam1$reflo==neighbours$reflo[i])
			#if the neighbour was censused after the focal squirrel sometimes the neighbour was only present at the current midden after the date of interest. In this case the neighbour will not show up in the fam3 dataframe and there will be no minimum date at which the neighbour first occupied their neighbouring midden, leading to a -Inf in the data. This code solves that issue
			if(nrow(fam3) == 0){
				nbor <- neighbours$Focal.Date[i]
			} else{
				#selet the earliest date that the neighbouring squirrel lived at this reflo
				nbor <- min(fam3$date)
			}
			
			#take the latest date of these two (this is the earliest date that these individuals lived next to each other  -- it does not take into account the possibility that the neighbouring squirrel moved from another nearby midden, but anecdotal evidence from the field suggests that anytime a neighbour moves to another nearby midden the surrounding squirrels treat that individual as a "new" neighbour)
			f <- max(own,nbor)
			#subtract this date from the current date to get the length of time these individuals have lived next to each other
			f <- neighbours$Focal.Date[i]-f
			neighbours[i,"Nbor.familiarity"] <- f
		}
		
		#NOTE: Because neighbours are occassionally censused in the same month but a few days after the 'owner' this code occassionally creates negative familiarity values. For example if the owner census date is May 15, 2001 and the neighbour census date is May 17, 2001, and the first date that both squirrels are observed on neighbouring middens is May 17, 2001 this will lead to a negative familarity value of -2 (May 15, 2001 - May 17, 2001). To avoid these negative familiarity values you can change all negative values to zeros using the code below. This is a rough approximation but appropriate given that our measure of familiarity is also an approximation (i.e. familiarity is only updated twice per year).
		
		#neighbours %<>% 
		#	mutate(Nbor.ten = ifelse(Nbor.ten < 0, 0, Nbor.ten))
		
		#As discussed above- turn NA's into 0's since these instances represent cases where the neighboring individual was censused AFTER the focal and had no other entries in the census prior... meaning this neighbour and the focal individual's familiarity was 0 at the time
		
		neighbours %<>%
			mutate(Nbor.familiarity = ifelse(is.na(Nbor.familiarity), 0, Nbor.familiarity))
		
		#Calculate averages for the neighbourhood	
		Avg.familiarity = mean(neighbours$Nbor.familiarity, na.rm=T)
		
		#Put it all together 
		assay_fam[j,"assay_avg_fam"] <- Avg.familiarity 
		
	} else{
		assay_fam[j,"assay_avg_fam"] <- NA
	}
	
}

clean_assay_h<-assay_fam %>% select(c(year, squirrel_id, assay_avg_fam)) %>% mutate(assay_avg_fam=ifelse(is.na(assay_avg_fam), 0, assay_avg_fam))

clean_assay<-left_join(clean_assay_d, clean_assay_h,  by=c("squirrel_id"="squirrel_id", "year"="year"))