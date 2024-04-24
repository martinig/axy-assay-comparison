#consecutive seven minute sampling

#code to calculate familiarity scores (axys dataset)
#original code by E. R. Siracusa
#last updated on Feb 13, 2024 by A. R. Martinig

#run the following prior to running script:
start-up code.R
axy data subsets.R
local density.R

axy.neighbours.all <- data.frame() #create an empty data frame to store the iterations of your loop in

##################################################
##generate combined dataset for axys ------
##################################################

axy_fam <- left_join(min_info_axy, census_forcombining, by=c("squirrel_id"="squirrel_id", "axy_yr"="year", "axy_month"="month")) %>% 
	select(-axy_month) %>% #axy_month had been set to either May or August for ease of matching it up with the census data
	mutate(axy.local.density = as.numeric(0), #this creates a new column in which to store your calculated local density
		date = as.character(date),
		axy_month = month(axy_date), #use the actual month that the axy was conducted to calculate familiarity accurately
        	census_month=month(date),
        locX=ifelse(squirrel_id==13412, -7.7, locX),
		locY=ifelse(squirrel_id==13412, 12.7, locY)) %>% 
	mutate(date = ymd(date),
	axy_month = 
		ifelse(axy_month==3 & census_month==4, 4,
		ifelse(axy_month<=6 & census_month==5, 5,
		ifelse(axy_month==8 & census_month==5, 5,
		ifelse(axy_month <=6 & census_month==6, 8, #must be August because there are no other census records that month and it breaks the loop if I leave it as June
		ifelse(axy_month>=7 & census_month==8, 8,
		ifelse(axy_month>=7 & census_month==9, 9,
		 	axy_month))))))) %>%
	select(-c(sex)) %>%
#squirrels with two spring OR two fall census records, keep only census record closest to august or may in the season (e.g., if there was a census in august and september, we kept the august record)	
	filter(!(squirrel_id == 19257 & reflo == "E7"),
		!(squirrel_id == 10355 & reflo == "Q6"), 
		!(squirrel_id == 12435 & reflo == "K12")) 

summary(axy_fam)
(axy_fam) %>% as_tibble() %>% dplyr::count(squirrel_id) %>% nrow() #335 inds
nrow(axy_fam) #428


###########################
#familiarity for axys ------
###########################

n_f <- length(axy_fam$squirrel_id) #This is the length of your data (i.e. the squirrels that you want to create neighbourhoods and calculate density for). You can substitute squirrel_id for any column. This is the data the code below will loop through. 


#for axys
for (j in 1:n_f) {
	print(j)
	#this selects neighbours in the same grid, year, month and within 130 m of your axy individual.
	neighbours <- subset(census, 
		census$grid==axy_fam$grid[j] & 
		census$year==axy_fam$axy_yr[j] & 
		census$month==axy_fam$axy_month[j] & (30*axy_fam$locX[j]-30*census$locX)^2+(30*axy_fam$locY[j]-30*census$locY)^2<=(distance)^2)
	
	#this makes sure your axy squirrel is not included in your neighbours.
	neighbours <- subset(neighbours, !neighbours$squirrel_id==axy_fam$squirrel_id[j])
	
	#Add an if/else clause to deal with squirrels that were off grid and therefore have 0 neighbours… as this causes the loop to fail. We are inputting NAs for the average familiarity for these individuals	
	
	if(length(neighbours$squirrel_id > 0)){
		#this creates new columns in your 'neighbours' dataframe so that you can identify which axy individual you just created a neighbourhood for and adds back some of the information about the axy individual
		neighbours$axy.ID <- axy_fam$squirrel_id[j]
		neighbours$axy.locX <- axy_fam$locX[j]
		neighbours$axy.locY <- axy_fam$locY[j]
		neighbours$axy.reflo <- axy_fam$reflo[j]
		neighbours$axy.Date <- axy_fam$date[j]
		#make sure neighbours aren't duplicated (i.e. because an individual has more than one primary midden) - this shouldn't happen because of how we subsetted the census data above 
		neighbours %<>% filter(!duplicated(squirrel_id))
		
		#Calculate Distance			
		n <- length(neighbours$axy.ID)
		for(k in 1:n){
			dis <- sqrt((30*(neighbours$axy.locX[k])-30*(neighbours$locX[k]))^2+(30*(neighbours$axy.locY[k])-30*(neighbours$locY[k]))^2)
			neighbours[k,"Nbor.dis"] <- dis
		}	
		
		#this serves as a double check and helps catch any mistakes in the database where two squirrels were assigned the same primary midden				
		neighbours %<>% filter(Nbor.dis > 0)
		
		#Calculate Familiarity - here we are going to cycle through each neighbour and see how long the axy squirrel and neighbouring squirrel have occupied their current territories next to each other
		n <- length(neighbours$axy.ID)
		for(i in 1:n){
			#this selects only census information that is on or before the date that you want to calculate familiarity from	
			fam1 <- subset(census, census$date<=neighbours$axy.Date[i])
			
			#select all instances where the axy squirrel lived at this reflo
			fam2 <- subset(fam1, fam1$squirrel_id==neighbours$axy.ID[i] & fam1$reflo==neighbours$axy.reflo[i])
			#because we have juveniles for whom we are using mom's midden as their "home" there will be instances where the axy squirrel is not found in the census before the date of interest. This will cause a -Inf in the data. These should be zeros in the data
			if(nrow(fam2)==0){
				own <- neighbours$axy.Date[i]
			} else {
				#select the earliest date that that the axy squirrel lived at this reflo
				own <- min(fam2$date)
			}
			
			#select all instances where the neighbouring squirrel lived at this reflo
			fam3 <- subset(fam1, fam1$squirrel_id==neighbours$squirrel_id[i] & fam1$reflo==neighbours$reflo[i])
			#if the neighbour was censused after the axy squirrel sometimes the neighbour was only present at the current midden after the date of interest. In this case the neighbour will not show up in the fam3 dataframe and there will be no minimum date at which the neighbour first occupied their neighbouring midden, leading to a -Inf in the data. This code solves that issue
			if(nrow(fam3) == 0){
				nbor <- neighbours$axy.Date[i]
			} else{
				#selet the earliest date that the neighbouring squirrel lived at this reflo
				nbor <- min(fam3$date)
			}
			
			#take the latest date of these two (this is the earliest date that these individuals lived next to each other  -- it does not take into account the possibility that the neighbouring squirrel moved from another nearby midden, but anecdotal evidence from the field suggests that anytime a neighbour moves to another nearby midden the surrounding squirrels treat that individual as a "new" neighbour)
			f <- max(own,nbor)
			#subtract this date from the current date to get the length of time these individuals have lived next to each other
			f <- neighbours$axy.Date[i]-f
			neighbours[i,"Nbor.familiarity"] <- f
		}
		
		#NOTE: Because neighbours are occassionally censused in the same month but a few days after the 'owner' this code occassionally creates negative familiarity values. For example if the owner census date is May 15, 2001 and the neighbour census date is May 17, 2001, and the first date that both squirrels are observed on neighbouring middens is May 17, 2001 this will lead to a negative familarity value of -2 (May 15, 2001 - May 17, 2001). To avoid these negative familiarity values you can change all negative values to zeros using the code below. This is a rough approximation but appropriate given that our measure of familiarity is also an approximation (i.e. familiarity is only updated twice per year).
		
		#neighbours %<>% 
		#	mutate(Nbor.ten = ifelse(Nbor.ten < 0, 0, Nbor.ten))
		
		#As discussed above- turn NA's into 0's since these instances represent cases where the neighboring individual was censused AFTER the axy and had no other entries in the census prior... meaning this neighbour and the axy individual's familiarity was 0 at the time
		
		neighbours %<>%
			mutate(Nbor.familiarity = ifelse(is.na(Nbor.familiarity), 0, Nbor.familiarity))
		
		#Calculate averages for the neighbourhood	
		Avg.familiarity = mean(neighbours$Nbor.familiarity, na.rm=T)
		
		#Put it all together 
		axy_fam[j,"axy_avg_fam"] <- Avg.familiarity 
		
	} else{
		axy_fam[j,"axy_avg_fam"] <- NA
	}
	
}

clean_axy_h<-axy_fam %>% select(c(axy_yr, squirrel_id, axy_avg_fam)) %>% mutate(axy_avg_fam=ifelse(is.na(axy_avg_fam), 0, axy_avg_fam))

clean_axy<-left_join(clean_axy_d, clean_axy_h,  by=c("squirrel_id"="squirrel_id", "axy_yr"="axy_yr"))