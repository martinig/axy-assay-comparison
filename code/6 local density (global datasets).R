#code to calculate local density
#original code by E. R. Siracusa
#last updated on Feb 13, 2024 by A. R. Martinig

#run the following prior to running script:
start-up code.R
axy data subsets.R
PCA Generation Code - Assays.R

library(magrittr)
library(krsp)

distance <- 130 #establish your radius for collecting neighbourhood data
neighbours.all <- data.frame() #create an empty data frame to store the iterations of your loop in

#Connect to database - replace with your username below

#APRIL's connection to database
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                  dbname ="krsp",
             user="amartinig",
           password = keyring::key_get("krsp")
)  

#JONAS's connection to database
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com", dbname ="krsp", user="jsanders", password = keyring::key_get("krsp") )


#Bring in the census - depending on what data you're working with you may need the dbamidden file or the census file or both

new.census = tbl(con, "census") %>%
			collect() %>%	
	mutate(date = ymd(census_date),
			 year = year(date),
			 month = month(date),
			 locX = loc_to_numeric(as.character(locx)), #KRSP function to deal with this
			 locY = loc_to_numeric(locy)) %>%
	select(squirrel_id, grid=gr, date, reflo, locX, locY, year, month, sex, Ft=sq_fate)


old.census <- tbl(con, "dbamidden") %>%
	collect() %>%
	mutate(date = ymd(date),
			 year = year(date),
			 month = month(date),
			 locX = loc_to_numeric(locX),
			 locY = loc_to_numeric(locY),
			 sex=Sex) %>%
	select(-c(id, obs, tagLft, tagRt, tagLftNum, tagRtNum, col, color_left, color_right, Sex, fate, mcon, act, ft, cones, nest, comments)) %>%
	dplyr::rename(Ft=def) %>%
	filter(Ft == "4")

#Separate out primary middens by different years due to differences in the fates 

censuspre16 <- new.census %>%
	filter(date < "2016-07-01",
		Ft=="1"|Ft=="2"|Ft=="3"|Ft=="4"|Ft=="9"|Ft=="10"|Ft=="11"|Ft=="13")

censusfall16 <- new.census %>%
	filter(date >= "2016-07-01" & date < "2017-01-01",
		Ft=="1"|Ft=="3"|Ft=="6"|Ft=="10")

censuspost17 <- new.census %>%
	filter(date >= "2017-01-01",
		Ft=="1"|Ft=="2"|Ft=="15"|Ft=="16"|Ft=="18"|Ft=="20")

#add missing census records
#records retrieved from any source available, including a combination of trapping, behaviour observations, and trial csv records
missed<-data.frame(
	squirrel_id=c(11537, 21058, 11398, 6423, 10767, 8592, 20600, 21465, 22428, 21246, 22389, 23903, 21435, 22009, 23256, 22497, 22052, 21930, 21880, 22602, 11180, 6312, 7075, 6340, 8036, 10517, 6624, 8386, 8304, 10736, 8299, 11488, 11615, 10736, 13235, 11721, 20914, 22495, 21355, 20778, 21128, 21842, 21351, 23262, 21903, 20998, 23816, 21999, 21770, 22042, 22042, 20372, 21348, 20373, 23244, 23244, 23306, 23271, 23271, 24062, 23286, 23286, 20371, 21304, 23213, 23213, 23215, 23218, 23258, 23261, 23263, 23276, 23298, 23307, 23308, 23331, 23759, 23775),
	grid=c("KL", "SU", "KL", "KL", "AG", "KL", "SU", "JO", "AG", "AG", "AG", "SU", "AG", "KL", "SU", "AG", "AG", "AG", "AG", "JO", "JO", "KL", "SU", "AG", "KL", "KL", "AG", "KL", "KL", "KL", "KL", "KL", "KL", "KL", "SU", "JO", "KL", "SU", "KL", "KL", "AG", "AG", "AG", "AG", "AG", "KL", "KL", "KL", "KL", "LL", "LL", "LL", "LL", "LL", "LL", "LL", "LL", "LL", "LL", "LL", "LL", "LL", "LL", "LL", "LL", "LL", "LL", "LL", "LL", "LL", "LL", "LL", "LL", "LL", "LL", "LL", "LL", "LL"),
	date=c("2014-05-15", "2017-05-20", "2010-06-01", "2005-08-16", "2009-07-01", "2005-08-31", "2017-05-21", "2019-08-22", "2018-07-09", "2018-07-08", "2018-07-18", "2019-08-06", "2018-07-17", "2017-05-26", "2017-07-25", "2018-07-11", "2018-07-18", "2018-07-18", "2018-07-15", "2019-07-25", "2005-08-15", "2005-05-15", "2005-08-15", "2005-08-15", "2005-05-15", "2005-08-15", "2005-07-03", "2008-05-15", "2008-05-15", "2008-05-15", "2008-05-15", "2010-05-15", "2010-05-15", "2010-05-15", "2012-07-21", "2012-05-15", "2019-04-08", "2016-07-03", "2019-04-06", "2019-04-11", "2016-08-25", "2015-09-16", "2015-09-24", "2018-08-15", "2015-05-15", "2015-09-17", "2019-04-04", "2015-08-15", "2015-09-25", "2017-09-03", "2018-09-04", "2017-08-29", "2017-08-29", "2017-09-04", "2018-08-22", "2017-09-06", "2017-09-06", "2018-08-27", "2017-09-07", "2019-08-22", "2019-08-31", "2018-08-27", "2017-09-04", "2018-08-15", "2018-08-22", "2017-09-02", "2017-09-03", "2019-08-31", "2019-09-13", "2017-09-18", "2017-09-03", "2017-09-06", "2017-09-18", "2017-09-06", "2019-09-10", "2018-09-01", "2019-08-31", "2018-09-04"),
	reflo=c("O.2.", "G4", "I.21", "J1", "D31", "-613", "-1.4.", "T.1", "K16", "N23", "D.22", "-8.17", "L.20.", "K.0.", "D2", "T2.", "F12", "E19", "R.19.", "U13", "M.6.", "B.10.", "T3", "T.13.", "J.9", "N.2.", "B.9.", "M14", "M20", "P14", "O15", "-414", "-6.12", "M.13", "D.2.", "M.4", "-610", "O8", "D7", "E3.", "I.21.", "Q11", "A18", "A0", "H.0", "D4", "J.9", "-59", "J.7.", "C.5", "C.5", "H0.", "B2", "L2", "G0.", "G0.", "-32", "M.6", "M.6", "-82.", "N3.", "S.3.", "M.6", "M.0.", "J.0.", "J.0.", "-91", "W4", "Q.5.", "H.4.", "-3.0", "-65.", "E.4", "-52.", "V0", "J2.", "E.1.", "H.4."),
	locX=c(15.3, 7, 9.5, 10, 4, -6, -1.5, 20.5, 11, 14, 4.5, -8.5, 12.5, 11.5, 4, 20, 6, 5, 18.5, 21, 13.5, 2.6, 20.1, 20.9, 10, 14.4, 2.5, 13, 13, 16, 15, -4, -6.5, 13.5, 4.5, 13.5, -6, 14.8, 4.3, 5, 9.5, 17.8, 1, 1, 8.5, 4, 10.6, -5, 10.5, 3.5, 3.5, 8.5, 2.0, 12, 7, 7, -3, 13.5, 13.5, -8, 14, 19.5, 13.5, 13.5, 10.5, 10.5, -9, 23, 22.5, 8.5, -3.5, -6, 5.5, -5, 22, 10, 5.5, 8.5),
	locY=c(2.5, 4, 21, 1, 31, 13, 4.5, 1.0, 16, 23, 22, 17, 20.5, 0.5, 2, 2.5, 12, 19, 19.5, 13, 6.7, 10.5, 3, 13.9, 9, 2.4, 9.5, 14, 20, 14, 15, 14, 12, 15, 2.5, 4, 10, 8.1, 7, 3.5, 21.5, 11.3, 18.2, 0.5, 0, 4, 9.2, 9, 7.5, 5, 5, 0.5, 2, 2, 0.5, 0.5, 2, 6, 6, 2.5, 3.5, 3.5, 6, 0.5, 0.5, 0.5, 1, 4, 5.5,  4.5, 0, 5.5, 4, 2.5, 0, 2.5, 1.5, 4.5),
	Ft=c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
	year=c(2014, 2017, 2010, 2005, 2009, 2005, 2017, 2019, 2018, 2018, 2018, 2019, 2018, 2017, 2017, 2018, 2018, 2018, 2018, 2019, 2005, 2005, 2005, 2005, 2008, 2009, 2005, 2008, 2008, 2008, 2008, 2010, 2010, 2010, 2012, 2012, 2019, 2016, 2019, 2019, 2016, 2015, 2015, 2018, 2015, 2015, 2019, 2015, 2015, 2017, 2018, 2017, 2017, 2017, 2018, 2017, 2017, 2018, 2017, 2019, 2019, 2018, 2017, 2018, 2018, 2017, 2017, 2019, 2019, 2017, 2017, 2017, 2017, 2017, 2019, 2018, 2019, 2018),
	month=c(5, 5, 6, 8, 7, 8, 5, 8, 7, 7, 7, 8, 7, 5, 7, 7, 7, 7, 7, 7, 8, 5, 8, 8, 5, 8, 7, 5, 5, 5, 5, 5, 5, 5, 7, 5, 4, 7, 4, 4, 8, 9, 9, 8, 5, 9, 4, 8, 9, 9, 9, 8, 8, 9, 8, 9, 9, 8, 9, 8, 8, 8, 9, 8, 8, 9, 9, 8, 9, 9, 9, 9, 9, 9, 9, 9, 8, 9),
	sex=c("F", "F", "M", "M", "M", "F", "F", "F", "M", "F", "M", "F", "M", "F", "F", "F", "M", "M", "F", "F", "M", "M", "M", "F", "M", "F", "F", "M", "M", "M", "M", "M", "M", "M", "M", "F", "M", "F", "M", "M", "F", "F", "F", "M", "F", "M", "M", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "M", "F", "F", "F", "F", "F", "F", "F", "M", "F", "F",  "F", "F", "F", "F", "F", "M", "F", "F"))


#Bind these back together
census <- rbind(old.census, censuspre16,censusfall16,censuspost17,missed) %>%
	group_by(squirrel_id, year, month) %>% #need to include month here so that we capture both may and august census data
	filter(row_number()==1) %>% #squirrels >1 primary midden only get counted once, second censuses not included
	mutate(locX = loc_to_numeric(as.character(locX)),
		   locY = loc_to_numeric(locY),
	reflo=ifelse(squirrel_id==11031, "Q8", reflo)) %>%
	mutate(locX=ifelse(squirrel_id==22996 & year==2017, 13.5, 
	ifelse(squirrel_id==20839 & year==2015, 17, 
	ifelse(squirrel_id==21496 & year==2015, -3, 
	ifelse(squirrel_id==11020 & year==2013, -5, 
	ifelse(squirrel_id==10746 & year==2009, 16,
	ifelse(squirrel_id==10376 & year==2009, 12,
	locX)))))), 
	locY=ifelse(squirrel_id==22996 & year==2017, 8, 
	ifelse(squirrel_id==20839 & year==2015, 9, 
	ifelse(squirrel_id==11020 & year==2013, 12, 
	ifelse(squirrel_id==21496 & year==2015, 2, 
	locY)))))  %>%
	ungroup() 

#Create a census file with just may and august data to make it easier to match up with axy and assay data
census_forcombining <- census %>%
	filter(month==4 | month==5 | month==6 | month==7 | month==8 | month==9 | month==10) %>%
	mutate(month = ifelse(month <= 6,  5, 
		ifelse(month >= 7, 8, month)))


##################################################
##generate combined dataset for assays ------
##################################################

#Bring in the data that you want to calculate local density for

min_info_assay<-personality_all %>% 
		ungroup() %>% 
		mutate(trial_month = month(trialdate)) %>%
		group_by(squirrel_id, year) %>% #only need to keep 1 record per squirrel per assay year
		filter(row_number()==1) %>%
		ungroup()%>%
		select(squirrel_id, year, trialdate, grid, trial_month) 
		
(min_info_assay) %>% as_tibble() %>% dplyr::count(squirrel_id) %>% nrow() #811 inds


min_info_assay %<>%
#make sure all of the assay months are either May or August
	mutate(trial_month = 
		ifelse(trial_month <= 6, 5,  
		ifelse(trial_month >= 7, 8, trial_month))) %>% 
#fix some months so that they line up with the census data we have 
	mutate(trial_month = 
		ifelse(squirrel_id %in% c("10112",  "6538", "7609", "11180", "7075") & year == "2005", 8,
		ifelse(squirrel_id %in% c("19257", "12303", "13445") & year == "2012", 8,
		ifelse(squirrel_id %in% c("12210") & year == "2013", 8, #"11031",
		ifelse(squirrel_id == "19654" & year == "2014", 8,
		ifelse(squirrel_id %in% c("22891", "22657", "19742", "22979", "22978") & year == "2017", 8,
		ifelse(squirrel_id %in% c("6456", "6398", "7886", "6343") & year == "2005", 5, 
		ifelse(squirrel_id %in% c("10405") & year == "2009", 5, 
		ifelse(squirrel_id == "11398" & year == "2010", 5,
		ifelse(squirrel_id %in% c("11721", "10994") & year == "2012", 5,
		ifelse(squirrel_id %in% c("21058", "22951") & year == "2017", 5,
		ifelse(squirrel_id %in% c("13023", "13142", "19974", "20415", "20863", "21153", "22021", "21583", "23689", "20307", "13579", "20231", "22209", "22352", "21085", "21556", "22200", "21599", "22569", "22251", "22150", "20255", "13141", "19976", "20184") & year == "2018", 5,	
	 trial_month))))))))))))

nrow(min_info_assay)	 #931
head(min_info_assay)


assay_local_density <-left_join(min_info_assay, census_forcombining, by=c("squirrel_id"="squirrel_id", "year"="year", "trial_month"="month")) %>% 
	mutate(assay.local.density = as.numeric(0), #this creates a new column in which to store your calculated local density
		grid=grid.x,
		date = as.character(date),
		locX = 
			ifelse(squirrel_id==10135 & year == "2005", 14, 
			ifelse(squirrel_id==12705 &reflo=="G10.", 7.0,
			ifelse(squirrel_id==10192 & year=="2009", 9.2, 
			locX))),
		locY=
		ifelse(squirrel_id==10135&year=="2005",9,
		ifelse(squirrel_id==12705 & reflo=="G10.", 10.5,
		ifelse(squirrel_id==10192 & year =="2009", 4.9, 
			locY)))) %>%
	mutate(
	grid=ifelse(squirrel_id==11721, 	"JO", as.character(grid)), #to match census
	date=ifelse(squirrel_id==10135 & year=="2005", 			"2005-08-15", date)) %>%
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


#we have a lot of squirrels with assays or axys in their first year of life
#as juveniles, all won't have a territory in may and most won't have one in august, so I need to pull in the mothers census data

litters<- tbl(con,"litter") %>%
	collect() %>%
	select (litter_id=id, mom_id=squirrel_id, cohort=yr)

juv<-tbl(con, "juvenile") %>%
	select (litter_id, juv_id=squirrel_id) %>%
	collect()	
	
mothers<-left_join(litters, juv, by="litter_id") %>%
		left_join(census, by=c("mom_id"="squirrel_id", "cohort"="year")) %>%
		select(mom_id, cohort, juv_id, date, reflo, locX, locY, month)
	
head(mothers)

missing<-assay_local_density %>%
	filter(is.na(reflo)) %>%
	left_join(mothers, by=c("squirrel_id"="juv_id", "year"="cohort", "trial_month"="month")) %>%
	select(squirrel_id, year, trialdate, trial_month, date=date.y, reflo=reflo.y, locX=locX.y, locY=locY.y, Ft, assay.local.density, grid)
	
summary(missing)
nrow(missing) 
head(missing)

	
assay_local_density %<>% 
	filter(!is.na(reflo))
	
assay_local_density<-rbind(assay_local_density, missing)	
		
summary(assay_local_density)
(assay_local_density) %>% as_tibble() %>% dplyr::count(squirrel_id) %>% nrow() #811 inds
nrow(assay_local_density) #931


##################################################
#density for the assays ------
##################################################

n_a <- length(assay_local_density$squirrel_id) #This is the length of your data (i.e. the squirrels that you want to create neighbourhoods and calculate density for). You can substitute squirrel_id for any column. This is the data the code below will loop through.

for (i in 1:n_a) {
	print(i)
	neighbours <- subset(census, 
	census$grid==assay_local_density$grid[i] & 
	census$year==assay_local_density$year[i] & 
	census$month==assay_local_density$trial_month[i] & 
	(30* assay_local_density$locX[i]-30*census$locX)^2+(30*assay_local_density$locY[i]-30*census$locY)^2<=(distance)^2) 
	#This selects neighbours in the same grid, year, month and within 130 m of your axy individual.
	
	#this creates a column in your new 'neighbours' dataframe so that you can identify which axy individual you just created a neighbourhood for. 
	neighbours <- subset(neighbours, !neighbours$squirrel_id==assay_local_density$squirrel_id[i]) 
	#this makes sure your axy squirrel is not included in your neighbours.
	
	#Calculate Density (squirrels per hectare: 53,092 m2 = 5.3 hectares)
	num.indiv <-  length(unique(neighbours$squirrel_id))
	density <- num.indiv/((pi*distance^2)/10000)
	
	#Put it all together
	assay_local_density[i,"assay.local.density"] <- density
	
}

#clean dataset with local density for assays
clean_assay_d<-assay_local_density%>%select(c(squirrel_id, year, assay.local.density))
summary(clean_assay_d)


##################################################
##generate combined dataset for axys ------
##################################################

min_info_axy<-axy1 %>% 
		ungroup() %>% 
		mutate(axy_month = month(axy_date)) %>%
		group_by(squirrel_id, axy_yr) %>% #only need to keep 1 record per squirrel per assay year and axy year
		filter(row_number()==1) %>%
		ungroup()%>%
		select(squirrel_id, axy_yr, axy_date, axy_month) 
		
(min_info_axy) %>% as_tibble() %>% dplyr::count(squirrel_id) %>% nrow() #250 inds


min_info_axy %<>%
#make sure all of the months are either May or August
	mutate(axy_month = 
		ifelse(axy_month <= 6, 5, 
		ifelse(axy_month >= 7, 8, axy_month))) #%>%
#fix some months so that they line up with the census data we have 
	#mutate(axy_month = 
	#ifelse(squirrel_id==10702 & axy_yr=="2008", 5,
	#ifelse(squirrel_id==21496 & axy_yr=="2015", 5,
	#ifelse(squirrel_id %in% c(13023, 20782, 12522, 21787, 19718) & axy_yr=="2016", 5, 
 	#ifelse(squirrel_id %in% c(22912, 22893, 22585, 21496, 19970, 23210, 22889) & axy_yr=="2018", 5,
	#ifelse(squirrel_id %in% c(12958, 21533, 21496, 21496) & axy_yr == "2015", 8, 
	#axy_month))))))

nrow(min_info_axy) #327


axy_local_density <- left_join(min_info_axy, census_forcombining, by=c("squirrel_id"="squirrel_id", "axy_yr"="year", "axy_month"="month")) %>% 
	mutate(axy.local.density = as.numeric(0), #this creates a new column in which to store your calculated local density
		date = as.character(date),
		axy_month=month(axy_date),
		census_month=month(date),
		locX=
			ifelse(squirrel_id==13412, -7.7,
			ifelse(squirrel_id==11537, 15.3, locX)), 
		locY=
			ifelse(squirrel_id==13412, 12.7, 
			ifelse(squirrel_id==11537, 2.5, locY))) %>%
	mutate(date=ifelse(squirrel_id==11537, "2014-05-15", date)) %>%
	mutate(date = ymd(date)) %>%
	select(-c(sex)) %>%
#squirrels with two spring OR two fall census records, keep only census record closest to august or may in the season (e.g., if there was a census in august and september, we kept the august record)	
	filter(!(squirrel_id == 19257 & reflo == "E7"),
		!(squirrel_id == 10355 & reflo == "Q6"), 
		!(squirrel_id == 12435 & reflo == "K12"))
	
	
axy_local_density<-axy_local_density %>% mutate(axy_month = 
		ifelse(axy_month==3 & census_month==4, 4,
		ifelse(axy_month<=6 & census_month==5, 5,
		ifelse(axy_month>=7 & census_month==5, 5,
		ifelse(axy_month<=6 & census_month==6, 6,
		ifelse(axy_month>=7 & census_month==8, 8,
		ifelse(axy_month>=7 & census_month==9, 9,
		 	axy_month)))))))

	
summary(axy_local_density)
(axy_local_density) %>% as_tibble() %>% dplyr::count(squirrel_id) %>% nrow() #335 inds
nrow(axy_local_density) #428


##################################################
#density for the axys ------
##################################################

n_f <- length(axy_local_density$squirrel_id) #This is the length of your data (i.e. the squirrels that you want to create neighbourhoods and calculate density for). You can substitute squirrel_id for any column. This is the data the code below will loop through.

for (i in 1:n_f) {
	print(i)
	neighbours <- subset(census, 
	census$grid== axy_local_density$grid[i] & 
	census$year== axy_local_density$axy_yr[i] & 
	census$month== axy_local_density$axy_month[i] & 
	(30*axy_local_density$locX[i]-30*census$locX)^2+(30*axy_local_density$locY[i]-30*census$locY)^2<=(distance)^2) 
	#This selects neighbours in the same grid, year, month and within 130 m of your axy individual.

	#this creates a column in your new 'neighbours' dataframe so that you can identify which axy individual you just created a neighbourhood for. 
	neighbours <- subset(neighbours, !neighbours$squirrel_id==axy_local_density$squirrel_id[i]) 
	#this makes sure your axy squirrel is not included in your neighbours.
	
	#Calculate Density (squirrels per hectare: 53,092 m2 = 5.3 hectares)
	num.indiv <-  length(unique(neighbours$squirrel_id))
	density <- num.indiv/((pi*distance^2)/10000)
	
	#Put it all together
	axy_local_density[i,"axy.local.density"] <- density
	
}

#clean dataset with local density for axys
clean_axy_d<-axy_local_density%>%select(c(squirrel_id, axy_yr, axy.local.density))
summary(clean_axy_d)