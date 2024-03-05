#This script loads all the KRSP squirrel accelerometer data, identifies time of day and summarizes behaviour by day.

# Input is: Classified squirrel accelerometer files from the KRSP database that Matt Gaidica has at UW.

# Output is:  1 daily summary file per squirrel axy
#             1 axy file with TOD identifies per squirrel axy file
#             1 compiled file of daily summaries from all squirrel axy 2014-2022

# Written by E.K.Studd
## Last edits: Mar 3, 2024 by E.K.Studd

#### LOAD LIBRARIES ####
library(data.table)
library(lubridate)
library(suncalc)
library(tidyverse)

### CREATE FUNCTION ####
# this function loads each file, identifies time of day, calculates daily summary of beahviour by tod of day, and saves the output as a new file.  Also saves the raw data plus tod designation as a new file for future use.

compileSqr=function(axyfile) {
  axy<-fread(axyfile)  #load file
  axy<-mutate(axy, date=as.Date(dmy_hms(datetime)), datetime=dmy_hms(datetime, tz="America/Los_Angeles")) # set time as pacific, create date column

  #get sunrise and sunset times for squirrel camp
  sun=getSunlightTimes(date=seq.Date(min(axy$date), max(axy$date), by=1), lat=60.57, lon=-138.2, tz="America/Los_Angeles")  
  sun$date=as.Date(sun$date)
  sun<-mutate(sun, dusk2=fifelse(as.Date(dusk)==as.Date(dawn), dusk, dusk-days(1)))
  sun$dusk2<- fifelse(date(sun$dusk)==date(sun$dawn), sun$dusk, sun$dusk-days(1)) # makes adjustment for when dusk is after midnight which is needed for classifying squirrel times
  sun=select(sun, date, sunrise, sunset, dusk, dusk2, dawn) # choose columns to keep

  #### join sun times to axy data
  axy=left_join(axy, sun)

  # label each row of data as being dawn, day, dusk, or night
  axy2=mutate(axy, tod=ifelse(datetime>sunrise &datetime<sunset, "day", ifelse(datetime>dawn &datetime<sunset, "dawn", ifelse(dusk2==dusk & (datetime<dawn |datetime>dusk), "night",ifelse (dusk2!=dusk & datetime>dusk2 & datetime<dawn, "night", "dusk")))))
  #for week around solstice there is not end of dusk/start of dawn.  So for these squirrels, the above code will put an NA for tod. Adjust these to read "dawn"
  axy2<-mutate(axy2, tod=replace_na(tod, "dawn"))
  
  # Calculate the count of each behaviour per time period of each day.
  sum<-axy2 %>% group_by(date, tod, All) %>% summarise(n=n())
  sum<-pivot_wider(sum, names_from=All, values_from=n)
  #add id
  sum$id=sub("squirrel", "", scan(text=axyfile, what=character(), sep="_", quiet=T)[2])
  #replace NAs with 0
  sum[is.na(sum)]<-0
  # calculate number of seconds in each time period of the day. 
  sum<-mutate(sum, Total=Feed+Forage+Travel+NestMove+NestNotMove+NotMoving)

  # select the columns in the raw data that we will keep. 
  axy2<-select(axy2, datetime, temp, odba, Nest, All, date, tod)


  #### save the table of daily values, and the raw data plus tod classification.  
  write.csv(sum, paste("~/Desktop/Data_behaviorClassCompile_20240109/", unlist(strsplit(axyfile, "[.]"))[1], "daily.csv", sep=""), row.names=F)
  write.csv(axy2, paste("~/Desktop/Data_behaviorClassCompile_20240109/", unlist(strsplit(axyfile, "[.]"))[1], "rawTOD.csv", sep=""), row.names=F)

}


#####################################################-
### RUN Compile function on all files #####
setwd("~/Desktop/20240119")
files=dir(pattern="*.csv")
files[] %>% map(compileSqr)


###################################################################--
#### LOAD ALL the daily value files and COMPILE into one file. #####

#### Create function to fix the ids when they are loaded to be compiled.
#id must be correct in the file name. 
fix_id<-function(axyfile){
  data<-fread(axyfile)
  #fix IDs to match file name (a couple are wrong in the original files. This corrects them)
  data$id<-sub("squirrel", "", scan(text=axyfile, what=character(), sep="_", quiet=T)[2])
  return(data)
}

#### Load and Compile
setwd("~/Desktop/Data_behaviorClassCompile_20240109")
files=dir(pattern="*daily.csv")
daily<-files %>% map(fix_id) %>% reduce(rbind)
daily=daily %>% group_by(id, date) %>% mutate(Tot=sum(Total))
daily=filter(daily, Tot==86400)

daily=select(daily, -Tot)

#save compiled file. 
write.csv(daily, "~/Desktop/KRSP_sqr_axy_all_2014_2022_dailybyTOD_20240303.csv", row.names=F)





