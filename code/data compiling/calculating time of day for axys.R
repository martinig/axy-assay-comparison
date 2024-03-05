#I am preparing the axy data here
#adapted from code written by E. K. Studd
#Last edited on Oct 20, 2023 by A. R. Martinig 

#Delete previous information stored 
rm(list=ls(all=T))

##set wd to the folder with all your csv's in it
setwd("~/Documents/Files/Post-docs/UofC/Jonas (ECOL 530)/Data analysis")

#############
#DO NOT EDIT ANYTHING BELOW THIS
#############

#############
#DO NOT EDIT ANYTHING BELOW THIS
#############

#############
#DO NOT EDIT ANYTHING BELOW THIS
#############

#############
#DO NOT EDIT ANYTHING BELOW THIS
#############

#############
#DO NOT EDIT ANYTHING BELOW THIS
#############

#############
#DO NOT EDIT ANYTHING BELOW THIS
#############

#############
#DO NOT EDIT ANYTHING BELOW THIS
#############

#############
#DO NOT EDIT ANYTHING BELOW THIS
#############

#############
#DO NOT EDIT ANYTHING BELOW THIS
#############

options(scipen=999, dplyr.width = Inf, tibble.print_min = 50, repos='http://cran.rstudio.com/') #scipen forces outputs to not be in scientific notation #dplyr.width will show all columns for head() function and tibble.print_min sets how many rows are printed and repos sets the cran mirror

#load libraries
packages=c("ggplot2", "dplyr", "lubridate", "tidyverse", "broom", "FSA", "tidyr", "DescTools", "grid", "lattice", "suncalc")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
lapply(packages, library, character.only = TRUE)

select<-dplyr::select
filter<-dplyr::filter


##############################
#         time of day calculation          #
##############################


todCalc=function(file){
  axy=fread(file)
  axy$date=ymd(axy$date)
  axy$dtime=ymd_hms(axy$dtime, tz="America/Los_Angeles")
  sun=getSunlightTimes(date=seq.Date(ymd("2014-01-01"), ymd("2017-12-31"), by=1), lat=60.57, lon=-138.2, tz="America/Los_Angeles")
  sun$date=as.Date(sun$date)
  sun=select(sun, date, sunrise, sunset, dusk, dawn)
  axy=left_join(axy, sun)
  axy=mutate(axy, tod=ifelse(dtime>sunrise &dtime<sunset, "day", ifelse(dtime>dawn &dtime<sunset, "dawn", ifelse(dtime>dawn &dtime<dusk, "dusk","night" ))))
  axy2=axy %>% group_by(Squirrel, date, tod, All) %>% summarise(Seconds=n())
  axy2=spread(axy2, All, Seconds)
  axy2[is.na(axy2)]=0
  axy2$total=rowSums(axy2[4:9])
  write.csv(axy2, paste("tod2", file, sep="_"))
}