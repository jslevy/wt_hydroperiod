#This code is designed to determine the hydropattern at the static station. It does not output graphic content, but rather dates

setwd("~/Dropbox/R_projects/hydropattern_station")


library(lubridate)
library(tidyverse)


#import from raw csv

data <- read.csv("raw_data/Static_Station_Running_Total_09101112131415_dotplot_career.csv")

#clip out dead columns

data <- data[,0:32]

#get unique years in record
years <- unique((data$Year))

#make hour a useful number

data$Hour <- data$Hour/100

data$fracoday <- data$Hour/24

#convert Excel_Data to date and time

data$date <- as.Date(data$ExcelDate, format = "%m/%d/%y")

#get summer data

data$month <- month(data$date)
data$day <- day(data$date)

summer_data <- data[data$month %in% c(1,2,3,10,11,12),]

summer_data$datestamp <- summer_data$day + summer_data$fracoday

summer_data$dayofsummer <- ifelse(summer_data$month == 11,
                                  summer_data$day + summer_data$fracoday,
                                  ifelse(summer_data$month == 12,
                                         30+summer_data$day + summer_data$fracoday,
                                         ifelse(summer_data$month == 1,
                                                61+summer_data$day + summer_data$fracoday,
                                                   ifelse(summer_data$month == 2,
                                                       92+summer_data$day + summer_data$fracoday,
                                                          ifelse(summer_data$month == 3,
                                                              120+summer_data$day + summer_data$fracoday,
                                                       ifelse(summer_data$month == 10,
                                                               (summer_data$day + summer_data$fracoday)-32,0))))))


#combine the first half of the season from one year with the second half of the season from the next year

data_log <- data.frame(oct_baseline=double(),
                       oct_std=double(),
                       march_baseline=double(),
                       march_std=double(),
                       first_thaw=double(),
                       last_thaw=double(),
                       year=integer(),
                       t_thaw=double(),
                       t_freeze=double(),
                       stringsAsFactors=FALSE) 

for(i in years){
  
spring_data <- summer_data[summer_data$Year == i & summer_data$month %in% c(10,11,12),]

fall_data <- summer_data[summer_data$Year == i+1 & summer_data$month %in% c(1,2,3),]

#merge previous data frames

this_summer_data <- rbind(spring_data, fall_data)

oct_data <- this_summer_data[this_summer_data$month == 10,]

oct_baseline <- mean(oct_data$VWCA)
oct_std <- sd(oct_data$VWCA)

above_oct <- this_summer_data[this_summer_data$VWCA > (oct_baseline+(5*oct_std)),]

first_thaw <- above_oct[1,38]
t_thaw <- above_oct[1,7]

mar_data <- this_summer_data[this_summer_data$month == 3,]

mar_baseline <- mean(mar_data$VWCA)
mar_std <- sd(mar_data$VWCA)

above_mar <- this_summer_data[this_summer_data$VWCA > (mar_baseline+(5*mar_std)),]

last_thaw <- above_mar[nrow(above_mar),38]
t_freeze <- above_mar[nrow(above_mar),7]

this_year_log <- data.frame(oct_baseline=oct_baseline, oct_std=oct_std, mar_baseline=mar_baseline, mar_std=mar_std, first_thaw=first_thaw, last_thaw=last_thaw, year=i, t_thaw=t_thaw, t_freeze=t_freeze)

data_log <- rbind(data_log,this_year_log)

}


#import CSV of observations and fix the date and time stamps

obs <- read.csv("WHC_WT_obs.csv")

obs$Date <- as.Date(obs$Date, format = "%Y-%m-%d")

obs_combined_dt <- obs %>% mutate(datetime = ymd_hms(paste(Date,Time)))

obs <- obs_combined_dt

#Separate YMD_HMS

obs$seq <- 1:nrow(obs)

obs$year <- year(obs$datetime)
obs$month <- month(obs$datetime)
obs$day <- day(obs$datetime)
obs$hour <- hour(obs$datetime)
obs$minute <- minute(obs$datetime)
obs$fracoday <- (obs$hour/24)+(obs$minute/(24*60))

#Specify Year within dataset

obs_summer_data <- obs[obs$month %in% c(1,2,10,11,12),]

#Combine spring + fall day for just the current season

obs_spring_data <- obs_summer_data[obs_summer_data$year == start_of_season & obs_summer_data$month %in% c(11,12),]

obs_fall_data <- obs_summer_data[obs_summer_data$year == end_of_season & obs_summer_data$month %in% c(1,2,3),]

this_summer_obs <- rbind(obs_spring_data, obs_fall_data)

#Create FID labels for each row

this_summer_obs$dayofsummer <- ifelse(this_summer_obs$month == 11, this_summer_obs$day + this_summer_obs$fracoday,
                                      ifelse(this_summer_obs$month == 12, 30 + this_summer_obs$day + this_summer_obs$fracoday,
                                             ifelse(this_summer_obs$month == 1, 61 + this_summer_obs$day + this_summer_obs$fracoday,
                                                    ifelse(this_summer_obs$month == 2, 92 + this_summer_obs$day + this_summer_obs$fracoday,
                                                           ifelse(this_summer_obs$month == 10, (this_summer_obs$day + this_summer_obs$fracoday)-32,0)))))

#convert to local time

this_summer_obs$dayofsummer <- this_summer_obs$dayofsummer+0.5


#add vertical lines at instances of WT activity

for (i in this_summer_obs$seq){
  abline(v = this_summer_obs$dayofsummer, col = paste(ifelse(this_summer_obs$WT.present. == "Y", "purple",
                                                             ifelse(this_summer_obs$WT.present. == "MY", "pink",
                                                                    ifelse(this_summer_obs$WT.present. == "N", "black",
                                                                           "white")))))
}

#Create Legend

legend("topleft", legend = c("T @ 0c cm", "T @ 5 cm", "T @ 10 cm"),
       col = c("red", "green", "blue"), lty = 1, cex = 0.8, bg = "white")

dev.off()



plot(summer_data$TsoilA~summer_data$dayofsummer)
points(summer_data$dayofsummer,summer_data$VWCA*100)


#clip out RH and radn and soil and air

site_rh_radn <- merge(radn_data[,c(1,3:10)],rh_data[,3:4], by = "DATE_TIME")
site_rh_radn_air <- merge(site_rh_radn,air_data[,3:4], by = "DATE_TIME")
site_rh_radn_air_soil <- merge(site_rh_radn_air,soil_data[,3:4], by = "DATE_TIME")

#clip out jan night data

site_rh_radn_air_soil_jan = site_rh_radn_air_soil[site_rh_radn_air_soil$Month == 1,]

site_rh_radn_air_soil_jan_midnight = site_rh_radn_air_soil_jan[site_rh_radn_air_soil_jan$Hour == 0,]
site_rh_radn_air_soil_jan_0100 = site_rh_radn_air_soil_jan[site_rh_radn_air_soil_jan$Hour == 1,]

site_rh_radn_air_soil_jan_midnight_to_2am <- rbind(site_rh_radn_air_soil_jan_midnight,site_rh_radn_air_soil_jan_0100)

#recalculates reflectivity

site_rh_radn_air_soil_jan_midnight_to_2am$Reflectivity <- site_rh_radn_air_soil_jan_midnight_to_2am$SWRADOUT/site_rh_radn_air_soil_jan_midnight_to_2am$SWRADIN

#recalculate ground T-->RH. Now using Magnus eqns 

site_rh_radn_air_soil_jan_midnight_to_2am$satvappresair <- ifelse(site_rh_radn_air_soil_jan_midnight_to_2am$AIRT3M > 0, 610.94*exp((17.625*site_rh_radn_air_soil_jan_midnight_to_2am$AIRT3M)/(243.04+site_rh_radn_air_soil_jan_midnight_to_2am$AIRT3M)), 611.21*exp((22.587*site_rh_radn_air_soil_jan_midnight_to_2am$AIRT3M)/(273.86+site_rh_radn_air_soil_jan_midnight_to_2am$AIRT3M))) 
site_rh_radn_air_soil_jan_midnight_to_2am$satvapsoil <- ifelse(site_rh_radn_air_soil_jan_midnight_to_2am$SOILT0CM > 0, 610.94*exp((17.625*site_rh_radn_air_soil_jan_midnight_to_2am$SOILT0CM)/(243.04+site_rh_radn_air_soil_jan_midnight_to_2am$SOILT0CM)), 611.21*exp((22.587*site_rh_radn_air_soil_jan_midnight_to_2am$SOILT0CM)/(273.86+site_rh_radn_air_soil_jan_midnight_to_2am$SOILT0CM)))

site_rh_radn_air_soil_jan_midnight_to_2am$pvapair <- (site_rh_radn_air_soil_jan_midnight_to_2am$RH/100)*site_rh_radn_air_soil_jan_midnight_to_2am$satvappresair

site_rh_radn_air_soil_jan_midnight_to_2am$soilRH <- (site_rh_radn_air_soil_jan_midnight_to_2am$pvapair/site_rh_radn_air_soil_jan_midnight_to_2am$satvapsoil)*100
  
#clean out snow pixels with reflectivity > 0.4 and remove NAs

site_rh_radn_air_soil_jan_mto2_nosnow <- site_rh_radn_air_soil_jan_midnight_to_2am[site_rh_radn_air_soil_jan_midnight_to_2am$Reflectivity<0.4,]

site_rh_radn_air_soil_jan_mto2_nosnow_noNA <- site_rh_radn_air_soil_jan_mto2_nosnow[complete.cases(site_rh_radn_air_soil_jan_midnight_to_2am),]

#split by year

maxyear <- max(site_rh_radn_air_soil_jan_mto2_nosnow_noNA$Year, na.rm=TRUE)
minyear <- min(site_rh_radn_air_soil_jan_mto2_nosnow_noNA$Year, na.rm=TRUE)
years <- c(minyear:maxyear)

#start doing work by year. Note currently set at 2 sd! 

#make a log file

coef_log <- data.frame(slope=double(),
                 intercept=double(),
                 pval=double(),
                 refl_min=double(),
                 refl_max=double(),
                 pct_change_alb=double(),
                 year=integer(),
                 stringsAsFactors=FALSE) 

filtered_coef_log <- data.frame(slope=double(),
                       intercept=double(),
                       pval=double(),
                       refl_min=double(),
                       refl_max=double(),
                       pct_change_alb=double(),
                       year=integer(),
                       stringsAsFactors=FALSE) 


for(i in years){
  thisyearsdata <- subset(site_rh_radn_air_soil_jan_mto2_nosnow_noNA, Year == i, select=c(Year,RH,Reflectivity,soilRH,Day))
  
  refl_sd_thisyear <- sd(thisyearsdata$Reflectivity, na.rm=TRUE)
  
  #makes the linear model for filtering the data
  
  refl_lm_thisyear <- lm(Reflectivity~soilRH, data=thisyearsdata)
  
  refl_lm_coeffs_thisyear <- summary(refl_lm_thisyear)
  
  intercept_thisyear <- refl_lm_coeffs_thisyear$coefficients[1,1]
  
  slope_thisyear <- refl_lm_coeffs_thisyear$coefficients[2,1]
  
  slope_pval_thisyear <- refl_lm_coeffs_thisyear$coefficients[2,4]
  
  refl_min <- min(thisyearsdata$Reflectivity, na.rm=TRUE)
  
  refl_max <- max(thisyearsdata$Reflectivity, na.rm=TRUE)
  
  pct_change_alb <- ((refl_max-refl_min)/refl_max)
  
  this_year_coeffs <- data.frame(slope=slope_thisyear, intercept=intercept_thisyear, pval=slope_pval_thisyear, refl_min=refl_min, refl_max=refl_max, pct_change_alb, year=i)
  
  coef_log <- rbind(coef_log,this_year_coeffs)
  
  thisyearsdata$prediction <- (slope_thisyear*thisyearsdata$soilRH) + intercept_thisyear + (2*refl_sd_thisyear)
  
  thisyearsdata_filtered <- subset(thisyearsdata, Reflectivity < prediction, select=c(Year,soilRH,Reflectivity,Day))
  
  #makes the linear model for plotting and reporting
  
  refl_lm_thisyear_filt <- lm(Reflectivity~soilRH, data=thisyearsdata_filtered)
  
  refl_lm_coeffs_thisyear_filt <- summary(refl_lm_thisyear_filt)
  
  intercept_thisyear_filt <- refl_lm_coeffs_thisyear_filt$coefficients[1,1]
  
  slope_thisyear_filt <- refl_lm_coeffs_thisyear_filt$coefficients[2,1]
  
  slope_pval_thisyear_filt <- refl_lm_coeffs_thisyear_filt$coefficients[2,4]
  
  refl_min_filt <- min(thisyearsdata_filtered$Reflectivity, na.rm=TRUE)
  
  refl_max_filt <- max(thisyearsdata_filtered$Reflectivity, na.rm=TRUE)
  
  pct_change_alb_filt <- ((refl_max_filt-refl_min_filt)/refl_max_filt)
  
  this_year_coeffs_filt <- data.frame(slope=slope_thisyear_filt, intercept=intercept_thisyear_filt, pval=slope_pval_thisyear_filt, refl_min=refl_min_filt, refl_max=refl_max_filt, pct_change_alb_filt, year=i)
  
  filtered_coef_log <- rbind(filtered_coef_log,this_year_coeffs_filt)
  
  #figure making
  
  quartz()
 
  # colorRamp produces custom palettes, but needs values between 0 and 1
  colorFunction <- colorRamp(c("darkblue", "black", "red"))
  daycolor <- with(thisyearsdata_filtered, (Day - min(Day)) / (max(Day)-min(Day)))
  
  # Apply colorRamp and switch to hexadecimal representation
  zMatrix <- colorFunction(daycolor)
  zColors <- rgb(zMatrix, maxColorValue=255)
  plot(thisyearsdata_filtered$Reflectivity~thisyearsdata_filtered$soilRH, col = zColors,
       xlab = paste(i,"Soil RH at",friendlyname,"(%)",sep = " "),
       ylab = "Reflectivity",
       xlim=c(0, 100), ylim=c(0.0, 0.4),
       pch = 16,)
  
  abline(refl_lm_thisyear_filt)
  
  #round coeffs to show better output
  
  cf <- round(coef(refl_lm_coeffs_thisyear_filt),4)
  
  eq <- paste0("Reflectivity =", cf[2,1], " • soilRH + ",cf[1,1])
  pval <- paste0(ifelse(cf[2,4]<0.01, " P < 0.01 ", " P > 0.01 "))
  
  # printing of the equation
  
  mtext(eq, 3, line=-2)    
  mtext(pval, 3, line=-3)
  
  #       col = yeardata$cols)
  
  #export the csv
  
  write.csv(thisyearsdata_filtered, file=paste(path_output,"site_rh_radn_air_soilRH_",sitename,"_",substr(i,1,4),".csv", sep = ""))
  
  #export the figure
  
  quartz.save(file=paste(path_output,"soilRH_refl_daycolor_",sitename,"_",substr(i,1,4),".png",sep = ""), type = "png", device = dev.cur(), dpi = 600)
  
  #make the qq and residual plots
  
  library(car)
  
  png(file=paste(path_output,"soilRH_refl_",sitename,"_",substr(i,1,4),"_residual.png",sep = ""),
      width=1076, height=489)
  residualPlot(refl_lm_thisyear, main=paste(i,"QQ for",friendlyname,sep = " "))
  dev.off()
  
  png(file=paste(path_output,"soilRH_refl_",sitename,"_",substr(i,1,4),"_qq.png",sep = ""),
      width=1076, height=489)
  qqPlot(refl_lm_thisyear, main=paste(i,"QQ for",friendlyname,sep = " "))
  dev.off()
  
  quartz()
  # Resolution of the legend
  n <- 10
  
  # colorRampPalette produces colors in the same way than colorRamp
  plot(x=NA, y=NA, xlim=c(0,n), ylim=0:1, xaxt="n", yaxt="n", xlab="z", ylab="")
  pal <- colorRampPalette(c("darkblue", "black", "red"))(n)
  rect(xleft=0:(n-1), xright=1:n, ybottom=0, ytop=1, col=pal)
  
  # Custom axis ticks (consider pretty() for an automated generation)
  lab <- c(min(thisyearsdata_filtered$Day), median(thisyearsdata_filtered$Day), max(thisyearsdata_filtered$Day))
  at <- (lab - min(thisyearsdata_filtered$Day)) / (max(thisyearsdata_filtered$Day) - min(thisyearsdata_filtered$Day)) * n
  axis(side=1, at=at, labels=lab)
  
  quartz.save(file=paste(path_output,"day_color_soilRH_refl_",sitename,"_",substr(i,1,4),"_legend.png",sep = ""), type = "png", device = dev.cur(), dpi = 600)
  
  
}

#summarize the coeff log

plot(filtered_coef_log$slope~filtered_coef_log$pval,
     xlab = "p Value",
     ylab = "Slope",
     main = paste("Soil-RH Corrected Slope vs. p value at",friendlyname,sep = " "),
     log = 'x')

#output the coefficient log

write.csv(filtered_coef_log, file=paste(path_output,"filtered_soil_corr_RH_coef_log_",sitename,".csv", sep = ""))

#how long is RH > 50% and T between -4 and +8? 

jan_night_wet_records <- subset(site_rh_radn_air_soil_jan_mto2_nosnow_noNA, soilRH>40 & SOILT0CM > -4 & SOILT0CM < 8, select=c(Year,RH,Reflectivity,soilRH))

jan_night_frac_obs <- nrow(jan_night_wet_records) / nrow(site_rh_radn_air_soil_jan_mto2_nosnow_noNA)

#recalculates reflectivity for whole record

site_rh_radn_air_soil$Reflectivity <- site_rh_radn_air_soil$SWRADOUT/site_rh_radn_air_soil$SWRADIN

#recalculate ground T-->RH for whole record

site_rh_radn_air_soil$satvappresair <- ifelse(site_rh_radn_air_soil$AIRT3M > 0, 610.94*exp((17.625*site_rh_radn_air_soil$AIRT3M)/(243.04+site_rh_radn_air_soil$AIRT3M)), 611.21*exp((22.587*site_rh_radn_air_soil$AIRT3M)/(273.86+site_rh_radn_air_soil$AIRT3M))) 
site_rh_radn_air_soil$satvapsoil <- ifelse(site_rh_radn_air_soil$SOILT0CM > 0, 610.94*exp((17.625*site_rh_radn_air_soil$SOILT0CM)/(243.04+site_rh_radn_air_soil$SOILT0CM)), 611.21*exp((22.587*site_rh_radn_air_soil$SOILT0CM)/(273.86+site_rh_radn_air_soil$SOILT0CM)))

site_rh_radn_air_soil$pvapair <- (site_rh_radn_air_soil$RH/100)*site_rh_radn_air_soil$satvappresair

site_rh_radn_air_soil$soilRH <- (site_rh_radn_air_soil$pvapair/site_rh_radn_air_soil$satvapsoil)*100

#clean out snow pixels with reflectivity > 0.4 and remove NAs

site_rh_radn_air_soil_nosnow <- site_rh_radn_air_soil[site_rh_radn_air_soil$Reflectivity<0.4,]
site_rh_radn_air_soil_nosnow_noNA <- site_rh_radn_air_soil_nosnow[complete.cases(site_rh_radn_air_soil),]

all_wet_records <- subset(site_rh_radn_air_soil, soilRH>50 & SOILT0CM > -4 & SOILT0CM < 8, select=c(Year,RH,Reflectivity,soilRH, SOILT0CM, Hour))

all_frac_obs <- nrow(all_wet_records) / nrow(site_rh_radn_air_soil)

#plot RH vs. soilRH

plot(site_rh_radn_air_soil_jan_mto2_nosnow_noNA$soilRH~site_rh_radn_air_soil_jan_mto2_nosnow_noNA$RH)
abline(a=0,b=1)

#make fraction of year histogram

#hist worked better for hours per year of wet records--note, y axis needs to be divided by 4
hist(all_wet_records$Year, breaks=18, main="Potentially 'Wet' Conditions", xlab="Hour of Day", ylab="Hours per Year")

#hour of occurrence of wet conditions. note, y axis needs to be divided by 4

wet_hours <- table(unlist(all_wet_records$Hour))
barplot(wet_hours, space=0, main = "Timing of Potential 'Wet' Conditions", xlab = "Hour of Day", ylab="Hours per Year")
