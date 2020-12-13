########################
# This file pulls data from the MA DPH for weekly updates on public health data,
# in order to process data regarding health disparities in racial/ethnic populations
# in MA. Specifically: White, Black, Asian, and Hispanic populations. The state 
# does not collect more granular data.
# 
# This script excludes initial data from the file as it reflects cumulative 
# updates from all time prior to June 1, 2020. There are additional early spikes
# as adjustments were made.
#
# Also, data are in weekly cumulative format from November 1 onward. Adjustments
# have been made for that.

# This script generates a number of plots, as well as formats data for upload
# into the main tool to generate Shewhart c-charts and log10 charts for each
# population noted above.
#
# Liz Turi
# github: lturi
# 12/12/2020
############################################################3

library(ggplot2)
library(directlabels)
library(tidyverse)
library(readxl)
library(utils)
library(httr)
library(DT)
library(lubridate)
library(vioplot)
source("helper.R")
setwd("~/workspace/COVID_control_chart_public")

defStartdate <- NA
defBuffer <- 7

#defBaseline is the default value of points to use to compute the exponential growth control limits
defBaseline <- 21

#get most recent date data is available since data are published Thursdays at 5pm

most_recent_sunday<-cut(Sys.Date(),breaks='week',start.on.monday=F)
most_recent_thursday<-as.Date(most_recent_sunday)+4
file_format<-format(most_recent_thursday,"%Y-%m-%d")
file_format_out<-format(most_recent_thursday,"%m/%d/%Y")
data_file_ma   <- paste0('data/ma_state_weeky_re_data_', file_format, '.csv')


url_format<-format(most_recent_thursday,"%B-%d-%Y")
ma_url<-paste0('https://www.mass.gov/doc/weekly-public-health-report-raw-data-',url_format,'/download')

#if (!file.exists(data_file_ma)) {
  
  response_ma <- try(httr::GET(url = ma_url,
                                    authenticate(':', ':', type='ntlm'),
                                    config(timeout = 20),
                                    write_disk('data/temp.xslx', overwrite = TRUE)))
#}

#get just the race/ethnicity tab from the weekly excel spreadsheet
#if (!file.exists(data_file_ma)) {
race_ethnicity_data_ma <- read_excel('data/temp.xslx', sheet = "RaceEthnicity")
write.csv(race_ethnicity_data_ma,data_file_ma)

file.remove('data/temp.xslx')
#}else{
 # race_ethnicity_data_ma<-read.csv(data_file_ma)
#}

#convert from cumulative numbers to daily
df <- as.data.frame(race_ethnicity_data_ma)


df$`Race/Ethnicity`<-as.factor(df$`Race/Ethnicity`)
gr <- df %>%
  group_by(df$`Race/Ethnicity`)
sp_gr<-group_split(gr)
sp_gr[[2]]

#there are 6 groups. Ignore [[1]],it's a note
# [[2]] - Hispanic
# [[3]] - Non-Hispanic Asian
# [[4]] - Non-Hispanic Black
# [[5]] - Non-Hispanic Other
# [[6]] - Non-Hispanic White
# [[7]] - Unknown/Missing (see weekly report for what this means)

hispanic <- as.data.frame(sp_gr[[2]])
hispanic$daily_cases <-diff(c(0,hispanic$`All Cases`))
hispanic$daily_hospitalized <-diff(c(0,hispanic$`Ever Hospitaltized`))
hispanic$daily_death <- diff(c(0,hispanic$Deaths))
hispanicwk<-hispanic
hispanicwk<-hispanicwk %>% group_by(week = week(Date)) %>% summarise(avg_cases = mean(daily_cases),avg_deaths = mean(daily_death),avg_hosp=mean(daily_hospitalized))
hispanicwk<-hispanicwk[-1,]
hispanicwk[23,]<-hispanicwk[23,]/3
hispanicwk[23,1]<-45
hispanicwk[c(24,25,26,27),]<-hispanicwk[c(24,25,26,27),]/7
hispanicwk[c(24,25,26,27),1]<-c(46,47,48,49)
hispanicwk[28,]<-hispanicwk[28,]/5
hispanicwk[28,1]<-50


asian <- as.data.frame(sp_gr[[3]])
asian$daily_cases <-diff(c(0,asian$`All Cases`))
asian$daily_hospitalized <-diff(c(0,asian$`Ever Hospitaltized`))
asian$daily_death <- diff(c(0,asian$Deaths))
asianwk<-asian
asianwk<-asianwk %>% group_by(week = week(Date)) %>% summarise(avg_cases = mean(daily_cases),avg_deaths = mean(daily_death),avg_hosp=mean(daily_hospitalized))
asianwk<-asianwk[-1,]
asianwk[23,]<-asianwk[23,]/3
asianwk[23,1]<-45
asianwk[c(24,25,26,27),]<-asianwk[c(24,25,26,27),]/7
asianwk[c(24,25,26,27),1]<-c(46,47,48,49)
asianwk[28,]<-asianwk[28,]/5
asianwk[28,1]<-50

black <- as.data.frame(sp_gr[[4]])
black$daily_cases <-diff(c(0,black$`All Cases`))
black$daily_hospitalized <-diff(c(0,black$`Ever Hospitaltized`))
black$daily_death <- diff(c(0,black$Deaths))
blackwk<-black
blackwk<-blackwk %>% group_by(week = week(Date)) %>% summarise(avg_cases = mean(daily_cases),avg_deaths = mean(daily_death),avg_hosp=mean(daily_hospitalized))
blackwk<-blackwk[-1,]
blackwk[23,]<-blackwk[23,]/3
blackwk[23,1]<-45
blackwk[c(24,25,26,27),]<-blackwk[c(24,25,26,27),]/7
blackwk[c(24,25,26,27),1]<-c(46,47,48,49)
blackwk[28,]<-blackwk[28,]/5
blackwk[28,1]<-50


white <- as.data.frame(sp_gr[[6]])
white$daily_cases <-diff(c(0,white$`All Cases`))
white$daily_hospitalized <-diff(c(0,white$`Ever Hospitaltized`))
white$daily_death <- diff(c(0,white$Deaths))
whitewk<-white
whitewk<-whitewk %>% group_by(week = week(Date)) %>% summarise(avg_cases = mean(daily_cases),avg_deaths = mean(daily_death),avg_hosp=mean(daily_hospitalized))
whitewk<-whitewk[-1,]
whitewk[23,]<-whitewk[23,]/3
whitewk[23,1]<-45
whitewk[c(24,25,26,27),]<-whitewk[c(24,25,26,27),]/7
whitewk[c(24,25,26,27),1]<-c(46,47,48,49)
whitewk[28,]<-whitewk[28,]/5
whitewk[28,1]<-50

txfm <-data.frame(hispanic$Date,hispanic$daily_cases,hispanic$daily_hospitalized,hispanic$daily_death,
                        asian$daily_cases,asian$daily_hospitalized,asian$daily_death,
                        black$daily_cases,black$daily_hospitalized,black$daily_death,
                        other$daily_cases,other$daily_hospitalized,other$daily_death,
                        white$daily_cases,white$daily_hospitalized,white$daily_death
                        )
names(txfm) <- c("Date","Hispanic_daily_cases","Hispanic_daily_hosp","Hispanic_daily_death",
                 "Asian_daily_cases","Asian_daily_hosp","Asian_daily_death",
                 "Black_daily_cases","Black_daily_hosp","Black_daily_death",
                 "White_daily_cases","White_daily_hosp","White_daily_death"
)
txfm$all_daily_cases<-txfm$Hispanic_daily_cases+txfm$Asian_daily_cases+txfm$Black_daily_cases+txfm$White_daily_cases

txfm$all_daily_hospital<-txfm$Hispanic_daily_hosp+txfm$Asian_daily_hosp+txfm$Black_daily_hosp+txfm$White_daily_hosp

txfm$all_daily_death<-txfm$Hispanic_daily_death+txfm$Asian_daily_death+txfm$Black_daily_death+txfm$White_daily_death

txfm[,c("Asian_daily_cases","Hispanic_daily_cases","Black_daily_cases","White_daily_cases")]


txfmwk <-data.frame(hispanicwk$week,hispanicwk$avg_cases,hispanicwk$avg_hosp,hispanicwk$avg_deaths,
                  asianwk$avg_cases,asianwk$avg_hosp,asianwk$avg_deaths,
                  blackwk$avg_cases,blackwk$avg_hosp,blackwk$avg_deaths,
                 whitewk$avg_cases,whitewk$avg_hosp,whitewk$avg_deaths
)
names(txfmwk) <- c("Week","Hispanic_wk_cases","Hispanic_wk_hosp","Hispanic_wk_death",
                 "Asian_wk_cases","Asian_wk_hosp","Asian_wk_death",
                 "Black_wk_cases","Black_wk_hosp","Black_wk_death",
                 "White_wk_cases","White_wk_hosp","White_wk_death"
)



#calc case rate adjusted by racial/ethnic background using 2017 data
# total pop 6,859,819
# Black %: 7% = 480187.3
# White %: 71.5% = 
# Asian %: 6.6% = 
# Hispanic %: 11.8% = 
total_pop=6859819
black_adjust = 100000/(total_pop*.07)
white_adjust = 100000/(total_pop*.715)
asian_adjust=100000/(total_pop*.066)
hispanic_adjust=100000/(total_pop*.118)

bcr_adjusted <- txfmwk$Black_wk_cases*black_adjust
wcr_adjusted <- txfmwk$White_wk_cases*white_adjust
acr_adjusted <-txfmwk$Asian_wk_cases*asian_adjust
hcr_adjusted <- txfmwk$Hispanic_wk_cases*hispanic_adjust


vioplot(bcr_adjusted,wcr_adjusted,acr_adjusted,hcr_adjusted, names=c("Black","White","Asian","Hispanic"),col="purple",
        xlab="",ylab="weekly case rate",main="Adjusted AverageCOVID-19 Weekly Case Rate by Cohort 100,000 persons\nJune 2, 2020 to December 10, 2020")

bdr_adjusted <- txfmwk$Black_wk_death*black_adjust
wdr_adjusted <- txfmwk$White_wk_death*white_adjust
adr_adjusted <-txfmwk$Asian_wk_death*asian_adjust
hdr_adjusted <- txfmwk$Hispanic_wk_death*hispanic_adjust

vioplot(bdr_adjusted,wdr_adjusted,adr_adjusted,hdr_adjusted, names=c("Black","White","Asian","Hispanic"),col="purple",
        xlab="",ylab="weekly death rate",main="Adjusted Average  COVID-19  Weekly Death Rate by Cohort 100,000 persons\nJune 2, 2020 to December 10, 2020")


bhr_adjusted <- txfmwk$Black_wk_hosp*black_adjust
whr_adjusted <- txfmwk$White_wk_hosp*white_adjust
ahr_adjusted <-txfmwk$Asian_wk_hosp*asian_adjust
hhr_adjusted <- txfmwk$Hispanic_wk_hosp*hispanic_adjust

vioplot(bhr_adjusted,whr_adjusted,ahr_adjusted,hhr_adjusted, names=c("Black","White","Asian","Hispanic"),col="purple",
        xlab="",ylab="weekly ever hospitalized rate",main="Adjusted Average COVID-19 Weekly Ever Hospitalized Case Rate by Cohort 100,000 persons\nJune 2, 2020 to December 10, 2020")




p <- ggplot(txfm, aes(x=as.Date(Date)))+
  geom_line( aes(y=Hispanic_daily_death/all_daily_cases*hispanic_adjust,color="Hispanic")) + 
  geom_line( aes(y=Black_daily_death/all_daily_cases*black_adjust,color="Black")) +
  geom_line( aes(y=Asian_daily_death/all_daily_cases*asian_adjust,color="Asian")) +
  geom_line( aes(y=White_daily_death/all_daily_cases*white_adjust,color="White")) +
  ylim(y=c(0,.025))+
  labs(x = "",y="Death Rate",
       title = "Daily COVID-19 Death Rate \n(Normalized and Adjusted for Cohort by 100,000 persons)",
       color = "Legend") +
  scale_x_date(limit=c(as.Date("2020-06-08"),as.Date("2020-11-02"))) +
  scale_color_manual(
                     values = c( "Hispanic" = "steelblue", "Black" = "red", 
                                 "Asian" = "purple","White"="darkgreen"),
                     labels = c("Asian", "Black", "Hispanic","White"))+
  theme(axis.text.x=element_text(angle=60, hjust=1))
  p +theme(legend.position = "bottom")

casesp <- ggplot(txfm, aes(x=as.Date(Date)))+
  geom_line( aes(y=Hispanic_daily_cases/all_daily_cases*hispanic_adjust,color="Hispanic")) + 
  geom_line( aes(y=Black_daily_cases/all_daily_cases*black_adjust,color="Black")) +
  geom_line( aes(y=Asian_daily_cases/all_daily_cases*asian_adjust,color="Asian")) +
  geom_line( aes(y=White_daily_cases/all_daily_cases*white_adjust,color="White")) +
  labs(x = "",y="Daily Case Rate",
       title = "Daily COVID-19 Case Rate \n(Normalized and Adjusted for Cohort by 100,000 persons)",
       color = "Legend") +
  scale_color_manual(
                     values = c( "Hispanic" = "steelblue", "Black" = "red", 
                                 "Asian" = "purple","White"="darkgreen"),
                     labels = c("Asian", "Black", "Hispanic","White"))+
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  scale_x_date(limit=c(as.Date("2020-06-01"),as.Date("2020-11-02"))) 

casesp+theme(legend.position = "bottom")

hospp <- ggplot(txfm, aes(x=as.Date(Date)))+
  geom_line( aes(y=Hispanic_daily_hosp/all_daily_hospital*hispanic_adjust,color="Hispanic")) + 
  geom_line( aes(y=Black_daily_hosp/all_daily_hospital*black_adjust,color="Black")) +
  geom_line( aes(y=Asian_daily_hosp/all_daily_hospital*asian_adjust,color="Asian") )+
  geom_line( aes(y=White_daily_hosp/all_daily_hospital*white_adjust,color="White")) +
  labs(x = "",y="Ever Hospitalized Rate",
       title = "Daily COVID-19 Ever Hospitalized Rate \n(Normalized and Adjusted for Cohort by 100,000 persons)",
       color = "Legend") +
  scale_color_manual(
                     values = c( "Hispanic" = "steelblue", "Black" = "red", 
                                 "Asian" = "purple","White"="darkgreen"),
                     labels = c("Asian", "Black", "Hispanic","White"))+
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  scale_x_date(limit=c(as.Date("2020-06-01"),as.Date("2020-11-02"))) #+
hospp+theme(legend.position = "bottom")



lubridate::ymd( "2020-01-01" ) + lubridate::weeks( df$Week - 1 )


txfmwk<-txfmwk[-1,]

p <- ggplot(txfmwk, aes(x=lubridate::ymd( "2020-01-01" ) + lubridate::weeks( Week - 1 )))+
  geom_line( aes(y=Hispanic_wk_death*hispanic_adjust,color="Hispanic")) + 
  geom_line( aes(y=Black_wk_death*black_adjust,color="Black")) +
  geom_line( aes(y=Asian_wk_death*asian_adjust,color="Asian")) +
  geom_line( aes(y=White_wk_death*white_adjust,color="White")) +
  geom_vline( aes(xintercept=as.Date("2020-09-02",color="black"))) +
  geom_text(aes(x=as.Date("2020-08-31",family=sans,fontface=plain,size=10), label="Phase 3 Step II Begins", y=1), colour="black", angle=90)+
  geom_vline( aes(xintercept=as.Date("2020-06-08",color="black"))) +
  geom_text(aes(x=as.Date("2020-06-06",family=sans,fontface=plain,size=10), label="Phase 3 Step II Begins", y=.75), colour="black", angle=90)+
  geom_vline( aes(xintercept=as.Date("2020-06-19",color="black"))) +
  geom_text(aes(x=as.Date("2020-06-17",family=sans,fontface=plain,size=10), label="Phase 3 Step II Begins", y=.75), colour="black", angle=90)+
  geom_vline( aes(xintercept=as.Date("2020-07-06",color="black"))) +
  geom_text(aes(x=as.Date("2020-07-04",family=sans,fontface=plain,size=10), label="Phase 3 Step II Begins", y=.75), colour="black", angle=90)+
  geom_vline( aes(xintercept=as.Date("2020-10-05",color="black"))) +
  geom_text(aes(x=as.Date("2020-10-03",family=sans,fontface=plain,size=10), label="Phase 3 Step II Begins", y=.75), colour="black", angle=90) +
  geom_vline( aes(xintercept=as.Date("2020-09-16",color="black"))) +
  geom_text(aes(x=as.Date("2020-09-14",family=sans,fontface=plain,size=10), label="Public K-12 Schools Start", y=.75), colour="black", angle=90) +
  geom_vline( aes(xintercept=as.Date("2020-09-16",color="black"))) +
  geom_text(aes(x=as.Date("2020-09-14",family=sans,fontface=plain,size=10), label="Public K-12 Schools Start", y=.75), colour="black", angle=90) +
  geom_vline( aes(xintercept=as.Date("2020-08-16",color="black"))) +
  geom_text(aes(x=as.Date("2020-08-14",family=sans,fontface=plain,size=10), label="College Move In Begins",y=.75), colour="black", angle=90) +
  geom_vline( aes(xintercept=as.Date("2020-11-25",color="black"))) +
  geom_text(aes(x=as.Date("2020-11-23",family=sans,fontface=plain,size=10), label="Thanksgiving",y=.75), colour="black", angle=90) +
    labs(x = "",y="weekly death rate",
       title = "Avg weekly COVID-19 Death Rate \n(Adjusted for cohort by 100,000 persons)",
       color = "Legend") +
  scale_color_manual(
    values = c( "Hispanic" = "steelblue", "Black" = "red", 
                "Asian" = "purple","White"="darkgreen"),
    labels = c("Asian", "Black", "Hispanic","White"))+
  theme(axis.text.x=element_text(angle=60, hjust=1)) 

p +theme(legend.position = "bottom")

casesp <- ggplot(txfmwk, aes(x=lubridate::ymd( "2020-01-01" ) + lubridate::weeks( Week - 1 )))+
  geom_line( aes(y=Hispanic_wk_cases*hispanic_adjust,color="Hispanic")) + 
  geom_line( aes(y=Black_wk_cases*black_adjust,color="Black")) +
  geom_line( aes(y=Asian_wk_cases*asian_adjust,color="Asian")) +
  geom_line( aes(y=White_wk_cases*white_adjust,color="White")) +
  geom_vline( aes(xintercept=as.Date("2020-09-02",color="black"))) +
  geom_text(aes(x=as.Date("2020-08-31",family=sans,fontface=plain,size=10), label="Phase 3 Step II Begins", y=100), colour="black", angle=90)+
  geom_vline( aes(xintercept=as.Date("2020-06-08",color="black"))) +
  geom_text(aes(x=as.Date("2020-06-06",family=sans,fontface=plain,size=10), label="Phase 3 Step II Begins", y=100), colour="black", angle=90)+
  geom_vline( aes(xintercept=as.Date("2020-06-19",color="black"))) +
  geom_text(aes(x=as.Date("2020-06-17",family=sans,fontface=plain,size=10), label="Phase 3 Step II Begins", y=100), colour="black", angle=90)+
  geom_vline( aes(xintercept=as.Date("2020-07-06",color="black"))) +
  geom_text(aes(x=as.Date("2020-07-04",family=sans,fontface=plain,size=10), label="Phase 3 Step II Begins", y=100), colour="black", angle=90)+
  geom_vline( aes(xintercept=as.Date("2020-10-05",color="black"))) +
  geom_text(aes(x=as.Date("2020-10-03",family=sans,fontface=plain,size=10), label="Phase 3 Step II Begins", y=100), colour="black", angle=90) +
  geom_vline( aes(xintercept=as.Date("2020-09-16",color="black"))) +
  geom_text(aes(x=as.Date("2020-09-14",family=sans,fontface=plain,size=10), label="Public K-12 Schools Start", y=100), colour="black", angle=90) +
  geom_vline( aes(xintercept=as.Date("2020-09-16",color="black"))) +
  geom_text(aes(x=as.Date("2020-09-14",family=sans,fontface=plain,size=10), label="Public K-12 Schools Start", y=100), colour="black", angle=90) +
  geom_vline( aes(xintercept=as.Date("2020-08-16",color="black"))) +
  geom_text(aes(x=as.Date("2020-08-14",family=sans,fontface=plain,size=10), label="College Move In Begins",y=100), colour="black", angle=90) +
  geom_vline( aes(xintercept=as.Date("2020-11-25",color="black"))) +
  geom_text(aes(x=as.Date("2020-11-23",family=sans,fontface=plain,size=10), label="Thanksgiving",y=100), colour="black", angle=90) +
  
  labs(x = "",y="weekly case rate",
       title = "Avg weekly COVID-19 Case Rate \n(Adjusted for cohort by 100,000 persons)",
       color = "Legend") +
  scale_color_manual(
    values = c( "Hispanic" = "steelblue", "Black" = "red", 
                "Asian" = "purple","White"="darkgreen"),
    labels = c("Asian", "Black", "Hispanic","White"))+
  theme(axis.text.x=element_text(angle=60, hjust=1)) 

casesp+theme(legend.position = "bottom")

hospp <- ggplot(txfmwk, aes(x=lubridate::ymd( "2020-01-01" ) + lubridate::weeks( Week - 1 )))+
  geom_line( aes(y=Hispanic_wk_hosp*hispanic_adjust,color="Hispanic")) + 
  geom_line( aes(y=Black_wk_hosp*black_adjust,color="Black")) +
  geom_line( aes(y=Asian_wk_hosp*asian_adjust,color="Asian") )+
  geom_line( aes(y=White_wk_hosp*white_adjust,color="White")) +
  geom_vline( aes(xintercept=as.Date("2020-09-02",color="black"))) +
  geom_text(aes(x=as.Date("2020-08-31",family=sans,fontface=plain,size=10), label="Phase 3 Step II Begins", y=1), colour="black", angle=90)+
  geom_vline( aes(xintercept=as.Date("2020-06-08",color="black"))) +
  geom_text(aes(x=as.Date("2020-06-06",family=sans,fontface=plain,size=10), label="Phase 3 Step II Begins", y=1), colour="black", angle=90)+
  geom_vline( aes(xintercept=as.Date("2020-06-19",color="black"))) +
  geom_text(aes(x=as.Date("2020-06-17",family=sans,fontface=plain,size=10), label="Phase 3 Step II Begins", y=1), colour="black", angle=90)+
  geom_vline( aes(xintercept=as.Date("2020-07-06",color="black"))) +
  geom_text(aes(x=as.Date("2020-07-04",family=sans,fontface=plain,size=10), label="Phase 3 Step II Begins", y=1), colour="black", angle=90)+
  geom_vline( aes(xintercept=as.Date("2020-10-05",color="black"))) +
  geom_text(aes(x=as.Date("2020-10-03",family=sans,fontface=plain,size=10), label="Phase 3 Step II Begins", y=1), colour="black", angle=90) +
  geom_vline( aes(xintercept=as.Date("2020-09-16",color="black"))) +
  geom_text(aes(x=as.Date("2020-09-14",family=sans,fontface=plain,size=10), label="Public K-12 Schools Start", y=1), colour="black", angle=90) +
  geom_vline( aes(xintercept=as.Date("2020-09-16",color="black"))) +
  geom_text(aes(x=as.Date("2020-09-14",family=sans,fontface=plain,size=10), label="Public K-12 Schools Start", y=1), colour="black", angle=90) +
  geom_vline( aes(xintercept=as.Date("2020-08-16",color="black"))) +
  geom_text(aes(x=as.Date("2020-08-14",family=sans,fontface=plain,size=10), label="College Move In Begins", y=1), colour="black", angle=90) +
  geom_vline( aes(xintercept=as.Date("2020-11-25",color="black"))) +
  geom_text(aes(x=as.Date("2020-11-23",family=sans,fontface=plain,size=10), label="Thanksgiving", y=1), colour="black", angle=90) +
  
    labs(x = "",y="weekly ever hospitalized",
       title = "Avg Weekly COVID-19 Ever Hospitalized \n(Adjusted for cohort by 100,000 persons)",
       color = "Legend") +
  scale_color_manual(
    values = c( "Hispanic" = "steelblue", "Black" = "red", 
                "Asian" = "purple","White"="darkgreen"),
    labels = c("Asian", "Black", "Hispanic","White"))+
  theme(axis.text.x=element_text(angle=60, hjust=1)) 
hospp+theme(legend.position = "bottom")


#write dataframes to csv for upload to shewhart chart generator 

# Look at cases

out_cases <- cbind.data.frame("date"=format(hispanic$Date,"%m/%d/%Y"),"location"="Massachusetts",
                              "Hispanic"=hispanic$daily_cases,"Black"=black$daily_cases,"White"=white$daily_cases,"Asian"=asian$daily_cases)
out_cases<-out_cases[c(-1,-154,-155,-156,-157,-158,-159),]
write.csv(out_cases,file=paste0("data/cases_",file_format,".csv"))


# Look at deaths
out_deaths <- cbind.data.frame("date"=format(hispanic$Date,"%m/%d/%Y"),"location"="Massachusetts",
                              "Hispanic"=hispanic$daily_death,"Black"=black$daily_death,"White"=white$daily_death,"Asian"=asian$daily_death)
out_deaths<-out_deaths[c(-1,-154,-155,-156,-157,-158,-159),]
write.csv(out_deaths,file=paste0("data/deaths_",file_format,".csv"))

# Look at Hospitalizations
# Look at deaths
out_hosp <- cbind.data.frame("date"=format(hispanic$Date,"%m/%d/%Y"),"location"="Massachusetts",
                               "Hispanic"=hispanic$daily_hospitalized,"Black"=black$daily_hospitalized,"White"=white$daily_hospitalized,"Asian"=asian$daily_hospitalized)
out_hosp<-out_hosp[c(-1,-154,-155,-156,-157,-158,-159),]
write.csv(out_hosp,file=paste0("data/hosps_",file_format,".csv"))

# Look at weekly cases

out_weekly_cases <- cbind.data.frame("date"=format(lubridate::ymd( "2020-01-01" ) + lubridate::weeks( hispanicwk$week - 1 ),"%m/%d/%Y"),"location"="Massachusetts",
                              "Hispanic"=hispanicwk$avg_cases,"Black"=blackwk$avg_cases,"White"=whitewk$avg_cases,"Asian"=asianwk$avg_cases)
out_weekly_cases<-out_weekly_cases[-1,]
write.csv(out_weekly_cases,file=paste0("data/cases_weekly_",file_format,".csv"))


# Look at weekly deaths
out_weekly_deaths <- cbind.data.frame("date"=format(lubridate::ymd( "2020-01-01" ) + lubridate::weeks( hispanicwk$week - 1 ),"%m/%d/%Y"),"location"="Massachusetts",
                               "Hispanic"=hispanicwk$avg_deaths,"Black"=blackwk$avg_deaths,"White"=whitewk$avg_deaths,"Asian"=asianwk$avg_deaths)
out_weekly_deaths<-out_weekly_deaths[-1,]
write.csv(out_weekly_deaths,file=paste0("data/deaths_weekly_",file_format,".csv"))

# Look at weekly Hospitalizations
out_weekly_hosp <- cbind.data.frame("date"=format(lubridate::ymd( "2020-01-01" ) + lubridate::weeks( hispanicwk$week - 1 ),"%m/%d/%Y"),"location"="Massachusetts",
                             "Hispanic"=hispanicwk$avg_hosp,"Black"=blackwk$avg_hosp,"White"=whitewk$avg_hosp,"Asian"=asianwk$avg_hosp)
out_weekly_hosp<-out_weekly_hosp[-1,]
write.csv(out_weekly_hosp,file=paste0("data/hosps_weekly_",file_format,".csv"))

