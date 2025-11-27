## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
   fig.align = "center"
)


## ----set up-------------------------------------------------------------------
#load the package

library(CLABSIalertR)
library(ggplot2)



## -----------------------------------------------------------------------------
# 1. Load the library
library(CLABSIalertR)

#2. Create/load the data frame:
  
    clabsi_data <- data.frame(
      year= rep(2024:2025, each=12), 
      month = rep(1:12, times=2),
      clabsi = c(1,2,0,2,0,3,2,2,4,0,2,3,1,0,1,2,1,0,3,4,3,3,5,6),
      line_days = c(3200,3100,3300,3150,3180,3000,3250,3100,3200,3050,3100,3150, 3200,3100,3300,3150,3180,3000,3250,3100,3200,3050,3100,3150)
      )
clabsi_data


# 3. Establish phase I (stable period/baseline): 
phase1<- subset(clabsi_data,
year==2024|(year==2025&month<=6)
)

phase1

# 4. Check dispersion

check_dispersion(phase1) #0.8–1.2: consistent with Poisson

# 5. use alert_poisson_clabsi()
  #computes the Poisson u-chart quantities

result <- alert_poisson_clabsi(clabsi_data)
result

# 6. use add_spc_rules()
add_spc_rules(result)


# 7. plot

plot_clabsi_alerts(result)



## -----------------------------------------------------------------------------

#example 1 stable process/no alerts triggered (from introduction)

#creating dataset
   df <- data.frame(
     year=rep(2022:2024,each=12),
      month = rep(1:12, times=3),
      clabsi = c(2,0,1,2,4,4,2,5,3,1,2,3,0,2,2,5,2,3,1,5,4,3,2,0,1,4,3,5,2,3,0,2,4,4,3,2),
      line_days = c(3200,3100,3300,3150,3180,3000,3250,3100,3200,3050,3100,3150,3200,3100,3300,3150,3180,3000,3250,3100,3200,3050,3100,3150,3200,3100,3300,3150,3180,3000,3250,3100,3200,3050,3100,3150)
      )
#checking dispersion of stable period: 24 months between 2022:2023
phase1<-subset(df, year%in%2022:2023)
check_dispersion(phase1)

resultx<-alert_poisson_clabsi(df)
add_spc_rules(resultx)
plot_clabsi_alerts(resultx)


# Example 2: 
#Rule 1: point beyond 3-sigma ---------------------------

#creating dataset
outbreak_df <- data.frame(
  year=rep(2023:2024, each=12),
  month=rep(1:12, times =2),
  clabsi =c(
  1, 0, 2, 1, 
  1, 2,0, 1, 
  2, 1, 0, 1, 
  1, 2, 1, 0, 
  2, 1, 6, 7, 
  5, 4, 1, 2),
line_days =c(
  3100, 3200, 3150, 3180, 3220, 3190,
  3250, 3170, 3210, 3200, 3180, 3230,
  3300, 3290, 3320, 3340, 3310, 3350,
  3400, 3420, 3390, 3370, 3330, 3320)
)

#test dispersion of stable period (18 months between 2023 and 2024)
phase1 <- subset(df, (year == 2023) | (year == 2024 & month <= 6))
check_dispersion(phase1)

result<-alert_poisson_clabsi(outbreak_df)
add_spc_rules(result)
plot_clabsi_alerts(result)


#example 3: --------------------------------------
#Rules 1 and 5 (slow shift towards outbreak)
#Points shifting upwards and beyond the 3-SD line or UCL.

set.seed(123)

#setting up data
years=rep(2023:2024,each=12)
line_days= round(rnorm(24,3800,sd=150))
clabsi_stable<-rpois(24,2.5)
clabsi_stable[19:24]<-c(5,6,7,9,11,10) #creating instability)

#create data frame
df<-data.frame(
  month=1:12,times=2,
  year=years,
  clabsi=clabsi_stable,
  line_days=line_days
)
#testing dispersion of 18 months (stable period) prior to spike

phase1 <- subset(df, (year == 2023) | (year == 2024 & month <= 6))
check_dispersion(phase1)

result<-alert_poisson_clabsi(df)
add_spc_rules(result)
plot_clabsi_alerts(result)


#example 4-----------------------------------------------------------------
# Rule 2: 8-point shift
# shows improvement or degradation such as a new practice implementation

#creating data set
set.seed(123)
years=rep(2023:2025, each=12)
clabsi8=rpois(36,3)
line_days=round(rnorm(36,4500,100)) 
clabsi8[27:35]<-c(6,7,7,6,8,6,7,7,8)

#building data frame
df1=data.frame(
  month=1:12,times=3,
  clabsi=clabsi8,
  line_days=line_days,
  year=years
)

#checking dispersion of 24 months prior to spike
phase1<-subset(df1, year<=2023:2024)
check_dispersion(phase1)

result2<-alert_poisson_clabsi(df1)
add_spc_rules(result2)
plot_clabsi_alerts(result2)


#example 5----------------------------------------------------------
#stable system, not alerts triggered

#creating data set
set.seed(122)
years=rep(2024:2026,each=12)
clabsi4=rpois(36, 2)
line_days=rnorm(36,3000,150)

#building data frame
df2<-data.frame(
  month=rep(1:12, times=3),
  year=years,
  clabsi=clabsi4,
  line_days=line_days
)

#checking dispersion of first 24 months
phase1<-subset(df2, year<=2024:2025)
check_dispersion(phase1)

result3<-alert_poisson_clabsi(df2)
add_spc_rules(result3)
plot_clabsi_alerts(result3)




