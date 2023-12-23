
######################################################################################
# This code demonstrates a practical use of different time series forecasting methods
# and analysis - as part of the Time series course i took in 2022. 
######################################################################################
# get the working directory
getwd()

## get the path of the package of library
.libPaths()

# change the path of the library
.libPaths("C:/RProgram/Rpackages")
.libPaths()

# from Introduction time series analysis - of UDEMY:
myts2 <- read_delim("C:/PROGRAMS/Projects/Ex1/ITstore_bidaily (1).csv", 
                        delim = ";", escape_double = FALSE, col_names = FALSE, 
                        trim_ws = TRUE)

View(myts2)

#conversion to a time series - create time series data class 
# in this case we crates a frequency of 12, as we dealing with weeks, we assume that in one week we have 12 observations -  2 obs per day with 6 business days of the store. 

mycounts <- ts(myts2$X2, start=1,
               frequency = 12)


#plot
plot(mycounts, ylab = "Customer counts", xlab = "weeks")

library(forecast)

# The month plot is calculates the average for each time unit, also the unit of time doesn't necessarily have to be
# a month, like in this case a one unit is half day.
# so in this case is 12 half equal to a week (12 half day units per week 1:12)
monthplot(mycounts, labels = 1:12, xlab = "Bidaily Units") 

# season plot allow us to compare the seasonal variation of the business, which are the weeks in this case.
seasonplot(mycounts, season.labels = F, xlab = "")

## Lessons 1-7
#####################################################################################

# forecast plot of additional 2 weeks.with automatically adjustment/fitting
plot(forecast(auto.arima(mycounts)))


########### data sets ######## lesson 9
plot(lynx)# lynx (big cat) - no seasonality, the plot looks stationary - with equal mean and variance.
#there is may some autocorrelation present because of the repetitive pattern. 

#No of observation - if its one observation per year it will give us the number of years. 
length(lynx)

plot(LakeHuron) ### Annual measurements of the level, in feet, of Lake Huron 1875-1972. 
# here we see also non seasonal data, of 98 years. look like random walk (changes in a variable follow no discernible pattern or trend)
length(LakeHuron)

# these data of 20 years of temperature measurement in Fahrenheit in Nottingham.  
# in this data there is no trend or change in variance. but this is good example of seasonality. 
# the temperature data is always seasonal.
plot(nottem)  
length(nottem) ## it has 240 observations (its monthly data set).

plot(AirPassengers); length(AirPassengers) ### has 144 observations
# there is upper trend and seasonality
# to this data it will be good idea to make transformation to make it more comfortable to use.

plot(EuStockMarkets) ; length(EuStockMarkets) ## this is multivariate time series. this class of data structure is mts (matrix data structure).
# it have clear trend for all 4 indices. and also have 7440 observations. The MTS class behave differently.  


plot(sunspot.year); length(sunspot.year) # has 289 observations (years in this case). 
# if we look at the chart it might look that some autocorrelation is present. 
# also the data set might require a more sophisticated model. 

plot(rnorm(n=50, mean = 0, sd=5)) # gives random numbers of normal distribution.

######################################
#### lesson 11 #######################

## POSIXt classes
X = as.POSIXct("2020-05-25 11:45:34") #nr for seconds
y = as.POSIXlt("2020-05-25 11:45:34")

unclass(x)
unclass(y)
(50 * 365 * 24 * 60 * 60) - (5.5 * 60 * 60) # 50 years and 5.5 hours in seconds. 

y$zone # extracting the elements from POSIXlt

x$zone # not possible since it is simply a number of seconds

x = as.Date("2020-05-25")

x; class(x)

unclass(x) # will give the number of days since 01-01-1970 > # 18407 = 52*365

# The chrone class. 
library(chron)
# it can be consider more friendly class for dates. 
# example of handle the dates and the hours - in not consist time zone:  

x <- chron("05/25/2020", "23:50:15")
x # [1] (05/25/20 23:50:15)

# example of "strptime" function - is to convert character to date and time format, because most of the time
# will be imported as character.

a = as.character(c("1993-12-30 23:45",
                   
                   "1994-11-05 11:43",
                   
                   "1992-03-09 21:54"))

class(a)



b = strptime(a, format = "%Y-%m-%d %H:%M")

b; class(b)

##################################################################

# lubridate package - has very useful time/date data functions
library(lubridate)

# different ways to input dates
ymd(19931123)

dmy(23111993)

mdy(11231993)

#using time and date together include the time zone.
mytimepoint <- ymd_hm("1993-11-23 11:23", tz = "Europe/Prague")

mytimepoint

class(mytimepoint)

# extracting each components
minute(mytimepoint)
day(mytimepoint)
hour(mytimepoint)
year(mytimepoint)
month(mytimepoint)

# change time values within our object
hour(mytimepoint) <- 14

mytimepoint

# Present the time zones we have available
OlsonNames()

# we can take a look at the most common time zones
# we also need to be aware that the time zone recognition also depends on our location and machine. 

# Check which day our time point is
wday(mytimepoint)

# label to display the name of the day, no abbreviation
wday(mytimepoint, label=T, abbr=F) 

# we can calculate which time our timepoint would be in another time zone
with_tz(mytimepoint, tz = "Asia/Jerusalem")

mytimepoint

# time intervals
time1 = ymd_hm("1993-09-23 11:23", tz = "Europe/Prague")

time2 = ymd_hm("1995-11-02 15:23", tz = "Europe/Prague")

# getting the interval
# It will give us the 2 times points combined.

myinterval = interval(time1, time2);
myinterval

######################################################################################################
#Exercise 1: 
# Create a Data Frame with lubridate
# lets build a dataframe with lubridate that contains date and time data
# Different input formats that are allowed in the ymd function

ddate = c("1998,11,11", "1983/01/23", "1982:09:04", "1945-05-09", 19821224, "1974.12.03", 19871210)

ddate = ymd(a, tz = "CET") ;ddate

# creating a time vector - using different notations of input
ttime = c("22 4 5", "04;09;45", "11:9:56", "23,15,12", "14 16 34", "8 8 23", "21 16 14")

ttime = hms(ttime); ttime

meass = rnorm(7,10);meass = round(meass, digits = 2); meass

kovez = cbind.data.frame(ddate, ttime, meass)

kovez
##########################

# Calculations with time
minutes(7)

# note that class "Period" needs integers
minutes(2.5) # Error in validObject(.Object) : invalid class “Period” object: periods must have integer values

# getting the duration in seconds
dminutes(3)
dminutes(3.5)

# add minutes and seconds
minutes(2) + seconds(5)
minutes(2) + seconds(75)

# Class "duration"
as.duration(minutes(2) + seconds(75))

# lubridate has many time classes. 

# which year was a leap year?
leap_year(2009:2014)

ymd(20140101) + years(1)
ymd(20140101) + dyears(1)

# lets do the whole thing with a leap year (shana meuberet) - 366 days instead of 365
leap_year(2016)

# adds 1 year to the date January 1, 2016 and returns the date January 1,
ymd(20160101) + years(1)

# dyears() function adds a decimal year to the date, which is equivalent to adding 365.25 days. 
#Therefore, adding 1 decimal year to January 1, 2016 results in December 31, 2016 at 6:00 AM UTC.
ymd(20160101) + dyears(1)

###################################################################################################

# Exercise Lubridate:
# create x, with time zone CET and a given time point in 2014 of your choosing
# I use "2014-04-12 23:12"
# the time point consists of year, months, day and hour
# change now the minute of x to 7 and check x in the same line of code
# see which time it would be in London
# create another time point y in 2015 and get the difference between those 2 points

x = ymd_hm(tz = "CET", "2014-04-12 23:12")

minute(x) = 7 ; x

with_tz(x, tz="Europe/London")

y = ymd_hm(tz = "CET", "2015-12-12 09:45")

y-x
###################################################

#################################################################
# part 3 time series data and pre-processing and visualization 
################################################################

# create 50 random observation. 
mydata <- runif(50, min = 1, max = 50)

# round
round(mydata,digits = 0)

# create a ts data that will start from the year 1960, with a 4 observation per a year for quarters (for monthly data we will usully write frequency of 12).
tsmydata = ts(data=mydata, start = c(1960,3), frequency = 4)

# chart 
plot(tsmydata)

# check the class
class(tsmydata)
#[1] "ts"

# checking the time stamp
time(tsmydata)

###############################################################################################################
# Part of the exercise

# create 450 random observations
x = cumsum(rnorm(n=450)) 
set.seed(x) # we want to save this random observations. 

### we put the random 450 numbers in y axis and create time component on x axis in a ts class
y =ts(data = x,frequency = 12, start = c(1914,11))

#we plot the chart
plot(y)
# we use lattice package to plot this graph
library(lattice)
xyplot.ts(y) 

#the nottem is seasonal data.
plot(nottem)


library(forecast)
# using the "decompose" function thaw will present all the time series components (trend, seasonal, random, actual)
plot(decompose(nottem))

# directly plotting a forecat model. 
# there is no need to adjust the model if we used the auto.forecast function
# becasue it do it already automatically. h=5 (for forecasting horizon of 5 years).
plot(forecast(auto.arima(nottem)), h = 5)

# random walk
plot.ts(cumsum(rnorm(500)))

# ggplot 2 for more advanced plots
library(ggplot2)

# Using the "autoplot" function will choose automatically 
# the form of plot from the ggplot2 package for example:
autoplot(nottem)

autoplot(nottem)+ggtitle("Autoplot of Nottingham temperature data")

# we can use the seasonal data specially in the plot for seasonal data like nottem
ggseasonplot(nottem)
# the idea of the seasonal plot is that each year represented in one line.
# in the x axis will presented the months, so we can compare the months. 

# Month plot
ggmonthplot(nottem) # will present the months, and the average of every month.

#############################
##### Exercise ############

seasonplot(AirPassengers, main = "Seasonal plot of dataset Airpassenger", col=rainbow(12), year.labels=TRUE )

# The type = "l" - for line chart. type="s" - for survival chart. 
# type = "p" - for point chart. bty=7, will give more boxes line around the graph.
# cex is the size factor of the text.

seasonplot(AirPassengers, xlab = "", col = c("red", "blue"), 
           year.labels = T, labelgap = 0.65, type = "l", bty= "l", cex = 0.55,
          main = "Seasonal plot of dataset Airpassenger")

# working with irregular time series
# data set irregular sensor. 
irregular_sensor <- read.csv("C:/PROGRAMS/Projects/irregular_sensor.csv", header=FALSE)
View(irregular_sensor)

# Check the data: 
class(irregular_sensor$V1)
#[1] "character"
# we understand the first line it's character, the other column(v2) it numeric.
# we can't use character class to Time series modeling.
# in this irregular TS we see there is in some days 3 observations, and in other days only one, 
# additionally the measurement column have high standard deviation.
# we will aggregate the data to daily events, and if there is more than one event,we will take the mean. 
# for this solution we will use the "zoo" package. 

library(zoo) # for general irregular time series.
library(tidyr) # for function separate.

# first solution - removing the time component (as the seconds and hour is not relevant for us). 
irreg.split = separate(irregular_sensor, col = V1,
                       into = c('date', 'time'),
                       sep = 8, remove = T)

# this will separate the date from hours and seconds of v1 column.
# and create 2 new columns of date and time. 
# it will take only the first 8 characters (like 05/16/17 10:34 AM) to separate.
# so it will stay with 05/16/17 in one column and 10:34 AM in the other column. 
#  sep = 8 is the separation point from the left side.
# if we write sep = -8 it will cut from the right side until the 8 position.
# remove = T, delete the initial column. 
# after we run this code we will stay with 3 columns (date, time V2)
#  all this columns are still with character class, we only separate the column. 

# Converting the character to date class, using only the date column.
sensor.date = strptime(irreg.split$date, '%m/%d/%y')

# creating a data.frame object for orientation. 
# call the new object irregts.df
# we take the 'sensor.date' POSIXlt object as the date, for proper date format from POSIXlt.
irregts.df = data.frame(date = as.Date(sensor.date),
                        measurment = irregular_sensor$V2)

# to aggregate the data we will use zoo
# package and converting it to get a zoo object. 
irreg.dates = zoo(irregts.df$measurment,
                  order.by = irregts.df$date)
# here we take the measurement of the 'irregular.df' data frame, 
# and order the measurement by the chronologically, by dates column of the data frame. 
# if we look for at "irreg.dates object, we will see that every measurement is going by date chronologically.

# aggregate those measurements. if it has more than one measurement in per day
# it will give us the mean value for each specific day.
# we will do this by 'aggregate' function. in observations that has only value the per a day, this value will stay the same as a day mean.
ag.irregtime = aggregate(irreg.dates, as.Date, mean)
# at the zoo package there is a lot of different options for aggregation and not only by mean and date, also can be by median.

# we can see that the number of observations reduced to 16 (from 25).
length(ag.irregtime)
# we have one observation per day. 
# now in this step we more easily converted it to ts class. 
# Here we separate the days from time and hours. 

# let's see the solution 2. METHO2 2 - keeping the date and time component.
#  we convert the char class to date class. 
sensor.date1 = strptime(irregular_sensor$V1,
                        '%m/%d/%y %I:%M %p')

# 'p' for pm (24 hours) or am, I for hours, M for minutes. 
sensor.date1
class(sensor.date1) # we can see it's POSIXlt and POSIXt date format. 

#creating the zoo object. and order by date and time.
irreg.dates1 = zoo(irregular_sensor$V2,
                   order.by = sensor.date1)
irreg.dates1

# plot
plot(irreg.dates1)

# aggregate as previously
ag.irregtime1 = aggregate(irreg.dates1,
                          as.Date, mean)
ag.irregtime1

# from here we can do standard time series analysis.
plot(ag.irregtime1)

# covert to ts class.
myts = ts(ag.irregtime1)

plot(myts)
###############################################################################

# Working with outliers and missing data include outliers detection and visualization
#######################################################################################

library(readr)
# import the csv file
mydata <- read_csv("C:/PROGRAMS/Projects/ts_NAandOutliers.csv")
View(mydata)

#  convert the column we need to ts class object.
myts = ts(mydata$mydata) # the data is a measurement that made from chemical sensor device in lab environment. 
myts

## in the steps before we run the analysis we need to assure the data is good to continue with the further steps of the analysis.
# we can use the follow comannds: summary(), plot()
summary(myts)
## we got 5 NA'S (of missing data). 

plot(myts) # in the plot we see that is a 5 NA'S and 4 potential outliers.
# in this case we get 3.6% of corrupted data (9/250*100 = 3.6%).
# The finger rule that we should not use data with more than 20% of corrupted data.

library(tsoutliers)
library(outliers)
#  Automatic outliers detection:
# function detects five types of outliers: 
# innovational outliers (IO), 
#additive outliers (AO), 
#level shifts (LS), 
#temporary changes (TC),
#and seasonal level shifts (SLS).

# and it gives us an ARIMA model is used to forecast the time series after the outliers have been removed, 
# The model is fitted using the arima() function in R 
tso(myts)

# we also can plot it and we get 2 plot the original data with arima model, and second plot with outliers effect on time series.
# the effect of the outliers shows how much the time series deviates from its expected behavior due to the presence of the outliers.
plot(tso(myts))

#library 'forecast' tsoutliers() - this function also suggestion values for replacing the outliers.
tsoutliers(myts)

myts1 <- tsoutliers(myts) # creating a myts object which replace the outliers. 

# handling with missing data with zoo or forecast package:
# in zoo package, function na.locf()
library(zoo)
myts.NAlocf = na.locf(myts) # this function takes the missing value and replace with value that comes before the missing value.

# this function will fill all NA values with the value of 33. 
myts.NAfill = na.fill(myts, 33) 

# In addition there is the na.trim function from the zoo package that allow to trim
# the missing values at the beginning or the end of our data.

# in the forecast package there is also some function for dealing with the missing data.
# for example the function na.interp(), a function that match the local linear interpolation for the missing NA value. 
# so if the data is seasonal it will be based on exponential smoothing. the replaced values will be similar to the average from the 2 numbers on each side of NA. 

myts.NAinterp = na.interp(myts)

myts.NAinterp

#############################################################################
# Another very powerful function from the forecast package.
# is: tsclean() function, it's combine 2 functions: na.interp(), tsoutliers()
# this one function has both handling with outliers and the missing data.
##########################################################################
mytsclean = tsclean(myts)

plot(mytsclean) # as we can see now the plot looks good without outliers or NA's. 
## now with this function the data is more ready for the analysis. 
summary(mytsclean) # so now we see the values of the data looks more logical. 

###############################################################
### Time vectors and lags ####################################
print(lynx)

## but we also see the time pattern of this vector.
# that time is co-responded to the observation. and this time component is used on the x-axis.
time(lynx)

tail(lynx) # last 6 observation. 
# [1]  485  662 1000 1590 2657 3396 

mean(lynx); median(lynx) 
#[1] 1538.018 )(mean)
#[1] 771 (median)
# as we can see at the lynx data the mean and the median is very different
# so why is that? if we run the plot of lynx we will see the peaks

plot(lynx) # we see that have few high but short peaks, so the most of the observation are below the peaks.
# these peaks effect on the average of the observations of whole vector.
# Making the average high. but the peaks are not have effect on the median, because
# it counts only one observation in the middle from all the observations
# (that have 50% of the obs. above and 50% of the obs. below), and the values or the absolute values are not matter for the "median".
# in this case it will choose one middle obs. from total of 114 obs. 

# how we can see this values in the middle? 
# we can run the sort function for the 57,58 positions of lynx data: 
sort(lynx) [c(57,58)]
# [1] 758 784 # the average of this 2 values is 771. 
# median is where there are will be 50% of the observation smaller or equal than this values (771 in this case). 

quantile(lynx) # gives us the the minimum and the maximum values as we as the three Quarters values. 

# and if we want all the 10 th decile so we can run this command.
quantile(lynx, probs = seq(0, 1, length =11), type = 5)
#  0%    10%    20%    30%    40%    50%    60%    70%    80% 
# 39.0  146.7  259.2  380.5  546.6  771.0 1470.1 2165.6 2818.0 
# 90%   100% 
#  3790.4 6991.0

# example: quantile(x, probs = seq(0, 1, 0.25), na.rm = FALSE,names = TRUE, type = 7)


# x: numeric vector
# probs: numeric vector of probabilities with values in [0,1]
# na.rm: logical; if true any NA and NaN's are removed
#names: logical; if true, the result has a names
# type: an integer between 1 and 9 selecting one of the nine quantile algorithms.

#########################################
# Simple forecast methods
########################################
set.seed(95) # The mean will stay at 0 and std of 1. 
myts <- ts(rnorm(200), start = 1818)
plot(myts)

# we will use the 3 naive method on this data to explain how these methods are work. 
# run the forecast package because these 3 methods is from the forecast package. 
library(forecast)

meanm <- meanf(myts, h=20) # mean method ## h = 20,  is for forecasting for 20 years.
naivem <- naive(myts, h=20) # naive method
driftm <- rwf(myts, h=20, drift = T) # drift method

# we can extract particular element from the object in the method we apply. 
# fitted values: the forecasted values
# residuals, mean values: the forecasted values of the same mean.
# to extract those values use the dollar sign $ - e.g type $mean to get the mean values. 

#################################
# creating a comparasion plot
################################
 # plot.conf = F is allow to get more several line on the plot
 # line() function to add more lines, naivem$mean provide the 20 forecase values of each object. 
 # drinftm$mean provide forecast of 20 values to each object. col is to add color. lwd = 2 to get the line bit thicker 
#ity = 1 is the the color line in the legend box that explain the models. 

plot(meanm, plot.conf = F, main = "MY CHART")
lines(naivem$mean, col=123, lwd = 2)
lines(driftm$mean, col=22, lwd = 2)
legend("topleft",lty=1,col=c(4,123,22),
       legend=c("Mean method","Naive method","Drift Method"))

## the location of the box can be in other places: 
# The location may also be specified by setting x 
#to a single keyword from the list:
# "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center".

#################################################
###### accuracy and model comparison, lesson 30
################################################
set.seed(95)
myts <- ts(rnorm(200), start = (1818)) # the data is for 200 years it's end in 2018
mytstrain <- window(myts, start = 1818, end = 1988) # split the ts data, so here we take only 170 years
#, and for 30 other years we plan to do forecast by test and train models.
plot(mytstrain)

library(forecast) #  we can to compare with these simple methods. 
meanm <- meanf(mytstrain, h=30) # h=30, for 30 years forecasting.
naivem <- naive(mytstrain, h=30)
driftm <- rwf(mytstrain, h=30, drift = T)

mytstest <- window(myts, start = 1988) # notice it start from 1988 to test the models. 

accuracy(meanm, mytstest) # training set RMSE 1.003956, test set RMSE 1.138760
accuracy(naivem, mytstest) # training set RMSE 1.323311, test set RMSE 1.413766  
accuracy(driftm, mytstest) # training set RMSE 1.323311, test set RMSE 1.415768 

#############################
###### Residuals, lesson 31 
#############################
set.seed(95)
myts <- ts(rnorm(200), start = (1818))
plot(myts)

library(forecast)
meanm <- meanf(myts, h=20)
naivem <- naive(myts, h=20)
driftm <- rwf(myts, h=20, drift = T)# we can access to the residuals with dollar sign $. 

var(meanm$residuals) #for the variance
mean(meanm$residuals) # for the mean. 
var(myts)
mean(myts)
sd(myts)

mean(naivem$residuals)

naivwithoutNA <- naivem$residuals
naivwithoutNA <- naivwithoutNA[2:200]
var(naivwithoutNA)
mean(naivwithoutNA)

driftwithoutNA <- driftm$residuals
driftwithoutNA <- driftwithoutNA[2:200]
var(driftwithoutNA)
mean(driftwithoutNA)

# histogram plot of the residuals
hist(driftm$residuals)

# ACF plot
acf(driftwithoutNA)

############################################
### Stationarity lesson 32 #############
###########################################

x <- rnorm(1000) # taking a 1000 random numbers
# the data should be stationary - without unit-root inside it.

library(tseries) # adf test require tseries package
adf.test(x) # augmented Dickey Fuller Test

plot(nottem) # Let's see the nottem dataset
# we can see there is a clear seasonality in this data set
# but the mean is look quite constant. 

plot(decompose(nottem))# decompose the nottem data set


adf.test(nottem) # run adf test to nottem dataset in order to see if it's stationary or not.
 #p-value = 0.01 - the data is stationary.

y <- diffinv(x) # now we will take non-stationary dataset
# by taking the inverse of differences of the
# data set of (x) - that was randomly distributed, and set it as y 

# run the plot
plot(y) # we can see that the data is no constant mean or variance (there is downward trend)
# in stationary time series data the mean, variance, and autocorrelation are constant over time.

adf.test(y)#  run the adf test - as expected the data is non-stationary.    

#################################
### Autocorrelation lesson 33
################################

# Durbin Watson test for autocorrelation

length(lynx); 
head(lynx);tail(lynx);
 head(lynx[-1]); head(lynx[-114]) # check the required traits for the test

library(lmtest) # we run the test and cutting the first observation and last one.
dwtest(lynx[-114] ~ lynx[-1]) # DW = 1.1296, p-value = 1.148e-06 (positive correlation of the residuals)

#The Durbin-Watson test uses the following hypotheses:
  
# H0 (null hypothesis): There is no correlation among the residuals.
# HA (alternative hypothesis): The residuals are autocorrelated.
# The test statistic for the Durbin-Watson test, typically denoted d, is calculated as follows:
# T: The total number of observations
# et: The tth residual from the regression model
# The test statistic always ranges from 0 to 4 where:
  
# d = 2 indicates no autocorrelation
# d < 2 indicates positive serial correlation
# d > 2 indicates negative serial correlation

x = rnorm(700) # an opposite example 

# we run the test and cutting the first observation and last one (of x).
plot(x)
dwtest(x[-700] ~ x[-1]) # DW = 1.9982, p-value = 0.4904, we doesn't have autocorrelation of the residuals.

plot(nottem) #nottem dataset
length(nottem) # to see how many
#observation have so we can cut the first and the last observation

dwtest(nottem[-240] ~ nottem[-1])

###############################
### ACF and PACF #
###############################

acf(lynx, lag.max = 20) #  we ignore the first lag, lag.max for maximum number of the first 20 lags that will be calculated - 20 is quite enough 

pacf(lynx, lag.max =20, plot = F)
# to see the actual coefficients values, we can cancel the plot and dyspley the values 

pacf(lynx, lag.max =20)

acf(rnorm(500), lag.max = 20)
# here we can see random numbers from the normal distribution, the first lag we ignore
# we see non of observation show any significant correlation 

library(forecast)
tsdisplay(lynx) # this function put both the acf and the pacf plot together

##########################
## work with messy data
##########################

# crate a messy data
set.seed(54)
myts <- ts(c(rnorm(50, 34, 10), 
             rnorm(67, 7, 1), 
             runif(23, 3, 14)))
plot(myts)

# forecast by naive methods
library(forecast)
meanm <- meanf(myts, h=10)
naivem <- naive(myts, h=10)
driftm <- rwf(myts, h=10, drift = T)

## plot 
plot(meanm, main = "", bty = "l")
lines(naivem$mean, col=123, lwd = 2)
lines(driftm$mean, col=22, lwd = 2)
legend("bottomleft",lty=1,col=c(4,123,22), bty = "n", cex = 0.75,
       legend=c("Mean method","Naive method","Drift Method"))

# split to train data and test data
length(myts)
mytstrain <- window(myts, start = 1, end = 112 )
mytstest <- window(myts, start = 113)

# forecast by each method the train data
meanma <- meanf(mytstrain, h=28)
naivema <- naive(mytstrain, h=28)
driftma <- rwf(mytstrain, h=28, drift = T)

# check the accuracy of the naive methods
library(forecast)
accuracy(meanma, mytstest)
accuracy(naivema, mytstest) # got the lowest MAPE
accuracy(driftma, mytstest)

plot(naivem$residuals)

mean(naivem$residuals[2:140]) # check the mean of the residuals


hist(naivem$residuals) #check the normal distribution of the residuals. 

shapiro.test(naivem$residuals) # test for normal distribution, normal distribution will be be rejected
# if the p-value is less than 0.05. in this case that we don't have normal distribution (p-value = 2.061e-08)

acf(naivem$residuals[2:140]) # autocorrelation test,if more than one bar 
#is going from the limits it meant the autocorrelation. in this case
# the autocorrelation is present.

myts1 <- log(myts) # make logarithmic transformation

plot(myts1) # plot of the transform data.

# forecast the transofm data with vaive methods
library(forecast)
meanm1 <- meanf(myts1, h=10)
naivem1 <- naive(myts1, h=10)
driftm1 <- rwf(myts1, h=10, drift = T)

# plot 
plot(meanm1, main = "", bty = "l")
lines(naivem1$mean, col=123, lwd = 2)
lines(driftm1$mean, col=22, lwd = 2)
legend("bottomleft",lty=1,col=c(4,123,22), bty = "n", cex = 0.75,
       legend=c("Mean method","Naive method","Drift Method"))

# split to train data and test data
length(myts1)
mytstrain1 <- window(myts1, start = 1, end = 112 )
mytstest1 <- window(myts1, start = 113)

# forecast with the naive methods on a train data
meanma1 <- meanf(mytstrain1, h=28)
naivema1 <- naive(mytstrain1, h=28)
driftma1 <- rwf(mytstrain1, h=28, drift = T)

# check the accuracy of methods on the testset
library(forecast)
accuracy(meanma1, mytstest1)
accuracy(naivema1, mytstest1)
accuracy(driftma1, mytstest1)

plot(naivem1$residuals)

mean(naivem1$residuals[2:140]) #check the mean of the residuals

hist(naivem1$residuals) # check the normal distribution of the residuals. 

shapiro.test(naivem1$residuals) #p-value = 0.0005413 - not normally distributed

acf(naivem1$residuals[2:140]) # autocorrelation test,iin this case
# the autocorrelation is present.

####################################################
# lesson 38 seasonal decomposition
### Decomposing Time Series (U)
# for the example we will the nottem data set
####################################################

plot(nottem) # we can see there is a clear trend of seasonality in the data set
# we can see it have stable seasonality and there is no trend in the data
#Thus, we can perfectly describe it by the additive model 

frequency(nottem) # when we use the decompose function we need to check
# that frequency of time series data - in this case is 12, for 12 months

length(nottem)# the length shows 240, it's exactly 20 years - from 1920-1940.

decompose(nottem, type = "additive") # the results give us 
# firstly the raw values of nottem than the values for trend
# and for seasonality 


plot(decompose(nottem, type = "additive")) # view the plot of the decomposition of additive method

library(forecast)
library(ggplot2)

# library(ggplot2 and forecast)
autoplot(decompose(nottem, type = "additive"))

# alternatively the function stl could be used
plot(stl(nottem, s.window="periodic")) # the s is for seasonal
# we also can change the 'periodic to some odd number like 7 or higher.
# with the s.window argument we avoid the NA in the beginning and the end of the result. 

stl(nottem, s.window="periodic") # we can run and see the values to validate
#there is no NA in the beginning or in the end of observations. 

###################################
# lesson 39 
# seasonal adjustment
###################################

# let's say we want to eliminate the seasonal effect of the nottem data set
# to do that, we first will create new object and call it mynottem 
# and store there the decompose data
mynottem = decompose(nottem, "additive") 

class(mynottem) # we see that the class of the object is decompose time-series

# we are subtracting the seasonal element from nottem to make seasonal adjustment
# 'nottemadjusted'- the data adjusted to seasonality - in other words now the nottem data is without seasonality. 
nottemadjusted = nottem - mynottem$seasonal

# Now let's plot the seasonal adjustment object
plot(nottemadjusted)

# we also can plot only the seasonal component
plot(mynottem$seasonal) 

# we can use a stl function in order forecast the compose part 
#with combination use of stl from the forecast package 
library(forecast)
plot(stlf(nottem, method = "arima"))
# the forecast method can be changed to naive ets or rw drift... 
# we have the option to control the forecast period, we can add H=20, or other forecast period as we need. 
stlf(nottem, method = "arima") # it will gives the forecast values include the LO95 and HI95 confidence interval.

######################################
# lesson 40
### Exercise Decomposition
######################################

plot(AirPassengers)

frequency(AirPassengers) # 12

# create objects for both models the additive and the multiplicative. 
mymodel1 = decompose(AirPassengers, type = "additive")
mymodel2 = decompose(AirPassengers, type = "multiplicative")

plot(mymodel1)
plot(mymodel2)

plot(mymodel1$trend + mymodel1$random) # first way to display it
plot(AirPassengers - mymodel1$seasonal) # second way to display it 

# fit with exponential smoothing method 
fit <- ets(AirPassengers, model = "ZZZ") # zzz means it try all the possibilities and decide the best model base on the Akaike information criteria (AIC).
# Here are all the ETS possibilities:

# 1. ETS(A,N,N): Simple exponential smoothing with additive errors.
# 2. ETS(M,N,N): Simple exponential smoothing with multiplicative errors.
# 3. ETS(A,A,N): Holt’s linear method with additive errors (also known as Holt linear simple exponential smoothing model with additive errors, additive trend, and no seasonality)
# 4. ETS(M,M,N): Holt’s linear method with multiplicative errors.
# 5. ETS(A,A,A): Additive Holt-Winters’ method.
# 6. ETS(M,M,M): Multiplicative Holt-Winters’ method.
# 7. ETS(A,A,M): Additive Holt-Winters’ method with multiplicative seasonality.
# 8. ETS(M,A,M): Multiplicative Holt-Winters’ method with additive seasonality.
# 9. ETS(M,A,N): Damped trend exponential smoothing with additive errors.
# 10. ETS(M,M,S): Multiplicative Holt-Winters’ method with additive seasonality and a damped trend.
# 11. ETS(M,Ad,M): Multiplicative Holt-Winters’ method with damped trend and multiplicative seasonality.
# 12. ETS(A,N,A): Simple exponential smoothing with additive seasonality and no trend.

fit # We got (M,Ad,M) 
plot(fit)
plot(forecast(fit))

########################################
  # lesson 41
 ### SMOOTHING SMA
########################################
library("TTR")

# in order to identify trends, we can use smoothers
# like a simple moving avg

# n identfies the order or the SMA - we can experiment with this parameter

x = c(1,2,3,4,5,6,7) #we create the object of x that contain no 1-7
#there are several ways to calculate SMA in this example we use the ttr function
# it is also possible to write your own function to do this.

SMA(x, n = 3) # SMA with TTR package, here is SMA of 3. 
# we can see some NA at the beginning, because at those stages there
# is not enough data for the calculation, only when we had 3 values available SMA can be calculated. 

# This TTR package has more types of moving average available, for example it
# has EMA that is moving average that put more weight on the recent events. 

# working with lynx.
# we want to calculate smoother and compare it to the original data set.
# we create an object call lynxsmoothed with SMA method.
# in this case is by the estimated average of 4 years

lynxsmoothed = SMA(lynx, n = 4); lynxsmoothed
# the first values get NA because is not enough obs. to calculate the first values of the moving average.
# we can compare the smoothed vs the original lynx data

plot(lynx)
plot(lynxsmoothed)
# when we plot the data we see the smoothedlynx is more round
# and look more smoother than the original one. 

lynxsmoothed = SMA(lynx, n = 9); lynxsmoothed
#we even can make it more smoother by increasing value of SMA to 9

plot(lynx)
plot(lynxsmoothed)

##################################################
# Exponential Smoothing with ets - Lesson 42
##################################################

## to activate the ets function need the forecast library
library(forecast)
plot(nottem)
frequency(nottem)
# for this example we will use the nottem data set
# Using function ets, we let R do the whole calculation for us.
# and then get the model type it choose for us. in the console
etsmodel = ets(nottem); etsmodel # in this case it choose ETS(A,N,A) - Simple exponential smoothing with additive seasonality and no trend.

#let's see how the model of ets is look like compare to the original
# Plotting the model vs original
plot(nottem, col="blue", lwd = 2)
lines(etsmodel$fitted, col = "red")

# now as we created an adjusted model for the data set, we can forecast it. 
# Plotting the forecast, the H is for one cycle, as our freqency is 12, and we want a forecast for one year we will write h=12.
plot(forecast(etsmodel, h = 12))
#the forecast prediction interval default comes with 80% and 90% accuracy 

#we can Change the prediction interval only to 95%.
plot(forecast(etsmodel, h = 12, level = 95))

# Manually setting the ets model, lets say we want to set the error and
# the seasonality to multiplicative, but for the trend we let the method decide what is best
# in this case we will write:
etsmodmult = ets(nottem, model ="MZM"); etsmodmult # we got ETS(M,N,M)

# Plot as comparison the new menual model
plot(nottem, lwd = 3) 
lines(etsmodmult$fitted, col = "red")

####################################
### ARIMA models - lesson 47 
## ARIMA with auto.arima
####################################

plot(lynx)
library(forecast)
tsdisplay(lynx) 
library(tseries) # to apply the adf test, to test the stationarity
adf.test(lynx) # test stationarity - we see the data is indeed stationary. 

auto.arima(lynx, seasonal = TRUE, trace = T) # the basic version, the chosen model: ARIMA(2,0,2) 

auto.arima(lynx, trace = T)

# recommended settings
plot(auto.arima(lynx, trace = T, 
           stepwise = F, 
           approximation = F)) # in this case the chosen model is ARIMA(4,0,0)

########################################################
### lesson 48
##  Continue with ARIMA models eqation and calculations
########################################################

# 
# We can set the ARIMA model manually - for example let's set AR(2) model, 

myarima = arima(lynx, order = c(2,0,0))
myarima

errorAR = arima(lynx, order = c(2,1,0))
errorAR

tail(lynx)

residuals(myarima)

# Check the equation for AR(2)
(2657 - 1545.45)*1.147 - (1590 - 1545.45)*0.6 + 601.84

3396 - 1545.45

# MA(2) model # now we will see how to calculate the moving average part
myarima = arima(lynx, order = c(0,0,2)) # it is a bad model, but it's only 
# example how done the calculation in the arima model
myarima

residuals(myarima)

# Check the equation for MA(2) it  may will be slightly different as we round the numbers to do the calculations.
844.72*1.141 + 255.91*0.47 + 766.83 

3396 - 1545.37

########################################
## ARIMA time series simulations
# lesson 49 
#######################################

set.seed(123) # we make seed for reproduction of the same numbers.

# simulation, at least n of 1000 
asim <- arima.sim(model = list(order = c(1,0,1), ar = c(0.4), ma = c(0.3)), n = 1000) + 10

plot(asim)

library(zoo) 
plot(rollmean(asim, 50)) 
plot(rollmean(asim, 25))

# Stationarity
library(tseries) 
adf.test(asim)

# Autocorrelation
library(forecast)
tsdisplay(asim)

# run the auto arima algorithm
auto.arima(asim, trace = T,  stepwise = F,   approximation = F)

###############################
## ARIMA parameter selection
#lesson 50 
###############################
library(tseries)
library(forecast)

adf.test(lynx)
acf(lynx)
pacf(lynx)
tsdisplay(lynx)

# Manuel Arima from forecast package
myarima <- Arima(lynx, order = c(4,0,0))

checkresiduals(myarima)

# Same Example of MA time series
set.seed(123) # for reproduction

# Simulation, +10 is for not zero mean of 10. 
#the moving average coefficient will be 0.3 and 0.7
myts <- arima.sim(model = list(order = c(0,0,2),
                               ma = c(0.3, 0.7)), n = 1000) + 10

#the first thing we do is test for stationarity. 
adf.test(myts) # we see the data set have Stationarity

tsdisplay(myts) # ACF and PACF plots

# Arima, let's set our arima model with 0,0,2
myarima <- Arima(myts, order = c(0,0,2))

checkresiduals(myarima)

auto.arima(myts, trace = T, 
           stepwise = F, 
           approximation = F) 

class(myts)
class(myarima)

######################################
# ARIMA Forecasting - Lesson 52
######################################

# The model 
myarima <- auto.arima(lynx, 
                      stepwise = F, 
                      approximation = F) # ARIMA(4,0,0) 

# Forecast of 10 years
arimafore <- forecast(myarima, h = 10)

plot(arimafore)

# See the forecasted mean values
arimafore$mean

#zoom in the Plot last observations and the forecast
plot(arimafore, xlim = c(1930, 1944))

# if we want to compare the Ets model with arima model
# first let's create ets model of the lynx model
myets <- ets(lynx)

# we create ets forecast vector for 10 years
etsfore <- forecast(myets, h = 10)

# Comparison plot for 2 models
library(ggplot2)

autoplot(lynx) +
  forecast::autolayer(etsfore$mean, series = 'ETS model') +
  forecast::autolayer(arimafore$mean, series = 'ARIMA model') +
  xlab('year') + ylab('Lynx Trappings') + 
  guides(colour = guide_legend(title = 'Forecast Method')) +
  theme(legend.position = c(0.8, 0.8))

############################################################
## lesson 53 
## ARIMA with Explanatory Variables - Dynamic Regression
## Importing the cyprinidae dataset
###########################################################

cyprinidae=read.csv('C:/PROGRAMS/projects/cyprinidae.csv')

## change the column name from ...1 to X1
names(cyprinidae)[names(cyprinidae) == "...1"] <- "X1"

cyprinidae

# Display the multivariate dataset
library(ggplot2)
ggplot(cyprinidae,
       aes(y = concentration, x = X)) + 
  geom_point(aes(colour = predator_presence))

# Convert the variables into time series
x = ts(cyprinidae$concentration)
## if we want convert the boolian to zero or 1 we need to multiply the whole by 1. 
y = cyprinidae$predator_presence*1

# Arima model creation
library(forecast)
mymodel = auto.arima(x, xreg = y, 
                     stepwise = F,
                     approximation = F)
mymodel

# Quick check of model quality
checkresiduals(mymodel)

# Expected predator presence at future 10 time points
y1 = c(T,T,F,F,F,F,T,F,T,F)*1

# Getting a forecast based on future predator presence
plot(forecast(mymodel, xreg = y1))

plot(forecast(mymodel, xreg = y1),
     xlim  = c(230,260))

######################################################
### Multivariate Time Series Datasets
### lesson 57 
######################################################

# Generating a random dataframe x,y,z with 100 numbers each. 
set.seed(40)
x = rnorm(100, 1) # with avg of 1.
y = rnorm(100, 30) # with avg of 30
z = rnorm(100, 500) # with avg of 500. 

xyz = data.frame(x, y, z)

class(xyz)


# Converting the data.frame into mts
mymts = ts(xyz,
           
           frequency = 12,
           
           start = c(1940, 4))


# Standard exploratory tools
plot(mymts)
head(mymts)
class(mymts)

# further exercise dataset
class(EuStockMarkets)
head(EuStockMarkets)

# Main packages for moltivariate time series. The problem: both have different functions for VAR
library(vars)
library(MTS)

##############################
# Testing for stationarity
# lesson 59
#############################
library(tseries)

# adt.test
apply(EuStockMarkets, 2, adf.test)


# Alternative for the adf test is use the function from lib of unitroots
library(fUnitRoots)

apply(EuStockMarkets, 2, adfTest, lags=0, type="c")

###############################
# Differencing the whole mts
###############################
library(MTS)

stnry = diffM(EuStockMarkets) # we apply the differencing and store it as snry

# Retest and check if 1 differencing is enough or we need to do a second diffrencing.  
apply(stnry, 2, adf.test)

#column selection, for example if we want to do defrencing for particular column
x = stnry[,c(3,4)]; head(x)

##########################
# lesson 61
## VAR modeling
#########################

plot.ts(stnry)

# Lag order identification
library(vars)

VARselect(stnry, type = "none", lag.max = 10)

# Creating a VAR model with vars, double dots is to let R to choose the VAR package
var.a <- vars::VAR(stnry,
                   
                   lag.max = 10,
                   
                   ic = "AIC",
                   
                   type = "none")

# models summary info 
summary(var.a)

# lesson 62
# Residual diagnostics
serial.test(var.a)


#############################
# Granger test for causality
#### lesson 63 ##############

#The Granger causality test measures whether one time series is useful in forecasting another. 
#The instantaneous causality test, on the other hand, measures whether knowing the future of one variable helps predict the future of another variable .
# the var.a dataset was used to determine whether the DAX variable Granger-causes SMI, CAC, and FTSE variables. 

causality(var.a, cause = c("DAX"))
# we got the following output: 

# $granger
#Granger causality H0: DAX do not Granger-cause SMI CAC FTSE 
#data:  VAR object var.a
#F-Test = 1.7314, df1 = 27, df2 = 7256, p-value = 0.01074

# Therefore, we can reject the null hypothesis and conclude that there is evidence of 
# Granger causality between DAX and SMI, CAC, and FTSE variables. 
#This means that the past values of DAX are useful in forecasting the future values of SMI,CAC, and FTSE variables. 


# Second plot: 
# $Instant
# H0: No instantaneous causality between: DAX and SMI CAC FTSE
# data:  VAR object var.a
# Chi-squared = 759.19, df = 3, p-value < 2.2e-16
# the null hypothesis (H0) is that there is no instantaneous causality between DAX and SMI, CAC, and FTSE variables. 

# in this case this means that knowing the future values of DAX helps predict the future values of SMI, CAC, and FTSE variables 

###########################
## lesson 64
## Forecasting VAR models
###########################

fcast = predict(var.a, n.ahead = 25)
plot(fcast)

# Forecasting the DAX index, we create only the object of DAX info. 
DAX = fcast$fcst[1]; DAX # type list

# Extracting the forecast column
x = DAX$DAX[,1]; x

#########################################

y = c(1,10,20,30,40)
y1 = cumsum(y)
y1
tail(EuStockMarkets)

# Inverting the differencing
x = cumsum(x) + 5473.72
plot(x)
plot.ts(x)

# Adding data and forecast to one time series, it will start at the year 1991, 130 work days after.
# 130 is to note when specific the ts will start in this case 130, of work days because we put
#  the frequency of 260 (work days)

DAXinv =ts(c(EuStockMarkets[,1], x),
           
           start = c(1991,130), frequency = 260)

plot(DAXinv)

### plot for the latest data. 
plot.ts(DAXinv[1786:1885])
######################################################
## Creating an advanced plot with visual separation
library(lattice)
library(grid)
library(zoo)
#######################################################
# Converting to object zoo
x = zoo(DAXinv[1786:1885])

# Advanced xyplot from lattice package, data set is X that contain 100 observations. 
# we will activate the grid in the plot. the panel contain the color for the lines. 
# we use this function to set the colors.the red will be for the forecast data.
# the green for the original data. is done by the grid.clip, that meant it will cut in the position 76.

xyplot(x, grid=TRUE, panel = function(x, y, ...){
  
  panel.xyplot(x, y, col="red", ...)
  
  grid.clip(x = unit(76, "native"), just=c("right"))
  
  panel.xyplot(x, y, col="green", ...) })

#########################################
### lesson 71 ################
# Neural Networks in Time Series Analysis
#########################################
library(readr)
library(forecast)

# Import the APTelectricity.csv file as APTelectric
APTelectric <- read_csv("C:/PROGRAMS/Projects/APTelectricity.csv")
set.seed(34)

# Generate an ARIMA model
myts = ts(APTelectric$watt, frequency = 288)
arimamyts = auto.arima(myts, trace = T, approximation = T) #ARIMA(5,1,3)(0,1,0)[288]  

# Generate a forecast
fcst = forecast(arimamyts, h = 400)

# Plot the forecast
plot(fcst, xlab = "Time", ylab = "Wattage")

# Generate a forecast object
fcst = forecast(arimamyts, h = 400)

# Plot the forecast
autoplot(fcst, xlab = "Time", ylab = "Wattage")

############################################################
# Continue with Neural Networks in Time Series Analysis 
# Practical implementation of an autoregressive Neural net 
# working with nnetar function
############################################################
library(forecast)
library(ggplot2)

fit = nnetar(myts)
# we keep the default of all options, it meant it will repeat 20 time, 
# and have 1 seasonal lag. and the model have 1 hidden layer. 

nnetforecast <- forecast(fit, h = 400, PI = F)

autoplot(nnetforecast)

## Using an external regressor in a neural net
fit2 = nnetar(myts, xreg = APTelectric$appliances)

# Defining the vector which we want to forecast
y =rep(2, times = 12*10)

nnetforecast <- forecast(fit2, xreg = y, PI = F)
#for example if we want to add k to x-reg. nnetforecast <- forecast(fit2, xreg=c(y,k) , PI = F) 

autoplot(nnetforecast) # our model is NNAR (16,1,10), this means we have 16 lags, 1 seasonal lag, and 10 neural junctions.






