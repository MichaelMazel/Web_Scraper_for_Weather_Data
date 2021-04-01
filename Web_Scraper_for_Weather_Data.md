Weather Scraper
================
Michael Mazel
1/30/2021

``` r
library(XML)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyr)
library(stringi)
library(rvest)
```

    ## Loading required package: xml2

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:XML':
    ## 
    ##     xml

``` r
library(RCurl) 
```

    ## 
    ## Attaching package: 'RCurl'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     complete

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(weathermetrics)
library(openxlsx)
library(dotenv)
```

# Using the MESONET API, we will be scraping weather data by the hour for a given month

Example URL to parse:
<https://api.synopticdata.com/v2/stations/timeseries?stid=ktpa&start=202012012353&end=202012012353&vars=air_temp&output=xml&token=your_token_here>  
We will insert the MO\_DAY and TIME variables using the month\_scraper
function below.  
Structure of each URL: PRE MO\_DAY TIME MID MO\_DAY TIME POST TOKEN.  
First, we will create variables to store the PRE MID POST and TOKEN. The
weather variables we will retrieve include air\_temp,
relative\_humidity, and a general weather\_summary.

``` r
PRE <- "https://api.synopticdata.com/v2/stations/timeseries?stid=ktpa&start=" #ktpa is the code for Tampa Int Airport
MID <- "&end="
POST <- "&vars=air_temp,relative_humidity,weather_summary&output=xml&token="

load_dot_env()
TOKEN = Sys.getenv("token") # My token to use the API is hidden. Insert your token here
```

``` r
month_scraper <- function(month, year){
  # create a base df to add observations. Note, the column names don't matter yet and will be distorted in the scraping process 
  df <- data.frame(date_time=character(),air_temp=integer(),relative_humidity=integer(),weather_summary=character(),stringsAsFactors=FALSE)
  
  # set default days in a month to 31 days
  days_in_month <-31
  # create a list of months with 30 days and update the appropriate months
  # update February for leap/non-leap years
  days_30 <- c(4,6,9,11)
  days_28_29 <- 2
  if (is.element(month, days_30)){
    days_in_month <- 30
  }
  if (is.element(month, days_28_29) & (year %% 4 != 0)){
    days_in_month <- 28
  }
  if (is.element(month, days_28_29) & (year %% 4 == 0)){
    days_in_month <- 29
  }
  
  # make every month a 2 digit string by adding a 0 if necessary
  month <- toString(month)
  if (stri_length(month) == 1) {
    month <- paste("0",month,sep="")
    }
  for (day in seq(from=7, to=days_in_month)){
  day <- toString(day)
  
  # make every day a 2 digit string by adding a 0 if necessary
  if (stri_length(day) == 1) {
  day <- paste("0",day,sep="")}
 
  # adjust "by=" if you would like to scrape by an interval other than hour
  # I chose to scrape for every hour at 53 minutes in, because this API produces results most often and most accurately for this minute.
    for(time in seq(from=0053, to=2359, by=100)){
      time <- toString(time)
      # make all times 4 digits
      if (stri_length(time) == 1) {
        time <- paste("000",time,sep="")}
      if (stri_length(time) == 2) {
        time <- paste("00",time,sep="")}
      if (stri_length(time) == 3) {
        time <- paste("0",time,sep="")}
      
      # now we will filter for only numbers that are real times (military time). E.g. 1230, 0245, 1800, 2359
      if ((((substr(time, 1, 1)  == "0") | (substr(time, 1, 1)  == "1")) | ((substr(time, 1, 1)  == "2") & (substr(time, 2, 2) < "4"))) & (substr(time, 3, 3)  < "6")){
        
        # create URL based off for-loops
        mo_day <- paste(month, day, sep = "")
        combined <-(paste(PRE,year, mo_day,time,MID,year,mo_day,time,POST,TOKEN,sep = ""))
        link_tool <- getURL(combined)
        # To debug function, use: print(combined)
        
        # scrape data from the URL
        doc <- xmlParse(link_tool)
        date_time <- xpathSApply(doc,"//OBSERVATIONS//date_time",xmlValue)
        air_temp <- xpathSApply(doc,"//OBSERVATIONS//air_temp_set_1",xmlValue)
        relative_humidity <- xpathSApply(doc,"//OBSERVATIONS//relative_humidity_set_1",xmlValue)
        weather_summary <- xpathSApply(doc,"//OBSERVATIONS//weather_summary_set_1d",xmlValue)
        
        # enter observation into the df
        observation <- c(date_time,air_temp,relative_humidity,weather_summary)
        df <- rbind(df, observation)
      }
    }
  }
  return(df)
}
```

Use the month\_scraper function for Feb 2021

``` r
Feb21 <- month_scraper(2,2021)
weather <- Feb21
# The Tampa station for this month returned 528 values, but we expected 672 (28 days x 24 hours). The API often has null values. This can be compensated by looking at other minutes close to the desired one. We will not do that here however
```

Example if scraping multiple months:

``` r
# Feb21 <- month_scraper(2,2021)
# Mar21 <- month_scraper(3,2021)
# weather <- rbind(Feb21,Mar21)
```

``` r
# rename columns
weather <- weather %>% rename(date_time = 1, temp = 2, humidity = 3, weather_summary = 4)

head(weather)
```

    ##              date_time temp humidity weather_summary
    ## 1 2021-02-07T00:53:00Z 20.0    89.96           clear
    ## 2 2021-02-07T01:53:00Z 20.0    89.96          broken
    ## 3 2021-02-07T02:53:00Z 20.0    89.96          broken
    ## 4 2021-02-07T03:53:00Z 20.0    89.96        overcast
    ## 5 2021-02-07T04:53:00Z 20.0    89.96        overcast
    ## 6 2021-02-07T05:53:00Z 20.0    89.96        overcast

Now, we will clean up our data frame by changing the time zone,
converting the temp to F, and filtering out minutes and seconds from
date\_time

``` r
# reformat date_time so we can convert to America/New_York time
substr(weather$date_time, 11, 11) <- " "
weather$date_time = substr(weather$date_time,1,nchar(weather$date_time)-1)
weather$date_time <- force_tzs(ymd_hms(weather$date_time), tzones = "UTC", tzone_out = "America/New_York") # due to this conversion, a few hours at the end of the month will not be scraped

# Convert temp to fahrenheit
weather$temp <- as.numeric(weather$temp)
weather$temp <- celsius.to.fahrenheit(weather$temp, round = 1)

# We are only interested in the hour, so we will remove minutes and seconds from date_time
weather$date_time <- as.character(weather$date_time)
weather$date_time <- substr(weather$date_time,1,nchar(weather$date_time)-6)

head(weather)
```

    ##       date_time temp humidity weather_summary
    ## 1 2021-02-06 19   68    89.96           clear
    ## 2 2021-02-06 20   68    89.96          broken
    ## 3 2021-02-06 21   68    89.96          broken
    ## 4 2021-02-06 22   68    89.96        overcast
    ## 5 2021-02-06 23   68    89.96        overcast
    ## 6 2021-02-07 00   68    89.96        overcast

Let’s see unique values from the weather summary column

``` r
weather$weather_summary %>% unique()
```

    ##  [1] "clear"                        "broken"                      
    ##  [3] "overcast"                     "mist"                        
    ##  [5] "light rain/thunderstorm,mist" "scattered"                   
    ##  [7] "thin scattered"               "fog"                         
    ##  [9] "light rain"                   "thunder,light rain,mist"     
    ## [11] "light rain,mist"              "thunder,light rain"          
    ## [13] "rain,mist"

The weather\_summary variable includes information about cloud coverage
and precipitation. We will create a new column specifically for cloud
coverage.

Some terms like clear, thin scattered, overcast directly identify the
percent of the sky covered. We will convert these to its percent value.

Other terms include precipitation info such as mist or heavy rain. These
also have an approximate cloud coverage percent we can use. I researched
the approximate percentages to use.

``` r
weather <- weather %>% mutate(cloud_cover = weather$weather_summary)

weather$cloud_cover[weather$cloud_cover == "clear"] <- 0
weather$cloud_cover[weather$cloud_cover == "thin scattered"] <- 20
weather$cloud_cover[weather$cloud_cover == "scattered"] <- 45
weather$cloud_cover[weather$cloud_cover == "broken"] <- 75
weather$cloud_cover[weather$cloud_cover == "overcast"] <- 100

# Below are the cloud coverage estimates
weather$cloud_cover[weather$cloud_cover == "light drizzle"] <- 75
weather$cloud_cover[weather$cloud_cover == "light rain"] <- 75
weather$cloud_cover[weather$cloud_cover == "light rain,mist"] <- 100
weather$cloud_cover[weather$cloud_cover == "mist"] <- 100
weather$cloud_cover[weather$cloud_cover == "rain,mist"] <- 100
weather$cloud_cover[weather$cloud_cover == "heavy rain,mist"] <- 100
weather$cloud_cover[weather$cloud_cover == "fog"] <- 75
weather$cloud_cover[weather$cloud_cover == "haze"] <- 45
# Note, this is not a complete list based off all the above unique weather_summary categories

weather$cloud_cover <- as.numeric(weather$cloud_cover)
```

    ## Warning: NAs introduced by coercion

We will create a new column for precipitation. We will convert to the
approximate precipitation rate in mm/hr. This may not be exactly what
was measured from the API, but it should be close

``` r
weather$precip[weather$weather_summary == "light drizzle"] <- 1
weather$precip[weather$weather_summary == "light rain"] <- 1.25
weather$precip[weather$weather_summary == "light rain,mist"] <- 1.5
weather$precip[weather$weather_summary == "mist"] <- 4.5
weather$precip[weather$weather_summary == "rain,mist"] <- 5
weather$precip[weather$weather_summary == "heavy rain,mist"] <- 10
weather$precip[weather$weather_summary == "fog"] <- 0
weather$precip[weather$weather_summary == "haze"] <- 0
weather$precip[weather$weather_summary == "clear"] <- 0
weather$precip[weather$weather_summary == "thin scattered"] <- 0
weather$precip[weather$weather_summary == "scattered"] <- 0
weather$precip[weather$weather_summary == "broken"] <- 0
weather$precip[weather$weather_summary == "overcast"] <- 0
```

Create a heat index column. This is calculated from the air temperature
and humidity

``` r
weather$humidity <- as.numeric(weather$humidity)
weather <- weather %>% 
mutate(heat_index = heat.index(t = weather$temp,rh = weather$humidity, temperature.metric = 'fahrenheit', output.metric = 'fahrenheit', round = 1))
```

``` r
head(weather)
```

    ##       date_time temp humidity weather_summary cloud_cover precip heat_index
    ## 1 2021-02-06 19   68    89.96           clear           0      0       68.7
    ## 2 2021-02-06 20   68    89.96          broken          75      0       68.7
    ## 3 2021-02-06 21   68    89.96          broken          75      0       68.7
    ## 4 2021-02-06 22   68    89.96        overcast         100      0       68.7
    ## 5 2021-02-06 23   68    89.96        overcast         100      0       68.7
    ## 6 2021-02-07 00   68    89.96        overcast         100      0       68.7
