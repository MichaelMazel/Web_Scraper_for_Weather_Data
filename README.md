<a name="BackToTop"></a>

# Weather Scraper for Weather Data

>This code uses the Mesonet API from Synoptic Developers to automatically scrape local weather data for any given month. The code can easily be modified to scrape data for every 5 minutes, hour, every other hour, etc. Feel free to check out my code above, or read further to see how to use for yourself.

---

<a name="HowtoUse"></a>

## How to Use
You will need to sign up for a token in order for your code to run. You can modify the "stid=ktpa", to select a unique weather station. Most U.S. airports have a station available. You can also filter by state, which you can then use to determine your desired station's 4 digit I.D. I found most U.S. locations have information for air temperature, humidity, and a general weather summary (includes information for cloud coverage, precipitation, fog, etc).

#### Technologies
Mesonet API. Found at: https://developers.synopticdata.com/mesonet/  
R Studio  
R version 4.0.3

[Back to Top](#BackToTop)