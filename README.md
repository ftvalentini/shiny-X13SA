# shiny-X13SA
## Seasonal adjustment of time series with [X-13ARIMA-SEATS](https://www.census.gov/srd/www/x13as/)

How to use:

1. Create .xlsx, .xls or .csv file with the time series on the first sheet. Multiple time series can be loaded. The first column should have dates formatted as dates and the first row should have the names of the series. 
2. Clone repository. Run `source("run.R")` in R shell from shiny-X13SA directory.
3. Click *browse* to choose the file.
4. Click *Run & Download* to run the seasonal adjustment and download the results.

~~Before running the app, download the X13 distribution files from the [US Census Bureau website](https://www.census.gov/srd/www/winx13/winx13_down.html). x13as.exe must be located in C:/WinX13/x13as/~~ 
