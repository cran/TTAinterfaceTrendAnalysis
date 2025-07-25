\name{FULLoption}
\alias{FULLoption}
\title{
Main function
}
\description{
This is the core function of the interface. It receive arguments from the
 interface (see the function <TTAinterface>) and build regularized time series
 , perform diagnostics and analyses.
}
\usage{ 
FULLoption(param, depth=NULL, sal = NULL, site=NULL, rawdata="NO", select="NO", 
resume.reg="NO", test.normality="NO", plotB = "NO", selectBox="ByYears", log.trans="NO",
plotZ="NO", datashow="NO", help.timestep = "NO", auto.timestep = "NO", time.step = NULL, 
help.aggreg = "NO", auto.aggreg = "NO", aggreg = NULL, mix = "YES", outliers.re = "NO", 
na.replace="NO", start = NULL, end = NULL, months = c(1:12), norm = "NO", npsu = 30, 
test.on.remaider = "NO", autocorr = "NO", spectrum="NO", anomaly="NO", a.barplot="NO", 
zsmooth="NO", test.pettitt="NO", local.trend = "NO", test= "MK", OnOK4=NULL)
}
\arguments{
  \item{param}{
The name of the parameter you want to analyse
 it must be the name of the column where are your data.
 Have to be enter like this : "yourparam".
}
  \item{depth}{
If existing, the depth interval where your data will be
 analyse. If values are different from depth max and depth min, missing value are exclude
 Depth column must be name as 'DEPTH'.
 Enter the value like this : c(a,b).
 For analysis at one specific depth you can enter c(a,a).
}
  \item{sal}{
Same thing as for the depth
 Salinity column must be name as 'S'.
}
  \item{site}{
Labels of sampling site as they appears in the database
 Enter the value like this c("S1", "S2").
}
  \item{rawdata}{
Peform desciptive statistics on raw database, 
 can be "YES" or "NO" (the default).
}
  \item{select}{
Peform desciptive statistics on selected parameter and site,
 can be "YES" or "NO" (the default). 
}
  \item{resume.reg}{
Peform desciptive statistics on regularized time series, 
 can be "YES" or "NO" (the default).
}
  \item{test.normality}{
Perform a Shapiro-Wilk normality test on selected parameter,
 can be "YES" or "NO" (the default).
}
  \item{plotB}{
Display a boxplot of rawdata with outliers identified as cirle,
 can be "YES" or "NO" (the default).
}
  \item{selectBox}{
Options for plotB: allow to choose between boxplot by years or by months.
}
  \item{log.trans}{
This option transform your data in log(x+1) prior to perform analysis.
}
  \item{plotZ}{
Display a plot of the regularized time series, can be "YES" or "NO" (the default).
}
  \item{datashow}{
Show a table of the regularized data, can be "YES" or "NO" (the default).
}
  \item{help.timestep}{
Display an advice for time step selection, base on the mean time that
 separate two successive measurments. Can be "YES" or "NO" (the default). 
}
  \item{auto.timestep}{
Autoexecute the advice without diplay it.
}
  \item{time.step}{
Choice of the time step for data aggregation during the build of the time series,
 can be "Fortnight", "Semi-fortnight", "Mensual", "Annual" or "Mono-mensual" for an
 aggregation of the data of a month of all years (e.g. all January data).
}
  \item{help.aggreg}{
Display an advice for method of aggregation selection, base on Wilcoxon p.value
 between rawdata and the different method. Can be "YES" or "NO" (the default).
}
  \item{auto.aggreg}{
Autoexecute the advice without diplay it.
}
  \item{aggreg}{
Choice of the method of aggregation during the build of the time series, 
 can be "Mean", "Median", "Max" for maximal value selection or "Quantile" 
 for selection of the quantile 90%.
}
  \item{mix}{
Allow to mix the data of all sampling site during analysis. 
 Permanently set to "YES" and removed from the GUI since version 1.5. 
}
  \item{outliers.re}{
Remove the outliers from the rawdata, the outliers list is save in a .csv file.
 (for outliers visual identification see boxplot section).
}
  \item{na.replace}{
Replace missing values with median of the corresponding cycle (week, month...) 
 for lags longer than 3 days and linear regression for shorter missed period.
 Can be "YES" or "NO" (the default).
}
  \item{start}{
Define the first year of data analysis (by default the first of the database).
}
  \item{end}{
Define the last year of data analysis (by default the last of the database).
}
  \item{months}{
Define the months of data analysis (by default the twelve months).
}
  \item{norm}{
Compute normalised values of nutrients at the salinity npsu for each years,
 can be "YES" or "NO" (the default).
}
  \item{npsu}{
Compute normalised values of nutrients at the salinity npsu for each years,
 30 by default. 
}
  \item{test.on.remaider}{
Extract the reminders from the data series using the stl package functions to 
perform statistical analysis. 
}
  \item{autocorr}{
Display the autocorrelation diagramme of the regularized time series using the acf
 function with arguments : lag.max = ((nrow(TimeSerie))/2), na.action = na.pass.
 Can be "YES" or "NO" (the default)
}
  \item{spectrum}{
Display the Fourrier spectrum of the regularized time series using a 
Smoothed Periodogram (spec.pgram). Can be "YES" or "NO" (the default). 
}
  \item{anomaly}{
Display a color box (function filled.contour) plot by year each time.step (months or weeks)
 minus the mean of the time.step of all years. Red colors show positive 
 anomalies and blue colors negative anomalies. Can be "YES" or "NO" (the default).
}
  \item{a.barplot}{
Display an anomaly barplot as a function of the time.step. Red colors show positive 
 anomalies and blue colors negative anomalies. Can be "YES" or "NO" (the default).
}
  \item{zsmooth}{
Display a detrended plot of the time series using the stl function with arguments
 s.window="periodic", na.action=na.fail. Can be "YES" or "NO" (the default).
}
  \item{local.trend}{
Display the interactive cusum plot of the time series (local.trend of the pastecs package)
 that allow to manually identify the period of change in the tendency using the 
 function identify and perform a Kendall familly test on each idenfified period (see test section). 
 Can be "YES" or "NO" (the default).
}
  \item{test}{
Perform a test to evaluate the presence and the magnitude of a temporal trend on the time series.
 Can be "MK" for Seasonal Mann-Kendall test (the default), "SMK" for the same test with detail for
 each time step, "LOESS" that fit a polynomial surface determined by one or more numerical predictors, 
 using local fitting; a MK is perform on this fitting.
}
  \item{test.pettitt}{
Performes a non-parametric pettitt test (from package "BreakPoints") to the time series.
The year, month or week of significant break in the time series is return.
}
  \item{OnOK4}{
button to call temporal analysis functions
}
 
}

\value{

Results are return as .png figures or .txt files
 Results are also directly readable directly in the right part 
 of the interface.
 
 Savepath can be choose using the option 'Select directory'
 (see the function <selectdirectory> more more informations)
 
 Name of saved filed follow the nomenclature :
 Original.file.name_analysis.name_parameter.txt/.png
 
 or for multiple period analysis (see cusum for more details) :
 Original.file.name_analysis.name_parameter_starting.year_ending.years.txt.
 
 analysis.names are :
 
 _Boxplot_  for boxplot figure (.png).
 _Outliers_  for the save of removed outliers (.txt).
 _TimeSeries_  for the plot of the regularized time series (.png).
 _Regularised_data_  for the table of regularized time series (.txt).
 _Autocor_  for the autocorelation diagram (.png).
 _Spectrum_  for the Fourier spectrum plot (.png) .
 _ColorPlot_  for the anomaly color.plot (.png).
 _Anomaly BarPlot_  for the anomaly barplot (.png)
 _Detrended_  for detrended plot (.png).
 _Local_Global Trend_  for result of Seasonal Mann Kendall apply to local trend (.txt).
 _Local_Seasonal Trend_  same as above with detail for each time step (.txt).
 _Global Trend_  for result of Seasonal Mann Kendall (.txt).
 _Seasonal Trend_  same as above with detail for each time step (.txt).
 _LOESSplot_  for loess plot (.png). 
 _NormalNutri_  for analysis of normalized values of nutrients (.png).

 See values output of corresponding functions.
}

\author{
David Devreker
}


\seealso{
\code{\link[e1071]{impute}}
\code{\link[pastecs]{local.trend}}
\code{\link[wql]{mannKen}}
\code{\link[wql]{seasonTrend}}
\code{\link[wql]{seaKen}}
}
