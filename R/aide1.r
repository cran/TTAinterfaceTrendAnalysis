Aide1 <- function () {

aide1 <- tktoplevel()
tktitle(aide1) <- "Help1 - Data_managment"
AIDE1 <- tktext(aide1, height=36, width=120)
tkpack(AIDE1)
tkinsert(AIDE1, "end", paste("This is the first panel of the Temporal Trend Analysis Interface", "\n\n",
"This panel shows 6 options/buttons:", "\n\n",
"     _The <Import CSV File> button give you the possibility to import your data file in the interface.", "\n\n",
"      This file must be a 'csv' file. 'csv' files can be created using Microsoft Excel", "\n",
"      or OpenOffice Calc. In a 'csv' file data column are separated with ';'." ,"\n",
"      Decimal separator must be '.' (dote) and not ',' (comma).", "\n\n",
"     _The <Select your save directory> button give you the possibility to change the path where results and figures" ,"\n",
"      will be saved (by default it's the same path as your .csv file). Each time you import a new","\n",
"      data file, the path is return to default.", "\n\n",
"     _The <Show Data> button give you the possibility to show the data you just import in a data frame", "\n\n",
"     _The <Summary> button call the descriptive statistics of the raw data, display in the result window" ,"\n",
"      (the right part of the interface) and in the front of the screen.", "\n\n",
"     _The <Fix Data> give you the possibility to fix some problem that can occur during the importation" ,"\n",
"      of your data.", "\n\n",
"      The most common problem is the absence of parameter/dates or stations in the interface." ,"\n",
"      This can sometimes be fix by clicking on the fix button that call a modifiable data.frame with your data." ,"\n",
"      Then you can change the name of the column, the value of each case and the category of the data" ,"\n",
"      by clicking on the column header (two choice : numeric or character). Choose" ,"\n",
"      numeric for your parameters and character for stations and date." ,"\n",
"      Date column must be named as 'DATES' and date must be formatted as dd/mm/yyyy." ,"\n",
"      Sampling site column must be names as 'STATIONS'" ,"\n",
"      Salinity column must be names as 'S'" ,"\n",
"      Depth column must be names as 'DEPTH'", "\n\n",
"      Each time the fix button is used, the new file will be saved as filename_fixed.csv", "\n\n",
"The text between button 1 and 2 show the active file (also show in the header of the interface)" ,"\n",
"The text between button 2 and 3 show the current path where results are saved", sep=""))

tkpack(tkbutton(aide1, text="Ok", command=function(){ tkdestroy(aide1) }, width=20 ), side="bottom")
tkconfigure(AIDE1, state="disabled")

}