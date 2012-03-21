Lib <- 
function() {
       
# si la librairie n'est pas présente, l'interface le télécharge automatiquement sur le site du CRAN

 if(!require("pastecs")){
    install.packages("pastecs",repos="http://cran.fr.r-project.org")
    require("pastecs")}
 if(!require("reshape")){
    install.packages("reshape",repos="http://cran.fr.r-project.org")
    require("reshape")}
 if(!require("wq")){
    install.packages("wq",repos="http://cran.fr.r-project.org")
    require("wq")}
 if(!require("stats")){
    install.packages("stats",repos="http://cran.fr.r-project.org")
    require("stats")}
 if(!require("e1071")){
    install.packages("e1071",repos="http://cran.fr.r-project.org")
    require("e1071")}
 if(!require("nlme")){
    install.packages("nlme",repos="http://cran.fr.r-project.org")
    require("nlme") }
 if(!require("timeSeries")){
    install.packages("timeSeries",repos="http://cran.fr.r-project.org")
    require("timeSeries") }
 if(!require("Hmisc")){
    install.packages("Hmisc",repos="http://cran.fr.r-project.org")
    require("Hmisc") }
 if(!require("fBasics")){
    install.packages("fBasics",repos="http://cran.fr.r-project.org")
    require("fBasics") }
 if(!require("tcltk")){
    install.packages("tcltk",repos="http://cran.fr.r-project.org")
    require("tcltk") }
  if(!require("relimp")){
    install.packages("relimp",repos="http://cran.fr.r-project.org")
    require("relimp") } 
    
    
# Charge toutes les librairies necessaires au bon fonctionnement de l'interface

library(tcltk)
tclRequire("BWidget")
library(relimp)
library(Hmisc)
library(pastecs, pos=4)
library(reshape, pos=4)
library(wq, pos=4)
library(stats, pos=4)
library(e1071, pos=4)
library(nlme, pos=4)
library(timeSeries, pos=4)
library(fBasics, pos=4)


}