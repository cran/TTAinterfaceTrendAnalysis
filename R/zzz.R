.onAttach <- function(lib,pkg) {
  startup <- tktoplevel()
  tktitle(startup) <- "Start Panel"
  LOAD1 <- function() { Lib()
  TTAinterface()   }
  LOAD1.but <- tkbutton(startup, text=" Ready to Start ! ", command=LOAD1)
  
  tkgrid(tklabel(startup, text="         "), row=1, column=0)
  tkgrid(LOAD1.but, row=2, column=1)
  tkgrid(tklabel(startup, text="         "), row=3, column=2)
   
  subtext <- tklabel(startup,text= "TTAinterface v1.01 launch panel")                       
  tkconfigure(subtext, font=tkfont.create(size=7))                                              
  tkgrid(subtext, column=1, row=4)
   
}

if(getRversion() >= "2.15.1") globalVariables(names=c("STATIONS","S","DEPTH","MONTHS", "param",
"depth","sal","site","npsu"), 
package="TTAinterfaceTrendAnalysis")