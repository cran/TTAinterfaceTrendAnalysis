.onAttach <- function(lib,pkg) {
  startup <- tktoplevel()
  tkwm.geometry(startup, "230x80")
  tkwm.resizable(startup, 0,0)
  tktitle(startup) <- "Start Panel"
  LOAD1 <- function() { Lib()
  TTAinterface()   }
  imgStart <- tclVar()                                                                                                
  tcl("image","create","photo",imgStart,file=file.path(.path.package("TTAinterfaceTrendAnalysis"),"aide","imgStart.gif",fsep=.Platform$file.sep))
  LOAD1.but <- tkbutton(startup, image=imgStart, text=" Ready to Start ! ", compound="right", command=LOAD1, height=18)
  
  tkpack(tklabel(startup, text=""), side="top")
  tkpack(LOAD1.but, side="top")
  tkpack(tklabel(startup, text=""), side="top")
   
  subtext <- tklabel(startup,text= "TTAinterface v1.03 launch panel")                       
  tkconfigure(subtext, font=tkfont.create(size=7))                                              
  tkpack(subtext, side="top")
  
  #logoRcran <- tclVar()                                                                                                
  #tcl("image","create","photo",logoRcran,file=file.path(.path.package("TTAinterfaceTrendAnalysis"),"aide","Small_Logo_R.gif",fsep=.Platform$file.sep))
  #imgAsLabel2 <- tklabel(startup,image=logoRcran)
  #tkgrid(imgAsLabel2, column=0, row=1, sticky="e")
   
}

if(getRversion() >= "2.15.2") globalVariables(names=c("STATIONS","S","DEPTH","MONTHS", "param",
"depth","sal","site","npsu"), 
package="TTAinterfaceTrendAnalysis")