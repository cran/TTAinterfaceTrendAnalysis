selectdirectory <-
function ( )  {

 Envir$save.WD <- tclvalue(tkchooseDirectory(mustexist=TRUE,title="Choose a save directory, then click OK."))
        if(Envir$save.WD == "") { Envir$save.WD <- Envir$default.save.WD }  
    
##### copier ici a partir de 'Save.directory <- function...' jusqu'a 'open <- tkbutton...'  #####   
   
     Save.directory <- function() { selectdirectory() }
     save.select <- tkbutton(Envir$rawdata, text="Select your save directory", command=Save.directory, width=22)        # bouton select save path
     tkgrid(save.select, column=1, row=3, sticky="w")                                                         

     fixdata1 <- function() { fixdata( ) }                                                                     # appel la fonction et les arguments necessaires
     fixdata.but <- tkbutton(Envir$rawdata, text="Fix Data", command=fixdata1, width=8)                        # bouton fixdata
     tkconfigure(fixdata.but, foreground="red")
     tkgrid(fixdata.but, column=1, row=7, sticky="w")

     showdata <- function()  { showData(Envir$Data) }                                                          # fonction showData
     showdata <- tkbutton(Envir$rawdata,text="Show Data",command=showdata, width=10)                           # bouton showdata
     tkgrid(showdata, column=1, row=5, sticky="w")

     Envir$Text <- tklabel(Envir$rawdata,text= paste("Current active file: ", Envir$Name.split, "                   "))    # texte montrant le fichier actif
     tkgrid(Envir$Text, column=1, row=2, sticky="w")
     tktitle(Envir$tt) <- paste("Temporal Trend Analysis: ", Envir$Name.split)

     Envir$Text2 <- tklabel(Envir$rawdata,text= paste("Current save directory: ", Envir$save.WD, "                  "))    # texte montrant chemin de sauvegarde
     tkgrid(Envir$Text2, column=1, row=4, sticky="w")

     STAT1 <- function () { FULLoption( param, depth, sal, site, rawdata="YES")  }                             # bouton appelant l'argument rawdata 
     STAT1.but <- tkbutton(Envir$rawdata, text=" Summary ",command=STAT1, width=8)
     tkgrid(STAT1.but, column=1, row=6, sticky="w")
                                                                                                                  
     tkgrid(tklabel(Envir$rawdata, text="  "), column=1, row=8)                                                # lignes vides (espaces)
     tkgrid(tklabel(Envir$rawdata, text="  "), column=1, row=9)

     AdviceFrame <- tkwidget(Envir$rawdata,"labelframe",text="Important : how to well import your data in 9 steps",padx=30,pady=8)      # cadre de texte
     tkgrid(AdviceFrame, column=1, row=10, sticky="w")
     tkgrid(tklabel(AdviceFrame, text="Your data must be in a .csv file (save as '.csv' format in Excel)"), sticky="w")
     tkgrid(tklabel(AdviceFrame, text="Decimal separtor must be '.'"), sticky="w")
     tkgrid(tklabel(AdviceFrame, text="Missing value must be empty case"), sticky="w")
     tkgrid(tklabel(AdviceFrame, text="Dates must be in  'dd/mm/yyyy'  format"), sticky="w")
     tkgrid(tklabel(AdviceFrame, text=" Dates column -> DATES"), sticky="w")
     tkgrid(tklabel(AdviceFrame, text=" Sampling site column -> STATIONS"), sticky="w")
     tkgrid(tklabel(AdviceFrame, text=" Salinity column -> S"), sticky="w")
     tkgrid(tklabel(AdviceFrame, text=" Depth column -> DEPTH"), sticky="w")
     tkgrid(tklabel(AdviceFrame, text="If your parameters don't appear, select them as 'numeric' "), sticky="w")
     Adv1 <- tklabel(AdviceFrame, text="You can use the 'Fix Data' button to change column labels and data category")
     tkconfigure(Adv1, foreground="red")
     tkgrid(Adv1, sticky="w")
     
     tkgrid(tklabel(Envir$rawdata, text="      "), column=1, row=12)
      
     HELP1.but <- tkbutton(Envir$HelpFrame, text=" Help ",command=function() {                                                   # bouton d'aide
     browseURL(file.path(.path.package("TTAinterfaceTrendAnalysis"),"aide","HELP1.txt",fsep=.Platform$file.sep)) })            # fichier txt a aller chercher
     tkgrid(HELP1.but, column=1, row=1, sticky="nw")

#_______________________________________________________________________________________________________________________Onglet Selection des parametres

     Env <- new.env()                                                                                         # cree un environnement pour les listes
     st <- levels(as.factor(Envir$Data$STATIONS))                                                             # label des stations
     Env$variables <- c("-All-", st)                                                                          # on ajoute '-All-' a la liste
     Env$variables.selectionnees.temp <- integer()

     liste1 <- tklistbox(Envir$Select,selectmode="extended", activestyle="dotbox", height=10, width=20)       # cree la premiere liste
                for (i in 1:length(Env$variables)) { tkinsert(liste1,"end",Env$variables[i]) }                # rempli la premiere liste avec le nom des stations
     liste2 <- tklistbox(Envir$Select,selectmode="extended", activestyle="dotbox", height=10, width=22)       # cree la deuxieme liste (de selection)

     bouton1 <- tkbutton(Envir$Select,text="->",width=5,height=2,command=function() {                         # bouton de selection
                if (tclvalue(tkcurselection(liste1))!="") {
                      val <- unlist(strsplit(tclvalue(tkcurselection(liste1)), "\\ "))                           # les stations selectionnees sont stockees dans 'val'
                      selection <- as.numeric(val)+1                                                             # puis sont transformees (+1 car la premiere valeur de la liste = 0)
                      for (i in min(selection):max(selection)) { tkinsert(liste2,"end",Env$variables[i]) }       # insertion des valeurs selectionnees dans la liste 2
                      Env$variables.selectionnees.temp <- c(Env$variables.selectionnees.temp, selection) }       # les valeurs finales selectionnees sont stockees
                else { tkmessageBox(message="No station selected !",type="ok",icon="info", title="!Warning!") }})
     bouton2 <- tkbutton(Envir$Select,text="<-",width=5,height=2,command=function() {                            # bouton de deselection
                if (tclvalue(tkcurselection(liste2))!="") {
                      val <- unlist(strsplit(tclvalue(tkcurselection(liste2)), "\\ "))
                      selection <- as.numeric(val)+1
                      for (i in 1:length(selection)) { tkdelete(liste2, min(selection)-1) }
                      for (i in 1:length(selection)) { Env$variables.selectionnees.temp <- Env$variables.selectionnees.temp[-min(selection)]  }}
                else { tkmessageBox(message="No station selected !",type="ok",icon="info", title="!Warning!") } })

     tkgrid(tklabel(Envir$Select, text="Select your station(s)"), row=0, column=0,rowspan=2)                     # titres des listes
     tkgrid(tklabel(Envir$Select, text="Selected station(s)"), row=0, column=2,rowspan=2)
     tkgrid(liste1,row=2,column=0,rowspan=2)
     tkgrid(bouton1,row=2,column=1)
     tkgrid(bouton2,row=3,column=1)
     tkgrid(liste2,row=2,column=2,rowspan=2)

     cb <- tkcheckbutton(Envir$Select)                                                # check case
     cbValue <- tclVar("1")
     tkconfigure(cb,variable=cbValue)
     tkgrid(tklabel(Envir$Select,text="Aggregate stations ?"), column=0, row=4)
     tkgrid(cb, column=1, row=4)

     ListParam <- NULL
     for (i in 1:ncol(Envir$Data))
     if (class(Envir$Data[ ,i]) == "numeric") {                                       # seletionne les colonnes dans lesquelles les valeurs sont 'numeric'
     d <- names(Envir$Data[i])                                                        # garde le nom de ces colonnes
     ListParam <- as.character(c(ListParam, d)) }                                     # en fait la liste des parametres

     Env2 <- new.env()                                                                # idem que pour les stations
     Env2$variables <- ListParam
     Env2$variables.selectionnees.temp <- integer()

     liste3 <- tklistbox(Envir$Select,selectmode="single", activestyle="dotbox", height=6, width=20)
               for (i in 1:length(Env2$variables)) { tkinsert(liste3,"end",Env2$variables[i]) }
     liste4 <- tklistbox(Envir$Select,selectmode="single", activestyle="dotbox", height=6, width=22)

     bouton3 <- tkbutton(Envir$Select,text="->",width=5, height=6, command=function() {
                if (tclvalue(tkcurselection(liste3))!="") {
                   val2 <- unlist(strsplit(tclvalue(tkcurselection(liste3)), "\\ "))
                   selection2 <- as.numeric(val2)+1
                   for (i in min(selection2):max(selection2)) {
                   tkdelete(liste4, 0)                                                          # supprime la valeur precedemment presente dans la liste 2
                   tkinsert(liste4,"end",Env2$variables[i]) }                                   #     et la remplace par la nouvelle valeur
                   Env2$variables.selectionnees.temp <- c(selection2) }
                else { tkmessageBox(message="No variable selected !",type="ok",icon="info", title="!Warning!") }})

     tkgrid(tklabel(Envir$Select, text="Select a parameter"), row=11, column=0,rowspan=2)            # titre des listes
     tkgrid(tklabel(Envir$Select, text="Selected parameter"), row=11, column=2,rowspan=2)            #       "
     tkgrid(liste3,row=13,column=0,rowspan=2)
     tkgrid(bouton3,row=13,column=1)
     tkgrid(liste4,row=13,column=2,rowspan=2)

     tkgrid(tklabel(Envir$Select, text="      "), column=0, row=15)
     tkgrid(tklabel(Envir$Select, text="      "), column=2, row=15)

     if (any(colnames(Envir$Data) == "S")) {                                                                            # effectue les lignes suivantes si une colonne S est presente
        if (class(Envir$Data$S) == "numeric") {                                                                         #      et si c'est bien une valeur numerique
          sal1 <- tclVar(min(Envir$Data$S, na.rm=TRUE))                                                                
          slider1 <- tkscale(Envir$Select, from=min(Envir$Data$S, na.rm=TRUE), to=max(Envir$Data$S, na.rm=TRUE), length=100,       # slider coulissant entre les valeurs min et max de la colonne S
                     showvalue=FALSE, variable=sal1,                                                                                   
                     resolution=0.5, orient="horizontal", command=function(...) {
                     tkconfigure(Label1,text=paste(" Salinity min :", tclvalue(sal1), "psu ")) })
          Label1 <- tklabel(Envir$Select,text=paste(" Salinity min :",tclvalue(sal1),"psu "))
          tkgrid(Label1, column=0, row=16)
          tkgrid(slider1, column=0, row=17)

          sal2 <- tclVar(max(Envir$Data$S, na.rm=TRUE))
          slider2 <- tkscale(Envir$Select, from=min(Envir$Data$S, na.rm=TRUE), to=max(Envir$Data$S, na.rm=TRUE), length=100,
                     showvalue=FALSE, variable=sal2,
                     resolution=0.5, orient="horizontal", command=function(...) {
                     tkconfigure(Label2,text=paste(" Salinity max :", tclvalue(sal2), "psu ")) })
          Label2 <- tklabel(Envir$Select,text=paste(" Salinity max :",tclvalue(sal2),"psu "))
          tkgrid(Label2, column=0, row=18)
          tkgrid(slider2, column=0, row=19) } 
     else{ tkmessageBox(message=paste("A salinity column is present but don't contain any 'numeric' values", "\n\n"
                         , "You can fix it using the fix data button (use Help for more info)", sep="")
                         , icon = "warning", type = "ok", title="!Warning!") } }
     else{ tkgrid(tklabel(Envir$Select, text="                                  "), column=0, row=16)                 # s'il n'y a pas de colonne S
            tkgrid(tklabel(Envir$Select, text="No salinity values in "), column=0, row=17)
            tkgrid(tklabel(Envir$Select, text="     the data frame      "), column=0, row=18)
            tkgrid(tklabel(Envir$Select, text="                                  "), column=0, row=19) }

     if (any(colnames(Envir$Data) == "DEPTH")) {                                                                      # idem pour la profondeur
       if (class(Envir$Data$DEPTH) == "numeric") {
          depth1 <- tclVar(min(Envir$Data$DEPTH, na.rm=TRUE))
          slider3 <- tkscale(Envir$Select, from=min(Envir$Data$DEPTH, na.rm=TRUE), to=max(Envir$Data$DEPTH, na.rm=TRUE), length=100,
                     showvalue=FALSE, variable=depth1,
                     resolution=0.5, orient="horizontal", command=function(...) {
                     tkconfigure(Label3,text=paste("Depth min :", tclvalue(depth1), "m")) })
          Label3 <- tklabel(Envir$Select,text=paste("Depth min :",tclvalue(depth1),"m"))
          tkgrid(Label3, column=0, row=20)
          tkgrid(slider3, column=0, row=21)

          depth2 <- tclVar(max(Envir$Data$DEPTH, na.rm=TRUE))
          slider4 <- tkscale(Envir$Select, from=min(Envir$Data$DEPTH, na.rm=TRUE), to=max(Envir$Data$DEPTH, na.rm=TRUE), length=100,
                     showvalue=FALSE, variable=depth2,
                     resolution=0.5, orient="horizontal", command=function(...) {
                     tkconfigure(Label4,text=paste("Depth max :", tclvalue(depth2), "m")) })
          Label4 <- tklabel(Envir$Select,text=paste("Depth max :",tclvalue(depth2),"m"))
          tkgrid(Label4, column=0, row=22)
          tkgrid(slider4, column=0, row=23)  }
       else{ tkmessageBox(message=paste("A depth column is present but don't contain any 'numeric' values", "\n\n"
                         , "You can fix it using the fix data button (use Help for more info)", sep="")
                         , icon = "warning", type = "ok", title="!Warning!") } }
     else{ tkgrid(tklabel(Envir$Select, text="                                  "), column=0, row=20)
            tkgrid(tklabel(Envir$Select, text="  No depth values in  "), column=0, row=21)
            tkgrid(tklabel(Envir$Select, text="    the data frame    "), column=0, row=22)
            tkgrid(tklabel(Envir$Select, text="                                  "), column=0, row=23)  }

     if (any(colnames(Envir$Data) == "DATES")) {                                                                      # idem pour les annees

            d <- as.Date(Envir$Data$DATES, format="%d/%m/%Y")
            years <- as.numeric(format(d, format = "%Y"))

            year1 <- tclVar(min(years, na.rm=TRUE))
            SpinBox1 <- tkwidget(Envir$Select
                                 ,"spinbox"
                                 ,from=min(years, na.rm=TRUE)
                                 ,to=max(years, na.rm=TRUE)
                                 ,readonlybackground="white"
                                 ,width=16
                                 ,textvariable=year1)
            tkgrid(tklabel(Envir$Select, text="       Start year at :       "), column=2, row=16)
            tkgrid(SpinBox1, column=2, row=17)

            year2 <- tclVar(max(years, na.rm=TRUE))
            SpinBox2 <- tkwidget(Envir$Select
                                 ,"spinbox"
                                 ,from=min(years, na.rm=TRUE)
                                 ,to=max(years, na.rm=TRUE)
                                 ,readonlybackground="white"
                                 ,width=16
                                 ,textvariable=year2)
           tkgrid(tklabel(Envir$Select, text="        End year at :        "), column=2, row=18)
           tkgrid(SpinBox2, column=2, row=19)

           emptybox20<- tklabel(Envir$Select, text="                                          ")
           tkgrid(emptybox20, column=2, row=20)

           mois <- tclVar(c(1:12))                                                               # les mois sous forme de liste de 1 a 12 (separes par un espace)
           entry.mois <-tkentry(Envir$Select,width="21",textvariable=mois)
           tkgrid(tklabel(Envir$Select,text="Select month(s)"), column=2, row=21)
           tkgrid(entry.mois, column=2, row=22) }

     else { emptybox10 <- tklabel(Envir$Select, text="!No date in your data!")                   # s'il n'y a pas de colonne DATES
            tkconfigure(emptybox10, foreground="red")
            tkgrid(emptybox10, column=2, row=16)
            tkgrid(tklabel(Envir$Select, text="                                    "), column=2, row=17)
            tkgrid(tklabel(Envir$Select, text="Check if the column"), column=2, row=18)
            tkgrid(tklabel(Envir$Select, text="  is correctely labelled   "), column=2, row=19)
            tkgrid(tklabel(Envir$Select, text="      as DATES      "), column=2, row=20)
            tkgrid(tklabel(Envir$Select, text="                                          "), column=2, row=21)
            tkgrid(tklabel(Envir$Select, text="                                          "), column=2, row=22)   }

#_______________________________________________________________________________bouton d'appel de la fonction 'select'
     STAT2 <- function () {
            param <- Env2$variables[unique(Env2$variables.selectionnees.temp)]
            site <- Env$variables[unique(Env$variables.selectionnees.temp)]
            if (any(site == "-All-")) { site <- st  }
            if (length(param)==0 | length(site)==0 )
               { return(tkmessageBox(message="Please select a parameter and a station before to proceed"
                                     , icon = "warning", type = "ok", title="!Warning!")) }
            else{}
            cbVal <- as.character(tclvalue(cbValue))
           if (cbVal=="1"){ mix <- "YES" }
           else { mix <- "NO" }
           if (any(colnames(Envir$Data) == "S")) {
            Sal1 <- as.numeric(tclvalue(sal1))
            Sal2 <- as.numeric(tclvalue(sal2))
            sal <- c(Sal1, Sal2) }
           else{ sal <- "NULL" }
           if (any(colnames(Envir$Data) == "DEPTH")) {
            Depth1 <- as.numeric(tclvalue(depth1))
            Depth2 <- as.numeric(tclvalue(depth2))
            depth <- c(Depth1, Depth2) }
           else{ depth <- "NULL" }
           if (any(colnames(Envir$Data) == "DATES")) {
            start <- as.numeric(tclvalue(year1))
            end <- as.numeric(tclvalue(year2))
            months <- as.numeric(unlist(strsplit((tclvalue(mois)),"\\ "))) }
           else { tkmessageBox(message="No date selected!", icon = "warning", type = "ok", title="!Warning!")}
           FULLoption(param, depth, sal, site, rawdata="NO", select="YES", resume.reg="NO", test.normality="NO", 
                     plotB="NO", plotZ="NO", datashow="NO",
                     help.timestep="NO", auto.timestep="NO", time.step="NULL", help.aggreg="NO", auto.aggreg="NO", aggreg="NULL",
                     mix, outliers.re="NO", na.replace="NO", start, end, months)  }
     STAT2.but <- tkbutton(Envir$Select, text="Summary",command=STAT2)
     tkgrid(STAT2.but, column=0, row=24)
#_______________________________________________________________________________fin du bouton

     tkgrid(tklabel(Envir$Select, text="      "), column=0, row=25)

     HELP2.but <- tkbutton(Envir$Select, text=" Help ",command=function() {                           # bouton d'aide n°2
     browseURL(file.path(.path.package("TTAinterfaceTrendAnalysis"),"aide","HELP2.txt",fsep=.Platform$file.sep)) }) 
     tkgrid(HELP2.but, column=0, row=26, sticky="w")

#_________________________________________________________________________________________________________________________________Onglet regularisation

     LabeledFrame1 <- tkwidget(Envir$datam,"labelframe",text="Data interaction",padx=25,pady=10)      # cadre
     tkgrid(LabeledFrame1, column=0, row=0, sticky="w")

         cb2 <- tkcheckbutton(LabeledFrame1)                                                          # check button pour missing values
         cb2Value <- tclVar("0")
         tkconfigure(cb2,variable=cb2Value)
         tkgrid(tklabel(LabeledFrame1,text="Replace missing values ?"), column=0, row=0, sticky="w")
         tkgrid(cb2, column=1, row=0)

         cb3 <- tkcheckbutton(LabeledFrame1)                                                           # check button pour outliers
         cb3Value <- tclVar("0")
         tkconfigure(cb3,variable=cb3Value)
         tkgrid(tklabel(LabeledFrame1,text="Remove outliers ?"), column=0, row=1, sticky="w")
         tkgrid(cb3, column=1, row=1)
#_______________________________________________________________________________bouton d'appel de la boxplot
     BoxPlot <- function()  {
            param <- Env2$variables[unique(Env2$variables.selectionnees.temp)]
            site <- Env$variables[unique(Env$variables.selectionnees.temp)]
            if (any(site == "-All-")) { site <- st  }
            if (length(param)==0 | length(site)==0 )
               { return(tkmessageBox(message="Please select a parameter and a station before to proceed"
                                    , icon = "warning", type = "ok", title="!Warning!")) }
            else{}
            cbVal <- as.character(tclvalue(cbValue))
           if (cbVal=="1"){ mix <- "YES" }
           else { mix <- "NO" }
           if (any(colnames(Envir$Data) == "S")) {
            Sal1 <- as.numeric(tclvalue(sal1))
            Sal2 <- as.numeric(tclvalue(sal2))
            sal <- c(Sal1, Sal2)}
           else{ sal <- "NULL" }
           if (any(colnames(Envir$Data) == "DEPTH")) {
            Depth1 <- as.numeric(tclvalue(depth1))
            Depth2 <- as.numeric(tclvalue(depth2))
            depth <- c(Depth1, Depth2) }
           else{ depth <- "NULL" }
           if (any(colnames(Envir$Data) == "DATES")) {
            start <- as.numeric(tclvalue(year1))
            end <- as.numeric(tclvalue(year2))
            months <- as.numeric(unlist(strsplit((tclvalue(mois)),"\\ "))) }
           else { tkmessageBox(message="no date selected", icon = "warning", type = "ok", title="!Warning!")}
           plotB <- "YES"
           FULLoption(param, depth, sal, site, rawdata="NO", select="NO", resume.reg="NO",test.normality="NO",
                     plotB="YES", plotZ="NO", datashow="NO",
                     help.timestep="NO", auto.timestep="NO", time.step="NULL", help.aggreg="NO", auto.aggreg="NO", aggreg="NULL",
                     mix, outliers.re="NO", na.replace="NO", start, end, months) }
     BoxPlot.but <- tkbutton(LabeledFrame1, text="Show boxplot",command=BoxPlot)
     tkgrid(BoxPlot.but, column=0, row=2, sticky="w")
#_______________________________________________________________________________fin du bouton

     tkgrid(tklabel(Envir$datam, text="              "), column=0, row=1)

     LabeledFrame2 <- tkwidget(Envir$datam,"labelframe",text="Select the time step for your final time series",padx=25,pady=10)
     tkgrid(LabeledFrame2, column=0, row=2, sticky="w")
     
         rb1 <- tkradiobutton(LabeledFrame2)                                                     # radio buttons pour le time step
         rb2 <- tkradiobutton(LabeledFrame2)
         rb3 <- tkradiobutton(LabeledFrame2)
         rb4 <- tkradiobutton(LabeledFrame2)
         rb5 <- tkradiobutton(LabeledFrame2)
         rb6 <- tkradiobutton(LabeledFrame2)
         rb7 <- tkradiobutton(LabeledFrame2)
         rb1Value <- tclVar("auto")
         tkconfigure(rb1,variable=rb1Value,value="Annual")
         tkconfigure(rb2,variable=rb1Value,value="Mensual")
         tkconfigure(rb3,variable=rb1Value,value="Fortnight")
         tkconfigure(rb4,variable=rb1Value,value="Semi-fortnight")
         tkconfigure(rb5,variable=rb1Value,value="Mono-mensual")
         tkconfigure(rb6,variable=rb1Value,value="help")
         tkconfigure(rb7,variable=rb1Value,value="auto")
         tkgrid(tklabel(LabeledFrame2,text="Yearly "),rb1, sticky="w")
         tkgrid(tklabel(LabeledFrame2,text="Monthly "),rb2, sticky="w")
         tkgrid(tklabel(LabeledFrame2,text="Fortnightly "),rb3, sticky="w")
         tkgrid(tklabel(LabeledFrame2,text="Semi-fortnightly "),rb4, sticky="w")
         tkgrid(tklabel(LabeledFrame2,text="Monomensualy "),rb5, sticky="w")
         tkgrid(tklabel(LabeledFrame2,text="Guidance to choose the time step "),rb6, sticky="w")
         tkgrid(tklabel(LabeledFrame2,text="Auto "),rb7, sticky="w")

     tkgrid(tklabel(Envir$datam, text="              "), column=0, row=3)

     LabeledFrame3 <- tkwidget(Envir$datam,"labelframe",text="Select the method to aggregate your data",padx=25,pady=10)
     tkgrid(LabeledFrame3, column=0, row=4, sticky="w")

         rb1 <- tkradiobutton(LabeledFrame3)                                                     # radio buttons pour la methode mathematique
         rb2 <- tkradiobutton(LabeledFrame3)
         rb3 <- tkradiobutton(LabeledFrame3)
         rb4 <- tkradiobutton(LabeledFrame3)
         rb5 <- tkradiobutton(LabeledFrame3)
         rb6 <- tkradiobutton(LabeledFrame3)
         rb2Value <- tclVar("auto")
         tkconfigure(rb1,variable=rb2Value,value="Mean")
         tkconfigure(rb2,variable=rb2Value,value="Median")
         tkconfigure(rb3,variable=rb2Value,value="Quantile")
         tkconfigure(rb4,variable=rb2Value,value="Max")
         tkconfigure(rb5,variable=rb2Value,value="help")
         tkconfigure(rb6,variable=rb2Value,value="auto")
         tkgrid(tklabel(LabeledFrame3,text="Mean "),rb1, sticky="w")
         tkgrid(tklabel(LabeledFrame3,text="Median "),rb2, sticky="w")
         tkgrid(tklabel(LabeledFrame3,text="Quantile 0.9 "),rb3, sticky="w")
         tkgrid(tklabel(LabeledFrame3,text="Maximum "),rb4, sticky="w")
         tkgrid(tklabel(LabeledFrame3,text="Guidance to choose the method "),rb5, sticky="w")
         tkgrid(tklabel(LabeledFrame3,text="Auto "),rb6, sticky="w")

         tkgrid(tklabel(Envir$datam, text="              "), column=2, row=2)
         
     LabeledFrame7 <- tkwidget(Envir$datam,"labelframe",text="Show regularised time series",padx=10,pady=0)
     tkgrid(LabeledFrame7, column=3, row=2, sticky="n")
     tkgrid(tklabel(LabeledFrame7, text="                                                                                                                                                                            "
                                 , font=tkfont.create(size=1)), column=0, row=0)
     tkgrid(tklabel(LabeledFrame7, text=" ", font=tkfont.create(size=1)), column=0, row=2)

#_______________________________________________________________________________bouton d'appel de la figure de la serie regularisee
     OnOK <- function()  {
            param <- Env2$variables[unique(Env2$variables.selectionnees.temp)]
            site <- Env$variables[unique(Env$variables.selectionnees.temp)]
            if (any(site == "-All-")) { site <- st  }
            if (length(param)==0 | length(site)==0 )
               { return(tkmessageBox(message="Please select a parameter and a station before to proceed"
                                    , icon = "warning", type = "ok", title="!Warning!")) }
            else{}
            cbVal <- as.character(tclvalue(cbValue))
           if (cbVal=="1"){ mix <- "YES" }
           else { mix <- "NO" }
           if (any(colnames(Envir$Data) == "S")) {
            Sal1 <- as.numeric(tclvalue(sal1))
            Sal2 <- as.numeric(tclvalue(sal2))
            sal <- c(Sal1, Sal2)}
           else{ sal <- "NULL" }
           if (any(colnames(Envir$Data) == "DEPTH")) {
            Depth1 <- as.numeric(tclvalue(depth1))
            Depth2 <- as.numeric(tclvalue(depth2))
            depth <- c(Depth1, Depth2) }
           else{ depth <- "NULL" }
           if (any(colnames(Envir$Data) == "DATES")) {
            start <- as.numeric(tclvalue(year1))
            end <- as.numeric(tclvalue(year2))
            months <- as.numeric(unlist(strsplit((tclvalue(mois)),"\\ "))) }
           else { tkmessageBox(message="no date selected", icon = "warning", type = "ok", title="!Warning!")}

          cb2Value <- as.character(tclvalue(cb2Value))
          if (cb2Value=="1"){ na.replace <- "YES" }
           else { na.replace <- "NO" }
          cb3Value <- as.character(tclvalue(cb3Value))
          if (cb3Value=="1"){ outliers.re <- "YES" }
           else { outliers.re <- "NO" }

          rb1Value <- as.character(tclvalue(rb1Value))
          if (rb1Value=="Annual"){ time.step <- "Annual" }
          if (rb1Value=="Mensual"){ time.step <- "Mensual" }
          if (rb1Value=="Fortnight"){ time.step <- "Fortnight" }
          if (rb1Value=="Semi-fortnight"){ time.step <- "Semi-fortnight" }
          if (rb1Value=="Mono-mensual"){ time.step <- "Mono-mensual" }
          if (rb1Value=="help"){ help.timestep <- "YES"
                                 time.step <- "NULL" }
          else{ help.timestep <- "N0" }
          if (rb1Value=="auto"){ auto.timestep <- "YES"
                                 time.step <- "NULL" }
          else{ auto.timestep <- "NO" }

          rb2Value <- as.character(tclvalue(rb2Value))
          if (rb2Value=="Mean"){ aggreg <- "Mean" }
          if (rb2Value=="Median"){ aggreg <- "Median" }
          if (rb2Value=="Quantile"){ aggreg <- "Quantile" }
          if (rb2Value=="Max"){ aggreg <- "Max" }
          if (rb2Value=="help"){ help.aggreg <- "YES"
                                 aggreg <- "NULL" }
          else{ help.aggreg <- "N0" }
          if (rb2Value=="auto"){ auto.aggreg <- "YES"
                                 aggreg <- "NULL" }
          else{ auto.aggreg <- "N0" }
          FULLoption(param, depth, sal, site, rawdata="NO", select="NO", resume.reg="NO", test.normality="NO",
                 plotB="NO", plotZ="YES", datashow="NO",
                 help.timestep, auto.timestep, time.step, help.aggreg, auto.aggreg, aggreg,
                 mix, outliers.re, na.replace, start, end, months, norm="NO", npsu,
                 autocorr = "NO", spectrum="NO", anomaly="NO", zsmooth="NO", local.trend="NO", test="NO") }
     
     OK.but <- tkbutton(LabeledFrame7, text="    Plot    ",command=OnOK)
     tkgrid(OK.but, column=0, row=1, sticky="w")
#_______________________________________________________________________________bouton d'appel du tableau des donnees regularisees
     OnOK1 <- function()  {
            param <- Env2$variables[unique(Env2$variables.selectionnees.temp)]
            site <- Env$variables[unique(Env$variables.selectionnees.temp)]
            if (any(site == "-All-")) { site <- st  }
            if (length(param)==0 | length(site)==0 )
               { return(tkmessageBox(message="Please select a parameter and a station before to proceed"
                                     , icon = "warning", type = "ok", title="!Warning!")) }
            else{}
            cbVal <- as.character(tclvalue(cbValue))
           if (cbVal=="1"){ mix <- "YES" }
           else { mix <- "NO" }
           if (any(colnames(Envir$Data) == "S")) {
            Sal1 <- as.numeric(tclvalue(sal1))
            Sal2 <- as.numeric(tclvalue(sal2))
            sal <- c(Sal1, Sal2)}
           else{ sal <- "NULL" }
           if (any(colnames(Envir$Data) == "DEPTH")) {
            Depth1 <- as.numeric(tclvalue(depth1))
            Depth2 <- as.numeric(tclvalue(depth2))
            depth <- c(Depth1, Depth2) }
           else{ depth <- "NULL" }
           if (any(colnames(Envir$Data) == "DATES")) {
            start <- as.numeric(tclvalue(year1))
            end <- as.numeric(tclvalue(year2))
            months <- as.numeric(unlist(strsplit((tclvalue(mois)),"\\ "))) }
           else { tkmessageBox(message="no date selected", icon = "warning", type = "ok", title="!Warning!")}

          cb2Value <- as.character(tclvalue(cb2Value))
          if (cb2Value=="1"){ na.replace <- "YES" }
           else { na.replace <- "NO" }
          cb3Value <- as.character(tclvalue(cb3Value))
          if (cb3Value=="1"){ outliers.re <- "YES" }
           else { outliers.re <- "NO" }

          rb1Value <- as.character(tclvalue(rb1Value))
          if (rb1Value=="Annual"){ time.step <- "Annual" }
          if (rb1Value=="Mensual"){ time.step <- "Mensual" }
          if (rb1Value=="Fortnight"){ time.step <- "Fortnight" }
          if (rb1Value=="Semi-fortnight"){ time.step <- "Semi-fortnight" }
          if (rb1Value=="Mono-mensual"){ time.step <- "Mono-mensual" }
          if (rb1Value=="help"){ help.timestep <- "YES"
                                 time.step <- "NULL" }
          else{ help.timestep <- "N0" }
          if (rb1Value=="auto"){ auto.timestep <- "YES"
                                 time.step <- "NULL" }
          else{ auto.timestep <- "NO" }

          rb2Value <- as.character(tclvalue(rb2Value))
          if (rb2Value=="Mean"){ aggreg <- "Mean" }
          if (rb2Value=="Median"){ aggreg <- "Median" }
          if (rb2Value=="Quantile"){ aggreg <- "Quantile" }
          if (rb2Value=="Max"){ aggreg <- "Max" }
          if (rb2Value=="help"){ help.aggreg <- "YES"
                                 aggreg <- "NULL" }
          else{ help.aggreg <- "N0" }
          if (rb2Value=="auto"){ auto.aggreg <- "YES"
                                 aggreg <- "NULL" }
          else{ auto.aggreg <- "N0" }
          FULLoption(param, depth, sal, site, rawdata="NO", select="NO", resume.reg="NO", test.normality="NO",
                 plotB="NO", plotZ="NO", datashow="YES",
                 help.timestep, auto.timestep, time.step, help.aggreg, auto.aggreg, aggreg,
                 mix, outliers.re, na.replace, start, end, months, norm="NO", npsu,
                 autocorr = "NO", spectrum="NO", anomaly="NO", zsmooth="NO", local.trend="NO", test="NO") }
     OK1.but <- tkbutton(LabeledFrame7, text=" Table ",command=OnOK1)
     tkgrid(OK1.but, column=0, row=1)

#_______________________________________________________________________________bouton d'appel des stat desciptives sur donnees regularisees
     OnOK2 <- function()  {
            param <- Env2$variables[unique(Env2$variables.selectionnees.temp)]
            site <- Env$variables[unique(Env$variables.selectionnees.temp)]
            if (any(site == "-All-")) { site <- st  }
            if (length(param)==0 | length(site)==0 )
               { return(tkmessageBox(message="Please select a parameter and a station before to proceed"
                                    , icon = "warning", type = "ok", title="!Warning!")) }
            else{}
            cbVal <- as.character(tclvalue(cbValue))
           if (cbVal=="1"){ mix <- "YES" }
           else { mix <- "NO" }
           if (any(colnames(Envir$Data) == "S")) {
            Sal1 <- as.numeric(tclvalue(sal1))
            Sal2 <- as.numeric(tclvalue(sal2))
            sal <- c(Sal1, Sal2)}
           else{ sal <- "NULL" }
           if (any(colnames(Envir$Data) == "DEPTH")) {
            Depth1 <- as.numeric(tclvalue(depth1))
            Depth2 <- as.numeric(tclvalue(depth2))
            depth <- c(Depth1, Depth2) }
           else{ depth <- "NULL" }
           if (any(colnames(Envir$Data) == "DATES")) {
            start <- as.numeric(tclvalue(year1))
            end <- as.numeric(tclvalue(year2))
            months <- as.numeric(unlist(strsplit((tclvalue(mois)),"\\ "))) }
           else { tkmessageBox(message="no date selected", icon = "warning", type = "ok", title="!Warning!")}

          cb2Value <- as.character(tclvalue(cb2Value))
          if (cb2Value=="1"){ na.replace <- "YES" }
           else { na.replace <- "NO" }
          cb3Value <- as.character(tclvalue(cb3Value))
          if (cb3Value=="1"){ outliers.re <- "YES" }
           else { outliers.re <- "NO" }

          rb1Value <- as.character(tclvalue(rb1Value))
          if (rb1Value=="Annual"){ time.step <- "Annual" }
          if (rb1Value=="Mensual"){ time.step <- "Mensual" }
          if (rb1Value=="Fortnight"){ time.step <- "Fortnight" }
          if (rb1Value=="Semi-fortnight"){ time.step <- "Semi-fortnight" }
          if (rb1Value=="Mono-mensual"){ time.step <- "Mono-mensual" }
          if (rb1Value=="help"){ help.timestep <- "YES"
                                 time.step <- "NULL" }
          else{ help.timestep <- "N0" }
          if (rb1Value=="auto"){ auto.timestep <- "YES"
                                 time.step <- "NULL" }
          else{ auto.timestep <- "NO" }

          rb2Value <- as.character(tclvalue(rb2Value))
          if (rb2Value=="Mean"){ aggreg <- "Mean" }
          if (rb2Value=="Median"){ aggreg <- "Median" }
          if (rb2Value=="Quantile"){ aggreg <- "Quantile" }
          if (rb2Value=="Max"){ aggreg <- "Max" }
          if (rb2Value=="help"){ help.aggreg <- "YES"
                                 aggreg <- "NULL" }
          else{ help.aggreg <- "N0" }
          if (rb2Value=="auto"){ auto.aggreg <- "YES"
                                 aggreg <- "NULL" }
          else{ auto.aggreg <- "N0" }
          FULLoption(param, depth, sal, site, rawdata="NO", select="NO", resume.reg="YES", test.normality="NO",
                 plotB="NO", plotZ="NO", datashow="NO",
                 help.timestep, auto.timestep, time.step, help.aggreg, auto.aggreg, aggreg,
                 mix, outliers.re, na.replace, start, end, months, norm="NO", npsu,
                 autocorr = "NO", spectrum="NO", anomaly="NO", zsmooth="NO", local.trend="NO", test="NO")   }
     OK2.but <- tkbutton(LabeledFrame7, text=" Summary ",command=OnOK2)
     tkgrid(OK2.but, column=0, row=1, sticky="e")
#_______________________________________________________________________________fin du bouton      

     tkgrid(tklabel(Envir$datam, text="      "), column=0, row=5)

     HELP3.but <- tkbutton(Envir$datam, text=" Help ",command=function() {                            # bouton d'aide
     browseURL(file.path(.path.package("TTAinterfaceTrendAnalysis"),"aide","HELP3.txt",fsep=.Platform$file.sep)) })
     tkgrid(HELP3.but, column=0, row=6, sticky="w")

#________________________________________________________________________________________________________________________________________Onglet analyses

     LabeledFrame4 <- tkwidget(Envir$trend,"labelframe",text="Diagnostics (optional)",padx=25,pady=10)
     tkgrid(LabeledFrame4, column=0, row=0, sticky="w")

     rb17 <- tkradiobutton(LabeledFrame4)                                                             # radio button du choix du diagnostic
     rb18 <- tkradiobutton(LabeledFrame4)
     rb19 <- tkradiobutton(LabeledFrame4)
     rb20 <- tkradiobutton(LabeledFrame4)
     rb21 <- tkradiobutton(LabeledFrame4)
     rb5Value <- tclVar("2")
     tkconfigure(rb17,variable=rb5Value,value="1")
     tkconfigure(rb18,variable=rb5Value,value="2")
     tkconfigure(rb19,variable=rb5Value,value="3")
     tkconfigure(rb20,variable=rb5Value,value="4")
     tkconfigure(rb21,variable=rb5Value,value="5")
     tkgrid(tklabel(LabeledFrame4,text="Spectrum analysis*    "),rb17, sticky="w")
     tkgrid(tklabel(LabeledFrame4,text="Autocorrelation    "),rb18, sticky="w")
     tkgrid(tklabel(LabeledFrame4,text="Shapiro normality test    "),rb19, sticky="w")
     tkgrid(tklabel(LabeledFrame4,text="Anomaly (color.plot)    "),rb20, sticky="w")
     tkgrid(tklabel(LabeledFrame4,text="Seasonal decomposition*   "),rb21, sticky="w")

#_______________________________________________________________________________bouton de diagnostic
     OnOK3 <- function()  {
            param <- Env2$variables[unique(Env2$variables.selectionnees.temp)]
            site <- Env$variables[unique(Env$variables.selectionnees.temp)]
            if (any(site == "-All-")) { site <- st  }
            if (length(param)==0 | length(site)==0 )
               { return(tkmessageBox(message="Please select a parameter and a station before to proceed"
                                    , icon = "warning", type = "ok", title="!Warning!")) }
            else{}
            cbVal <- as.character(tclvalue(cbValue))
           if (cbVal=="1"){ mix <- "YES" }
           else { mix <- "NO" }
           if (any(colnames(Envir$Data) == "S")) {
            Sal1 <- as.numeric(tclvalue(sal1))
            Sal2 <- as.numeric(tclvalue(sal2))
            sal <- c(Sal1, Sal2)}
           else{ sal <- "NULL" }
           if (any(colnames(Envir$Data) == "DEPTH")) {
            Depth1 <- as.numeric(tclvalue(depth1))
            Depth2 <- as.numeric(tclvalue(depth2))
            depth <- c(Depth1, Depth2) }
           else{ depth <- "NULL" }
           if (any(colnames(Envir$Data) == "DATES")) {
            start <- as.numeric(tclvalue(year1))
            end <- as.numeric(tclvalue(year2))
            months <- as.numeric(unlist(strsplit((tclvalue(mois)),"\\ "))) }
           else { tkmessageBox(message="no date selected", icon = "warning", type = "ok", title="!Warning!")}

          cb2Value <- as.character(tclvalue(cb2Value))
          if (cb2Value=="1"){ na.replace <- "YES" }
           else { na.replace <- "NO" }
          cb3Value <- as.character(tclvalue(cb3Value))
          if (cb3Value=="1"){ outliers.re <- "YES" }
           else { outliers.re <- "NO" }

          rb1Value <- as.character(tclvalue(rb1Value))
          if (rb1Value=="Annual"){ time.step <- "Annual" }
          if (rb1Value=="Mensual"){ time.step <- "Mensual" }
          if (rb1Value=="Fortnight"){ time.step <- "Fortnight" }
          if (rb1Value=="Semi-fortnight"){ time.step <- "Semi-fortnight" }
          if (rb1Value=="Mono-mensual"){ time.step <- "Mono-mensual" }
          if (rb1Value=="help"){ help.timestep <- "YES"
                                 time.step <- "NULL" }
          else{ help.timestep <- "N0" }
          if (rb1Value=="auto"){ auto.timestep <- "YES"
                                 time.step <- "NULL" }
          else{ auto.timestep <- "NO" }

          rb2Value <- as.character(tclvalue(rb2Value))
          if (rb2Value=="Mean"){ aggreg <- "Mean" }
          if (rb2Value=="Median"){ aggreg <- "Median" }
          if (rb2Value=="Quantile"){ aggreg <- "Quantile" }
          if (rb2Value=="Max"){ aggreg <- "Max" }
          if (rb2Value=="help"){ help.aggreg <- "YES"
                                 aggreg <- "NULL" }
          else{ help.aggreg <- "N0" }
          if (rb2Value=="auto"){ auto.aggreg <- "YES"
                                 aggreg <- "NULL" }
          else{ auto.aggreg <- "N0" }

          rb5Val <- as.character(tclvalue(rb5Value))
          if (rb5Val=="1"){ spectrum <- "YES" }
           else { spectrum <- "NO" }
          if (rb5Val=="2"){ autocorr <- "YES" }
           else { autocorr <- "NO" }
          if (rb5Val=="3"){ test.normality <- "YES" }
          else { test.normality <- "NO" }
          if (rb5Val=="4"){ anomaly <- "YES" }
          else { anomaly <- "NO" }
          if (rb5Val=="5"){ zsmooth <- "YES" }
          else { zsmooth <- "NO" }

          FULLoption(param, depth, sal, site, rawdata="NO", select="NO", resume.reg="NO",test.normality, 
                 plotB="NO", plotZ="NO", datashow="NO",
                 help.timestep, auto.timestep, time.step, help.aggreg, auto.aggreg, aggreg,
                 mix, outliers.re, na.replace, start, end, months, norm="NO", npsu,
                 autocorr, spectrum, anomaly, zsmooth, local.trend="NO", test="NO") }
     OK3.but <- tkbutton(LabeledFrame4, text="   Run   ",command=OnOK3)
     tkgrid(OK3.but, column=2, row=2)
#_______________________________________________________________________________fin du bouton
     
     tkgrid(tklabel(Envir$trend, text="  "), column=0, row=2)

     LabeledFrame5 <- tkwidget(Envir$trend,"labelframe",text="Trend Analyses",padx=25,pady=10)
     tkgrid(LabeledFrame5, column=0, row=3, sticky="w")

     cb10 <- tkcheckbutton(LabeledFrame5)                                                        # check button pour cusum
     cb10Value <- tclVar("0")
     tkconfigure(cb10,variable=cb10Value)
     tkgrid(tklabel(LabeledFrame5,text="Cumulative sum"), column=0, row=1, sticky="w")
     tkgrid(cb10, column=1, row=1)

     rb12 <- tkradiobutton(LabeledFrame5)                                                        # radio button du choix de l'analyse
     rb13 <- tkradiobutton(LabeledFrame5)
     rb14 <- tkradiobutton(LabeledFrame5)
     rb15 <- tkradiobutton(LabeledFrame5)
     rb16 <- tkradiobutton(LabeledFrame5)
     rb4Value <- tclVar("seasonMann")
     tkconfigure(rb12,variable=rb4Value,value="seasonMann")
     tkconfigure(rb13,variable=rb4Value,value="MannKen")
     tkconfigure(rb14,variable=rb4Value,value="MixingDiagram")
     tkconfigure(rb15,variable=rb4Value,value="Extended")
     tkconfigure(rb16,variable=rb4Value,value="Lowess")
     tkgrid(tklabel(LabeledFrame5,text="Seasonal Trend "),rb12, row=2, sticky="w")
     tkgrid(tklabel(LabeledFrame5,text="Global Trend "),rb13, row=3, sticky="w")
     tkgrid(tklabel(LabeledFrame5,text="Using Mixing Diagram"),rb14, row=6, sticky="w")
     tkgrid(tklabel(LabeledFrame5,text="Trend based on LOESS   "),rb16, row=4, sticky="w")

     Npsu <- tclVar(c(30))                                                                       # choix de la valeur de la salinite de standardisation
     entry.npsu <-tkentry(LabeledFrame5, width="4", textvariable=Npsu)
     tkgrid(tklabel(LabeledFrame5, text="      --> select psu"), row= 7, column=0, sticky="w")
     tkgrid(entry.npsu, row=7, column =0, sticky="e")

#_______________________________________________________________________________bouton d'appel de l'analyse temporelle
     OnOK4 <- function()  {
            param <- Env2$variables[unique(Env2$variables.selectionnees.temp)]
            site <- Env$variables[unique(Env$variables.selectionnees.temp)]
            if (any(site == "-All-")) { site <- st  }
            if (length(param)==0 | length(site)==0 )
               { return(tkmessageBox(message="Please select a parameter and a station before to proceed"
                                     , icon = "warning", type = "ok", title="!Warning!")) }
            else{}
            cbVal <- as.character(tclvalue(cbValue))
           if (cbVal=="1"){ mix <- "YES" }
           else { mix <- "NO" }
           if (any(colnames(Envir$Data) == "S")) {
            Sal1 <- as.numeric(tclvalue(sal1))
            Sal2 <- as.numeric(tclvalue(sal2))
            sal <- c(Sal1, Sal2)}
           else{ sal <- "NULL" }
           if (any(colnames(Envir$Data) == "DEPTH")) {
            Depth1 <- as.numeric(tclvalue(depth1))
            Depth2 <- as.numeric(tclvalue(depth2))
            depth <- c(Depth1, Depth2) }
           else{ depth <- "NULL" }
           if (any(colnames(Envir$Data) == "DATES")) {
            start <- as.numeric(tclvalue(year1))
            end <- as.numeric(tclvalue(year2))
            months <- as.numeric(unlist(strsplit((tclvalue(mois)),"\\ "))) }
           else { tkmessageBox(message="no date selected", icon = "warning", type = "ok", title="!Warning!")}

          cb2Value <- as.character(tclvalue(cb2Value))
          if (cb2Value=="1"){ na.replace <- "YES" }
           else { na.replace <- "NO" }
          cb3Value <- as.character(tclvalue(cb3Value))
          if (cb3Value=="1"){ outliers.re <- "YES" }
           else { outliers.re <- "NO" }

          rb1Value <- as.character(tclvalue(rb1Value))
          if (rb1Value=="Annual"){ time.step <- "Annual" }
          if (rb1Value=="Mensual"){ time.step <- "Mensual" }
          if (rb1Value=="Fortnight"){ time.step <- "Fortnight" }
          if (rb1Value=="Semi-fortnight"){ time.step <- "Semi-fortnight" }
          if (rb1Value=="Mono-mensual"){ time.step <- "Mono-mensual" }
          if (rb1Value=="help"){ help.timestep <- "YES"
                                 time.step <- "NULL" }
          else{ help.timestep <- "N0" }
          if (rb1Value=="auto"){ auto.timestep <- "YES"
                                 time.step <- "NULL" }
          else{ auto.timestep <- "NO" }

          rb2Value <- as.character(tclvalue(rb2Value))
          if (rb2Value=="Mean"){ aggreg <- "Mean" }
          if (rb2Value=="Median"){ aggreg <- "Median" }
          if (rb2Value=="Quantile"){ aggreg <- "Quantile" }
          if (rb2Value=="Max"){ aggreg <- "Max" }
          if (rb2Value=="help"){ help.aggreg <- "YES"
                                 aggreg <- "NULL" }
          else{ help.aggreg <- "N0" }
          if (rb2Value=="auto"){ auto.aggreg <- "YES"
                                 aggreg <- "NULL" }
          else{ auto.aggreg <- "N0" }

          cb10Value <- as.character(tclvalue(cb10Value))
          if (cb10Value=="1"){ local.trend <- "YES" }
           else { local.trend <- "NO" }

          rb4Value <- as.character(tclvalue(rb4Value))
          if (rb4Value=="seasonMann"){ test <- "SMK" }
          if (rb4Value=="MannKen"){ test <- "MK" }
          if (rb4Value=="Extended"){ test <- "ELM" }
          if (rb4Value=="Lowess"){ test <- "LOWESS" }
          if (rb4Value=="MixingDiagram"){ norm <- "YES"
                                          test <- "NO" }
          else {  norm <- "NULL" }
          npsu <- as.numeric(tclvalue(Npsu))
          FULLoption(param, depth, sal, site, rawdata="NO", select="NO", resume.reg="NO",test.normality="NO", 
                 plotB="NO", plotZ="NO", datashow="NO",
                 help.timestep, auto.timestep, time.step, help.aggreg, auto.aggreg, aggreg,
                 mix, outliers.re, na.replace, start, end, months, norm, npsu,
                 autocorr = "NO", spectrum="NO",anomaly="NO", zsmooth="NO", local.trend, test) }
     OK4.but <- tkbutton(LabeledFrame5, text="   Run   ",command=OnOK4)
     tkgrid(OK4.but, column=2, row=3)
#_______________________________________________________________________________fin du bouton      

     tkgrid(tklabel(Envir$trend, text="* Cannot be perform with missing values", font=tkfont.create(size=7)), column=0, row=4, sticky="w")
     tkgrid(tklabel(Envir$trend, text="      "), column=0, row=5)

     HELP4.but <- tkbutton(Envir$trend, text=" Help ",command=function() {
     browseURL(file.path(.path.package("TTAinterfaceTrendAnalysis"),"aide","HELP4.txt",fsep=.Platform$file.sep)) })
     tkgrid(HELP4.but, column=0, row=6, sticky="w")
}
