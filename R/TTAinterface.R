TTAinterface <- function() {
                                                                                                          # appel la fonction Lib.R

#_________________________________________________________________________________________________Creation de la fenetre principale (tt) et des onglets
 Envir$tt <- tktoplevel()                                                                                      # creation de la page principale
     tktitle(Envir$tt) <- paste("Temporal Trend Analysis interface (v", Envir$pversion, ")", sep="")           # on donne un nom visible à cette page
     tkwm.geometry(Envir$tt, "1200x700")                                                                       # taille de la fenetre principale
                                                                 
     Envir$topframe <- tkwidget(Envir$tt,"labelframe", text= "",padx=0,pady=0)                                 # cadre de texte pour afficher les 2 boutons
     tkconfigure(Envir$topframe, borderwidth=0)                                                                     
     tkpack(Envir$topframe, fill='both',expand=1, side="left")
     
     Envir$onglets <- tkwidget(Envir$topframe, "NoteBook")                                                     # on appel le widget onglet
     
     Envir$rawdata <- .Tk.newwin(tclvalue(tkinsert(Envir$onglets, "end", "1-Data_managment", "-text", "1-Data_managment")))               # creation de chaque onglet
     Envir$Select <- .Tk.newwin(tclvalue(tkinsert(Envir$onglets, "end", "2-Parameters_selection", "-text", "2-Parameters_selection")))
     Envir$datam <- .Tk.newwin(tclvalue(tkinsert(Envir$onglets, "end", "3-TimeSeries_building", "-text", "3-TimeSeries_building")))
     Envir$trend <- .Tk.newwin(tclvalue(tkinsert(Envir$onglets, "end", "4-Diagnostics/TrendAnalyses", "-text", "4-Diagnostics/TrendAnalyses")))
 
#_______________________________________________________________________________________________________________________________________Onglet Raw Data

 getfile <- function(inipath) {

     ExampleValue <- tclvalue(ExampleValue)
     if (ExampleValue=="1"){ inipath = file.path(path.package("TTAinterfaceTrendAnalysis"),"extdata","SRNDunkerque.txt", fsep=.Platform$file.sep )  }
     else { inipath = "C:/" }

     Envir$Name <- tclvalue(tkgetOpenFile(filetypes="{{TXT Files} {.txt}}", initialdir =inipath))              # le chemin du fichier est assigne a Name
     if (Envir$Name=="") { return() };                                                                         # si on ne selectionne rien on revient a la paga d'origine                                                                           
     Envir$Data <- read.table(Envir$Name, sep="\t", na.strings="", dec=".", header=T, check.names=FALSE)       # les donnees sont enregistre dans Data
      
     Envir$Name.split <- data.frame(strsplit(Envir$Name, "/"))                                                 # morcelle le chemin du fichier (Name)

     Envir$Namelength <- nchar(Envir$Name)
     Envir$FileLength <- nchar(as.character(Envir$Name.split[nrow(Envir$Name.split), ]))
     Envir$save.WD <- substr(Envir$Name, 1, Envir$Namelength-Envir$FileLength-1)                               # on ne garde que le chemin du fichier
     
     Envir$Name.split <- as.character(Envir$Name.split[length(Envir$Name.split[, 1]), ])                       # garde le dernier morceau (nom du fichier)
     Envir$File.Name <- data.frame(strsplit(Envir$Name.split, "\\."))                                          # on fait pareil pour retirer l'extension
     Envir$File.Name <- as.character(Envir$File.Name[1, ])

     Save.directory <- function() { selectdirectory() }                                                                 # appel la fonction selectdirectory.R
     save.select <- tkbutton(Envir$rawdata, text="Select your save directory", command=Save.directory, width=22)        # bouton select save path
     tkgrid(save.select, column=1, row=3, sticky="w")                                                         

     fixdata1 <- function() { fixdata( ) }                                                                     # appel la fonction fixdata.R
     imgFixdata <- tclVar()                                                                                                
     tcl("image","create","photo",imgFixdata,file=file.path(path.package("TTAinterfaceTrendAnalysis"),"aide","imgFixdata.gif",fsep=.Platform$file.sep))
     fixdata.but <- tkbutton(Envir$rawdata, image=imgFixdata, text=" Fix Data ", compound="right", command=fixdata1)    # bouton fixdata
     tkgrid(fixdata.but, column=1, row=7, sticky="w")

     showdata <- function()  { showData(Envir$Data) }                                                          # appel la fonction showData
     imgShowdata <- tclVar()                                                                                                
     tcl("image","create","photo",imgShowdata,file=file.path(path.package("TTAinterfaceTrendAnalysis"),"aide","imgShowdata.gif",fsep=.Platform$file.sep))
     showdata <- tkbutton(Envir$rawdata, image=imgShowdata, text= " Show Data ", compound="right",  height=18,  command=showdata)      # bouton showdata
     tkgrid(showdata, column=1, row=5, sticky="w")

     empty <- "                                                                                                                                 "

     Envir$Text <- tklabel(Envir$rawdata,text= paste("Current active file: ", Envir$Name.split, empty))        # texte montrant le fichier actif
     tkgrid(Envir$Text, column=1, row=2, sticky="w")
    
     tktitle(Envir$tt) <- paste("Temporal Trend Analysis interface (v", Envir$pversion, ") : ", Envir$Name.split, sep="")   # nom du fichier dans le top panel

     Envir$Text2 <- tklabel(Envir$rawdata,text= paste("Current save directory: ", Envir$save.WD, empty))       # texte montrant chemin de sauvegarde
     tkgrid(Envir$Text2, column=1, row=4, sticky="w")

     STAT1 <- function () { FULLoption( param, depth, sal, site, rawdata="YES")  }                             # bouton appelant l'argument rawdata 
     STAT1.but <- tkbutton(Envir$rawdata, text=" Summary ",command=STAT1, width=8)
     tkgrid(STAT1.but, column=1, row=6, sticky="w")
                                                                                                                  
     tkgrid(tklabel(Envir$rawdata, text="  "), column=1, row=8)                                                # lignes vides (espaces)
     tkgrid(tklabel(Envir$rawdata, text="  "), column=1, row=9)

     AdviceFrame <- tkwidget(Envir$rawdata,"labelframe",text="Important : how to well import your data in 9 steps",padx=30,pady=8)      # cadre de texte
     tkgrid(AdviceFrame, column=1, row=10, sticky="w")                                                                                  # du premier onglet
     tkgrid(tklabel(AdviceFrame, text="Your data must be in a .txt file (save as '.txt' format in your Spreadsheet)"), sticky="w")
     tkgrid(tklabel(AdviceFrame, text="Decimal separtor must be '.'"), sticky="w")
     tkgrid(tklabel(AdviceFrame, text="Missing value must be empty case"), sticky="w")
     tkgrid(tklabel(AdviceFrame, text="Dates must be in  'yyyy-mm-dd' (ISO 8601 time format)"), sticky="w")
     tkgrid(tklabel(AdviceFrame, text=" Dates column -> Dates"), sticky="w")
     tkgrid(tklabel(AdviceFrame, text=" Categorical factors column (Taxa, Stations...) -> Category"), sticky="w")
     tkgrid(tklabel(AdviceFrame, text=" Salinity column -> S"), sticky="w")
     tkgrid(tklabel(AdviceFrame, text=" Depth column -> Depth"), sticky="w")
     tkgrid(tklabel(AdviceFrame, text="If your parameters don't appear, select them as 'numeric' "), sticky="w")
     Adv1 <- tklabel(AdviceFrame, text="You can use the 'Fix Data' button to change column labels and data category")
     tkconfigure(Adv1, foreground="red")
     tkgrid(Adv1, sticky="w")
     
     tkgrid(tklabel(Envir$rawdata, text="      "), column=1, row=12)
     
     imgHelp <- tclVar()                                                                                                
     tcl("image","create","photo",imgHelp,file=file.path(path.package("TTAinterfaceTrendAnalysis"),"aide","imgHelp.gif",fsep=.Platform$file.sep))
     HELP1.but <- tkbutton(Envir$HelpFrame, image=imgHelp, text=" Help ", compound="right", command=function() { Aide1() })            # fichier txt a aller chercher
     tkgrid(HELP1.but, column=1, row=1, sticky="nw")
      
     #HELP1.but <- tkbutton(Envir$HelpFrame, image=imgHelp, text=" Help ", compound="right", command=function() {               # bouton d'aide n°1
     #browseURL(file.path(path.package("TTAinterfaceTrendAnalysis"),"aide","HELP1.txt",fsep=.Platform$file.sep)) })             # fichier txt a aller chercher
     #tkgrid(HELP1.but, column=1, row=1, sticky="nw")                                                                           # dans la dossier inst/aide

#_______________________________________________________________________________________________________________________Onglet Selection des parametres

     Env <- new.env()                                                                                         # cree un environnement pour les listes
     st <- levels(as.factor(Envir$Data$Category))                                                             # label des SERIES
     Env$variables <- c("-All-", st)                                                                          # on ajoute '-All-' a la liste
     Env$variables.selectionnees.temp <- integer()

     liste1 <- tklistbox(Envir$Select,selectmode="extended", activestyle="dotbox", height=10, width=25)       # cree la premiere liste
                for (i in 1:length(Env$variables)) { tkinsert(liste1,"end",Env$variables[i]) }                # rempli la premiere liste avec le nom des SERIES
     liste2 <- tklistbox(Envir$Select,selectmode="extended", activestyle="dotbox", height=10, width=25)       # cree la deuxieme liste (de selection)

     imgArrowright <- tclVar()                                                                                                
     tcl("image","create","photo",imgArrowright,file=file.path(path.package("TTAinterfaceTrendAnalysis"),"aide","imgArrowright.gif",fsep=.Platform$file.sep))
     imgArrowleft <- tclVar()                                                                                                
     tcl("image","create","photo",imgArrowleft,file=file.path(path.package("TTAinterfaceTrendAnalysis"),"aide","imgArrowleft.gif",fsep=.Platform$file.sep))
      
     bouton1 <- tkbutton(Envir$Select,image=imgArrowright,width=32,height=30,command=function() {                # bouton de selection
                if (tclvalue(tkcurselection(liste1))!="") {
                      val <- unlist(strsplit(tclvalue(tkcurselection(liste1)), "\\ "))                           # les SERIES selectionnees sont stockees dans 'val'
                      selection <- as.numeric(val)+1                                                             # puis sont transformees (+1 car la premiere valeur de la liste = 0)
                      for (i in min(selection):max(selection)) { tkinsert(liste2,"end",Env$variables[i]) }       # insertion des valeurs selectionnees dans la liste 2
                      Env$variables.selectionnees.temp <- c(Env$variables.selectionnees.temp, selection) }       # les valeurs finales selectionnees sont stockees
                else { tkmessageBox(message="No categorical factor selected !",type="ok",icon="info", title="!Warning!") }})
     bouton2 <- tkbutton(Envir$Select,image=imgArrowleft ,width=32,height=30,command=function() {                # bouton de deselection
                if (tclvalue(tkcurselection(liste2))!="") {
                      val <- unlist(strsplit(tclvalue(tkcurselection(liste2)), "\\ "))
                      selection <- as.numeric(val)+1
                      for (i in 1:length(selection)) { tkdelete(liste2, min(selection)-1) }
                      for (i in 1:length(selection)) { Env$variables.selectionnees.temp <- Env$variables.selectionnees.temp[-min(selection)]  }}
                else { tkmessageBox(message="No categorical factor selected !",type="ok",icon="info", title="!Warning!") } })

     tkgrid(tklabel(Envir$Select, text="Select your categorical factor(s)"), row=0, column=0,rowspan=2)                     # titres des listes
     tkgrid(tklabel(Envir$Select, text="Selected categorical factor(s)"), row=0, column=2,rowspan=2)                        # mise en page...
     tkgrid(liste1,row=2,column=0,rowspan=2)
     tkgrid(bouton1,row=2,column=1)
     tkgrid(bouton2,row=3,column=1)
     tkgrid(liste2,row=2,column=2,rowspan=2)

     #cb <- tkcheckbutton(Envir$Select)                                                # check case pour agreger les SERIES
     #cbValue <- tclVar("1")
     #tkconfigure(cb,variable=cbValue)
     #tkgrid(tklabel(Envir$Select,text="Aggregate SERIES ?"), column=0, row=4)
     #tkgrid(cb, column=1, row=4)
     tkgrid(tklabel(Envir$Select,text=" "), column=0, row=4)
     
     ListParam <- NULL                                                                # cree une liste vide a remplir avec les parametres present dans le csv
     for (i in 1:ncol(Envir$Data)) {                                                  # i entre 1 et le nbre de colonne
     if (class(Envir$Data[ ,i]) == "numeric") {                                       # pour les colonnes dans lesquelles les valeurs sont 'numeric'
     d <- names(Envir$Data[i])                                                        # on garde le nom de ces colonnes
     ListParam <- as.character(c(ListParam, d)) } }                                   # et on en fait la liste des parametres

     Env2 <- new.env()                                                                # on cree les listes comme pour les SERIES
     Env2$variables <- ListParam
     Env2$variables.selectionnees.temp <- integer()

     liste3 <- tklistbox(Envir$Select,selectmode="single", activestyle="dotbox", height=6, width=25)
               for (i in 1:length(Env2$variables)) { tkinsert(liste3,"end",Env2$variables[i]) }
     liste4 <- tklistbox(Envir$Select,selectmode="single", activestyle="dotbox", height=6, width=25)

     bouton3 <- tkbutton(Envir$Select,image=imgArrowright, width=32, height=81, command=function() {
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

     if (any(colnames(Envir$Data) == "Salinity") & any(!is.na(Envir$Data$Salinity))==TRUE) {                                                     # si une colonne S est presente
        if (class(Envir$Data$Salinity) == "numeric") {                                                                                    #      et si c'est bien une valeur numerique  :
          sal1 <- tclVar(min(Envir$Data$Salinity, na.rm=TRUE))                                                                            # prend la valeur mini de la salinite
          slider1 <- tkscale(Envir$Select, from=min(Envir$Data$Salinity, na.rm=TRUE), to=max(Envir$Data$Salinity, na.rm=TRUE),        # slider coulissant entre... 
                     length=100, showvalue=FALSE, variable=sal1,                                                                               #    ...les valeurs min et max de la colonne S
                     resolution=0.5, orient="horizontal", command=function(...) {
                     tkconfigure(Label1,text=paste(" Salinity min :", tclvalue(sal1), "psu ")) })                                  # titre des sliders
          Label1 <- tklabel(Envir$Select,text=paste(" Salinity min :",tclvalue(sal1),"psu "))
          tkgrid(Label1, column=0, row=16)
          tkgrid(slider1, column=0, row=17)

          sal2 <- tclVar(max(Envir$Data$Salinity, na.rm=TRUE))                                                                           # idem pour la valeur de salinite max
          slider2 <- tkscale(Envir$Select, from=min(Envir$Data$Salinity, na.rm=TRUE), to=max(Envir$Data$Salinity, na.rm=TRUE), 
                     length=100, showvalue=FALSE, variable=sal2,
                     resolution=0.5, orient="horizontal", command=function(...) {
                     tkconfigure(Label2,text=paste(" Salinity max :", tclvalue(sal2), "psu ")) })
          Label2 <- tklabel(Envir$Select,text=paste(" Salinity max :",tclvalue(sal2),"psu "))
          tkgrid(Label2, column=0, row=18)
          tkgrid(slider2, column=0, row=19) } 
     else{ tkmessageBox(message=paste("A salinity column is present but don't contain any 'numeric' values", "\n\n"   # message d'erreur si probleme de formattage...
                         , "You can fix it using the fix data button (use Help for more info)", sep="")               #   ...des donnees dans la colonne S
                         , icon = "warning", type = "ok", title="!Warning!") } }
     else{ tkgrid(tklabel(Envir$Select, text="                                  "), column=0, row=16)                 # s'il n'y a pas de colonne S
            tkgrid(tklabel(Envir$Select, text="No salinity values in "), column=0, row=17)
            tkgrid(tklabel(Envir$Select, text="     the data frame      "), column=0, row=18)
            tkgrid(tklabel(Envir$Select, text="                                  "), column=0, row=19) }

     if (any(colnames(Envir$Data) == "Depth") & any(!is.na(Envir$Data$Depth))==TRUE ) {                                                                      # idem pour la profondeur
       if (class(Envir$Data$Depth) == "numeric" ) {
          depth1 <- tclVar(min(Envir$Data$Depth, na.rm=TRUE))
          slider3 <- tkscale(Envir$Select, from=min(Envir$Data$Depth, na.rm=TRUE), to=max(Envir$Data$Depth, na.rm=TRUE), length=100,
                     showvalue=FALSE, variable=depth1,
                     resolution=0.5, orient="horizontal", command=function(...) {
                     tkconfigure(Label3,text=paste("Depth min :", tclvalue(depth1), "m")) })
          Label3 <- tklabel(Envir$Select,text=paste("Depth min :",tclvalue(depth1),"m"))
          tkgrid(Label3, column=0, row=20)
          tkgrid(slider3, column=0, row=21)

          depth2 <- tclVar(max(Envir$Data$Depth, na.rm=TRUE))
          slider4 <- tkscale(Envir$Select, from=min(Envir$Data$Depth, na.rm=TRUE), to=max(Envir$Data$Depth, na.rm=TRUE), length=100,
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

     if (any(colnames(Envir$Data) == "Dates")) {                                                                      # idem pour les annees

            if (is.numeric(Envir$Data$Dates) == TRUE ) { years <- Envir$Data$Dates } else {
            d <- as.Date(Envir$Data$Dates, format="%Y-%m-%d")                                                         # formattage de la date
            years <- as.numeric(format(d, format = "%Y")) }                                                            # on recupere les annees

            year1 <- tclVar(min(years, na.rm=TRUE))                                                                   # boite de texte avec fleches
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

           if (is.numeric(Envir$Data$Dates) ==TRUE) { mois <- NULL } else {
           
           m <- as.factor(format(d, format = "%m"))
           mois <- tclVar(as.numeric(levels(m)))                                                 # uniquement les mois present dans la base de donnees (separes par un espace)
           entry.mois <-tkentry(Envir$Select,width="21",textvariable=mois)
           tkgrid(tklabel(Envir$Select,text="Select month(s)"), column=2, row=21)
           tkgrid(entry.mois, column=2, row=22) } }

     else { emptybox10 <- tklabel(Envir$Select, text="!No date in your data!")                   # s'il n'y a pas de colonne Dates
            tkconfigure(emptybox10, foreground="red")
            tkgrid(emptybox10, column=2, row=16)
            tkgrid(tklabel(Envir$Select, text="                                    "), column=2, row=17)
            tkgrid(tklabel(Envir$Select, text="   Check if the column   "), column=2, row=18)
            tkgrid(tklabel(Envir$Select, text="  is correctely labelled   "), column=2, row=19)
            tkgrid(tklabel(Envir$Select, text="      as Dates      "), column=2, row=20)
            tkgrid(tklabel(Envir$Select, text="                                          "), column=2, row=21)
            tkgrid(tklabel(Envir$Select, text="                                          "), column=2, row=22)   }

#_______________________________________________________________________________bouton d'appel de la fonction 'select' dans FULLoption
     STAT2 <- function () {                                                                  # on rentre tout les arguments precedemment selectionne dans la fonction STAT2
            param <- Env2$variables[unique(Env2$variables.selectionnees.temp)]               # les parametres               
            site <- Env$variables[unique(Env$variables.selectionnees.temp)]                  # les SERIES                
            if (any(site == "-All-")) { site <- st  }                                        # si ALL est selectionnee
            if (length(param)==0 | length(site)==0 )
               { return(tkmessageBox(message="Please select a parameter and a station before to proceed"      # erreur si aucun argument n'est selectionnede
                                     , icon = "warning", type = "ok", title="!Warning!")) }
            else{}
          # cbVal <- as.character(tclvalue(cbValue))
          # if (cbVal=="1"){ mix <- "YES" }                                                   # agreger les SERIES
          # else { mix <- "NO" }
           mix <- "YES"
           if (any(colnames(Envir$Data) == "Salinity") & any(!is.na(Envir$Data$Salinity))==TRUE) {          # valeurs de salinites
            Sal1 <- as.numeric(tclvalue(sal1))
            Sal2 <- as.numeric(tclvalue(sal2))
            sal <- c(Sal1, Sal2) }
           else{ sal <- "NULL" }
           if (any(colnames(Envir$Data) == "Depth") & any(!is.na(Envir$Data$Depth)) == TRUE) {   # valeurs de profondeurs
            Depth1 <- as.numeric(tclvalue(depth1))
            Depth2 <- as.numeric(tclvalue(depth2))
            depth <- c(Depth1, Depth2) }
           else{ depth <- "NULL" }
           if (any(colnames(Envir$Data) == "Dates")) {                                       
            start <- as.numeric(tclvalue(year1))                                             # annee de debut
            end <- as.numeric(tclvalue(year2))                                               # annee de fin
            if (is.numeric(Envir$Data$Dates) ==TRUE) { } else {
            months <- as.numeric(unlist(strsplit((tclvalue(mois)),"\\ "))) } }                # mois
           else { tkmessageBox(message="No date selected!", icon = "warning", type = "ok", title="!Warning!")}        # erreur si pas de dates
           FULLoption(param, depth, sal, site, rawdata="NO", select="YES", resume.reg="NO", test.normality="NO",      # applique la fonction FULLoption...
                     plotB="NO", selectBox, log.trans="NO", plotZ="NO", datashow="NO",                                #    ...avec les arguments
                     help.timestep="NO", auto.timestep="NO", time.step="NULL", help.aggreg="NO", auto.aggreg="NO", aggreg="NULL",
                     mix, outliers.re="NO", na.replace="NO", start, end, months)  }                                   
     STAT2.but <- tkbutton(Envir$Select, text="Summary",command=STAT2)                                                # bouton d'appel de la fonction STAT2
     tkgrid(STAT2.but, column=0, row=24)
#_______________________________________________________________________________fin du bouton

     tkgrid(tklabel(Envir$Select, text="      "), column=0, row=25)

     HELP2.but <- tkbutton(Envir$Select, image=imgHelp, text=" Help ", compound="right",command=function() { Aide2() } )     # bouton d'aide n°2  
     tkgrid(HELP2.but, column=0, row=26, sticky="w")
     
     #HELP2.but <- tkbutton(Envir$Select, image=imgHelp, text=" Help ", compound="right",command=function() {                # bouton d'aide n°2
     #browseURL(file.path(path.package("TTAinterfaceTrendAnalysis"),"aide","HELP2.txt",fsep=.Platform$file.sep)) }) 
     #tkgrid(HELP2.but, column=0, row=26, sticky="w")

#_________________________________________________________________________________________________________________________________Onglet regularisation

     LabeledFrame1 <- tkwidget(Envir$datam,"labelframe",text="Data interactions",padx=25,pady=10)      # cadre
     tkgrid(LabeledFrame1, column=0, row=0, sticky="w")
                 
         cb7 <- tkcheckbutton(LabeledFrame1)                                                          # check button pour missing values
         cb7Value <- tclVar("0")
         tkconfigure(cb7,variable=cb7Value)
         tkgrid(tklabel(LabeledFrame1,text="Log10(x+1) transform"), column=0, row=0, sticky="w")
         tkgrid(cb7, column=1, row=0)
         
         cb2 <- tkcheckbutton(LabeledFrame1)                                                          # check button pour missing values
         cb2Value <- tclVar("0")
         tkconfigure(cb2,variable=cb2Value)
         tkgrid(tklabel(LabeledFrame1,text="Replace missing values ?"), column=0, row=1, sticky="w")
         tkgrid(cb2, column=1, row=1)

         cb3 <- tkcheckbutton(LabeledFrame1)                                                           # check button pour outliers
         cb3Value <- tclVar("0")
         tkconfigure(cb3,variable=cb3Value)
         tkgrid(tklabel(LabeledFrame1,text="Remove outliers ?"), column=0, row=2, sticky="w")
         tkgrid(cb3, column=1, row=2)
         
         selectBox<-NULL
         rb21 <- tkradiobutton(LabeledFrame1)                                                     
         rb22 <- tkradiobutton(LabeledFrame1)
         rb20Value <- tclVar("ByYears")
         tkconfigure(rb21,variable=rb20Value,value="ByYears")                                       
         tkconfigure(rb22,variable=rb20Value,value="ByMonths")
         tkgrid(tklabel(LabeledFrame1,text="   |->By years "),rb21, row = 4, sticky="w")                            
         tkgrid(tklabel(LabeledFrame1,text="   |->By months "),rb22, row= 5, sticky="w")
         
#_______________________________________________________________________________bouton d'appel de la boxplot (argument plotB)
     BoxPlot <- function()  {
            param <- Env2$variables[unique(Env2$variables.selectionnees.temp)]
            site <- Env$variables[unique(Env$variables.selectionnees.temp)]
            if (any(site == "-All-")) { site <- st  }
            if (length(param)==0 | length(site)==0 )
               { return(tkmessageBox(message="Please select a parameter and a station before to proceed"
                                    , icon = "warning", type = "ok", title="!Warning!")) }
            else{}
           # cbVal <- as.character(tclvalue(cbValue))
           #if (cbVal=="1"){ mix <- "YES" }
           #else { mix <- "NO" }
           mix <- "YES"
           if (any(colnames(Envir$Data) == "Salinity") & any(!is.na(Envir$Data$Salinity))==TRUE) {
            Sal1 <- as.numeric(tclvalue(sal1))
            Sal2 <- as.numeric(tclvalue(sal2))
            sal <- c(Sal1, Sal2)}
           else{ sal <- "NULL" }
           if (any(colnames(Envir$Data) == "Depth") & any(!is.na(Envir$Data$Depth)) == TRUE) {
            Depth1 <- as.numeric(tclvalue(depth1))
            Depth2 <- as.numeric(tclvalue(depth2))
            depth <- c(Depth1, Depth2) }
           else{ depth <- "NULL" }
           if (any(colnames(Envir$Data) == "Dates")) {
            start <- as.numeric(tclvalue(year1))
            end <- as.numeric(tclvalue(year2))
            if (is.numeric(Envir$Data$Dates) ==TRUE) { } else {
            months <- as.numeric(unlist(strsplit((tclvalue(mois)),"\\ "))) }  }
           else { tkmessageBox(message="no date selected", icon = "warning", type = "ok", title="!Warning!")}
           rb20Value <- as.character(tclvalue(rb20Value))                           
           if (rb20Value=="ByYears"){ selectBox <- "ByYears" }                       # valeur de l'argument time.step pour chaque bouton
           if (rb20Value=="ByMonths"){ selectBox <- "ByMonths" }
           FULLoption(param, depth, sal, site, rawdata="NO", select="NO", resume.reg="NO",test.normality="NO",
                     plotB="YES", selectBox, log.trans="NO", plotZ="NO", datashow="NO",
                     help.timestep="NO", auto.timestep="NO", time.step="NULL", help.aggreg="NO", auto.aggreg="NO", aggreg="NULL",
                     mix, outliers.re="NO", na.replace="NO", start, end, months) }
     BoxPlot.but <- tkbutton(LabeledFrame1, text="Show boxplot",command=BoxPlot)
     tkgrid(BoxPlot.but, column=0, row=3, sticky="w")
#_______________________________________________________________________________fin du bouton

     tkgrid(tklabel(Envir$datam, text="              "), column=0, row=1)

     LabeledFrame2 <- tkwidget(Envir$datam,"labelframe",text="Select the data frequency in your final time series",padx=25,pady=10)
     tkgrid(LabeledFrame2, column=0, row=2, sticky="w")
     
         rb1 <- tkradiobutton(LabeledFrame2)                                                     # radio buttons pour le time step
         rb2 <- tkradiobutton(LabeledFrame2)
         rb3 <- tkradiobutton(LabeledFrame2)
         rb4 <- tkradiobutton(LabeledFrame2)
         rb5 <- tkradiobutton(LabeledFrame2)
         rb6 <- tkradiobutton(LabeledFrame2)
         rb7 <- tkradiobutton(LabeledFrame2)
         rb8 <- tkradiobutton(LabeledFrame2) 
         if (is.numeric(Envir$Data$Dates) ==TRUE) { rb1Value <- tclVar("Annual") } else {
         rb1Value <- tclVar("auto") }
         tkconfigure(rb1,variable=rb1Value,value="Annual")                                       # valeur de l'argument pour chaque bouton
         tkconfigure(rb2,variable=rb1Value,value="Monthly")
         tkconfigure(rb3,variable=rb1Value,value="Fortnight")
         tkconfigure(rb4,variable=rb1Value,value="Semi-fortnight")
         tkconfigure(rb5,variable=rb1Value,value="Mono-mensual")
         tkconfigure(rb6,variable=rb1Value,value="Daily")
         tkconfigure(rb7,variable=rb1Value,value="help")
         tkconfigure(rb8,variable=rb1Value,value="auto")
         tkgrid(tklabel(LabeledFrame2,text="Daily "),rb6, sticky="w")                            # texte afficher a cote de chaque bouton
         tkgrid(tklabel(LabeledFrame2,text="Semi-fortnightly "),rb4, sticky="w")
         tkgrid(tklabel(LabeledFrame2,text="Fortnightly "),rb3, sticky="w")
         tkgrid(tklabel(LabeledFrame2,text="Monthly "),rb2, sticky="w")
         tkgrid(tklabel(LabeledFrame2,text="Yearly "),rb1, sticky="w")                           
         tkgrid(tklabel(LabeledFrame2,text="Monomensualy (see help)"),rb5, sticky="w")
         tkgrid(tklabel(LabeledFrame2,text="Guidance to choose the frequency "),rb7, sticky="w")
         tkgrid(tklabel(LabeledFrame2,text="Auto "),rb8, sticky="w") 

     tkgrid(tklabel(Envir$datam, text="              "), column=0, row=3)

     LabeledFrame3 <- tkwidget(Envir$datam,"labelframe",text="Select the method to aggregate your data",padx=25,pady=10)
     tkgrid(LabeledFrame3, column=0, row=4, sticky="w")

         rb1 <- tkradiobutton(LabeledFrame3)                                                     # radio buttons pour la methode mathematique
         rb2 <- tkradiobutton(LabeledFrame3)
         rb3 <- tkradiobutton(LabeledFrame3)
         rb4 <- tkradiobutton(LabeledFrame3)
         rb5 <- tkradiobutton(LabeledFrame3)
         rb6 <- tkradiobutton(LabeledFrame3)
         if (is.numeric(Envir$Data$Dates) ==TRUE) { rb2Value <- tclVar("Mean") } else {
         rb2Value <- tclVar("auto") }
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
         tkgrid(tklabel(LabeledFrame3,text="Guidance to choose the method      "),rb5, sticky="w")
         tkgrid(tklabel(LabeledFrame3,text="Auto "),rb6, sticky="w") 

         tkgrid(tklabel(Envir$datam, text="              "), column=2, row=2)
         
     LabeledFrame7 <- tkwidget(Envir$datam,"labelframe",text="Show regularised time series",padx=10,pady=0)
     tkgrid(LabeledFrame7, column=3, row=2, sticky="n")
     tkgrid(tklabel(LabeledFrame7, text="                                                                                                                                                                            "
                                 , font=tkfont.create(size=1)), column=0, row=0)
     tkgrid(tklabel(LabeledFrame7, text=" ", font=tkfont.create(size=1)), column=0, row=2)

#_______________________________________________________________________________bouton d'appel de la figure de la serie regularisee (plotZ)
     OnOK <- function()  {
            param <- Env2$variables[unique(Env2$variables.selectionnees.temp)]
            site <- Env$variables[unique(Env$variables.selectionnees.temp)]
            if (any(site == "-All-")) { site <- st  }
            if (length(param)==0 | length(site)==0 )
               { return(tkmessageBox(message="Please select a parameter and a station before to proceed"
                                    , icon = "warning", type = "ok", title="!Warning!")) }
            else{}
           # cbVal <- as.character(tclvalue(cbValue))
           #if (cbVal=="1"){ mix <- "YES" }
           #else { mix <- "NO" }
           mix <- "YES"
           if (any(colnames(Envir$Data) == "Salinity") & any(!is.na(Envir$Data$Salinity))==TRUE) {
            Sal1 <- as.numeric(tclvalue(sal1))
            Sal2 <- as.numeric(tclvalue(sal2))
            sal <- c(Sal1, Sal2)}
           else{ sal <- "NULL" }
           if (any(colnames(Envir$Data) == "Depth") & any(!is.na(Envir$Data$Depth)) == TRUE) {
            Depth1 <- as.numeric(tclvalue(depth1))
            Depth2 <- as.numeric(tclvalue(depth2))
            depth <- c(Depth1, Depth2) }
           else{ depth <- "NULL" }
           if (any(colnames(Envir$Data) == "Dates")) {
            start <- as.numeric(tclvalue(year1))
            end <- as.numeric(tclvalue(year2))
            if (is.numeric(Envir$Data$Dates) ==TRUE) { } else {
            months <- as.numeric(unlist(strsplit((tclvalue(mois)),"\\ "))) } }
           else { tkmessageBox(message="no date selected", icon = "warning", type = "ok", title="!Warning!")}

          cb7Value <- as.character(tclvalue(cb7Value))                           # valeur de l'argument log transform
          if (cb7Value=="1"){ log.trans <- "YES" }
           else { log.trans <- "NO" }
          cb2Value <- as.character(tclvalue(cb2Value))                           # valeur de l'argument na.replace
          if (cb2Value=="1"){ na.replace <- "YES" }
           else { na.replace <- "NO" }
          cb3Value <- as.character(tclvalue(cb3Value))                           # valeur de l'argument outliers.re
          if (cb3Value=="1"){ outliers.re <- "YES" }
           else { outliers.re <- "NO" }

          rb1Value <- as.character(tclvalue(rb1Value))                           
          if (rb1Value=="Annual"){ time.step <- "Annual" }                       # valeur de l'argument time.step pour chaque bouton
          if (rb1Value=="Monthly"){ time.step <- "Monthly" }
          if (rb1Value=="Fortnight"){ time.step <- "Fortnight" }
          if (rb1Value=="Semi-fortnight"){ time.step <- "Semi-fortnight" }
          if (rb1Value=="Mono-mensual"){ time.step <- "Mono-mensual" }
          if (rb1Value=="Daily"){ time.step <- "Daily" }
          if (rb1Value=="help"){ help.timestep <- "YES"
                                 time.step <- "NULL" }
          else{ help.timestep <- "N0" }
          if (rb1Value=="auto"){ auto.timestep <- "YES"
                                 time.step <- "NULL" }
          else{ auto.timestep <- "NO" }

          rb2Value <- as.character(tclvalue(rb2Value))
          if (rb2Value=="Mean"){ aggreg <- "Mean" }                              # valeur de l'argument aggreg pour chauqe bouton
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
                 plotB="NO", selectBox, log.trans, plotZ="YES", datashow="NO",
                 help.timestep, auto.timestep, time.step, help.aggreg, auto.aggreg, aggreg,
                 mix, outliers.re, na.replace, start, end, months, norm="NO", npsu,
                 autocorr = "NO", spectrum="NO", anomaly="NO", a.barplot="NO", zsmooth="NO", local.trend="NO", test="NO") }
     
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
           # cbVal <- as.character(tclvalue(cbValue))
           #if (cbVal=="1"){ mix <- "YES" }
           #else { mix <- "NO" }
           mix <- "YES"
           if (any(colnames(Envir$Data) == "Salinity") & any(!is.na(Envir$Data$Salinity))==TRUE) {
            Sal1 <- as.numeric(tclvalue(sal1))
            Sal2 <- as.numeric(tclvalue(sal2))
            sal <- c(Sal1, Sal2)}
           else{ sal <- "NULL" }
           if (any(colnames(Envir$Data) == "Depth") & any(!is.na(Envir$Data$Depth)) == TRUE) {
            Depth1 <- as.numeric(tclvalue(depth1))
            Depth2 <- as.numeric(tclvalue(depth2))
            depth <- c(Depth1, Depth2) }
           else{ depth <- "NULL" }
           if (any(colnames(Envir$Data) == "Dates")) {
            start <- as.numeric(tclvalue(year1))
            end <- as.numeric(tclvalue(year2))
            if (is.numeric(Envir$Data$Dates) ==TRUE) { } else {
            months <- as.numeric(unlist(strsplit((tclvalue(mois)),"\\ "))) } }
           else { tkmessageBox(message="no date selected", icon = "warning", type = "ok", title="!Warning!")}
          
          cb7Value <- as.character(tclvalue(cb7Value))                           
          if (cb7Value=="1"){ log.trans <- "YES" }
           else { log.trans <- "NO" }
          cb2Value <- as.character(tclvalue(cb2Value))
          if (cb2Value=="1"){ na.replace <- "YES" }
           else { na.replace <- "NO" }
          cb3Value <- as.character(tclvalue(cb3Value))
          if (cb3Value=="1"){ outliers.re <- "YES" }
           else { outliers.re <- "NO" }

          rb1Value <- as.character(tclvalue(rb1Value))
          if (rb1Value=="Annual"){ time.step <- "Annual" }
          if (rb1Value=="Monthly"){ time.step <- "Monthly" }
          if (rb1Value=="Fortnight"){ time.step <- "Fortnight" }
          if (rb1Value=="Semi-fortnight"){ time.step <- "Semi-fortnight" }
          if (rb1Value=="Mono-mensual"){ time.step <- "Mono-mensual" }
          if (rb1Value=="Daily"){ time.step <- "Daily" }
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
                 plotB="NO", selectBox, log.trans, plotZ="NO", datashow="YES",
                 help.timestep, auto.timestep, time.step, help.aggreg, auto.aggreg, aggreg,
                 mix, outliers.re, na.replace, start, end, months, norm="NO", npsu,
                 autocorr = "NO", spectrum="NO", anomaly="NO", a.barplot="NO", zsmooth="NO", local.trend="NO", test="NO") }
     OK1.but <- tkbutton(LabeledFrame7, text=" Table ",command=OnOK1)
     tkgrid(OK1.but, column=0, row=1)

#_______________________________________________________________________________bouton d'appel des stats desciptives sur donnees regularisees
     OnOK2 <- function()  {
            param <- Env2$variables[unique(Env2$variables.selectionnees.temp)]
            site <- Env$variables[unique(Env$variables.selectionnees.temp)]
            if (any(site == "-All-")) { site <- st  }
            if (length(param)==0 | length(site)==0 )
               { return(tkmessageBox(message="Please select a parameter and a station before to proceed"
                                    , icon = "warning", type = "ok", title="!Warning!")) }
            else{}
           # cbVal <- as.character(tclvalue(cbValue))
           #if (cbVal=="1"){ mix <- "YES" }
           #else { mix <- "NO" }
           mix <- "YES"
           if (any(colnames(Envir$Data) == "Salinity") & any(!is.na(Envir$Data$Salinity))==TRUE) {
            Sal1 <- as.numeric(tclvalue(sal1))
            Sal2 <- as.numeric(tclvalue(sal2))
            sal <- c(Sal1, Sal2)}
           else{ sal <- "NULL" }
           if (any(colnames(Envir$Data) == "Depth") & any(!is.na(Envir$Data$Depth)) == TRUE) {
            Depth1 <- as.numeric(tclvalue(depth1))
            Depth2 <- as.numeric(tclvalue(depth2))
            depth <- c(Depth1, Depth2) }
           else{ depth <- "NULL" }
           if (any(colnames(Envir$Data) == "Dates")) {
            start <- as.numeric(tclvalue(year1))
            end <- as.numeric(tclvalue(year2))
            if (is.numeric(Envir$Data$Dates) ==TRUE) { } else {
            months <- as.numeric(unlist(strsplit((tclvalue(mois)),"\\ "))) } }
           else { tkmessageBox(message="no date selected", icon = "warning", type = "ok", title="!Warning!")}
          
          cb7Value <- as.character(tclvalue(cb7Value))                           
          if (cb7Value=="1"){ log.trans <- "YES" }
           else { log.trans <- "NO" }
          cb2Value <- as.character(tclvalue(cb2Value))
          if (cb2Value=="1"){ na.replace <- "YES" }
           else { na.replace <- "NO" }
          cb3Value <- as.character(tclvalue(cb3Value))
          if (cb3Value=="1"){ outliers.re <- "YES" }
           else { outliers.re <- "NO" }

          rb1Value <- as.character(tclvalue(rb1Value))
          if (rb1Value=="Annual"){ time.step <- "Annual" }
          if (rb1Value=="Monthly"){ time.step <- "Monthly" }
          if (rb1Value=="Fortnight"){ time.step <- "Fortnight" }
          if (rb1Value=="Semi-fortnight"){ time.step <- "Semi-fortnight" }
          if (rb1Value=="Mono-mensual"){ time.step <- "Mono-mensual" }
          if (rb1Value=="Daily"){ time.step <- "Daily" }
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
                 plotB="NO", selectBox, log.trans, plotZ="NO", datashow="NO",
                 help.timestep, auto.timestep, time.step, help.aggreg, auto.aggreg, aggreg,
                 mix, outliers.re, na.replace, start, end, months, norm="NO", npsu,
                 autocorr = "NO", spectrum="NO", anomaly="NO", a.barplot="NO", zsmooth="NO", local.trend="NO", test="NO")   }
     OK2.but <- tkbutton(LabeledFrame7, text=" Summary ",command=OnOK2)
     tkgrid(OK2.but, column=0, row=1, sticky="e")
#_______________________________________________________________________________fin du bouton      

     tkgrid(tklabel(Envir$datam, text="      "), column=0, row=5)

     HELP3.but <- tkbutton(Envir$datam, image=imgHelp, text=" Help ", compound="right", command=function() { Aide3() })       # bouton d'aide n°3
     tkgrid(HELP3.but, column=0, row=6, sticky="w")
     
     #HELP3.but <- tkbutton(Envir$datam, image=imgHelp, text=" Help ", compound="right", command=function() {                 # bouton d'aide n°3
     #browseURL(file.path(path.package("TTAinterfaceTrendAnalysis"),"aide","HELP3.txt",fsep=.Platform$file.sep)) })
     #tkgrid(HELP3.but, column=0, row=6, sticky="w")

#_______________________________________________________________________________________________________________________________________Onglet analyses

     LabeledFrame4 <- tkwidget(Envir$trend,"labelframe",text="Diagnostics (optional)",padx=25,pady=10)
     tkgrid(LabeledFrame4, column=0, row=0, sticky="w")
     
     diagFrame <- tkwidget(LabeledFrame4,"labelframe",padx=0,pady=0)
     tkconfigure(diagFrame, borderwidth=0)
     tkpack(diagFrame, side="left")

     rb17 <- tkradiobutton(diagFrame)                                                             # radio bouton du choix du diagnostic
     rb18 <- tkradiobutton(diagFrame)
     rb19 <- tkradiobutton(diagFrame)
     rb20 <- tkradiobutton(diagFrame)
     rb21 <- tkradiobutton(diagFrame)
     rb22 <- tkradiobutton(diagFrame)
     rb5Value <- tclVar("2")
     tkconfigure(rb17,variable=rb5Value,value="1")                                                    # valeur donnee a chaque bouton
     tkconfigure(rb18,variable=rb5Value,value="2")
     tkconfigure(rb19,variable=rb5Value,value="3")
     tkconfigure(rb20,variable=rb5Value,value="4")
     tkconfigure(rb22,variable=rb5Value,value="6")
     tkconfigure(rb21,variable=rb5Value,value="5")
     tkgrid(tklabel(diagFrame,text="Spectrum analysis*    "),rb17, sticky="w")                    # texte afficher avec chaque bouton
     tkgrid(tklabel(diagFrame,text="Autocorrelation    "),rb18, sticky="w")
     tkgrid(tklabel(diagFrame,text="Shapiro normality test    "),rb19, sticky="w")
     tkgrid(tklabel(diagFrame,text="Anomaly (color.plot)    "),rb20, sticky="w")
     tkgrid(tklabel(diagFrame,text="Anomaly (barplot)    "),rb22, sticky="w")
     tkgrid(tklabel(diagFrame,text="Seasonal decomposition*  "),rb21, sticky="w")
      
     imgProcess <- tclVar()                                                                                                
     tcl("image","create","photo",imgProcess,file=file.path(path.package("TTAinterfaceTrendAnalysis"),"aide","imgProcess.gif",fsep=.Platform$file.sep))

#_______________________________________________________________________________bouton de diagnostic
     OnOK3 <- function()  {
            param <- Env2$variables[unique(Env2$variables.selectionnees.temp)]
            site <- Env$variables[unique(Env$variables.selectionnees.temp)]
            if (any(site == "-All-")) { site <- st  }
            if (length(param)==0 | length(site)==0 )
               { return(tkmessageBox(message="Please select a parameter and a station before to proceed"
                                    , icon = "warning", type = "ok", title="!Warning!")) }
            else{}
           mix <- "YES"
           if (any(colnames(Envir$Data) == "Salinity") & any(!is.na(Envir$Data$Salinity))==TRUE) {
            Sal1 <- as.numeric(tclvalue(sal1))
            Sal2 <- as.numeric(tclvalue(sal2))
            sal <- c(Sal1, Sal2)}
           else{ sal <- "NULL" }
           if (any(colnames(Envir$Data) == "Depth") & any(!is.na(Envir$Data$Depth)) == TRUE) {
            Depth1 <- as.numeric(tclvalue(depth1))
            Depth2 <- as.numeric(tclvalue(depth2))
            depth <- c(Depth1, Depth2) }
           else{ depth <- "NULL" }
           if (any(colnames(Envir$Data) == "Dates")) {
            start <- as.numeric(tclvalue(year1))
            end <- as.numeric(tclvalue(year2))
            if (is.numeric(Envir$Data$Dates) ==TRUE) { } else {
            months <- as.numeric(unlist(strsplit((tclvalue(mois)),"\\ "))) } }
           else { tkmessageBox(message="no date selected", icon = "warning", type = "ok", title="!Warning!")}
 
          cb7Value <- as.character(tclvalue(cb7Value))                           
          if (cb7Value=="1"){ log.trans <- "YES" }
           else { log.trans <- "NO" }
          cb2Value <- as.character(tclvalue(cb2Value))
          if (cb2Value=="1"){ na.replace <- "YES" }
           else { na.replace <- "NO" }
          cb3Value <- as.character(tclvalue(cb3Value))
          if (cb3Value=="1"){ outliers.re <- "YES" }
           else { outliers.re <- "NO" }

          rb1Value <- as.character(tclvalue(rb1Value))
          if (rb1Value=="Annual"){ time.step <- "Annual" }
          if (rb1Value=="Monthly"){ time.step <- "Monthly" }
          if (rb1Value=="Fortnight"){ time.step <- "Fortnight" }
          if (rb1Value=="Semi-fortnight"){ time.step <- "Semi-fortnight" }
          if (rb1Value=="Mono-mensual"){ time.step <- "Mono-mensual" }
          if (rb1Value=="Daily"){ time.step <- "Daily" }
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

          rb5Val <- as.character(tclvalue(rb5Value))                             # valeur des arguments pour le bouton correspondant
          if (rb5Val=="1"){ spectrum <- "YES" }
           else { spectrum <- "NO" }
          if (rb5Val=="2"){ autocorr <- "YES" }
           else { autocorr <- "NO" }
          if (rb5Val=="3"){ test.normality <- "YES" }
          else { test.normality <- "NO" }
          if (rb5Val=="4"){ anomaly <- "YES" }
          else { anomaly <- "NO" }
          if (rb5Val=="6"){ a.barplot <- "YES" }
          else { a.barplot <- "NO" }
          if (rb5Val=="5"){ zsmooth <- "YES" }
          else { zsmooth <- "NO" }

          FULLoption(param, depth, sal, site, rawdata="NO", select="NO", resume.reg="NO",test.normality, 
                 plotB="NO", selectBox, log.trans, plotZ="NO", datashow="NO",
                 help.timestep, auto.timestep, time.step, help.aggreg, auto.aggreg, aggreg,
                 mix, outliers.re, na.replace, start, end, months, norm="NO", npsu,
                 autocorr, spectrum, anomaly, a.barplot, zsmooth, local.trend="NO", test="NO") }
     OK3.but <- tkbutton(LabeledFrame4, image=imgProcess, text=" Run ", compound="right", command=OnOK3)
     tkpack(OK3.but, side="right")
#_______________________________________________________________________________fin du bouton
     
     tkgrid(tklabel(Envir$trend, text="* Cannot be perform with missing values", font=tkfont.create(size=7)), column=0, row=2, sticky="w")
     tkgrid(tklabel(Envir$trend, text="      "), column=0, row=3)
     
     LabeledFrame5 <- tkwidget(Envir$trend,"labelframe",text="Trend Analyses",padx=25,pady=10)
     tkgrid(LabeledFrame5, column=0, row=4, sticky="w")
     
     testFrame <- tkwidget(LabeledFrame5,"labelframe",padx=0,pady=0)
     tkconfigure(testFrame, borderwidth=0)
     tkpack(testFrame, side="left")
     
     cb10 <- tkcheckbutton(testFrame)                                                        # check button pour cusum
     cb10Value <- tclVar("0")
     tkconfigure(cb10,variable=cb10Value)
     tkgrid(tklabel(testFrame,text="Cumulative sum*"), column=0, row=1, sticky="w")
     tkgrid(cb10, column=1, row=1)

     rb12 <- tkradiobutton(testFrame)                                                        # radio button du choix de l'analyse
     rb13 <- tkradiobutton(testFrame)
     rb14 <- tkradiobutton(testFrame)
     rb15 <- tkradiobutton(testFrame)
     rb16 <- tkradiobutton(testFrame)
     if (is.numeric(Envir$Data$Dates) ==TRUE) { rb4Value <- tclVar("MannKen") } else {
     rb4Value <- tclVar("seasonMann") }
     tkconfigure(rb12,variable=rb4Value,value="seasonMann")                                      # valeur de chaque bouton
     tkconfigure(rb13,variable=rb4Value,value="MannKen")
     tkconfigure(rb14,variable=rb4Value,value="MixingDiagram")
     tkconfigure(rb15,variable=rb4Value,value="Extended")
     tkconfigure(rb16,variable=rb4Value,value="Lowess")
     tkgrid(tklabel(testFrame,text="Seasonal Trend "),rb12, row=2, sticky="w")               # texte affiche
     tkgrid(tklabel(testFrame,text="Global Trend "),rb13, row=3, sticky="w")
     tkgrid(tklabel(testFrame,text="Using Mixing Diagram"),rb14, row=6, sticky="w")
     tkgrid(tklabel(testFrame,text="Trend based on LOESS   "),rb16, row=4, sticky="w")

     Npsu <- tclVar(c(30))                                                                       # choix de la valeur de la salinite de standardisation
     entry.npsu <-tkentry(testFrame, width="4", textvariable=Npsu)                           # (30 par defaut)
     tkgrid(tklabel(testFrame, text="      --> select psu"), row= 7, column=0, sticky="w")
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
           # cbVal <- as.character(tclvalue(cbValue))
           #if (cbVal=="1"){ mix <- "YES" }
           #else { mix <- "NO" }
           mix <- "YES"
           if (any(colnames(Envir$Data) == "Salinity") & any(!is.na(Envir$Data$Salinity))==TRUE) {
            Sal1 <- as.numeric(tclvalue(sal1))
            Sal2 <- as.numeric(tclvalue(sal2))
            sal <- c(Sal1, Sal2)}
           else{ sal <- "NULL" }
           if (any(colnames(Envir$Data) == "Depth") & any(!is.na(Envir$Data$Depth)) == TRUE) {
            Depth1 <- as.numeric(tclvalue(depth1))
            Depth2 <- as.numeric(tclvalue(depth2))
            depth <- c(Depth1, Depth2) }
           else{ depth <- "NULL" }
           if (any(colnames(Envir$Data) == "Dates")) {
            start <- as.numeric(tclvalue(year1))
            end <- as.numeric(tclvalue(year2))
            if (is.numeric(Envir$Data$Dates) ==TRUE) { } else {
            months <- as.numeric(unlist(strsplit((tclvalue(mois)),"\\ "))) } }
           else { tkmessageBox(message="no date selected", icon = "warning", type = "ok", title="!Warning!")}
            
          cb7Value <- as.character(tclvalue(cb7Value))                           
          if (cb7Value=="1"){ log.trans <- "YES" }
           else { log.trans <- "NO" }
          cb2Value <- as.character(tclvalue(cb2Value))
          if (cb2Value=="1"){ na.replace <- "YES" }
           else { na.replace <- "NO" }
          cb3Value <- as.character(tclvalue(cb3Value))
          if (cb3Value=="1"){ outliers.re <- "YES" }
           else { outliers.re <- "NO" }

          rb1Value <- as.character(tclvalue(rb1Value))
          if (rb1Value=="Annual"){ time.step <- "Annual" }
          if (rb1Value=="Monthly"){ time.step <- "Monthly" }
          if (rb1Value=="Fortnight"){ time.step <- "Fortnight" }
          if (rb1Value=="Semi-fortnight"){ time.step <- "Semi-fortnight" }
          if (rb1Value=="Mono-mensual"){ time.step <- "Mono-mensual" }
          if (rb1Value=="Daily"){ time.step <- "Daily" }
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

          rb4Value <- as.character(tclvalue(rb4Value))                           # valeur de l'argument pris en fonction du bouton
          if (rb4Value=="seasonMann"){ test <- "SMK" }
          if (rb4Value=="MannKen"){ test <- "MK" }
          if (rb4Value=="Extended"){ test <- "ELM" }
          if (rb4Value=="Lowess"){ test <- "LOWESS" }
          if (rb4Value=="MixingDiagram"){ norm <- "YES"
                                          test <- "NO" }
          else {  norm <- "NULL" }
          npsu <- as.numeric(tclvalue(Npsu))
          FULLoption(param, depth, sal, site, rawdata="NO", select="NO", resume.reg="NO",test.normality="NO", 
                 plotB="NO", selectBox, log.trans, plotZ="NO", datashow="NO",
                 help.timestep, auto.timestep, time.step, help.aggreg, auto.aggreg, aggreg,
                 mix, outliers.re, na.replace, start, end, months, norm, npsu,
                 autocorr = "NO", spectrum="NO",anomaly="NO", a.barplot="NO", zsmooth="NO", local.trend, test) }
     OK4.but <- tkbutton(LabeledFrame5, image=imgProcess, text=" Run ", compound="right", command=OnOK4)
     tkpack(OK4.but, side="right")
#_______________________________________________________________________________fin du bouton      

     tkgrid(tklabel(Envir$trend, text="* Selected periods should be longer than 1 year", font=tkfont.create(size=7)), column=0, row=5, sticky="w")
     tkgrid(tklabel(Envir$trend, text="      "), column=0, row=6)

     HELP4.but <- tkbutton(Envir$trend, image=imgHelp, text=" Help ", compound="right", command=function() { Aide4() })      # bouton d'aide n°4
     tkgrid(HELP4.but, column=0, row=7, sticky="w")
     
     #HELP4.but <- tkbutton(Envir$trend, image=imgHelp, text=" Help ", compound="right", command=function() {                # bouton d'aide n°4
     #browseURL(file.path(path.package("TTAinterfaceTrendAnalysis"),"aide","HELP4.txt",fsep=.Platform$file.sep)) })
     #tkgrid(HELP4.but, column=0, row=7, sticky="w")  
       
   }
#_______________________________________________________________________________fin de la boucle d'ouverture du fichier   
    Envir$openframe <- tkwidget(Envir$rawdata,"labelframe",text="",padx=0,pady=0)
    tkconfigure(Envir$openframe, borderwidth=0)
    tkgrid(Envir$openframe, column=1, row=1, sticky="w")
    open <- tkbutton(Envir$openframe,text="Import TXT File", command=getfile , width=13)   
    #open <- tkbutton(Envir$openframe,text="Import CSV File", command=function() {
    #getfile(inipath="C:/") } , width=13) 
    tkgrid(open, column=1, row=1, sticky="w")                                            
    
    tkgrid(tklabel(Envir$openframe, text="  "), column=2, row=1)
    
    Example <- tkcheckbutton(Envir$openframe)                                                        
    ExampleValue <- tclVar("0")
    tkconfigure(Example,variable=ExampleValue)
    tkgrid(tklabel(Envir$openframe,text="-> open data example (SRNDunkerque)"), column=4, row=1, sticky="w")
    tkgrid(Example, column=3, row=1, sticky="e")
      
#________________________________________________________ce qui suit est affiche avant que les donnees ne soit importees
    logoRcran <- tclVar()                                                                                                
    tcl("image","create","photo",logoRcran,file=file.path(path.package("TTAinterfaceTrendAnalysis"),"aide","Small_Logo_R.gif",fsep=.Platform$file.sep))
    imgAsLabel2 <- tklabel(Envir$openframe,image=logoRcran)
    tkgrid(imgAsLabel2, column=0, row=1, sticky="w")
    
    Text3 <- tklabel(Envir$rawdata,text= "Need R v2.15+ to work properly")                      # texte montrant la version minimum de R
    tkconfigure(Text3, font=tkfont.create(size=7))                                              
    tkgrid(Text3, column=1, row=11, sticky="w")

    tkgrid(tklabel(Envir$rawdata, text="  "), column=1, row=6)
    tkgrid(tklabel(Envir$rawdata, text="  "), column=1, row=7)
    tkgrid(tklabel(Envir$rawdata, text="  "), column=1, row=8)

    AdviceFrame <- tkwidget(Envir$rawdata,"labelframe",text="Important : how to well import your data in 9 steps",padx=25,pady=5)  
    tkgrid(AdviceFrame, column=1, row=10, sticky="w")                                                                               
    tkgrid(tklabel(AdviceFrame, text="Your data must be in a .txt file (save as '.txt' format in your Spreadsheet)"), sticky="w")
    tkgrid(tklabel(AdviceFrame, text="Decimal separtor must be '.'"), sticky="w")
    tkgrid(tklabel(AdviceFrame, text="Missing value must be empty case"), sticky="w")
    tkgrid(tklabel(AdviceFrame, text="Dates must be in  'yyyy-mm-dd' (ISO 8601 time format)"), sticky="w")
    tkgrid(tklabel(AdviceFrame, text=" Dates column -> Dates"), sticky="w")
    tkgrid(tklabel(AdviceFrame, text=" Categorical factors column (Taxa, Stations...) -> Category"), sticky="w")
    tkgrid(tklabel(AdviceFrame, text=" Salinity column -> Salinity"), sticky="w")
    tkgrid(tklabel(AdviceFrame, text=" Depth column -> Depth"), sticky="w")
    tkgrid(tklabel(AdviceFrame, text="If your parameters don't appear, select them as 'numeric' "), sticky="w")
    Adv1 <- tklabel(AdviceFrame, text="You can use the 'Fix Data' button to change column labels and data category")
    tkconfigure(Adv1, foreground="red")
    tkgrid(Adv1, sticky="w")
    
    tkgrid(tklabel(Envir$rawdata, text="      "), column=1, row=12)
    
    Envir$HelpFrame <- tkwidget(Envir$rawdata,"labelframe", text="", padx=0,pady=0)                                     # cadre de texte pour afficher les 2 boutons
    tkconfigure(Envir$HelpFrame, borderwidth=0)                                                                     
    tkgrid(Envir$HelpFrame, column=1, row=13, sticky="w")
    imgUserguide <- tclVar()                                                                                                
    tcl("image","create","photo",imgUserguide,file=file.path(path.package("TTAinterfaceTrendAnalysis"),"aide","imgUserguide.gif",fsep=.Platform$file.sep))
    HELP5.but <- tkbutton(Envir$HelpFrame, image=imgUserguide, text=" User Guide ", compound="right",command=function() {             # bouton d'appel du guide utilisateur
    browseURL(file.path(path.package("TTAinterfaceTrendAnalysis"),"doc","UserGuide.pdf",fsep=.Platform$file.sep)) })
    tkgrid(HELP5.but, column=0, row=1, sticky="nw")
    
    tkgrid(tklabel(Envir$HelpFrame, text="      "), column=1, row=1)
    tkgrid(tklabel(Envir$HelpFrame, text="                                                        "), column=2, row=1)

    #Example.data <- tkbutton(Envir$HelpFrame, text=" Data example ", compound="right", command=function() {          # bouton pour appeler le 
    #getfile( inipath <- file.path(path.package("TTAinterfaceTrendAnalysis"),"data","SRNDunkerque.csv", fsep=.Platform$file.sep )) })  # fichier example Gravelines
    #tkgrid(Example.data, column=0, row=2, sticky="sw")
       
    #logoifr <- tclVar()                                                                                                 # affichage du logo ifremer
    #tcl("image","create","photo",logoifr,file=file.path(path.package("TTAinterfaceTrendAnalysis"),"aide","Logo_Ifremer.gif",fsep=.Platform$file.sep))
    #imgAsLabel <- tklabel(Envir$HelpFrame,image=logoifr)
    #tkgrid(imgAsLabel, column=3, row=1, sticky="e")
    
    tkwm.protocol(Envir$tt, "WM_DELETE_WINDOW" , function ( ) {                                           # demande de confirmation avant fermeture de l'interface
                  response <- tkmessageBox (title="TTAinterface",
                  icon = "question" ,
                  message = paste("Anlysis of", Envir$Name.split, "in progress"),
                  detail = "Do you want to quit the interface?" ,
                  type = "yesno" ,
                  parent = Envir$tt)
                  if (as.character(response) == "no")
                  return( )
                  tkdestroy(Envir$tt) } )

#______________________________________________________________________________________________________________________fin du remplissage de l'interface
tkpack(Envir$onglets, fill="both",expand=1)
tcl(Envir$onglets,"raise","1-Data_managment")

    Envir$result <- tkwidget(Envir$tt,"labelframe", text= "",padx=0,pady=0)                                     
    tkconfigure(Envir$result, borderwidth=0)                                                                     
    tkpack(Envir$result, fill='both',expand=TRUE, side="right")
     
    textframe1 <- tkwidget(Envir$result,"labelframe",text="Messages window", padx=5, pady=5)              # cadre de la fenetre 1 (message)
    tkconfigure(textframe1, font=tkfont.create(size=10, weight="bold"),borderwidth=0)               
    textframe2 <- tkwidget(Envir$result,"labelframe",text="Results window", padx=5, pady=5)               # cadre de la fenetre 2 (resultats)
    tkconfigure(textframe2, font=tkfont.create(size=10, weight="bold"),borderwidth=0)
    tkpack(textframe1, fill='both',expand=1)
    tkpack(textframe2, fill='both',expand=1)

    yscr1 <- tkscrollbar(textframe1, command=function(...)tkyview(Envir$txt,...))                         # scroll bar verticale de la fenetre 1
    xscr1 <- tkscrollbar(textframe1, command=function(...)tkxview(Envir$txt,...),orient="horiz")          # scroll bar horizontale de la fenetre 1
    Envir$txt <- tktext(textframe1,bg="white",font="courier",                                             # fenetre de texte 1
	         yscrollcommand=function(...)tkset(yscr1,...),
	         xscrollcommand=function(...)tkset(xscr1,...),
	         wrap="none",
	         font=tkfont.create(family="courier",size=9), height=1)                                         # police de remplissage par defaut et hauteur/a la fenetre2
    tkpack(yscr1,side="right",fill="y")                                                                   # scroll bars placees et etendues a toute 
    tkpack(xscr1,side="bottom",fill="x")                                                                  #    la hauteur/largeur de la fenetre 1
    tkpack(Envir$txt,fill="both",expand=1)                                                                # fenetre de texte 1 extensible
    tkpack(tkbutton(textframe1, text="-Clear-", command=function(){ tkdelete(Envir$txt, "1.0", "end") } ), side="bottom", fill="x")   # bouton 'efface' etendu a
                                                                                                                                      #   la largeur de la fenetre
    yscr2 <- tkscrollbar(textframe2, command=function(...)tkyview(Envir$txt2,...))                        # idem pour la fenetre 2
    xscr2 <- tkscrollbar(textframe2, command=function(...)tkxview(Envir$txt2,...),orient='horiz')
    Envir$txt2 <- tktext(textframe2,bg="white",font="courier",
	         yscrollcommand=function(...)tkset(yscr2,...),
	         xscrollcommand=function(...)tkset(xscr2,...),
	         wrap='none',
	         font=tkfont.create(family="courier",size=9), height=18)
    tkpack(yscr2,side="right",fill="y")
    tkpack(xscr2,side="bottom",fill="x")
    tkpack(Envir$txt2,fill="both",expand=1)
    tkpack(tkbutton(textframe2, text="-Clear-", command=function(){ tkdelete(Envir$txt2, "1.0", "end") } ), side="bottom", fill="x")  
    
}