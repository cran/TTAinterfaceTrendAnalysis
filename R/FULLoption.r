FULLoption <-
function (param, depth=NULL, sal = NULL, site=NULL, rawdata="NO", select="NO", resume.reg="NO", 
test.normality="NO", plotB = "NO", plotZ="NO", datashow="NO",
help.timestep = "NO", auto.timestep = "NO", time.step = NULL, help.aggreg = "NO", auto.aggreg = "NO", aggreg = NULL ,  
mix = "YES", outliers.re = "NO", na.replace="NO", start = NULL, end = NULL, months = c(1:12), norm = "NO", npsu = 30,
autocorr = "NO", spectrum="NO", anomaly="NO", zsmooth="NO", local.trend = "NO", test= "MK")

#####################################################################################################################################################################################################################################################################################################################
#                                                                                                                                                                                                                                                                                                                   #                                                                                                                                                                                                                                                                                                                  #
#  param = parameters on which analysis will be performed; site = sampling site to treat                                                                                                                                                                                                                            #
#  depth = depths takeinto account; sal = salinities take into account (without missing values if sal is different from salinity max and salinity min)                                                                                                                                                              #
#  rawdata = desciptive statistics on raw data ; select = desciptive statistics on selected site and parameter ; resume.reg = desciptive statistics on regularized time series                                                                                                                                      #
#  test.nomrality = perform a Shapiro-Wilk normality test on selected parameter ; plotB = display a boxplot (by year) of rawdata ; plotZ = display a plot of the regularized time series ; datashow = show a table of the regularized data                                                                          #
#  time.step = choice of the time step for data aggregation ; help.timestep = time step advice ; auto.timestep = automaticaly apply the most relevant time step (mean time between two measurments)                                                                                                                 #
#  aggreg = choice of the aggregation method of the data ; help.aggreg = method advice ; auto.aggreg = automaticaly apply the best method (p of Wilcoxon)                                                                                                                                                           #
#  mix = aggregate the sampling site ; outliers.re = remove extreme values (boxplot method) ; na.replace = replace missing values                                                                                                                                                                                   #
#  start and end = select the first and last year to take into account in the analysis ; months = same thing as for the years                                                                                                                                                                                       #
#  norm and npsu = compute normalised values of nutrients at the salinity npsu                                                                                                                                                                                                                                      #
#  autocorr et spectrum = respectively display the autocorrelation diagramme and Fourrier spectrum of the regularized time series                                                                                                                                                                                   #
#  anomaly = display a color boxplot of the anomalies calculated for each time step of each year (value at time step of the year n - the median of the time step of all year)                                                                                                                                       #
#  zsmooth = display a detrended plot of the time series ; local.trend = display the cusum plot of the time series ; test = perform the selected temporel trend test                                                                                                                                                #
#                                                                                                                                                                                                                                                                                                                   #
#####################################################################################################################################################################################################################################################################################################################

# how to prepare your data base before importation :
# your datasheet must be in .csv file, if possible select only the interesting column before creation of the datasheet (parameters, stations names, dates...) 
# name exactly the dates column "DATES", the station one "STATIONS", the depth ones "DEPTH" et the salinity one "S"
# date format must be as followed : dd/mm/yyyy  and decimal separator must be '.' (dote)

{  
#____________________________________________________________________________________________________________stat descriptive sur les donnees de base
 {                                                                                              
  if (rawdata == "YES") {                                                                            # appel la fonction si rawdata = YES
  # capture la sortie de summary(Envir$Data)                                                                        
      sumdata <- capture.output(summary(Envir$Data))
  # insert le resultat de la capture dans la fenetre de texte 'Envir$txt2'                                                            
      tkinsert(Envir$txt2, "end", paste("-Desciptive statistics on raw data-", "\n"))                # mise en page
      tktag.add(Envir$txt2, "titre", "end -2 lines linestart","end -2 lines lineend")
      tktag.configure(Envir$txt2, "titre", font=tkfont.create(family="courier",size=9,weight="bold"))
      sumdata <- paste(sumdata, collapse="\n")
      tkinsert(Envir$txt2,"end",paste(sumdata, "\n\n"))
      
      ll <- ncol(Envir$Data)                                                                         # comptage de la quantite de donnee pas colonne
      res1 <- length(Envir$Data[1][!is.na(Envir$Data[1])])
      for (i in 2:ll) { res2 <- length(Envir$Data[i][!is.na(Envir$Data[i])])
      res1 <- data.frame(cbind(res1,res2)) } 
      nn <- names(Envir$Data)
      names(res1) <- nn
      DataQ <- capture.output(res1)
      DataQ <- paste(DataQ, collapse="\n")
      
      A <- tapply(as.numeric(format(as.Date(Envir$Data$DATES, format="%d/%m/%Y"), format = "%Y")), list(Envir$Data$STATIONS), max, na.rm=TRUE)  # nombre d'annee par stations
      B <- tapply(as.numeric(format(as.Date(Envir$Data$DATES, format="%d/%m/%Y"), format = "%Y")), list(Envir$Data$STATIONS), min, na.rm=TRUE)
      C <- A-B
      Res.Per <- data.frame(cbind(B,A,C))
      names(Res.Per) <- c("Min.","Max.","Length")
      DataP <- capture.output(Res.Per)
      DataP <- paste(DataP, collapse="\n")

      tkinsert(Envir$txt2,"end", paste("Data quantity: " ,"\n"))
      tksee (Envir$txt2,"end")
      tkinsert(Envir$txt2,"end", DataQ)
      tkinsert(Envir$txt2,"end","\n\n")
      tkinsert(Envir$txt2,"end", paste("Years sampled by stations: " ,"\n"))
      tkinsert(Envir$txt2,"end", DataP)
      tkinsert(Envir$txt2,"end","\n\n")
                                                               
 
  # fait appraitre le resultat dans une frame directement affichee à l'ecran                                             
      Stat1 <- NULL                                                                                  # tableau vide a remplir
      md <- as.matrix(summary(Envir$Data))                                                           # cree une matrice avec le resultat de summary
      ncol(md)                                                                                       
      for (i in 1:ncol(md)) {                                                                        # remplit le tableau vide avec... 
           mb <- md[, i]                                                                             # ...le contenu de la matrice
           Stat1 <- cbind(Stat1, mb) }                                                               
      Summary_RawData <- data.frame(Stat1)                                                           # transforme le tableau en dataframe
      names(Summary_RawData) <- names(Envir$Data)                                                    # nome les colonnes
      return(showData(Summary_RawData))                                                              # affiche le tableau
      }
  else{}   
 }
#________________________________________________________________________________________preparation du tableau de donnees par station(s) et parametre    
 {                                                          
  if (any(colnames(Envir$Data) == "STATIONS")) {                                             # effectue la commande si une colonne 'STATIONS' existe
      Ts <- subset(Envir$Data,STATIONS %in% site, drop =TRUE) }                              # nouveau tableau avec les stations selectionnees
  else { tkmessageBox(message=paste("No STATIONS column in your dataset")
         ,icon = "warning", type = "ok", title="!Warning!") }                                                                           
  
  if (any(colnames(Envir$Data) == "S")) {                                                    # effectue la commande si une colonne 'S' existe
      if (max(sal) >= round(max(Ts$S, na.rm=TRUE),0) & min(sal) <= round(min(Ts$S, na.rm=TRUE),0) ) { }         # si toutes les salinites sont selectionnees (de min à max) alors toutes les salinites, NA inclus, seront pris en compte.
      else { Sal <- na.omit(Ts$S[(Ts$S >= min(sal)) &  (Ts$S <= max(sal))])                  # selection des donnees comprises entre les salinites indiquees, NA exclues
             Ts <- subset(Ts, S %in% Sal, drop = TRUE) } }                                   # nouveau tableau avec les salinites selectionnees
  else{ }
     
  if (any(colnames(Envir$Data) == "DEPTH")) {                                                    # effectue la commande si une colonne 'DEPTH' existe
      if (max(depth) >= round(max(Ts$DEPTH, na.rm=TRUE),0) & min(depth) <= round(min(Ts$DEPTH, na.rm=TRUE),0) )     # si toutes les profondeurs sont selectionnees (de min à max) alors toutes les profondeurs, NA inclus, seront pris en compte.
      { }                                                                          
      else { Depth <- na.omit(Ts$DEPTH[(Ts$DEPTH >= min(depth)) &  (Ts$DEPTH <= max(depth))])    # selection des donnees comprises entre les profondeurs indiquees, NA exclues                                                     
      Ts <- subset(Ts, DEPTH %in% Depth, drop = TRUE) }  }                                       # nouveau tableau avec les profondeurs selectionnees
  else{}
  
  if (any(colnames(Envir$Data) == "DEPTH") & length(Ts$DEPTH)==0 | any(colnames(Envir$Data) == "S") & length(Ts$S)==0) {
  return(tkmessageBox(message=paste("The selected combinaison of salinity and depth doesn't exist in your dataset")
                               , icon = "warning", type = "ok", title="!Warning!")) }
  else{}

  if (any(colnames(Envir$Data) == "S")) {     
      TS <- data.frame(Ts$STATIONS, Ts$DATES, Ts$S)                                              # nouveau tableau avec les stations selectionnees, les dates et les salinites pour la standardisation des sels nut (les profondeurs sont prise en compte mais n'apparaissent pas)
      TS$param <- Ts[,names(Ts)==param]                                                          # le parametre selectionne est inclu dans le nouveau tableau
      names(TS) <- c("STATIONS", "DATES", "S", "param") }                                        # les colonnes sont nommees  (le paramètre restera "param" pour faciliter les analyses automatiques)
  else { TS <- data.frame(Ts$STATIONS, Ts$DATES)                                                 # meme chose mais s'il n'y a pas de colonne 'S'
      TS$param <- Ts[,names(Ts)==param]                                                       
      names(TS) <- c("STATIONS", "DATES", "param") }                                           
 }
#________________________________________________________________________________________________________Pour eviter des NOTES lors du check 
 {
 YEARS=DayYears=NULL
 }
#________________________________________________________________________________________________________Calcul du temps moyen entre 2 donnees en jour
 {
  if (is.numeric(TS$DATES) == TRUE) { }
  else{ TS$DATES <- as.Date(TS$DATES, format="%d/%m/%Y") }                  # les dates sont formatees
  TS <- TS[!is.na(TS$DATES), ]                                              # selection des cas ou les dates sont bien presentes
  if (mix == "YES"){                                                        # trie dans l'ordre toutes les mesures si les stations sont mixees
      TS <- TS[order(TS$DATES), ] }
  else {
      TS <- TS[order(TS$DATES), ]
      TS <- TS[order(TS$STATIONS), ]}                                       # trie dans l'ordre les mesures par stations si les stations ne sont pas mixees
  TSS <- TS
  ecarts <- TS$DATES[2:nrow(TS)]-TS$DATES[1:(nrow(TS)-1)]                   # calcul l'ecart en jour entre deux mesures successives
  xx <- as.numeric(ecarts)
  TS$time <- c(1, round(cumsum(xx)))                                        # somme cumulee des ecarts et integration au tableau (TS$time) : donne une échelle temporelle a la serie de donnees
  TS$time[TS$time==0] <- 1                                                                    
 }
#_____________________________________________________________________________________________Extraction des dates hors semaines (besoin du time step)
 {
  if (is.numeric(TS$DATES) == TRUE) { TS$YEARS <- TS$DATES }
  else {  
  TS$YEARS <- as.numeric(format(TS$DATES, format = "%Y"))                   # extraction des annees
  TS$MONTHS <- as.numeric(format(TS$DATES, format = "%m"))                  # extraction des mois
  TS$days <- as.numeric (format(TS$DATES, format = "%d"))                   # extraction des jours
  TS$DY <- as.numeric(format(TS$DATES, format="%j"))                               # extraction des jours/annee
  TS$week.month <- TS$days }
 
  if (is.null(start)) {                                                     # selection de l'annee de début de la série a analyser
      start <- min(TS$YEARS) } else {}
  if (is.null(end)) {                                                       # selection de l'annee de fin de la série a analyser
      end <- max(TS$YEARS) } else {} 
  TS <- TS[(TS$YEARS >= start & TS$YEARS <= end),]                          # tableau avec les donnees comprises entre les annees de début et de fin 
 }  
#______________________________________ Etabli la liste des stations pour les inclure dans les resultats sous forme de texte (3 sites maxi ensuite '...') 
{
 if (length(site)==1) liste.stations <- site[1]
 if (length(site)==2) liste.stations <- paste(site[1], site[2], sep=", ")
 if (length(site)==3) liste.stations <- paste(site[1], site[2], site[3], sep=", ")
 if (length(site)>3) { for (i in 1 : 3) { liste.stations <- paste(site[1], site[2], site[3], sep=", ") }
                       liste.stations <- paste(liste.stations, "...", sep=", ") }
}
#_________________________________Statistiques descriptives sur les donnees de base (option : select), prend en compte la selection des annees et mois
# meme principe que pour 'rawdata'
 {
  if (select == "YES"){
    if (is.numeric(TS$DATES) == TRUE) { } else {
    TS <- subset(TS, MONTHS %in% months, drop = TRUE) }
    Desciptive.Statistics <- summary(TS$param)
    sumdata2 <- capture.output(Desciptive.Statistics)  
    tkinsert(Envir$txt2, "end", paste("-Desciptive statistics on selected site and parameter-"
                                 , "\n", "Parameter: ", param , "   Site(s): ", liste.stations
                                 , sep="" , "\n\n")) 
    tktag.add(Envir$txt2, "titre", "end -4 lines linestart","end -4 lines lineend")
    tktag.configure(Envir$txt2, "titre", font=tkfont.create(family="courier",size=9,weight="bold"))
    sumdata2 <- paste(sumdata2,collapse="\n")
    tkinsert(Envir$txt2,"end",paste(sumdata2, "\n\n"))
    tkinsert(Envir$txt2,"end", paste("Data quantity (",param,") : ", length(TS$param[!is.na(TS$param)]), "\n\n", sep=""))
    tksee (Envir$txt2,"end")
       
    Desciptive.Statistics <- as.matrix(Desciptive.Statistics)
    Desciptive.Statistics <- as.data.frame(Desciptive.Statistics)
    names(Desciptive.Statistics) <- param
    return(showData(Desciptive.Statistics)) }
  else{}
 }
#_________________________________________________________________________Aide pour trouver le bon time.step (option : help.timestep et auto.timestep)
 {
  # Calcul de l'ecart moyen, en jours, entre 2 mesures successives (les mesures prise le meme jour sont considerees comme 1 seule) 
  
  TSS <- TSS[!is.na(TSS$param), ]                                                # reprend le tableau TS sans les valeurs manquantes
  ecarts <- TSS$DATES[2:nrow(TSS)]-TSS$DATES[1:(nrow(TSS)-1)]                    # calcule les ecarts reels entre chaque mesure presente
  Ecarts <- ecarts[ecarts!=0]                             
  Mean.time <- data.frame(mean(Ecarts))
  mt <- as.numeric(Mean.time)
 
  # Proposition du meilleur time step (option : help.timestep) en fonction de mt
 {  
 
  if (help.timestep == "NO" & auto.timestep == "NO" & is.null(time.step)) {              
      return() } 
  
  if (time.step != "NULL") {                                          # affiche la proposition mais effectue l'aggregation avec la méthode choisi par l'utilisateur
    
    if (mt<=5) {                                                       # si l'ecart moyen est inferieur à 5 jours
        T <- "Use daily frequency" }                                                                
     else { if (mt>5 & mt<=10) {                                      # si l'ecart moyen est entre 5 et 10 jours
          T <- "Use semi-fortnightly frequency"}
      else { if (mt>10 & mt<=24) {                                    # si l'ecart moyen est entre 10 jours et 24 jours
                 T <- "Use fortnightly frequency" }
             else { if (mt>24 & mt<=60) {                              # si l'ecart moyen est entre 24 jours et 60 jours
                        T <- "Use monthly frequency" }
                    else { if (mt>60) {                               # si l'ecart moyen est superieur à 60 jours
                               T <- "Use yearly frequency" }
                           else { return(print("No solution! Should be a problem somewhere...")) }}}}}           
      time.step <- time.step                                               
      tkinsert(Envir$txt, "end", paste("-Time step choice-" , "\n"))
      tktag.add(Envir$txt, "titre", "end -2 lines linestart","end -2 lines lineend")
      tktag.configure(Envir$txt, "titre", font=tkfont.create(family="courier",size=9,weight="bold"))
      tkinsert(Envir$txt, "end", paste("Parameter: ", param , "   Site(s): ", liste.stations, sep="" , "\n"))  
      tkinsert(Envir$txt, "end", paste("-> Time step manually selected ", "(", time.step, ")", sep="" , "\n"))
      tkinsert(Envir$txt, "end", paste("Advice: ", T, "\n\n")) 
      tksee (Envir$txt,"end") }
                       
  # affiche uniquement la proposition                     
  else { if (help.timestep == "YES") {                                                                           
            Mean.time <- data.frame(rbind("Mean time between two measurements (days):", as.character(round(mean(Ecarts),2))))
            names(Mean.time) <- "T"
            Data.range <- data.frame(rbind("Time range (days):                        ", t(t(as.character(range(Ecarts))))))
            names(Data.range) <- "T"
            Help.timestep <- rbind(Mean.time, Data.range)
 
       if (mt<=5) {
           T <- as.character("-> Use daily frequency             ") }
           else { if (mt>5 & mt<=10) {
                T <- as.character("-> Use semi-fortnightly frequency             ") }
            else { if (mt>10 & mt<=24) {
                       T <- as.character("-> Use fortnightly frequency       ") }
                   else { if (mt>24 & mt<=60) {
                              T <- as.character("-> Use monthly frequency       ") }
                          else { if (mt>60) {
                                     T <- as.character("-> Use yearly frequency           ") }
                                 else {return(print("No solution! Should be a problem somewhere..."))}}}}}
            
            tt <- paste("Mean time between two measurements:", "\n",
            "  ", as.character(round(mt, 2))," days", "\n",
            "Time range (min - max):", "\n",
            "  ", as.character(range(Ecarts))[1]," - ", as.character(range(Ecarts))[2], " days", "\n\n",
            "Advice ", T , sep="")

            tkmessageBox(title = "Frequency Choice Guidance", message = tt, icon = "info", type = "ok")
            return()   }
 
  # Selection auto du time step (option : auto.timestep) 
  else { if (auto.timestep == "YES")  {
          if (mt<=5) {
             Time.step <- "Daily frequency automatically use"
             time.step <- "Daily" }
          else { if (mt>5 & mt<=10) {
                  Time.step <- "Semi-fortnight frequency automatically use"
                  time.step <- "Semi-fortnight" }
             else { if (mt>10 & mt<=24) {
                        Time.step <- "Fortnight frequency automatically use" 
                        time.step <- "Fortnight" }
                    else { if (mt>24 & mt<=60) {
                               Time.step <- "Mensual frequency automatically use"
                               time.step <- "Mensual" }
                           else { if (mt>60) {
                                      Time.step <- "Annual frequency automatically use"
                                      time.step <- "Annual" }
                                  else {return(print("No solution! Should be a problem somewhere..."))}}}}}                

         tkinsert(Envir$txt, "end", paste("-Time step choice-", "\n"))
         tktag.add(Envir$txt, "titre", "end -2 lines linestart","end -2 lines lineend")                
         tktag.configure(Envir$txt, "titre", font=tkfont.create(family="courier",size=9,weight="bold"))
         tkinsert(Envir$txt, "end", paste("Parameter: ", param , "   Site(s): ", liste.stations, sep="" , "\n"))
         tkinsert(Envir$txt, "end", paste(Time.step, "\n\n"))
         tksee (Envir$txt,"end")             
 }
  else {}  
 } } } 
 }
#_____________________________________________________________________________________________________Extraction des semaines en fonction du time.step
 {
  if (is.numeric(TS$DATES) == TRUE) { } else { 
  if (time.step == "Fortnight") {                                                                                                                                                     
      TS$week.month[TS$days <= 15] <- 1                    # extraction du numero de semaines du mois (échelle hebdo = 2 quinzaines/mois)  
      TS$week.month[TS$days > 15] <- 2 }                                                                                                                                           
  else {                                                                                                                                                                           
      TS$week.month[TS$days <= 8] <- 1                     # extraction du numero de semaines du mois (autres échelle = 4 semi-quinzaines/mois) 
      TS$week.month[TS$days > 8 & TS$days <= 15] <- 2                                                                                                                              
      TS$week.month[TS$days > 15 & TS$days <= 23] <- 3                                                                                                                             
      TS$week.month[TS$days > 23] <- 4 }                                                                                                                                           
  TS$days <- NULL                                          # suppression de la colonne days pour plus de clarete (plus besoin)
 } }
#____________________________Valeurs normalisees en fonction de la salinite (npsu), pour les sels nutritifs. Effectue sur les donnees non regularisees
 {  
  if (norm == "YES") {
     if (any(colnames(Envir$Data) == "S")) { 
      if (local.trend == "YES") { return(tkmessageBox(message="Work only for Seasonal and Global Trend", icon = "warning", type = "ok", title="Warning")) }
      TS <- subset(TS, MONTHS %in% months, drop = TRUE)                         # selection des mois a traiter
      dir.create(paste(Envir$save.WD,"/",Envir$File.Name,"/", start,"-", end, "/", param, "/", sep= ""), recursive = TRUE)  # création du répertoir de sauvegarde
      
      a <- min(TS$YEARS, na.rm=TRUE)                                       
      b <- max(TS$YEARS, na.rm=TRUE)
      c <- sort(rep(a:b, length(months)))
      NormNutri <- matrix(c)                                                    # matrice vide que l'on va remplir avec les valeurs normalisees par annees (a:b)
      NormNutri <- data.frame(cbind(NormNutri, (rep(months, length(a:b)))))
      NormNutri <- data.frame(cbind(NormNutri, (rep(NA, length(c)))))
      names(NormNutri) <- c("YEARS", "MONTHS", "Normalized")                    # on nomme les colonnes par souci de clarete
      
      for (i in a:b) {                                                          # pour chaque annee i :
       for (j in months) {                                                      # et pour chaque mois j :    
           nutrient <- TS$param[TS$YEARS==i & TS$MONTHS==j]                     #      on recupere les valeurs du parametre à traiter
           sal <- TS$S[TS$YEARS==i & TS$MONTHS==j]                              #      on recupere les salinites a traiter
 
           if ( sum(nutrient*sal, na.rm=TRUE)== 0 | max(sal, na.rm=TRUE)<npsu ) { 
           NormNutri$Normalized[NormNutri$YEARS==i & NormNutri$MONTHS==j] <- NA }                                                                                                                     # si toute les conditions sont rempli (parametres present et salinites inferieures a la salinite de normalisation) :
           else { reg <- lm(nutrient~sal)                                                                                          #           on effectue la regression entre salinite et nutriment par annee
           NormNutri$Normalized[NormNutri$YEARS==i & NormNutri$MONTHS==j] <- (npsu * (reg$coefficients[2]))+(reg$coefficients[1])  #           on calcule la valeur normalisee
            
            save.mixingdia.path <- paste(Envir$save.WD,"/",Envir$File.Name,"/",start,"-",end,"/",param, "/", Envir$File.Name,"_MixingDiagram_",param,"_",j,",",i,".png", sep="")                        # sauve chaque mixing diagram (par année)
            png(save.mixingdia.path)
            plot(nutrient~sal, xlab="Salinity", ylab=param, main=paste("Mixing diagram of ", param, " in ", j, "/", i, sep=""))
             if( !is.na(reg$coefficients[2]) ) { abline(lm(nutrient~sal)) }                                                        # ajoute la droite de regression si elle est réalisable
             else{ }
            dev.off() }}}        
           
           save.mixingreg.path <- paste(Envir$save.WD,"/",Envir$File.Name,"/",start,"-",end,"/",param, "/", Envir$File.Name,"_Normalized_",param,"_at_",npsu,".csv", sep="")
           write.csv2(NormNutri, row.names=FALSE, file=save.mixingreg.path)
 
           if (sum(NormNutri$Normalized, na.rm=TRUE)== 0 | max(TS$S, na.rm=TRUE)<npsu ) {                                          # message d'avertissement si les conditions ne sont pas rempli
           return(tkmessageBox(message=paste("No parameters available or all the salinities in your database are under the 'selected psu'"
                               , "\n", "Try again by decreasing the 'selected psu' value", sep="")
                               , icon = "warning", type = "ok", title="!Warning!")) }
      
      z <- ts(NormNutri$Normalized, start = a, deltat=1/(length(months)))
      plot(z, xlab = "Years (at selected period)", ylab= "Normalized concentration", main=paste("Trend of", param,"at salinity", npsu))  # affiche la figure
      title(main=paste("\n\n\n", "at station(s): ", liste.stations), cex.main=0.8)
      minor.tick(nx=3, ny=2, tick.ratio=0.4)
              save.normnutri.path <- paste(Envir$save.WD,"/",Envir$File.Name,"/",start,"-",end,"/",param,"/", Envir$File.Name, "_NormalNutri_",param,".png", sep = "")
              png(save.normnutri.path)
              plot(z, xlab = "Years (at selected period)", ylab= "Normalized concentration", main=paste("Trend of", param,"at salinity", npsu))
              title(main=paste("\n\n\n", "at station(s): ", liste.stations), cex.main=0.8)
              minor.tick(nx=3, ny=2, tick.ratio=0.4)
              dev.off()
      tkinsert(Envir$txt2,"end", paste("-Global trend of",param,"at salinity", npsu,"-", "\n"))
      tktag.add(Envir$txt2, "titre", "end -2 lines linestart","end -2 lines lineend")
      tktag.configure(Envir$txt2, "titre", font=tkfont.create(family="courier",size=9,weight="bold"))
      tkinsert(Envir$txt2, "end", paste("Site(s): ", liste.stations, sep="" , "\n\n"))
      sK <- seaKen(z)
      save.normtrend.path <- paste(Envir$save.WD,"/",Envir$File.Name,"/",start,"-",end,"/",param,"/", Envir$File.Name, "_NormGlobalTrend_",param,".csv", sep = "")
      write.csv2(sK, row.names=FALSE, file=save.normtrend.path) 
      tkinsert(Envir$txt2,"end", paste("Trend (sen.slope): ", round(sK$sen.slope, 4), "  original units per year", "\n"
                                 , "%Trend (sen.slope.pct): ", round(sK$sen.slope.pct, 4), "  percent of mean quantity per year", "\n"  
                                 , "p.value: ", round(sK$p.value, 4), "\n\n\n", sep=""))
      if (sK$p.value <= 0.05) { tktag.add(Envir$txt2, "titre5", "end -4 lines linestart","end -4 lines lineend") 
      tktag.configure(Envir$txt2, "titre5", font=tkfont.create(family="courier",size=9,weight="bold"))} else {}
      tksee (Envir$txt2,"end")  
      return(showData(capture.output(sK), rowname.bar = NULL, colname.bar = NULL, title="Global Trend (Mann Kendall Test) results"))
 }
  else{ return(tkmessageBox(message="No salinity data available", icon = "warning", type = "ok", title="Warning")) } }
  else{ } 
 } 
#______________________________________________________________________________Suppression de la colonne salinité maintenant que l'on en a plus besoin
 { 
 if(any(colnames(TS) == "S")) { TS$S <- NULL }
 else { }                                   
 }
#_____________________________________________________________Extraction des outliers (option : outliers.re), pas necessaire pour un test Mann-Kendall
 {
  if (plotB == "YES") {
      if (is.numeric(TS$DATES) == TRUE) { } else {
      TS <- subset(TS, MONTHS %in% months, drop = TRUE) }
      dir.create(paste(Envir$save.WD,"/",Envir$File.Name,"/", start,"-", end, "/", param, "/", sep= ""), recursive = TRUE)
      save.boxplot.path <- paste(Envir$save.WD,"/",Envir$File.Name,"/", start,"-", end, "/", param, "/", Envir$File.Name,"_Boxplot_",param,".png", sep = "")
      png(save.boxplot.path)
      boxplot(TS$param~TS$YEARS, xlab="Time", ylab=paste(param, "concentration")        # enregistre la figure
              , main=paste("Boxplot of",param,"concentration","(o = outliers)"))
      title(main=paste("\n\n\n", "at station(s): ", liste.stations), cex.main=0.8) 
      dev.off()
      boxplot(TS$param~TS$YEARS, xlab="Time", ylab=paste(param, "concentration")        # affiche la boite à moustache par annees
              , main=paste("Boxplot of",param,"concentration","(o = outliers)"))
      title(main=paste("\n\n\n", "at station(s): ", liste.stations), cex.main=0.8)
      return() }    
  else{}

  if (outliers.re == "YES") {
      a <- min(TS$YEARS)                                                         # pose les arguments qui seront utilises 
      b <- max(TS$YEARS)
      Tf <- NULL
      Text <- NULL

      for (i in a:b) {                                                           # extraction des donnees tous les ans de la premiere (a) a la derniere (b)
           T <- TS$param[TS$YEARS==i]                                            # extrait les donnees a traite pour l'annee i
           EXT <- TS$param[TS$YEARS==i]                                          
           Q3 <- quantile (T, 0.75, na.rm=TRUE)                                  # calcul le 3eme quantile
           Q1 <- quantile (T, 0.25, na.rm=TRUE)                                  # calcul le 1er quantile
           Q <- Q3-Q1
           h <- Q3+(Q*1.5)                                                       # limite haute des outliers
           l <- Q1-(Q*1.5)                                                       # limite basse des outliers
           T[T>h]<-NA                                                            # suppression des donnees au dessus de la limite haute
           T[T<l]<-NA                                                            # suppression des donnees en dessous de la limite basse
           EXT[EXT<=h & EXT>=l]<-NA                                              # conserve les donnees supprimees dans EXT

           t <- list(T)                                                                    
           Tf <- rbind.data.frame(Tf, t)                                         # construit le tableau (Tf) annee par annee (donnees avec outliers supprimes)
           ext <- list(EXT)                                                                 
           Text <- rbind.data.frame(Text, ext) }                                 # construit le tableau (Text) des donnees supprimees annee apres annee
                                                                                 
      tf <- Tf[ ,1]
      names(tf) <- c("param")
      TS$param <- tf                                                             # remplace les donnees brute du tableau TS par les donnees (Tf) avec outliers retires

      Outliers <- data.frame(TS$STATIONS, Text[ ,1], TS$DATES)
      names(Outliers) <- c("STATIONS", "Outliers", "DATES")
      save.outl.path <- paste(Envir$save.WD,"/",Envir$File.Name,"/", start,"-", end, "/", param, "/", Envir$File.Name, "_Outliers_",param,".csv", sep = "")
      write.csv2(Outliers, row.names=FALSE, file=save.outl.path) }               # sauve les donnees supprimees dans un fichier csv

  else{}
 }
#____________________________________________________________________________________________Supprime la colonne TS$DATES dans TS pour plus de clarete 
 {
  DATES <- TS$DATES
  TS$DATES <- NULL                                                                               
 }
#_________________________________________________________________________Ajout d'un tableau temporel fictif pour combler les mois/semaines manquantes
 {
 # Tableau des années

  yy <- (min(TS$YEARS):max(TS$YEARS))                                             # suite de toutes les annees comprise entre l'annee de debut et de fin de la serie
  mm <- rep(1, length(yy))                                                        # remplis la nouvelle colonne mois avec des 1
  aa <- rep(NA, length(yy))
  if (is.numeric(DATES) == TRUE) {
  fic2 <- data.frame(aa,aa,aa,yy)                                                 # nouveau tableau fictif
  names(fic2) = c("STATIONS", "param", "time", "YEARS") } else {                  # ...
  
  fic2 <- data.frame(aa,aa,aa,yy,mm,aa,aa)                                           # nouveau tableau fictif
  names(fic2) = c("STATIONS", "param", "time", "YEARS", "MONTHS", "week.month", "DY")  }                

  TS <- rbind(fic2, TS)                                                           # fusion des tableaux
  
  if (is.numeric(DATES) ==TRUE) { } else {
  if (time.step == "Daily") {
    ddd <- c(1:366)
    www <- rep(NA, 366)
    
  fic3 <- data.frame(www,www,www,www,www,www,ddd)
  names(fic3) = c("STATIONS", "param", "time", "YEARS", "MONTHS", "week.month", "DY")

  TS <- rbind(fic3, TS) } }
 
 # Tableau des mois et semaines (meme principe, mais 24 semaines pour semi-hebdomadaire et 48 pour les autres)
 
  if (is.numeric(DATES) ==TRUE) { } else {
  if (time.step == "Fortnight") {

      w <- rep(1:2, 12)
      m <- gl(12, 2)
      y <- rep(-1000, 24)
      a <- rep(NA, 24)
      b <- rep(NA, 24)
      c <- rep(NA, 24)}
 
  else {
      w <- rep(1:4, 12)
      m <- gl(12, 4)
      y <- rep(-1000, 48)
      a <- rep(NA, 48)
      b <- rep(NA, 48)
      c <- rep(NA, 48) }
 
  fic1 <- data.frame(a,b,c,y,m,w,a)
  names(fic1) = c("STATIONS", "param", "time", "YEARS", "MONTHS", "week.month", "DY")

  TS <- rbind(fic1, TS)  }
 }
#________________________________________________________________ Tableaux croises avec toutes les methodes d'aggregation (temporelle et mathematique)
 {

 if(time.step == "Daily") {
    if (mix =="YES") {
        Mean <- tapply(TS$param, list(TS$YEARS, TS$DY), mean, na.rm=TRUE)
        Quantile <- tapply(TS$param, list(TS$YEARS, TS$DY), quantile, probs = (0.9), na.rm=TRUE)
        Median <- tapply(TS$param, list(TS$YEARS, TS$DY), median, na.rm=TRUE)
        Max <- tapply(TS$param, list(TS$YEARS, TS$DY), max, na.rm=TRUE) }
    else { 
        Mean <- tapply(TS$param, list(TS$YEARS, TS$DY, TS$STATIONS), mean, na.rm=TRUE)
        Quantile <- tapply(TS$param, list(TS$YEARS, TS$DY, TS$STATIONS), quantile, probs = (0.9), na.rm=TRUE)
        Median <- tapply(TS$param, list(TS$YEARS, TS$DY, TS$STATIONS), median, na.rm=TRUE)
        Max <- tapply(TS$param, list(TS$YEARS, TS$DY, TS$STATIONS), max, na.rm=TRUE)} }
  else { if (time.step == "Fortnight") {
      if (mix =="YES") {
          Mean <- tapply(TS$param, list(TS$YEARS, TS$week.month, TS$MONTHS), mean, na.rm=TRUE)
          Quantile <- tapply(TS$param, list(TS$YEARS, TS$week.month, TS$MONTHS), quantile, probs = (0.9), na.rm=TRUE)
          Median <- tapply(TS$param, list(TS$YEARS, TS$week.month, TS$MONTHS), median, na.rm=TRUE)
          Max <- tapply(TS$param, list(TS$YEARS, TS$week.month, TS$MONTHS), max, na.rm=TRUE) }
      else { 
          Mean <- tapply(TS$param, list(TS$YEARS, TS$week.month, TS$MONTHS, TS$STATIONS), mean, na.rm=TRUE)
          Quantile <- tapply(TS$param, list(TS$YEARS, TS$week.month, TS$MONTHS, TS$STATIONS), quantile, probs = (0.9), na.rm=TRUE)
          Median <- tapply(TS$param, list(TS$YEARS, TS$week.month, TS$MONTHS, TS$STATIONS), median, na.rm=TRUE)
          Max <- tapply(TS$param, list(TS$YEARS, TS$week.month, TS$MONTHS, TS$STATIONS), max, na.rm=TRUE)} }
     else { if (time.step=="Mensual") {
             if (mix == "YES") {
                 Mean <- tapply(TS$param, list(TS$YEARS, TS$MONTHS), mean, na.rm=TRUE)
                 Quantile <- tapply(TS$param, list(TS$YEARS, TS$MONTHS), quantile, probs = (0.9), na.rm=TRUE)
                 Median <- tapply(TS$param, list(TS$YEARS, TS$MONTHS), median, na.rm=TRUE)
                 Max <- tapply(TS$param, list(TS$YEARS, TS$MONTHS), max, na.rm=TRUE) }
             else {
                 Mean <- tapply(TS$param, list(TS$YEARS, TS$MONTHS, TS$STATIONS), mean, na.rm=TRUE)
                 Quantile <- tapply(TS$param, list(TS$YEARS, TS$MONTHS, TS$STATIONS), quantile, probs = (0.9), na.rm=TRUE)
                 Median <- tapply(TS$param, list(TS$YEARS, TS$MONTHS, TS$STATIONS), median, na.rm=TRUE)
                 Max <- tapply(TS$param, list(TS$YEARS, TS$MONTHS, TS$STATIONS), max, na.rm=TRUE)}}
         else { if (time.step=="Annual") {
                    if (mix == "YES") {
                        Mean <- tapply(TS$param, list(TS$YEARS), mean, na.rm=TRUE)
                        Quantile <- tapply(TS$param, list(TS$YEARS), quantile, probs = (0.9), na.rm=TRUE)
                        Median <- tapply(TS$param, list(TS$YEARS), median, na.rm=TRUE)
                        Max <- tapply(TS$param, list(TS$YEARS), max, na.rm=TRUE)}
                    else {
                        Mean <- tapply(TS$param, list(TS$YEARS, TS$STATIONS), mean, na.rm=TRUE)
                        Quantile <- tapply(TS$param, list(TS$YEARS, TS$STATIONS), quantile, probs = (0.9), na.rm=TRUE)
                        Median <- tapply(TS$param, list(TS$YEARS, TS$STATIONS), median, na.rm=TRUE)
                        Max <- tapply(TS$param, list(TS$YEARS, TS$STATIONS), max, na.rm=TRUE)}}
                else { if (time.step == "Mono-mensual") {
                           if (mix == "YES") {
                               Mean <- tapply(TS$param, list(TS$MONTHS), mean, na.rm=TRUE)
                               Quantile <- tapply(TS$param, list(TS$MONTHS), quantile, probs = (0.9), na.rm=TRUE)
                               Median <- tapply(TS$param, list(TS$MONTHS), median, na.rm=TRUE)
                               Max <- tapply(TS$param, list(TS$MONTHS), max, na.rm=TRUE) }
                           else {
                               Mean <- tapply(TS$param, list(TS$MONTHS, TS$STATIONS), mean, na.rm=TRUE)
                               Quantile <- tapply(TS$param, list(TS$MONTHS, TS$STATIONS), quantile, probs = (0.9), na.rm=TRUE)
                               Median <- tapply(TS$param, list(TS$MONTHS, TS$STATIONS), median, na.rm=TRUE)
                               Max <- tapply(TS$param, list(TS$MONTHS, TS$STATIONS), max, na.rm=TRUE)}}
                       else { if (time.step == "Semi-fortnight") {
                                  if (mix =="YES") {
                                      Mean <- tapply(TS$param, list(TS$YEARS, TS$week.month, TS$MONTHS), mean, na.rm=TRUE)
                                      Quantile <- tapply(TS$param, list(TS$YEARS, TS$week.month, TS$MONTHS), quantile, probs = (0.9), na.rm=TRUE)
                                      Median <- tapply(TS$param, list(TS$YEARS, TS$week.month, TS$MONTHS), median, na.rm=TRUE)
                                      Max <- tapply(TS$param, list(TS$YEARS, TS$week.month, TS$MONTHS), max, na.rm=TRUE) }
                                  else {
                                      Mean <- tapply(TS$param, list(TS$YEARS, TS$week.month, TS$MONTHS, TS$STATIONS), mean, na.rm=TRUE)
                                      Quantile <- tapply(TS$param, list(TS$YEARS, TS$week.month, TS$MONTHS, TS$STATIONS), quantile, probs = (0.9), na.rm=TRUE)
                                      Median <- tapply(TS$param, list(TS$YEARS, TS$week.month, TS$MONTHS, TS$STATIONS), median, na.rm=TRUE)
                                      Max <- tapply(TS$param, list(TS$YEARS, TS$week.month, TS$MONTHS, TS$STATIONS), max, na.rm=TRUE)}}     
                              else {return(print("You have to choose a time step to aggregate your data!"))}                                
  }}}}}
 }
#__________________________________________Choix de la methode d'aggregation avec les p.values du test Wilcoxon (options : help.aggreg et auto.aggreg)
 {
  if (help.aggreg == "YES" | auto.aggreg== "YES") {
 
      a <- data.frame(rep(NA, (max(TS$YEARS, na.rm=TRUE)+1-min(TS$YEARS[TS$YEARS > 0], na.rm=TRUE))*365.25))        # tableau vide avec nombre de jour total de la série regularisée 
      a <- data.frame(a, 1:nrow(a))                                                         # numerotation des lignes
      aa <- data.frame(TS$param[!is.na(TS$time)])                                           # on recupere les données parametres  
      aa <- data.frame(aa, TS$time[!is.na(TS$time)])                                        #     et l'on y accole les donnees temporelle
      regraw <- merge(a, aa, by.x = 2, by.y = 2, all = TRUE)                                # on fusionne les donnees brute (aa) et la serie temporelle
      zraw <- ts(regraw[, 3], start=min(TS$YEARS[TS$YEARS > 0], na.rm=TRUE), frequency=366) #     reguliere pour obtenir une serie regularisee (echelle de la journee) avec des NA
   
      v1 <- as.data.frame(Mean)                                                             # on recupere le tableau croise
      v2 <- as.data.frame(Quantile)
      v3 <- as.data.frame(Median)
      v4 <- as.data.frame(Max)
      v4[v4=="-Inf"]<-NA

      MEA <- reshape(v1, direction = "long", varying = list(1: ncol(v1)), times = colnames(v1))    # on le decroise
      QUA <- reshape(v2, direction = "long", varying = list(1: ncol(v2)), times = colnames(v2))
      MED <- reshape(v3, direction = "long", varying = list(1: ncol(v3)), times = colnames(v3))
      MAX <- reshape(v4, direction = "long", varying = list(1: ncol(v4)), times = colnames(v4))
 
      wcox1 <- wilcox.test(regraw[, 3], MEA[,2], alternative='two.sided', paired=FALSE)            # teste de wilcoxon entre les tableaux decroises 
      wcox2 <- wilcox.test(regraw[, 3], QUA[,2], alternative='two.sided', paired=FALSE)            #       et la serie de base regularisee
      wcox3 <- wilcox.test(regraw[, 3], MED[,2], alternative='two.sided', paired=FALSE)
      wcox4 <- wilcox.test(regraw[, 3], MAX[,2], alternative='two.sided', paired=FALSE)
 
      pMEA <- wcox1$p.value                                                                        # on recupere les p.values 
      pQUA <- wcox2$p.value
      pMED <- wcox3$p.value
      pMAX <- wcox4$p.value
 
      p.values <- rbind(pMEA,pQUA,pMED,pMAX)                                                       # tableau des p.values
 
      C1 <- "The means give better fit"
      C2 <- "The quantiles 0.9 give better fit"
      C3 <- "The medians give better fit"
      C4 <- "The maximums give better fit"

      choice <- rbind(C1,C2,C3,C4)                                                                 # tableau des differentes propositions

      R <- cbind(p.values,choice)
      r <- as.data.frame(R)                                                                        # fusion des tableaux
      r$V2 <- as.character(r$V2)                                                                   # on fignole le tableau...
      r$V1 <- as.character(r$V1)
      r$V1 <- as.numeric(r$V1)
      names(r)[c(1)] <- c("p.values")
      names(r)[c(2)] <- c("choice") }
  else{}

 # Proposition de la methode d'aggregation (option : help.aggreg)
 
  if (help.aggreg=="YES") {
      tkmessageBox(title = "Method of Aggregation Guidance", message = as.character(subset(r$choice, r$p.values==max(r$p.values))), icon = "info", type = "ok")
      return() }
  else{ }
 
 # Choix automatique de la methode d'aggregation (option :auto.aggreg)
 
  if (auto.aggreg == "YES") {
     if (which.max(r$p.values) == 1) { 
         aggreg <- "Mean" 
         Aggreg <- "Method of aggregation automatically use : mean" }
     else { if (which.max(r$p.values) == 2) { 
                aggreg <- "Quantile" 
                Aggreg <- "Method of aggregation automatically use : quantile(0.9)" }   
            else { if (which.max(r$p.values) == 3) { 
                       aggreg <- "Median" 
                       Aggreg <- "Method of aggregation automatically use : median" }
                   else { if (which.max(r$p.values) == 4) { 
                              aggreg <- "Max" 
                              Aggreg <- "Method of aggregation automatically use : maximum" }}}}
    
  tkinsert(Envir$txt,"end", paste("-Method of aggregation choice-","\n"))
  tktag.add(Envir$txt, "titre", "end -2 lines linestart","end -2 lines lineend")
  tkinsert(Envir$txt, "end", paste("Parameter: ", param , "   Site(s): ", liste.stations, sep="" , "\n")) 
  tkinsert(Envir$txt,"end", paste(Aggreg, "\n\n"))
  tksee (Envir$txt,"end")
           
 }} 
#_____________________________________________________________________________________________Remplacement des valeurs manquantes, option (na.replace) 
 {   
  v <- as.data.frame(get(aggreg))
  v[v=="-Inf"]<- NA
  v[v=="NaN"] <- NA

  if (na.replace=="YES") {
     wx <- v[-1, ]
   
     if (length(wx[is.na(wx)]) > ((length(wx))/20)) {                          # affiche un message de warning
         tkinsert(Envir$txt, "end", paste("-!Warning message!-", "\n"))
         tktag.add(Envir$txt, "titre2", "end -2 lines linestart","end -2 lines lineend") 
         tktag.configure(Envir$txt, "titre2", foreground="red", font=tkfont.create(family="courier",size=9,weight="bold")) 
         tkinsert(Envir$txt, "end", paste("Missing values represent more that 1/20 of the regularised data,", "\n"))
         tkinsert(Envir$txt, "end", paste("replacing them is not a good idea.", "\n\n"))
         tksee (Envir$txt,"end") 
         tkmessageBox(message=paste("Missing values represent more that 5% of the regularised data,", "\n", "replacing them is not a good idea.", sep=""), icon = "warning", type = "ok", title="!Warning!") }
     else{} 
     
     cc <- matrix(NA, nrow(v), ncol(v))                                            # matrice vide à remplir
     vc <- v
     vc[is.na(vc)] <- -5000                                                        # remplacement des NA par la valeur -5000 (evite les problemes avec les NA et les 0)
     ccc <- impute(v, what=c("median"))                                            ## impute = package 'e1071' ##
     
     if (time.step=="Annual" | time.step=="Mono-mensual") {
        for (i in 1:(nrow(v)-2)) {
           if ((vc[i,]+vc[i+1,]+vc[i+2,])== -15000) {                              # pour chaque serie de 3 valeurs successives dans vc, si leur somme = -15000 (valeurs manquantes) alors 
                cc[i,] <- ccc[i,] }                                                # la premiere donnee de cette serie est remplacee par la valeur correspondante dans la matrice des medianes
           else { cc[i,] <- v[i,] } } 
        cc[nrow(v)-1, ] <- v[nrow(v)-1, ]
        cc[nrow(v), ] <- v[nrow(v), ] } 
     
     else {                                                                        # matrice avec valeurs remplacees par la methode des medianes/cycle
        for (i in 1:nrow(v)) {
           for (j in 1:(ncol(v)-2)) {
              if ((vc[i,j]+vc[i,j+1]+vc[i,j+2])== -15000) {                        # pour chaque serie de 3 valeurs successives dans vc, si leur somme = -15000 (valeurs manquantes) alors 
                  cc[i,j] <- ccc[i,j] }                                            # la premiere donnee de cette serie est remplacee par la valeur correspondante dans la matrice des medianes
              else { cc[i,j] <- v[i,j] } } }                                       # sinon la valeur est laissee telle quelle (meme si c'est une valeurs manquante)                                                                                 
        cc[ , ncol(v)-1] <- v[ , ncol(v)-1]
        cc[ , ncol(v)] <- v[ , ncol(v)]    }
  
     if (is.na(cc[1,1])) {                              
         cc[2,1] <- ccc[2,1] }                            
     else{}                                             
     if (is.na(cc[nrow(cc),ncol(cc)])) {                
         cc[nrow(cc),ncol(cc)] <- ccc[nrow(cc),ncol(cc)] }
     else{}                                             
     
     row.names(cc) <- row.names(v)
     v <- as.data.frame(cc) 
     names(v) <- names(vc) }
  
  else { v <- as.data.frame(v) }
  
 }
#_____________________________________________________________________________Re-formation du tableau de donnees regularisees en fonction du time.step
 {
  TimeSerie <- reshape(v, direction = "long", varying = list(1: ncol(v)), times = colnames(v))        # Transpose le tableau croise en tableau colonne
 
  if (time.step == "Daily"){
  
  TimeSerie$DayYears <- do.call("rbind", strsplit(TimeSerie$time,"\\.")) [,1]
  TimeSerie$DayYears <- as.numeric(TimeSerie$DayYears)
          
  if (mix == "NO") {
  TimeSerie$STATIONS <- do.call("rbind", strsplit(TimeSerie$time,"\\.")) [,2]}
  else {}
   
  TimeSerie$time <- NULL
  names(TimeSerie)[c(1)] <- c("param")
  names(TimeSerie)[c(2)] <- c("YEARS")
  Y <- as.numeric(row.names(v))
  TimeSerie$YEARS <- c(Y)
   
  if (mix == "YES") {
  TimeSerie <- TimeSerie[order(TimeSerie$YEARS, TimeSerie$DayYears), ]}
  else {
  TimeSerie <- TimeSerie[order(TimeSerie$STATIONS, TimeSerie$YEARS, TimeSerie$DayYears), ]}

  TimeSerie <- TimeSerie[TimeSerie$YEARS!=(-1000), ]
  row.names(TimeSerie) <- 1:(nrow(TimeSerie))
  TimeSerie$time <- 1:(nrow(TimeSerie))
  
  Ja <- as.matrix(rep(1, 31))
  Fe <- as.matrix(rep(2, 29))
  Ma <- as.matrix(rep(3, 31))
  Av <- as.matrix(rep(4, 30))
  My <- as.matrix(rep(5, 31))
  Ju <- as.matrix(rep(6, 30))
  Jl <- as.matrix(rep(7, 31))
  Ao <- as.matrix(rep(8, 31))
  Se <- as.matrix(rep(9, 30))
  Oc <- as.matrix(rep(10, 31))
  No <- as.matrix(rep(11, 30))
  De <- as.matrix(rep(12, 31))
  Mois <- rbind(Ja,Fe,Ma,Av,My,Ju,Jl,Ao,Se,Oc,No,De)

  TimeSerie$MONTHS <- rep(Mois, max(TimeSerie$YEARS)+1-min(TimeSerie$YEARS)) 
  if (mix == "YES") {
  TimeSerie <- subset(TimeSerie, select=c(param, YEARS, MONTHS, DayYears, time)) }
  else { TimeSerie <- subset(TimeSerie, select=c(STATIONS, param, YEARS, MONTHS, DayYears, time)) } }
  
 else { if (time.step=="Annual"){

   if (mix == "NO") {
   TimeSerie$STATIONS <- do.call("rbind", strsplit(TimeSerie$time,"\\.")) [,1]}
   else {}

   TimeSerie$time <- NULL
   names(TimeSerie)[c(1)] <- c("param")
   names(TimeSerie)[c(2)] <- c("YEARS")
   Y <- as.numeric(row.names(v))
   TimeSerie$YEARS <- c(Y)

   if (mix == "YES") {
   TimeSerie <- TimeSerie[order(TimeSerie$YEARS), ]}
   else {
   TimeSerie <- TimeSerie[order(TimeSerie$STATIONS, TimeSerie$YEARS), ]}

   TimeSerie <- TimeSerie[TimeSerie$YEARS!=(-1000), ]
   row.names(TimeSerie) <- 1:(nrow(TimeSerie))
   TimeSerie$time <- 1:(nrow(TimeSerie))}
 
  else { if (time.step == "Mensual"){
          TimeSerie$MONTHS <- do.call("rbind", strsplit(TimeSerie$time,"\\.")) [,1]
          TimeSerie$MONTHS <- as.numeric(TimeSerie$MONTHS)

          if (mix == "NO") {
          TimeSerie$STATIONS <- do.call("rbind", strsplit(TimeSerie$time,"\\.")) [,2]}
          else {}

          TimeSerie$time <- NULL
          names(TimeSerie)[c(1)] <- c("param")
          names(TimeSerie)[c(2)] <- c("YEARS")
          Y <- as.numeric(row.names(v))
          TimeSerie$YEARS <- c(Y)

          if (mix == "YES") {
          TimeSerie <- TimeSerie[order(TimeSerie$YEARS, TimeSerie$MONTHS), ]}
          else {
          TimeSerie <- TimeSerie[order(TimeSerie$STATIONS, TimeSerie$YEARS, TimeSerie$MONTHS), ]}

          TimeSerie <- TimeSerie[TimeSerie$YEARS!=(-1000), ]
          row.names(TimeSerie) <- 1:(nrow(TimeSerie))
          TimeSerie$time <- 1:(nrow(TimeSerie))}
          
              else { if (time.step=="Fortnight"){
                     TimeSerie$MONTHS <- do.call("rbind", strsplit(TimeSerie$time,"\\.")) [,2]
                     TimeSerie$week.month <- do.call("rbind", strsplit(TimeSerie$time,"\\.")) [,1]
                     TimeSerie$MONTHS <- as.numeric(TimeSerie$MONTHS)

                     if (mix == "NO") {
                     TimeSerie$STATIONS <- do.call("rbind", strsplit(TimeSerie$time,"\\.")) [,3]}
                     else {}

                     TimeSerie$week.month <- as.numeric(TimeSerie$week.month)
                     TimeSerie$time <- NULL
                     names(TimeSerie)[c(1)] <- c("param")
                     names(TimeSerie)[c(2)] <- c("YEARS")
                     Y <- as.numeric(row.names(v))
                     TimeSerie$YEARS <- c(Y)

                     if (mix == "YES") {
                     TimeSerie <- TimeSerie[order(TimeSerie$YEARS, TimeSerie$MONTHS), ]}
                     else {
                     TimeSerie <- TimeSerie[order(TimeSerie$STATIONS, TimeSerie$YEARS, TimeSerie$MONTHS), ]}

                     TimeSerie <- TimeSerie[TimeSerie$YEARS!=(-1000), ]
                     row.names(TimeSerie) <- 1:(nrow(TimeSerie))
                     TimeSerie$time <- 1:(nrow(TimeSerie))
                     TimeSerie$week.year <- rep(1:24, length(yy))}
                     
                        else { if (time.step=="Semi-fortnight") {
                                TimeSerie$MONTHS <- do.call("rbind", strsplit(TimeSerie$time,"\\.")) [,2]
                                TimeSerie$week.month <- do.call("rbind", strsplit(TimeSerie$time,"\\.")) [,1]
                                TimeSerie$MONTHS <- as.numeric(TimeSerie$MONTHS)

                                if (mix == "NO") {
                                TimeSerie$STATIONS <- do.call("rbind", strsplit(TimeSerie$time,"\\.")) [,3]}
                                else {}

                                TimeSerie$week.month <- as.numeric(TimeSerie$week.month)
                                TimeSerie$time <- NULL
                                names(TimeSerie)[c(1)] <- c("param")
                                names(TimeSerie)[c(2)] <- c("YEARS")
                                Y <- as.numeric(row.names(v))
                                TimeSerie$YEARS <- c(Y)

                                if (mix == "YES") {
                                TimeSerie <- TimeSerie[order(TimeSerie$YEARS, TimeSerie$MONTHS), ]}
                                else {
                                TimeSerie <- TimeSerie[order(TimeSerie$STATIONS, TimeSerie$YEARS, TimeSerie$MONTHS), ]}

                                TimeSerie <- TimeSerie[TimeSerie$YEARS!=(-1000), ]
                                row.names(TimeSerie) <- 1:(nrow(TimeSerie))
                                TimeSerie$time <- 1:(nrow(TimeSerie))
                                TimeSerie$week.year <- rep(1:48, length(yy))}

                                   else { if (time.step == "Mono-mensual") {
                                               if (mix == "YES") {
                                               TimeSerie$time <- NULL
                                               names(TimeSerie)[c(1)] <- c("param")
                                               names(TimeSerie)[c(2)] <- c("MONTHS") }

                                               else {
                                               names(TimeSerie)[c(1)] <- c("STATIONS")
                                               names(TimeSerie)[c(2)] <- c("param")
                                               names(TimeSerie)[c(3)] <- c("MONTHS") }

                                               row.names(TimeSerie) <- 1:(nrow(TimeSerie))
                                               TimeSerie$time <- 1:(nrow(TimeSerie))}
                                               
                                          else {return(print("No otpions!"))}
                                         }
                                }
                      }
     }      }
 } 
#_________________________________________________________________________________________________________________________Selection des mois a traiter  
 {
  if (time.step == "Mono-mensual") { F <- 1 }  else {
      F <- length(TimeSerie$param[TimeSerie$YEARS == min(TimeSerie$YEARS)]) }           # on garde la frequence de la serie de base
  if (any(colnames(TimeSerie) == "MONTHS")) {
      TimeSerie <- subset(TimeSerie, MONTHS %in% months, drop = TRUE) }                 # on extrait les mois voulu
  else{}
 }
#__________________________________________________________________________________________________________________Creation de la serie temporelle 'z'
 {          
 # choix de la frequence en fonction du time.step et des mois selectionnes
           
  if (time.step=="Mono-mensual") {
      start.year <- min(TimeSerie$MONTHS) }
  else { start.year <- min(TimeSerie$YEARS) }
 
  if (time.step=="Mono-mensual") {
      freq <- 1 }
  else {
      freq <- 1/F }
 
  if (!is.null(months) & any(colnames(TimeSerie) == "MONTHS")) {
      freq <- 1/((F/12)*(length(months))) }
  else{}                 
 }
 # construction de la serie temporelle et remplacement des dernieres valeurs manquantes par regression lineaire
 { 
  if (mix=="YES") {
     z <- ts(as.numeric(TimeSerie$param), start = (start.year), deltat = freq)
     z1 <- z
       if (na.replace=="YES") {
         z <- interpNA(z, "linear")                                              # interpolation des NA restant en faisant une regression lineaire entre les valeur avant et apres
         z <- ts(z, start = (start.year), deltat = freq)
         z1 <- z                                                                 # garde la serie avec NA au début et à la fin (z1) pour afficher le tableau (evite les problemes de longueur de colonne) 
         z <- na.omit(z, method = c("ir"), interp = c("linear"))                 # supprime les NA en debut et fin de serie temporelle, la nouvelle serie commence a n+1 et finie a n-1.
         z <- ts(as.numeric(z), start = (start.year), deltat = freq) }           # on reconstruit la serie pour eliminer les probemes de serie temporelle multivariee                                                                        
       else{}    
       if (plotZ == "YES") {
         dir.create(paste(Envir$save.WD,"/",Envir$File.Name,"/", start,"-", end, "/", param, "/", "na.", na.replace, "-", "out.", outliers.re, "/", time.step, "-", aggreg, "/", sep= ""), recursive = TRUE)
         save.figure.path <- paste(Envir$save.WD,"/",Envir$File.Name,"/", start,"-", end, "/", param, "/", "na.", na.replace, "-", "out.", outliers.re, "/", time.step, "-", aggreg, "/", Envir$File.Name, "_TimeSeries_",param,".png", sep = "")
         png(save.figure.path)
         plot(z, ylab=paste(param, "concentration"), xlab="Years", main=paste("Regularised Time Series of",param,"concentration","\n"))
         title(main=paste("\n\n", "at station(s): ", liste.stations, "\n", "Time step: ", time.step, "   Method of aggregation: ", aggreg), cex.main=0.8)
         minor.tick(nx=5, ny=2, tick.ratio=0.4)  
         dev.off() 
         plot(z, ylab=paste(param, "concentration"), xlab="Years", main=paste("Regularised Time Series of",param,"concentration","\n"))
         title(main=paste("\n\n", "at station(s): ", liste.stations, "\n", "Time step: ", time.step, "   Method of aggregation: ", aggreg), cex.main=0.8)} 
       else{ } }
  else{ tkmessageBox(message=paste("Cannot build a time series if stations are dissociate", "\n\n", "only Table and Summary are available", "\n\n", "OK to continue", sep=""), icon = "warning", type = "ok", title="!Warning!")  }
 }            
#____________________________________________________________________________________Construction du tableau de donnees regularisees avec NA remplaces
 {
  if (mix == "YES") {
     Regularised.Data <- TimeSerie                      
     Param <- as.numeric(z1)                            
     Regularised.Data$param <- Param                    # tableau de donnees avec NA remplaces
     Regularised.data <- Regularised.Data               # on garde la serie avec le nom inchange pour les tests qui ont besoin de la colone standard 'param'
     Name <- names(Regularised.Data)
     Name[1] <- param 
     colnames(Regularised.Data) <- Name }
 
  else { Regularised.Data <- TimeSerie
     Regularised.data <- Regularised.Data 
     Name <- names(Regularised.Data)
     Name[1] <- param 
     colnames(Regularised.Data) <- Name } 
 
  if (datashow=="YES") { showData(Regularised.Data)                                                                      # affiche et sauve le tableau
     dir.create(paste(Envir$save.WD,"/",Envir$File.Name,"/", start,"-", end, "/", param, "/", "na.", na.replace, "-", "out.", outliers.re, "/", time.step, "-", aggreg, "/", sep= ""), recursive = TRUE)
     save.regdata.path <- paste(Envir$save.WD,"/",Envir$File.Name,"/", start,"-", end, "/", param, "/", "na.", na.replace, "-", "out.", outliers.re, "/", time.step, "-", aggreg, "/", Envir$File.Name, "_Regularised_data_",param,".csv", sep = "") 
     write.csv2(Regularised.Data, row.names=FALSE, file=save.regdata.path) }
 
  else{ dir.create(paste(Envir$save.WD,"/",Envir$File.Name,"/", start,"-", end, "/", param, "/", "na.", na.replace, "-", "out.", outliers.re, "/", time.step, "-", aggreg, "/", sep= ""), recursive = TRUE)
        save.regdata.path <- paste(Envir$save.WD,"/",Envir$File.Name,"/", start,"-", end, "/", param, "/", "na.", na.replace, "-", "out.", outliers.re, "/", time.step, "-", aggreg, "/", Envir$File.Name, "_Regularised_data_",param,".csv", sep = "")      # sauve uniquement le tableau
        write.csv2(Regularised.Data, row.names=FALSE, file=save.regdata.path) }
 }
#______________________________________________________________________________________________________Statistique desciptive sur la serie régularisee
 {  
  if (resume.reg=="YES") {                                                                   
    summary <- capture.output(summary(Regularised.data$param))
    tkinsert(Envir$txt2, "end", paste("-Desciptive statistics on regularised data-"
                                 , "\n", "Parameter: ", param , "   Site(s): ", liste.stations, "\n"
                                 , "Outliers.removed: ", outliers.re, "   NA.replaced: ", na.replace, "\n"
                                 , "Time.step: ", time.step, "   Method: ", aggreg, sep="" , "\n\n")) 
    tktag.add(Envir$txt2, "titre", "end -6 lines linestart","end -6 lines lineend")
    tktag.configure(Envir$txt2, "titre", font=tkfont.create(family="courier",size=9,weight="bold"))
    summary <- paste(summary,collapse="\n")
    tkinsert(Envir$txt2,"end",paste(summary, "\n\n"))
    tkinsert(Envir$txt2,"end", paste("Data quantity (",param,") : ", length(Regularised.data$param[!is.na(Regularised.data$param)]), "\n\n", sep=""))
    tksee (Envir$txt2,"end") 
    
    summary <- as.matrix(summary(Regularised.data$param))
    summary <- as.data.frame(summary)
    names(summary) <- param
    return(showData(summary)) }
    
  else{}
 }
#___________________________________________________________________________________Test de normalite sur la serie temporelle (option: test.normality)
 { 
  if (test.normality == "YES") {
      shap <- shapiro.test(z)                                                                  
      shap.p <- shap$p.value                                                             # extrait la valeur de p du test Shapiro et la compare au niveau de significativite
      shap.stat <- round(shap$statistic, digits = 4)  
      if (shap.p <= 0.05) {  
          tkinsert(Envir$txt2, "end", paste("-Shapiro-Wilk normality test result-", "\n"))
          tktag.add(Envir$txt2, "titre", "end -2 lines linestart","end -2 lines lineend")
          tktag.configure(Envir$txt2, "titre", font=tkfont.create(family="courier",size=9,weight="bold"))
          tkinsert(Envir$txt2, "end", paste("Parameter: ", param , "   Site(s): ", liste.stations, "\n"
                                      , "Outliers.removed: ", outliers.re, "   NA.replaced: ", na.replace, "\n"
                                      , "Time.step: ", time.step, "   Method: ", aggreg, sep="" , "\n\n"))                   
          tkinsert(Envir$txt2, "end", paste("Normality of the distribution is rejected","\n"))
          tkinsert(Envir$txt2, "end", paste("W =", shap.stat, "  p-value =",shap.p, "\n\n"))
          tksee (Envir$txt2,"end") 
          
          tkmessageBox (title="Shapiro-Wilk normailty test result",
                  icon = "info" ,
                  message = "Normality of the distribution is rejected",
                  detail = paste("W =", shap.stat, "p-value =",round(shap.p, 4)) ,
                  type = "ok")     }
       else{ 
          tkinsert(Envir$txt2, "end", paste("-Shapiro-Wilk normality test result-", "\n"))
          tktag.add(Envir$txt2, "titre", "end -2 lines linestart","end -2 lines lineend")
          tktag.configure(Envir$txt2, "titre", font=tkfont.create(family="courier",size=9,weight="bold"))
          tkinsert(Envir$txt2, "end", paste("Parameter: ", param , "   Site(s): ", liste.stations, "\n"
                                      , "Outliers.removed: ", outliers.re, "   NA.replaced: ", na.replace, "\n"
                                      , "Time.step: ", time.step, "   Method: ", aggreg, sep="" , "\n\n"))                  
          tkinsert(Envir$txt2, "end", paste("Normality of the distribution cannot be rejected","\n\n"))
          tkinsert(Envir$txt2, "end", paste("W =", shap.stat, "  p-value =",shap.p, "\n\n"))
          tksee (Envir$txt2,"end") 
          
          tkmessageBox (title="Shapiro-Wilk normailty test result",
                  icon = "info" ,
                  message = "Normality of the distribution cannot be rejected",
                  detail = paste("W =", shap.stat, "p-value =",round(shap.p, 4)) ,
                  type = "ok")   }}
  else{ } 
 }
#___________________________________________________Affichage du diagramme d'autocorrelation et du spectre de Fourier (options : autocorr et spectrum)
 { 
  if (autocorr == "YES") { 
      if (mix == "YES") {
          acf(z, lag.max = ((nrow(TimeSerie))/2), na.action = na.pass)
             dir.create(paste(Envir$save.WD,"/",Envir$File.Name,"/", start,"-", end, "/", param, "/", "na.", na.replace, "-", "out.", outliers.re, "/", time.step, "-", aggreg, "/", sep= ""), recursive = TRUE) 
             save.autocor.path <- paste(Envir$save.WD,"/",Envir$File.Name,"/", start,"-", end, "/", param, "/", "na.", na.replace, "-", "out.", outliers.re, "/", time.step, "-", aggreg, "/", Envir$File.Name, "_AutoCorr_",param,".png", sep = "")
             png(save.autocor.path)
             acf(z, lag.max = ((nrow(TimeSerie))/2), na.action = na.pass, main=paste("Regularised Time Series of",param,"\n"))
             title(main=paste("\n\n\n\n\n", "at station(s): ", liste.stations), cex.main=0.8, outer=TRUE)  
             dev.off() 
          }
      else{ return(tkmessageBox(message="Cannot perform autocorelation if stations are dissociate", icon = "warning", type = "ok", title="!Warning!")) } }
  else{} 
  
 # !! Les spectres de Fourier ne supporte pas les valeurs manquantes !!
 
  if (spectrum == "YES") {
      if (mix == "YES") {
          if (length(z[is.na(z)]) == 0) {                                               # ne fait pas de spectre si z contient des valeurs manquantes
              spectrum(z, spans=c(3,5))
                 dir.create(paste(Envir$save.WD,"/",Envir$File.Name,"/", start,"-", end, "/", param, "/", "na.", na.replace, "-", "out.", outliers.re, "/", time.step, "-", aggreg, "/", sep= ""), recursive = TRUE) 
                 save.spectrum.path <- paste(Envir$save.WD,"/",Envir$File.Name,"/", start,"-", end, "/", param, "/", "na.", na.replace, "-", "out.", outliers.re, "/", time.step, "-", aggreg, "/", Envir$File.Name, "_Spectrum_",param,".png", sep = "")
                 png(save.spectrum.path)
                 spectrum(z, spans=c(3,5), main=paste("Regularised Time Series of",param, "\n\n"))
                 title(main=paste("\n\n\n\n\n\n", "at station(s): ", liste.stations), cex.main=0.8, outer=TRUE)  
                 dev.off() 
              }
          else { return(tkmessageBox(message="Cannot perform spetrum alnaysis : missing values in time series", icon = "warning", type = "ok", title="!Warning!"))} }
      else { return(tkmessageBox(message="Cannot perform spectrum if stations are dissociate", icon = "warning", type = "ok", title="!Warning!")) } } 
  else{}
 }
#_________________________________________________________________________________________________________Montre un color plot des anomalies par annee  
 {
  if (anomaly=="YES") {
      if (mix == "YES") {
          if (time.step == "Mensual") {
              cc <- NULL                
              for (i in months) {          
                   l <- Regularised.data$param[Regularised.data$MONTHS==i]                                 # prend toutes les données au mois i
                   c <- l-(median(l, na.rm=TRUE))                                                          # soustrait la mediane de l'ensemble de ces données à chaque donnée
                   cc <- cbind(cc, c) }                                                                    # superpose les données obetnues pour chaque mois
              x <- (min(Regularised.data$YEARS):max(Regularised.data$YEARS))
              y <- (1:length(months))
              filled.contour(x,y,cc, nlevels=64, col=timPalette(64) , xlab="YEARS", ylab="MONTHS", main=paste("Time series anomaly of", param, "\n\n"))
              title(main=paste("\n\n\n\n\n\n", "at station(s): ", liste.stations), cex.main=0.8, outer=TRUE) 
              dir.create(paste(Envir$save.WD,"/",Envir$File.Name,"/", start,"-", end, "/", param, "/", "na.", na.replace, "-", "out.", outliers.re, "/", time.step, "-", aggreg, "/", sep= ""), recursive = TRUE)               
              save.colorplot.path <- paste(Envir$save.WD,"/",Envir$File.Name,"/", start,"-", end, "/", param, "/", "na.", na.replace, "-", "out.", outliers.re, "/", time.step, "-", aggreg, "/", Envir$File.Name, "_ColorPlot_",param,".png", sep = "")
              png(save.colorplot.path) 
              filled.contour(x,y,cc, nlevels=64, col=timPalette(64) , xlab="YEARS", ylab="MONTHS", main=paste("Time series anomaly of", param, "\n\n"))
              title(main=paste("\n\n\n\n\n\n", "at station(s): ", liste.stations), cex.main=0.8, outer=TRUE)
              dev.off() }
              
          else { 
              if (time.step == "Fortnight" | time.step =="Semi-fortnight") {
              cc <- NULL                                      
              for (i in levels(as.factor(Regularised.data$week.year))) {                               
                   l <- Regularised.data$param[Regularised.data$week.year==i]       
                   c <- l-(median(l, na.rm=TRUE))                    
                   cc <- cbind(cc, c) }                            
              x <- (min(Regularised.data$YEARS):max(Regularised.data$YEARS))
              y <- (1:length(levels(as.factor(Regularised.data$week.year))))                                     
              filled.contour(x,y,cc, nlevels=64, col=timPalette(64) , xlab="YEARS", ylab="WEEKS", main=paste("\n\n","Time series anomaly of" , param, "\n\n"))
              title(main=paste("\n\n\n\n\n\n", "at station(s): ", liste.stations), cex.main=0.8, outer=TRUE) 
              dir.create(paste(Envir$save.WD,"/",Envir$File.Name,"/", start,"-", end, "/", param, "/", "na.", na.replace, "-", "out.", outliers.re, "/", time.step, "-", aggreg, "/", sep= ""), recursive = TRUE)               
              save.colorplot.path <- paste(Envir$save.WD,"/",Envir$File.Name,"/", start,"-", end, "/", param, "/", "na.", na.replace, "-", "out.", outliers.re, "/", time.step, "-", aggreg, "/", Envir$File.Name, "_ColorPlot_",param,".png", sep = "")
              png(save.colorplot.path)
              filled.contour(x,y,cc, nlevels=64, col=timPalette(64) , xlab="YEARS", ylab="WEEKS", main=paste("\n\n","Time series anomaly of" , param, "\n\n"))
              title(main=paste("\n\n\n\n\n\n", "at station(s): ", liste.stations), cex.main=0.8, outer=TRUE)
              dev.off() } 
              else { return(tkmessageBox(message="Cannot perform anomaly color.plot with monomens, yearly or daily time step", icon = "warning", type = "ok", title="!Warning!")) } } } 
      else { return(tkmessageBox(message="Cannot perform spectrum if stations are dissociate", icon = "warning", type = "ok", title="!Warning!")) } }
  else{}
 }
#____________________________________________________________________________________________________Montre la courbe detendence ainsi que les residus
 {
  if (zsmooth=="YES")  { 
      if (length(z[is.na(z)])==0) {
          a <- stl(z, s.window="periodic", na.action=na.fail)
          plot(a)
            dir.create(paste(Envir$save.WD,"/",Envir$File.Name,"/", start,"-", end, "/", param, "/", "na.", na.replace, "-", "out.", outliers.re, "/", time.step, "-", aggreg, "/", sep= ""), recursive = TRUE)
            save.stl.path <- paste(Envir$save.WD,"/",Envir$File.Name,"/", start,"-", end, "/", param, "/", "na.", na.replace, "-", "out.", outliers.re, "/", time.step, "-", aggreg, "/", Envir$File.Name, "_Deseasonal_",param,".png", sep = "")
            png(save.stl.path)
            plot(a)
            dev.off()
          a$time.series[,3] }
      else { return(tkmessageBox(message="Cannot detrend with missing values", icon = "warning", type = "ok", title="!Warning!")) } }       
  else { } 
 } 
#________________________________Affichage de la courbe des sommes cumulees pour reperer la tendance generale et les 'cassures' (option : local.trend) 
 {      
  if (local.trend == "YES") {
      if (test != "MK" & test!= "SMK") { return(tkmessageBox(message="Choose a Kendall family test", icon = "warning", type = "ok", title="!Warning!")) } 
       z <- interpNA(z, "linear")                                                # interpolation des NA restant en faisant une regression lineaire entre les valeur avant et apres
       z <- ts(as.numeric(z), start = (start.year), deltat = freq)              
      Pna <- na.omit(z)
      w <- local.trend(Pna)                                                      ## local.trend = package 'pastecs' ##
      pos <- identify(w)
      ww <- pos$pos
      N <- length(ww)-1
      periods <- list()
      lc.mk <- list()
      lc.mk2 <- list()
      dir.create(paste(Envir$save.WD,"/",Envir$File.Name,"/", start,"-", end, "/", param, "/", "na.", na.replace, "-", "out.", outliers.re, "/", time.step, "-", aggreg, "/", sep= ""), recursive = TRUE)
    
    # Limite de detection des test Kendall (periodes de plus de 1 an minimum)
    LimitD <- ww[2:length(ww)]-ww[1:N]
    if (time.step == "Daily") { if (LimitD<=F) { return(tkmessageBox(message="Selected periods should be longer than 1 year", icon = "warning", type = "ok", title="!Warning!")) } else {} }
    else { if (time.step == "Annual") { if (LimitD<=F) { return(tkmessageBox(message="Selected periods should be longer than 1 year", icon = "warning", type = "ok", title="!Warning!")) } else {} }
           else{ if (time.step == "Mensual") { if (LimitD<=F) { return(tkmessageBox(message="Selected periods should be longer than 1 year", icon = "warning", type = "ok", title="!Warning!")) } else {} } 
                 else{ if (time.step == "Semi-fortnight") { if (LimitD<=F) { return(tkmessageBox(message="Selected periods should be longer than 1 year", icon = "warning", type = "ok", title="!Warning!")) } else {} } 
                       else{ if (time.step == "Fortnight") { if (LimitD<=F) { return(tkmessageBox(message="Selected periods should be longer than 1 year", icon = "warning", type = "ok", title="!Warning!")) } else {} } 
                             else{ if (time.step == "Mono-mensual") { if (LimitD<=F) { return(tkmessageBox(message="Selected periods should be longer than 1 year", icon = "warning", type = "ok", title="!Warning!")) } else {} } }}}}}
      
    # Suivi du test de detection de la tendance effectue sur chaque portion de la serie temporelle idenitfiee avec cusum (option : test)
  
    if (time.step == "Annual"){ for (i in 1:N) {
    periods[i] <- list(ts(Regularised.data$param[(ww[i]):(ww[i+1])], start = c(Regularised.data$YEARS[nrow=ww[i]]) , deltat = freq)) }}
    else { if (time.step == "Mensual"){ for (i in 1:N) {
      periods[i] <- list(ts(Regularised.data$param[(ww[i]):(ww[i+1])], start = c((Regularised.data$YEARS[nrow=ww[i]]), (Regularised.data$MONTHS[nrow=ww[i]])) , deltat = freq)) }}
        else { if (time.step == "Semi-fortnight") { for (i in 1:N) {
          periods[i] <- list(ts(Regularised.data$param[(ww[i]):(ww[i+1])], start = c((Regularised.data$YEARS[nrow=ww[i]]),(Regularised.data$week.year[nrow=ww[i]])) , deltat = freq)) }}
            else { if (time.step == "Fortnight") {  for (i in 1:N) {
            periods[i] <- list(ts(Regularised.data$param[(ww[i]):(ww[i+1])], start = c((Regularised.data$YEARS[nrow=ww[i]]),(Regularised.data$week.year[nrow=ww[i]])) , deltat = freq)) }}
                else { if (time.step == "Mono-mensual") { for (i in 1:N) {
                    periods[i] <- list(ts(Regularised.data$param[(ww[i]):(ww[i+1])], start = c(Regularised.data$MONTHS[nrow=ww[i]]) , deltat = freq)) }}
                       else { if (time.step == "Daily") { for (i in 1:N) {
                          periods[i] <- list(ts(Regularised.data$param[(ww[i]):(ww[i+1])], start = c((Regularised.data$YEARS[nrow=ww[i]]), (Regularised.data$DayYears[nrow=ww[i]])) , deltat = freq)) }}}}}}}
 
 
    if (test == "MK") { tkinsert(Envir$txt2,"end", paste("-Local trend results (global)-", "\n"))
                        tktag.add(Envir$txt2, "titre", "end -2 lines linestart","end -2 lines lineend")
                        tktag.configure(Envir$txt2, "titre", font=tkfont.create(family="courier",size=9,weight="bold"))
                        tkinsert(Envir$txt2, "end", paste( "Parameter: ", param , "   Site(s): ", liste.stations, "\n"
                                                    , "Outliers.removed: ", outliers.re, "   NA.replaced: ", na.replace, "\n"
                                                    , "Time.step: ", time.step, "   Method: ", aggreg, sep="" , "\n\n"))
                        tksee (Envir$txt2,"end")                             
                        for (i in 1:N) { lc.mk[i] <- lapply(periods[[i]], seaKen)
                                         save.localMK.path <- paste(Envir$save.WD,"/",Envir$File.Name,"/", start,"-", end, "/", param, "/", "na.", na.replace, "-", "out.", outliers.re, "/", time.step, "-", aggreg, "/", Envir$File.Name, "_Local_Global Trend_",param,"_"
                                                                    , start(periods[[i]])[1], ",", start(periods[[i]])[2], "-", end(periods[[i]])[1], ",", end(periods[[i]])[2], ".csv", sep = "")
                                         write.csv2(lc.mk[i], row.names=FALSE, file=save.localMK.path)
                                         tkinsert(Envir$txt2,"end", paste("Period ", i, " :  ", start(periods[[i]])[1], ",", start(periods[[i]])[2], " - ", end(periods[[i]])[1], ",", end(periods[[i]])[2]
                                                  , "\n", "Trend (sen.slope): ", round(lc.mk[[i]]$sen.slope, 4), "  original units per year"
                                                  , "\n", "%Trend (sen.slope.pct): ", round(lc.mk[[i]]$sen.slope.pct, 4), "  percent of mean quantity per year", "\n"
                                                  , "p.value: ", round(lc.mk[[i]]$p.value, 4)
                                                  , "\n", "Ref. value of the complete series: ",round(pos$k, 4), "   Mean trend compare to ref. value: ", round(pos$trends[i], 4),"\n\n", sep="")) 
                                         if (lc.mk[[i]]$p.value <= 0.05) { tktag.add(Envir$txt2, "titre3", "end -4 lines linestart","end -4 lines lineend") 
                                                                           tktag.configure(Envir$txt2, "titre3", font=tkfont.create(family="courier",size=9,weight="bold"))} else {} }}
       else { if (test == "SMK") { tkinsert(Envir$txt2,"end", paste("-Local trend results (seasonal)-", "\n"))
                                   tktag.add(Envir$txt2, "titre", "end -2 lines linestart","end -2 lines lineend")
                                   tktag.configure(Envir$txt2, "titre", font=tkfont.create(family="courier",size=9,weight="bold"))
                                   tkinsert(Envir$txt2, "end", paste("Parameter: ", param , "   Site(s): ", liste.stations, "\n"
                                                               , "Outliers.removed: ", outliers.re, "   NA.replaced: ", na.replace, "\n"
                                                               , "Time.step: ", time.step, "   Method: ", aggreg, "\n\n"
                                                               , "trend (Theil-Sen slope) = original units per year", "\n"
                                                               , "%trend = percent of mean quantity per year", "\n"
                                                               , "missing = proportion of missing slopes", "\n\n", sep=""))
                                   tksee (Envir$txt2,"end")                            
                                   for (i in 1:N) { lc.mk[i] <- lapply(periods[[i]], seasonTrend)
                                                    lc.mk2[i] <- lapply(periods[[i]], seasonTrend, type = c("slope.pct"))
                                                    smk <- cbind(lc.mk[[i]][-5], lc.mk2[[i]][1])
                                                    names(smk)[5]<- "%trend"
                                                    save.localSMK.path <- paste(Envir$save.WD,"/",Envir$File.Name,"/", start,"-", end, "/", param, "/", "na.", na.replace, "-", "out.", outliers.re, "/", time.step, "-", aggreg, "/", Envir$File.Name, "_Local_Seasonal Trend_",param,"_"
                                                                               , start(periods[[i]])[1], ",", start(periods[[i]])[2],"-", end(periods[[i]])[1], ",", end(periods[[i]])[2], ".csv", sep = "")
                                                    write.csv2(smk, row.names=FALSE, file=save.localSMK.path) 
                                                    Lc.Mk <- paste(capture.output(smk))
                                                    tkinsert(Envir$txt2,"end", paste("Period ", i, " :  ", start(periods[[i]])[1], ",", start(periods[[i]])[2], " - "
                                                                              , end(periods[[i]])[1], "," , end(periods[[i]])[2], "\n"))
                                                    tkinsert(Envir$txt2,"end", paste(Lc.Mk[1], "\n"))
                                                    for (j in 1:F) { tkinsert(Envir$txt2,"end", paste(Lc.Mk[j+1], "\n")) 
                                                       if (is.na(lc.mk[[i]]$p[j])) {} 
                                                       else { if (lc.mk[[i]]$p[j] <= 0.05) { tktag.add(Envir$txt2, "titre3", "end -2 lines linestart","end -2 lines lineend") 
                                                       tktag.configure(Envir$txt2, "titre3", font=tkfont.create(family="courier", size=9, weight="bold")) } else {}   } }
                                                                                                   
                                                    tkinsert(Envir$txt2,"end", paste("\n", "Ref. value of the complete series: ", round(pos$k, 4), "\n", "Mean trend compare to ref. value: ", round(pos$trends[i], 4), "\n\n", sep="")) }}
           else { return(tkmessageBox(message="Choose a Kendall family test", icon = "warning", type = "ok", title="!Warning!")) }} }
 }                                    
#___________________________________________________________________________Test de detection de la tendance effectue sur la serie temporelle complete
 {
  if (local.trend == "NO") {
   if ( (start-end) == 0 ) { return(tkmessageBox(message="Selected periods should be longer than 1 year", icon = "warning", type = "ok", title="!Warning!")) } else {}
  
  # Seasonal Kendall 
  if (test == "MK") {
      dir.create(paste(Envir$save.WD,"/",Envir$File.Name,"/", start,"-", end, "/", param, "/", "na.", na.replace, "-", "out.", outliers.re, "/", time.step, "-", aggreg, "/", sep= ""), recursive = TRUE)
      save.MK.path <- paste(Envir$save.WD,"/",Envir$File.Name,"/", start,"-", end, "/", param, "/", "na.", na.replace, "-", "out.", outliers.re, "/", time.step, "-", aggreg, "/", Envir$File.Name, "_Global Trend_",param,".csv", sep = "")
      write.csv2(seaKen(z), row.names=FALSE, file=save.MK.path)
      tkinsert(Envir$txt2,"end", paste("-Global trend results-", "\n"))
      tktag.add(Envir$txt2, "titre", "end -2 lines linestart","end -2 lines lineend")
      tktag.configure(Envir$txt2, "titre", font=tkfont.create(family="courier",size=9,weight="bold"))
      tkinsert(Envir$txt2, "end", paste( "Parameter: ", param , "   Site(s): ", liste.stations, "\n"
                                 , "Outliers.removed: ", outliers.re, "   NA.replaced: ", na.replace, "\n"
                                 , "Time.step: ", time.step, "   Method: ", aggreg, sep="" , "\n\n"))
      mk <- seaKen(z)                                                                                                            ## 'wq' package ##
      tkinsert(Envir$txt2,"end", paste("Trend (sen.slope): ", round(mk$sen.slope, 4), "  original units per year"
                                        , "\n", "%Trend (sen.slope.pct): ", round(mk$sen.slope.pct, 4), "  percent of mean quantity per year", "\n"
                                        , "p.value: ", round(mk$p.value, 4), "\n\n", sep=""))                                      
      if (mk$p.value <= 0.05) { tktag.add(Envir$txt2, "titre3", "end -3 lines linestart","end -3 lines lineend") 
      tktag.configure(Envir$txt2, "titre3", font=tkfont.create(family="courier",size=9,weight="bold"))} else {} 
      tksee (Envir$txt2,"end") }
  # Seasonal Kendall with details    
  else { if (test == "SMK") {
       dir.create(paste(Envir$save.WD,"/",Envir$File.Name,"/", start,"-", end, "/", param, "/", "na.", na.replace, "-", "out.", outliers.re, "/", time.step, "-", aggreg, "/", sep= ""), recursive = TRUE)
       save.smk.path <- paste(Envir$save.WD,"/",Envir$File.Name,"/", start,"-", end, "/", param, "/", "na.", na.replace, "-", "out.", outliers.re, "/", time.step, "-", aggreg, "/", Envir$File.Name, "_Seasonal Trend_",param,".csv", sep = "")
       tkinsert(Envir$txt2,"end", paste("-Seasonal trend results-", "\n"))
       tktag.add(Envir$txt2, "titre", "end -2 lines linestart","end -2 lines lineend")
       tktag.configure(Envir$txt2, "titre", font=tkfont.create(family="courier",size=9,weight="bold"))
       tkinsert(Envir$txt2, "end", paste( "Parameter: ", param , "   Site(s): ", liste.stations, "\n"
                                 , "Outliers.removed: ", outliers.re, "   NA.replaced: ", na.replace, "\n"
                                 , "Time.step: ", time.step, "   Method: ", aggreg, sep="" , "\n\n"))
       smk <- seasonTrend(z)
       smk2 <- seasonTrend(z, type=c("slope.pct"))
       SMK <- cbind(smk[-5], smk2[1])
       names(SMK)[5]<- "%trend"      
       write.csv2(SMK, row.names=FALSE, file=save.smk.path)
       SMK2 <- paste(capture.output(SMK)) 
       tkinsert(Envir$txt2,"end", paste( "trend (Theil-Sen slope) = original units per year", "\n"
                                         , "%trend = percent of mean quantity per year", "\n"
                                         , "missing = proportion of missing slopes", "\n", sep=""))
       tkinsert(Envir$txt2,"end", paste(SMK2[1], "\n"))
       tksee (Envir$txt2,"end")   
       for (i in 1:F) { tkinsert(Envir$txt2,"end", paste(SMK2[i+1], "\n")) 
         if (is.na(smk$p[i])) {}
         else{ if(smk$p[i]<= 0.05) { tktag.add(Envir$txt2, "titre3", "end -2 lines linestart","end -2 lines lineend") 
         tktag.configure(Envir$txt2, "titre3", font=tkfont.create(family="courier", size=9, weight="bold")) } else {}   } }
       tkinsert(Envir$txt2,"end", paste("\n\n", sep=""))  }
 
         else { if (test == "LOWESS") {
                   dir.create(paste(Envir$save.WD,"/",Envir$File.Name,"/", start,"-", end, "/", param, "/", "na.", na.replace, "-", "out.", outliers.re, "/", time.step, "-", aggreg, "/", sep= ""), recursive = TRUE)  
                   Loess <- loess(param ~ time, Regularised.data, family="gaussian", span=0.25, control = loess.control(surface = "direct"), na.action=na.exclude)
                   tsLoess <- ts(predict(Loess), start=(min(Regularised.data$YEARS)), deltat=freq)
                   save.loessplot.path <- paste(Envir$save.WD,"/",Envir$File.Name,"/", start,"-", end, "/", param, "/", "na.", na.replace, "-", "out.", outliers.re, "/", time.step, "-", aggreg, "/", Envir$File.Name, "_LOESSplot_",param,".png", sep = "")
                   png(save.loessplot.path)
                   plot(z, xlab="Years", ylab=paste(param, "concentration"), main=paste("Trend of", param, "based on LOESS", "\n"))
                   title(main=paste("\n\n", "at station(s): ", liste.stations, "\n", "Time step: ", time.step, "   Method of aggregation: ", aggreg), cex.main=0.8)
                   lines(tsLoess, col="red")
                   legend("topright", inset = 0.01, c("Time Series", "LOESS")
                          , col = c("black", "red"), lwd = 1)
                   dev.off()
                       plot(z, xlab="Years", ylab=paste(param, "concentration"), main=paste("Trend of", param, "based on LOESS", "\n"))
                       title(main=paste("\n\n", "at station(s): ", liste.stations, "\n", "Time step: ", time.step, "   Method of aggregation: ", aggreg), cex.main=0.8)      
                       lines(tsLoess, col="red")
                       legend("topright", inset = 0.01, c("Time Series", "LOESS")
                             , col = c("black", "red"), lwd = 1)                                                                                                                                                          
                   mk2 <- seaKen(tsLoess)
                   save.loess.path <- paste(Envir$save.WD,"/",Envir$File.Name,"/", start,"-", end, "/", param, "/", "na.", na.replace, "-", "out.", outliers.re, "/", time.step, "-", aggreg, "/", Envir$File.Name, "_LoessGlobalTrend_",param,".csv", sep = "")
                   write.csv2(mk2, row.names=FALSE, file=save.loess.path)                                                                                       ## Kendall package ##
                   showData(capture.output(mk2), rowname.bar = NULL, colname.bar = NULL, title="Global Trend on LOESS results")
                   tkinsert(Envir$txt2,"end", paste("-Global trend results on LOESS-", "\n"))
                   tktag.add(Envir$txt2, "titre", "end -2 lines linestart","end -2 lines lineend")
                   tktag.configure(Envir$txt2, "titre", font=tkfont.create(family="courier",size=9,weight="bold"))
                   tkinsert(Envir$txt2, "end", paste( "Parameter: ", param , "   Site(s): ", liste.stations, "\n"
                                               , "Outliers.removed: ", outliers.re, "   NA.replaced: ", na.replace, "\n"
                                               , "Time.step: ", time.step, "   Method: ", aggreg, sep="" , "\n\n"))                                                                            
                   tkinsert(Envir$txt2,"end", paste("Trend (sen.slope): ", round(mk2$sen.slope, 4), "  original units per year", "\n"
                                                     , "%Trend (sen.slope.pct): ", round(mk2$sen.slope.pct, 4), "  percent of mean quantity per year", "\n"
                                                     , "p.value: ", round(mk2$p.value, 4), "\n\n", sep="")) 
                   if (mk2$p.value <= 0.05) { tktag.add(Envir$txt2, "titre3", "end -3 lines linestart","end -3 lines lineend") 
                   tktag.configure(Envir$txt2, "titre3", font=tkfont.create(family="courier",size=9,weight="bold"))} else {} 
                   tksee (Envir$txt2,"end") }              
                }}      
      }     
      else{ if (test == "NO") { return() } }
 }
#___________________________________________________________________________________________________________________________________fin de la fonction 
}
