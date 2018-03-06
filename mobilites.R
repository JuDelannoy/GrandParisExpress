library(rgdal)
library(sf)
library(sp)

################################################
#DATA POUR OBTENIR LES KILOMETRES VEHICULES DONC LES DEPLACEMENTS POLLUANT LE PLUS


#opening aperitif
#pour les données de temps de parcours entre 2 villes en IDF, en voiture et en TC

timeBtwCities <- readRDS("DATA/aperitif-master/aperitif-master/data/listtimes.Rds", refhook = NULL)
tblTimeBtwCities <- as.data.frame(timeBtwCities)
#TC.Val temps en TC en minutes entre les 2 villes
#VPM : temps en voiture le matin en minutes
#VPS : temps en voiture le soir en minutes
#dist val : distance à vol d'oiseau en km entre les deux villes
tblTimeBtwCities$trajet <- paste(tblTimeBtwCities$TC.ORI,tblTimeBtwCities$TC.DES,sep=" ")


fluxBtwCities <- read.table("DATA/mobpro2013/mobpro2012.txt", sep= ";", header=TRUE)
fluxBtwCities$trajet <- paste(fluxBtwCities$CODGEO,fluxBtwCities$DCLT,sep=" ")

fluxBtwCities$dep <- substr(fluxBtwCities$trajet,1,2)
fluxBtwCities$idf <- ifelse (fluxBtwCities$dep== "75" | fluxBtwCities$dep== "92" |fluxBtwCities$dep== "77"|fluxBtwCities$dep== "78"|fluxBtwCities$dep== "91"|fluxBtwCities$dep== "93"|fluxBtwCities$dep== "94"|fluxBtwCities$dep== "95",TRUE,FALSE)
fluxBtwCities$idfhorsParis <- ifelse (fluxBtwCities$dep== "92" |fluxBtwCities$dep== "77"|fluxBtwCities$dep== "78"|fluxBtwCities$dep== "91"|fluxBtwCities$dep== "93"|fluxBtwCities$dep== "94"|fluxBtwCities$dep== "95",TRUE,FALSE)

  
fluxIDFBtwCities <- fluxBtwCities[fluxBtwCities$idf==TRUE,]
fluxCouronnesBtwCities <- fluxBtwCities[fluxBtwCities$idfhorsParis==TRUE,]

#on merge pour avoir le flux et la distance pour chaque trajet
kmvehiculetab <- merge(x = fluxIDFBtwCities, y = tblTimeBtwCities, by = "trajet")
kmvehiculetab $dep <- NULL
kmvehiculetab $VPM.ORI <- NULL
kmvehiculetab $VPM.DES <- NULL
kmvehiculetab $DIST.DES <- NULL
kmvehiculetab $DIST.ORI <- NULL
kmvehiculetab $idf <- NULL
kmvehiculetab $VPS.ORI <- NULL
kmvehiculetab $VPS.DES <- NULL
kmvehiculetab$kmvehicule <- kmvehiculetab$NBFLUX_C12_ACTOCC15P * kmvehiculetab$DIST.VAL

#on fait de meme uniquement pour la grande et petite couronne (sans Paris)
tab_couronnes <- merge(x = fluxCouronnesBtwCities, y = tblTimeBtwCities, by = "trajet")
tab_couronnes $dep <- NULL
tab_couronnes $TC.ORI <- NULL
tab_couronnes $TC.DES <- NULL
tab_couronnes $VPM.ORI <- NULL
tab_couronnes $VPM.DES <- NULL
tab_couronnes $DIST.DES <- NULL
tab_couronnes $DIST.ORI <- NULL
tab_couronnes $idf <- NULL
tab_couronnes $VPS.ORI <- NULL
tab_couronnes $VPS.DES <- NULL
tab_couronnes$kmvehicule <- tab_couronnes$NBFLUX_C12_ACTOCC15P * tab_couronnes$DIST.VAL


##################################################################################################
#liste des stations qui nous interesse
stations_GPE <- read.table("NouvellesStationGPE_v3.csv", sep= ",", header=TRUE,stringsAsFactors = FALSE)

#intersection entre stations du GPE et les villes sur lesquelles elles se trouvent
villes <- readOGR(dsn = "DATA/DONNEES TRAITEES EN L93/fond de carte/ENSEMBLE_COMMUNE_L93.shp",stringsAsFactors = FALSE)
GPE <- readOGR(dsn = "DATA/DONNEES TRAITEES EN L93/intersect_gpe_villes.shp",stringsAsFactors = FALSE)
TC <- readOGR(dsn = "DATA/intersection_tc_villes.shp",stringsAsFactors = FALSE)
TC$id <- as.integer(TC$id_ref_zdl)
TC$id <- as.character(TC$id)

#recuperer les codes insee de toutes les villes sur lesquelles on trouve une nouvelle station GPE
#pour chaque station
for (i in 1:nrow(GPE)){
  #si c'est une nouvelle station du GPE
  if (GPE[["LIBELLE"]][i] %in% stations_GPE$LIBELLE){
    #on prend les n plus gros trajets (en fonction du kilometre vehicule = flux*distance)
    table <- kmvehiculetab[kmvehiculetab$TC.ORI==GPE[["INSEE_COM"]][i],]
    table <- table[with(table,order(kmvehicule,decreasing = TRUE)),]
    tableshort <- table[1:15,]
    #pour chaque trajet depuis la ville origine etudiee
    for(j in 1:nrow(tableshort)){
      #ville dep et d'arrivee
      code_dep=tableshort[j,c("TC.ORI")]
      arret_dep=GPE[["LIBELLE"]][i]
      code_arr=tableshort$TC.DES[j]
      #decoupage du trajet en 2 partie : GPE + TC
      #calcul trajet GPE
      ligne <- stations_GPE[stations_GPE$LIBELLE==arret_dep,]
      idarret_int <- ligne$id
      tps_trajet_GPE=ligne$Temps_rac_TC
      #calcul TC
      ligne2 <- TC[TC$id==idarret_int,]
      code_int <- ligne2$INSEE_COM
      ligne3 <- tblTimeBtwCities[tblTimeBtwCities$TC.ORI==code_int & tblTimeBtwCities$TC.DES==code_arr,]
      tps_trajet_tc <- ligne3$TC.VAL
      #temps total GPE
      tps_gpe <- tps_trajet_GPE+tps_trajet_tc
      
      #comparaison avec le temps en voiture
      ligne4 <- tblTimeBtwCities[tblTimeBtwCities$TC.ORI==code_dep & tblTimeBtwCities$TC.DES==code_arr,]
      tps_voiture_matin <- ligne4$VPM.VAL
      tps_voiture_soir <- ligne4$VPS.VAL
      ville_dep <- villes[villes$INSEE_COM==code_dep,]$NOM_COM
      ville_arr <- villes[villes$INSEE_COM==code_arr,]$NOM_COM
      if (tps_gpe<tps_voiture_matin){
      print(paste(code_dep,ville_dep,code_arr,ville_arr))
      print(paste("res",tps_gpe,tps_voiture_matin,tps_voiture_soir))
      }
    }
  }
  #verifier que les stations non étudiées sont bien des stations existant deja sur le reseau
  else{
    #print(paste("faux",GPE[["LIBELLE"]][i]))
    }
  
}
