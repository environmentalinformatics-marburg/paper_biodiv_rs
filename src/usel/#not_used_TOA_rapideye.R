source("D:/UNI/Master/MA/exploratorien/scripts/00_set_environment.R")

alb<- stack(paste0(path_re,"2015-04-24T110941_RE1_1B-NAC_20835999_303429_ortho_alb.tif"))
hai<- stack(paste0(path_re,"2015-04-24T110857_RE1_1B-NAC_20835994_303428_ortho_hai.tif"))
sch<- stack(paste0(path_re,"2015-04-10T111339_RE1_1B-NAC_20835979_303426_ortho_sch.tif"))

## Funktion schreiben für die Radiance eines Rasters 
RADcalc<-function(x) {
  RADbands<-x
  for (i in 1:5) {
    RADbands[[i]]<- x[[i]] *0.01 #0,01 ist der scaling faktor in RE daten
    print(i) #zur Überprüfung ob die Bänder durchlaufen
  }
  return(RADbands) #ohne return keine Ausgabe im Dataframe später
}


#erstelle dataframe aus den EA values (stehen im Manual von RE)
EAI<-data.frame(bands=c("Blue","Green","Red","RedEdge","NIR"), EA_value=c(1997.8,1863.5,1560.4,1395.0,1124.2)) 

## Schreibe Funktion Top of Atmosphere aller Bänder im Raster mit Formel aus RE Manual: TOA= RAD * ((pi * SunDist²)/(EAI * cos(solarzenith))
TOAcalc <- function (RAD, SunDist, SolarElev){
  TOA <- RAD
  for (i in 1:nlayers(RAD)){
    TOA[[i]]<- (RAD[[i]] * ((pi * SunDist^2)/(EAI$EA_value[i]*cos((pi/180) *(90-SolarElev))))) #solarzenith:90-sunelevation wobei sunelevation
                                            #hierher kommt: https://www.esrl.noaa.gov/gmd/grad/solcalc/azel.html
                                            #NOTE: cos berechnet Radianten, keine Winkel! deswegen Umrechnung
    print(i)
  }
  return(TOA)
}

# Berechnung Radiance für alle Bänder im Raster xyz
RAD1<-RADcalc(alb) #funktionsaufruf von oben berechnen mit Raster alb
RAD2<-RADcalc(hai)
RAD3<-RADcalc(sch)

#Berechnung der TOA für alb
TOA_alb <- TOAcalc(RAD1, SunDist = 1.005949, SolarElev = 48.97)
TOA_hai <- TOAcalc(RAD2, SunDist = 1.005949, SolarElev = 48.93)
TOA_sch <- TOAcalc(RAD3, SunDist = 1.001949, SolarElev = 44.21)

# die Reflektion, welche in Atmosphäre gestreut wird, wird nun vom Raster abgezogen
#? TOA<-calc(RAD, TOAcalc)
Alb_corr<-alb-TOA_alb
Hai_corr<-hai-TOA_hai
Sch_corr<-sch-TOA_sch

writeRaster(Alb_corr,filename = "Alb_corr.tif")
writeRaster(paste0(path_rdata, Hai_corr,filename = "Hai_corr.tif"))

#what hanna did
calcPathRadDOS(100,1, data.frame(250,270),1,0, szen=40, esun=48)
test<-calcAtmosCorr(alb$X2015.04.24T110941_RE1_1B.NAC_20835999_303429_ortho_alb.1,99.9,48,20,model="DOS2")

calcPathRadDOS(1000,1,data.frame(250,270),1,0,szen=40,esun=48)
#output 999.9103
test=calcAtmosCorr(alb$X2015.04.24T110941_RE1_1B.NAC_20835999_303429_ortho_alb.1,999.9,48,20,model="DOS2")
min(values(alb$X2015.04.24T110941_RE1_1B.NAC_20835999_303429_ortho_alb.1)) #mit NAs einbezogen
#output NA
min(values(alb$X2015.04.24T110941_RE1_1B.NAC_20835999_303429_ortho_alb.1),na.rm=TRUE) #keine Nas mit einbezogen!
#output 2339
calcPathRadDOS(2339,1,data.frame(250,270),1,0,szen=40,esun=48)
#output 2338.91
test=calcAtmosCorr(alb$X2015.04.24T110941_RE1_1B.NAC_20835999_303429_ortho_alb.1,2338.91,48,20,model="DOS2")
{
class       : RasterLayer 
dimensions  : 4484, 6384, 28625856  (nrow, ncol, ncell)
resolution  : 5, 5  (x, y)
extent      : 512850, 544770, 5353800, 5376220  (xmin, xmax, ymin, ymax)
coord. ref. : +proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 
data source : D:\UNI\Master\MA\exploratorien\data\temp\r_tmp_2017-05-23_193419_12040_26883.grd 
names       : X2015.04.24T110941_RE1_1B.NAC_20835999_303429_ortho_alb.1 
values      : 0.006670824, 4684.111  (min, max)
} #checking test
# das ergebnis muss gegebenefalls logarithmiert werden um die untercheide zu sehen