


# installing/loading the package:
#if(!require(installr)) {
#  install.packages("installr"); require(installr)} #load / install+load installr
# using the package:
#updateR() # this will start the updating process of your R installation.  It will check for newer versions, and if one is available, will guide you through the decisions you'd need to make.







#local
#setwd("C:/Users/Frederik/Documents/scrape af boligdata/")

#ERST
setwd("C:/Users/ftp/OneDrive - Erhvervsstyrelsen/Github/Ejendomsvurdering-og-plandata/Ejendomsvurdering-og-plandata/")

#Home

load("Århus.Rda")
set.seed(42)


  #####################################################
  #                                                   #
  #               Pakker og afhængigheder             #
  #                                                   #
  #####################################################

  #Indhentning af pakker og afhængigheder
  
Packages <- c("sf",               "RCurl",
              "rwfs",              "data.table",
              "devtools",              "XML",
              "gdalUtils",              "rgdal",
              "tmap",              "sp",
              "raster",              "dplyr",
              "rvest",              "stringr",
              "ggplot2",              "tidyr",
              "viridis",              "broom",
              "maptools",              "caret",
              "dplyr",              "plyr",
              "scales",              "rgdal",
              "raster",               "dplyr",
              "geosphere",               "tmap", 
              "tmaptools",               "rgeos",
              "randomForest",              "ranger",
              "xgboost",              "ggmap",
              "pROC",              "geosphere",
              "maptools",              "rgeos",
              "ggmap",                "readr",
              "caret")

if(!require(Packages)){
  install.packages(Packages)
}

lapply(Packages, library, character.only = TRUE)



  #####################################################
  #                                                   #
  #           Download af Lokalplan i Århus           #
  #                                                   #
  #####################################################



# Kald af defination af scraper.funtions og selve udtrÅ¦kket er udkommenteret i nedenstÅ¥ende. 
# Afkommenteres, hvis de skal indgÅ¥r i den samlede analyse.
# Alternativt kan man nÅ¸jes med data loaded i linje 4, der giver samme resultat

source("Functions_Århus.r")
#source(Scraper_Århus.r)

  
  #####################################################
  #                                                   #
  #           Join af koordinat på boligsalg          #
  #                                                   #
  #####################################################
  
  
  # For at sikre samme typer af koordinater i LP og Boligsalg joiner jeg i det oprindelige dataset,
  #for at tildele de ETRS89 koordinater, der var blevet fjernet i Urups cleaner() funktion, frem for at pille udÅ¸digt i han funktioner


add <- readr::read_csv("http://dawa.aws.dk/adresser?format=csv&kommunekode=0751")
add2 <- add %>% 
  select('wgs84koordinat_bredde', 'wgs84koordinat_længde', 'etrs89koordinat_øst','etrs89koordinat_nord') %>% 
  dplyr::distinct(wgs84koordinat_bredde, wgs84koordinat_længde, .keep_all=TRUE)
rm(add)
# med ETRS89-coor i add2 datasættet kan det nu tilfÅ¸res Århus-boligsalg datasættet

BoligSalg <- Århus %>% 
  left_join(add2, by =  c('lat'='wgs84koordinat_bredde', 'lon'='wgs84koordinat_længde'))
write.table(BoligSalg, file = "Boligsalg.csv", sep=",")
class(BoligSalg)
rm(add2)

#Oprydning 1
fields <-  c("buysum", "m2", "date", "n_rooms", "build_year",  "height", "lat", "lon", "postnr", "floor", "etrs89koordinat_øst", "etrs89koordinat_nord")
BoligSalg <- BoligSalg[fields]





    #####################################################
    #                                                   #
    #      Download og join af Lokalplan i Århus        #
    #                                                   #
    #####################################################

#OBS en del parametre har jeg defineret i url'en for at forsimple OGR hentningen.
ÅRHUS_DSN <-"http://geoserver.plandata.dk/geoserver/wfs?servicename=WFS&request=GetFeature&Version=1.0.0&typeNames=theme_pdk_lokalplan_vedtaget_v&CQL_FILTER=komnr=751"
ogrinfo(ÅRHUS_DSN, so=TRUE) # finder hvilke temaer WFS'en indeholde - kun 1 grundet URL specifikantion
ogr2ogr(ÅRHUS_DSN, "lokalplan_ÅRHUS.shp", "theme_pdk_lokalplan_vedtaget_v")  # hentning af data fra WFS




# Før jeg kan joine bliver jeg nÅ¸dt til at fjerne observationer, hvor at ETRS joined indholder NA
# Går fra 16320 obs til 15600 obs
#BoligSalg <- tidyr::drop_na(BoligSalg, etrs89koordinat_øst, etrs89koordinat_nord)

BoligSalg <- BoligSalg[!is.na(BoligSalg$etrs89koordinat_øst),]
BoligSalg <- BoligSalg[!is.na(BoligSalg$etrs89koordinat_nord),]


#Derefter tildeles Boligsalg observationerne indholdet fra LP såfremt at de ligger inden for en lokalplan
sf::read_sf("lokalplan_ÅRHUS.shp", crs = 25832) %>% 
  sf::st_join( x =
                 sf::st_as_sf(BoligSalg, coords = c("etrs89koordinat_øst", "etrs89koordinat_nord"), 
                              crs = 25832), join = st_within, left = TRUE) %>% 
  sf::write_sf("bolig3.shp") #OBS skriver lokalfil
BoligSalg <- sf::read_sf("bolig3.shp", crs = 25832)

#
#   Der er et problem med, at der nogle steder optræder mere end én lokalplan.Det betyder at vi får flere observationer end der er i det oprindelige datasæt.
#
#

#her omdanner jeg informationerne til en dikotom variable.
BoligSalg$Lokalplan <- ifelse(BoligSalg$planid>0, 1, 0)
BoligSalg$Lokalplan  <- BoligSalg$Lokalplan %>% replace_na(0)




#BaseR.Replace udkonkurerer nedenstående ifelse, men ejg vil stadig gerne finde ud af hvorfor den ikke virker.
baseR.replace      <- function(x) { replace(x, is.na(x), 0) }
BoligSalg$anvgen <- baseR.replace(BoligSalg$anvgen)




#Oplysninger om anvendelse 
#Kristian

# BoligSalg$LP_ANV <- 0

# if(BoligSalg$anvgen == 11){
#   BoligSalg$LP_ANV <- 1;
# } else if(BoligSalg$anvgen = 21) {
#   BoligSalg$LP_ANV <- 2;
# } else if(BoligSalg$anvgen = 31) {
#   BoligSalg$LP_ANV <- 3;
# } else if(BoligSalg$anvgen = 41) {
#   BoligSalg$LP_ANV <- 4;
# } else if(BoligSalg$anvgen = 51) {
#   BoligSalg$LP_ANV <- 5;
# } else if(BoligSalg$anvgen = 61) {
#   BoligSalg$LP_ANV <- 6;
# } else if(BoligSalg$anvgen = 71) {
#   BoligSalg$LP_ANV <- 7;
# } else if(BoligSalg$anvgen = 81) {
#   BoligSalg$LP_ANV <- 8;
# } else {
#   BoligSalg$LP_ANV <- 0;
# }
# 
# x <-  12
# if(x < 11){
#   print("HEJ") ;
# } else if(x == 5) {
#   print("HEJHEJ");
# } else {
#   print("Farvel");
# }

fields <-  c("buysum", "m2", "date", "n_rooms", "build_year",  "height", "lat", "lon", "postnr", "floor",  "Lokalplan", "anvgen")
BoligSalg <- BoligSalg[fields]


#Da der i visse områder er 2 lokalplaner, er der blevet oprettet en række dubletter. 
#Overvej om disse kan fjernes med en distinct() funktionen senere, når der er ryddet ud i data









  #####################################################
  #                                                   #
  #      Afstandsanalyse til rekreative områder       #
  #                                                   #
  #####################################################

#Download af data
DSN_Kommuneplan_Ramme_751 <- "http://geoserver.plandata.dk/geoserver/wfs?servicename=WFS&request=GetFeature&Version=1.0.0&typeNames=theme_pdk_kommuneplanramme_vedtaget_v&CQL_FILTER=komnr=751"
ogrinfo(DSN_Kommuneplan_Ramme_751, so=TRUE)
ogr2ogr(DSN_Kommuneplan_Ramme_751, "Kommuneplanramme.shp", "theme_pdk_kommuneplanramme_vedtaget_v")


Rekreativeområder <- sf::read_sf("Kommuneplanramme.shp", crs = 25832)  %>%
  filter(anvgen == 51) #JF datamodellen


#Afstand fra boligpunkt til nÅ¦rmeste rekreative område 
BoligSalg$afstandR <- st_distance(BoligSalg,Rekreativeområder)[,1]








#####################################################
#                                                   #
#            Opysninger om bevaringsværdi           #
#                                                   #
#####################################################


#definerer tema og komkode i url for at reducere downloadet datamÅ¦ngde

slot <-  "http://www.kulturarv.dk/geoserver/wfs?service=WFS&version=1.0.0&request=GetCapabilities"
ogrinfo(slot, so=TRUE) # finder hvilke temaer WFS'en indeholde - kun 1 grundet URL specifikantion
ogr2ogr(slot, "Bevaringssag.shp", "fbb:view_bygning_bevaringssag")

Bevaringssag <- sf::read_sf("Bevaringssag.shp", crs = 25832)  %>%
  filter(kommune == "Århus")


#spatialjoin --> omtrent 4000 boliger for en bevaringssag tilknyttet 
BoligSalg <- st_join(BoligSalg, Bevaringssag, join = st_equals, left = TRUE)

# omdanner oplysningerne til en dikotom variable
BoligSalg$Bevaringssag <- ifelse(BoligSalg$bygningsid>0, 1, 0)
BoligSalg$Bevaringssag  <- BoligSalg$Bevaringssag %>% replace_na(0)



fields <-  c("buysum", "m2", "date", "n_rooms", "build_year",  "height", "lat", "lon", "postnr", "floor",  "Lokalplan", "anvgen", "afstandR", "Bevaringssag")
BoligSalg <- BoligSalg[fields]



#####################################################
#                                                   #
#                Lokalplandelområder                #
#                                                   #
#####################################################

#OBS en del parametre har jeg defineret i url'en for at forsimple OGR hentningen.
ÅRHUS_DSN <-"http://geoserver.plandata.dk/geoserver/wfs?servicename=WFS&request=GetFeature&Version=1.0.0&typeNames=theme_pdk_lokalplandelomraade_vedtaget_v&CQL_FILTER=komnr=751"
ogrinfo(ÅRHUS_DSN, so=TRUE) # finder hvilke temaer WFS'en indeholde - kun 1 grundet URL specifikantion
ogr2ogr(ÅRHUS_DSN, "lokalplan_del_ÅRHUS.shp", "theme_pdk_lokalplandelomraade_vedtaget_v")  # hentning af data fra WFS



#Derefter tildeles Boligsalg observationerne indholdet fra LP såfremt at de ligger inden for en lokalplan
sf::read_sf("lokalplan_del_ÅRHUS.shp", crs = 25832) %>% 
  sf::st_join( x =
                 sf::st_as_sf(BoligSalg, coords = c("etrs89koordinat_øst", "etrs89koordinat_nord"), 
                              crs = 25832), join = st_within, left = TRUE) %>% 
  sf::write_sf("bolig4.shp") #OBS skriver lokalfil
BoligSalg <- sf::read_sf("bolig4.shp", crs = 25832)

BoligSalg$DelLokalplan <- ifelse(BoligSalg$planid>0, 1, 0)
BoligSalg$DelLokalplan  <- BoligSalg$planid %>% replace_na(0)

fields <-  c("buysum", "m2", "date", "n_rooms", "buld_yr",  "height", "lat", "lon", "postnr", "floor",  "Loklpln", "anvgn_x", "afstndR", "Bvrngss","DelLokalplan")
BoligSalg <- BoligSalg[fields]














#####################################################
#                                                   #
#                    Oprydning                      #
#                                                   #
#####################################################


save(BoligSalg, file="data.Rda")
rm(Bevaringssag, Bolig_fra_CSV, Bolig_med_sag, BoligmedLP, BoligSalg, Rekreativeområder, Århus,slot, DSN_Kommuneplan_Ramme_751, ÅRHUS_DSN )




Data <- distinct(Data)
myvars <- c("buysum", "m2", "date", "n_rooms", "buld_yr",  "height", "lat", "lon", "postnr", "floor",  "Loklpln", "anvgen", "afstndR", "Bvrngss","DelLokalplan")
Data_fin <- Data[myvars]
saveRDS(Data_fin, "data.rds")




#####################################################
#                                                   #
# Indhentning af middel handelsværdi i nærområde    #
#                                                   #
#####################################################

mean_within <- function(df, dist, col) {
  buffer <- st_buffer(df, dist)
  res <- df
  res_col_name <- paste0("Lokalområde_", col)
  res[res_col_name] <- NA
  
  for(i in 1:nrow(buffer)) {
    res[i, res_col_name] <- mean(`$`(df[if_else(is.na(as.logical(st_within(df,  st_buffer(df, dist)[i,]))), FALSE, TRUE), col], "buysum"))  
  }
  res
}

#mean_within(Data_fin_NO, 200, "m2")
Data_fin_1 <-  mean_within(Data_fin, 200, "buysum")

save(Data_fin_1, file= "data2.Rda")







#####################################################
#                                                   #
#              Visualisering af data                #
#                                                   #
#####################################################

#Quite unfinished

map <- tm_shape(Data_fin_1) + tm_polygons("green", alpha=0.1) +
      tm_shape(Data_fin_1) + tm_dots(size=0.01, "red") 

# Take a look at the map
map









#####################################################
#                                                   #
#                 Dataeksploration                  #
#                                                   #
#####################################################


##Boxplots
png(filename="box1.png") 
boxplot(Data_fin$buysum/1000000, ylab = "Købssum (millioner)")
dev.off()

Visueldata <- Data_fin_1 %>%  
  filter(buysum < 8000000)  # fjernet de 200 dyreste boliger 


png(filename="Box2.png") 
boxplot(Visueldata$buysum/1000000, ylab = "Købssum (millioner)")
dev.off()


Visueldata2 <- Data_fin_1 %>%  
  filter(buysum < 5000000)  # fjernet de 200 dyreste boliger 


png(filename="Box3.png") 
boxplot(Visueldata2$buysum/1000000, ylab = "Købssum (millioner)")
dev.off()


cor(Data_fin_1$buysum, Data_fin_1$m2 )          #  0,26
cor(Data_fin_1$buysum, as.numeric(Data_fin_1$date))   #0,19       #
cor(Data_fin_1$buysum, Data_fin_1$buld_yr)      #0,11
cor(Data_fin_1$buysum, Data_fin_1$height)       # -0,13
cor(Data_fin_1$buysum, Data_fin_1$pstnrnv)      # 
cor(Data_fin_1$buysum, Data_fin_1$lat)          # -0,03
cor(Data_fin_1$buysum, Data_fin_1$lon)          # 0,105
cor(Data_fin_1$buysum, Data_fin_1$afstandR)     # -0,0023
cor(Data_fin_1$buysum, Data_fin_1$Bevaringssag) # 0,024
cor(Data_fin_1$buysum, Data_fin_1$Lokalplan)    #0,11
cor(Data_fin_1$buysum, Data_fin_1$Lokalomrøde_buysum)









##Kort 1
ÅrhusBase <- get_googlemap(center = c(lon = 10.19, lat = 56.15), maptype = "terrain", source = "google", zoom = 12, color = "bw")

map1 <-  ggmap(ÅrhusBase, base_layer=ggplot(aes(x=lon,y=lat), data=Data_fin_1), extent = "normal", maprange=TRUE) +
  scale_fill_discrete(name = "Title") +  
  geom_point(data = Visueldata2, aes( x = lon, y = lat, color = buysum/1000000), size = 1, alpha = 0.1) +
  coord_map(projection="mercator", 
            xlim=c(attr(ÅrhusBase, "bb")$ll.lon, attr(ÅrhusBase, "bb")$ur.lon),
            ylim=c(attr(ÅrhusBase, "bb")$ll.lat, attr(ÅrhusBase, "bb")$ur.lat)) +
  guides(color = guide_colorbar(barwidth = 1,
                                barheight = 10,
                                title = "Købssum i millioner",
                                title.position = "top"))
map1
ggsave("Kort1.png", map1)


  
  
#Jitterplot  
prikpåomrÅøde <- ggplot(data = Visueldata2) +
  geom_jitter(aes(x = pstnrnv, color = buysum/1000000, y = buysum/1000000), alpha = 0.1) + 
  scale_color_viridis(option = "viridis") +
  xlab("") +
  ylab("Pris i millioner") +
  guides(color = guide_colorbar(barwidth = 25,
                                barheight = 0.3,
                                title = "Pris i millioner",
                                title.position = "top")) +  
  theme(legend.position="bottom",
        axis.text.x = element_text(size = 10, angle= -90),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text.y = element_text(size = 10)
  )
prikpåområde

ggsave("prik.png", prikpåområde)





###############################################################################
#                                                                             #
#       Fjernelse af outliers og kÅ¸rsel af modeller                           #
#                                                                             #
###############################################################################

###############################################################################


# i dette har jeg valgt ikke at inddrage de outliers, der er i datasÅ¦ttet. 
#indledningsvidt har jeg haft kÅ¸rt mine modeller med dette data, men grundet de teorstiske overvejelser omkring forholdet mellem 
#luksusboliger og almindelige boliger har jeg fjernet disse igen. 
Data_fin_1  <- Data_fin_1[myvars] %>% 
  filter(Data_fin_1$buysum<8000000) #210 fÅ¦rre observationer




#####################################################
#                                                   #
#    Oprettelse af basale statistik funktioner      #
#                                                   #
#####################################################

#Root Mean Square Error
# tila t teste modellerne mod hinanden !
rmse <- function(error)
{
  sqrt(mean(error^2))
}







#####################################################
#                                                   #
#              PrÅ¦diktion + estiamation             #
#                                                   #
#####################################################


#Gennem indexet, der er styret af seed42 laver jeg trÅ¦ning og test settet

Index <-  sample(nrow(Data_fin_1), nrow(Data_fin_1)*0.75)
Bolig_train <- Data_fin_1[Index, ]
Bolig_test <-  Data_fin_1[-Index, ]



formular <- buysum ~ m2 + date +n_rooms + buld_yr + height + afstandR + Bevaringssag + Lokalplan
formular2 <- buysum ~ m2 + date +n_rooms + buld_yr + height + afstandR + Bevaringssag + Lokalplan + Lokalområde_buysum
formular2 <- buysum ~ m2 + date +n_rooms + buld_yr + height + afstandR + Bevaringssag + Lokalplan + Lokalområde_buysum + lat + lon


#oprettelse af fit control, der sikre en looping i trÅ¦ningsdataet, der gÅ¸r modelleringen endnu stÅ¦rkere. 
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)





##########################################
#              LineÅ¦rmodel 1+ 2+3        #
##########################################

#model 1
lm <-  lm(formular, data = Bolig_train)
summary(lm)
Bolig_test$P_lin1 = predict(lm, newdata = Bolig_test)
Bolig_test$RES_lin1 = Bolig_test$buysum - Bolig_test$P_lin1
rmse(Bolig_test$RES_lin1)
sd(Bolig_test$RES_lin1)
mean(Bolig_test$RES_lin1)


#model 2
lm2 <-  lm(formular2, data = Bolig_train)
summary(lm2)
Bolig_test$P_lin2 = predict(lm2, newdata = Bolig_test)
Bolig_test$RES_lin2 = Bolig_test$buysum - Bolig_test$P_lin2
rmse(Bolig_test$RES_lin2)
sd(Bolig_test$RES_lin2)
mean(Bolig_test$RES_lin2)


#model 3
lm3 <-  lm(formular3, data = Bolig_train)
summary(lm3)
Bolig_test$P_lin3 = predict(lm3, newdata = Bolig_test)
Bolig_test$RES_lin3 = Bolig_test$buysum - Bolig_test$P_lin3
rmse(Bolig_test$RES_lin3)
sd(Bolig_test$RES_lin3)
mean(Bolig_test$RES_lin3)




##########################################
#              XGB -modellen 1+ 2+3      #
##########################################

#Model 1
m_xgbLin0 = train(formular,
                 data = Bolig_train,
                 method = "xgbLinear",
                 trControl = fitControl,
                 preProcess = c("scale"),
                 search = "grid",
                 linout = T)

Bolig_test$pred_xgbLin = predict(m_xgbLin0, newdata = Bolig_test)
Bolig_test$resid_xgbLin = Bolig_test$buysum - Bolig_test$pred_xgbLin
mean(Bolig_test$resid_xgbLin) #-79903 kr
cor(Bolig_test$buysum, Bolig_test$pred_xgbLin) # +0.81 -> 
rsssum<- sum((Bolig_test$buysum - Bolig_test$pred_xgbLin)^2)
rsssum # 9,81.....
tsssum <-  sum((Bolig_test$buysum - mean(Bolig_test$pred_xgbLin))^2)
tsssum #2.83.......
rsq <- 1 - (rsssum/tsssum)
rsq # 0.654 <- jeg forklarer omrking 75% af variansen. 
rmse(Bolig_test$resid_xgbLin)
#rmse = 1390932
sd(Bolig_test$resid_xgbLin)
plot(m_xgbLin0)
summary(m_xgbLin0)





#model 2
m_xgbLin = train(formular2,
                 data = Bolig_train,
                 method = "xgbLinear",
                 trControl = fitControl,
                 preProcess = c("scale"),
                 search = "grid",
                 linout = T)

Bolig_test$pred_xgbLin2 = predict(m_xgbLin, newdata = Bolig_test)
Bolig_test$resid_xgbLin2 = Bolig_test$buysum - Bolig_test$pred_xgbLin2

mean(Bolig_test$resid_xgbLin2) #11835 kr
cor(Bolig_test$buysum, Bolig_test$pred_xgbLin2) # +0.86 -> 
rsssum<- sum((Bolig_test$buysum - Bolig_test$pred_xgbLin2)^2)
rsssum # 6.31.....
tsssum <-  sum((Bolig_test$buysum - mean(Bolig_test$pred_xgbLin2))^2)
tsssum #2.47.......
rsq <- 1 - (rsssum/tsssum)
rsq # 0.744 <- jeg forklarer omrking 75% af variansen. 
rmse(Bolig_test$resid_xgbLin2)






#model 3
m_xgbLin2 = train(buysum ~ m2 + date +n_rooms + buld_yr + height + afstandR + Bevaringssag + Lokalplan  + lat + lon,
                  data = Bolig_train,
                  method = "xgbLinear",
                  trControl = fitControl,
                  preProcess = c("scale"),
                  search = "grid",
                  linout = T)

Bolig_test$pred_xgbLin2 = predict(m_xgbLin, newdata = Bolig_test)
Bolig_test$resid_xgbLin2 = Bolig_test$buysum - Bolig_test$pred_xgbLin2

mean(Bolig_test$resid_xgbLin2) #28780 kr
cor(Bolig_test$buysum, Bolig_test$pred_xgbLin2) # +0.87 -> 
rsssum<- sum((Bolig_test$buysum - Bolig_test$pred_xgbLin2)^2)
rsssum # 6.31.....
tsssum <-  sum((Bolig_test$buysum - mean(Bolig_test$pred_xgbLin2))^2)
tsssum #2.47.......
rsq <- 1 - (rsssum/tsssum)
rsq # 0.75 <- jeg forklarer omrking 75% af variansen. 















#####################################################
#                                                   #
#      grafisk evaluering og sandkasse              #
#                                                   #
#####################################################


par(mfrow = c(1, 2))
boxplot(Bolig_test$resid_xgbLin3/1000000, ylab = "Residual (millioner)", ylim=c(-1.5,1.5)) #Viser at der er en rÅ¦kke outliers der gÅ¦tter meget forkert. MÅ¥ske man skulle overveje en anden model til boliger, der koster mange cash
boxplot(Bolig_test$RES_lin3/1000000,  ylim=c(-1.5,1.5))

par(mfrow = c(2, 2))
hist(Bolig_test$resid_xgbLin3/1000000, 
     main= "XGB-model 3", 
     xlab="fejlprÅ¦diktion i millioner kr", 
     border="red", 
     col="darkred",
     xlim=c(-5,5),
     las=0.1, 
     breaks=400,
     prob=TRUE)
hist(Bolig_test$RES_lin3/1000000, 
     main= "LineÅ¦r model 3", 
     xlab="fejlprÅ¦diktion i millioner kr", 
     border="blue", 
     col="darkblue",
     xlim=c(-5,5),
     las=0.1, 
     breaks=400,
     prob=TRUE)
boxplot(Bolig_test$resid_xgbLin3/1000000, ylab = "Residual (millioner)", ylim=c(-1.5,1.5)) #Viser at der er en rÅ¦kke outliers der gÅ¦tter meget forkert. MÅ¥ske man skulle overveje en anden model til boliger, der koster mange cash
boxplot(Bolig_test$RES_lin3/1000000,  ylim=c(-1.5,1.5))




quantile(Bolig_test$resid_xgbLin3)
quantile(Bolig_test$RES_lin3)

