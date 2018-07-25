
# Analysis of value of additional information from PlanDKS on accuracy and precison on predicting houseing prices


setwd("C:/Users/ftp/OneDrive - Erhvervsstyrelsen/Github/Ejendomsvurdering-og-plandata/Ejendomsvurdering-og-plandata/")
load("data.Rda") #named BoligSalg
set.seed(42)
BoligSalg <- na.omit(BoligSalg)

#
# Hvordan ligger data
#

map <-   tm_shape(BoligSalg) + tm_dots(size=0.01, "red") 

# Take a look at the map
map


##Boxplots
png(filename="box1.png") 
boxplot(BoligSalg$buysum/1000000, ylab = "Købssum (millioner)")
dev.off()





#Prædiktion
Index <-  sample(nrow(BoligSalg), nrow(BoligSalg)*0.75)
Bolig_train <- BoligSalg[Index, ]
Bolig_test <-  BoligSalg[-Index, ]



formular <- buysum ~ m2 + date + n_rooms + buld_yr + height + postnr + floor
formular2 <- buysum ~ m2 + date + n_rooms + buld_yr +  height + postnr + floor +  Loklpln + anvgn_x + afstndR + Bvrngss + DelLokalplan

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)




#Lineær model
lm <-  lm(formular, data = Bolig_train)
summary(lm)
Bolig_test$P_lin1 = predict(lm, newdata = Bolig_test)
Bolig_test$RES_lin1 = Bolig_test$buysum - Bolig_test$P_lin1
rmse(Bolig_test$RES_lin1)
sd(Bolig_test$RES_lin1)
mean(Bolig_test$RES_lin1)

#Lineær model
lm2 <-  lm(formular2, data = Bolig_train)
summary(lm2)
Bolig_test$P_lin2 = predict(lm2, newdata = Bolig_test)
Bolig_test$RES_lin2 = Bolig_test$buysum - Bolig_test$P_lin2
rmse(Bolig_test$RES_lin2)
sd(Bolig_test$RES_lin2)
mean(Bolig_test$RES_lin2)

##
##
##  Vi ser at gennemsnittet i residualen stiger, men at standardafvigelsen bliver mindre, når vi tilføjer plandatainformation
##
##







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
cor(Bolig_test$buysum, Bolig_test$pred_xgbLin)

rsssum<- sum((Bolig_test$buysum - Bolig_test$pred_xgbLin)^2)
rsssum # 9,81

tsssum <-  sum((Bolig_test$buysum - mean(Bolig_test$pred_xgbLin))^2)
tsssum 

rsq <- 1 - (rsssum/tsssum)
rsq 

rmse(Bolig_test$resid_xgbLin)

sd(Bolig_test$resid_xgbLin)
plot(m_xgbLin0)
summary(m_xgbLin0)


