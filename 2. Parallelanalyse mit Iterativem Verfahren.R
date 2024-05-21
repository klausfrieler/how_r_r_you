#Load packages ----
library(psych)
library(dplyr)
library(GPArotation)
library(readxl)
par(mar = c(1, 1, 1, 1))


#Datengrundlage für Faktoranalysen + Deutsche Benennungen ----
n.ger <- sum(data.german$`G1Q00001. Have you read and understood the Research Participant Information Sheet?` == "A1", na.rm=TRUE)
data.scale <- subset(data.german, select=c(26:57,59:83))

# step1 <- (names(data.scale) %>% str_split_fixed("The child... ", 2))[,2]
# step3 <- (trimws(step1) %>% str_split_fixed(" ", 2))[,2] %>% sapply(function(x) substr(x, 1, nchar(x) - 2), USE.NAMES = F)
# step4 <- snakecase::to_snake_case(step3)
# names(data.scale) <- step4
# names(data.scale) <- sprintf("V%d", 1:length(data.scale))

#EnglischeDeutscheNamen <- read_excel("EnglischeDeutscheNamen.xlsx", sheet = "Tabelle2")
#DeutscheNamen <- EnglischeDeutscheNamen$Deutsch
#rm(EnglischeDeutscheNamen)
#colnames(data.scale) <- DeutscheNamen
#rm(DeutscheNamen)
cormat1 <- cor(data.scale, use="pairwise.complete.obs")
cormat1 <- as.data.frame(cormat1)
write.csv2(cormat1,"cormat.csv")

#1. Iterationen ----
###Parallelanalyse 1
parallel1 <- fa.parallel(cormat1,n.obs=n.ger,fm="minres",fa="both",nfactors=1,
                         main="Parallel Analysis Scree Plots",
                         n.iter=200,error.bars=FALSE,se.bars=FALSE,SMC=FALSE,ylabel=NULL,show.legend=TRUE,
                         sim=TRUE,quant=.95,cor="cor",use="pairwise",plot=TRUE,correct=.5)
browser()
facparallel1 <- parallel1[["nfact"]]
pfa1 <- fa(cormat1, nfactors = facparallel1, n.obs=n.ger)
loadingspfa1 <- as.data.frame(unclass(pfa1$loadings))
communalities1 <- as.data.frame(unclass(pfa1$communalities))
loadingspfa1 <- loadingspfa1 %>%
  mutate(across(contains("MR"),
                ~ ifelse(.x >= 0.3, .x, NA)))
loadingspfa1$factors <- rowSums(!is.na(loadingspfa1))
cormat2 <- cormat1
cormat2$Fakt1 <-  loadingspfa1$factors
cormat2$Comm <- communalities1
cormat2 <- filter(cormat2, Fakt1>0 & Comm>=.4)
itemsstep2 <- rownames(cormat2)
cormat2 <- cormat2[itemsstep2]
rm(itemsstep2)
rm(facparallel1)
rm(communalities1)
rm(parallel1)
rm(pfa1)

###Parallelanalyse 2
parallel2 <- fa.parallel(cormat2,n.obs=n.ger,fm="minres",fa="both",nfactors=1,
                         main="Parallel Analysis Scree Plots",
                         n.iter=200,error.bars=FALSE,se.bars=FALSE,SMC=FALSE,ylabel=NULL,show.legend=TRUE,
                         sim=TRUE,quant=.95,cor="cor",use="pairwise",plot=TRUE,correct=.5)
facparallel2 <- parallel2[["nfact"]]
pfa2 <- fa(cormat2, nfactors = facparallel2, n.obs=n.ger)
loadingspfa2 <- as.data.frame(unclass(pfa2$loadings))
communalities2 <- as.data.frame(unclass(pfa2$communalities))
loadingspfa2 <- loadingspfa2 %>%
  mutate(across(contains("MR"),
                ~ ifelse(.x >= 0.3, .x, NA)))
loadingspfa2$factors <- rowSums(!is.na(loadingspfa2))
cormat3 <- cormat2
cormat3$Fakt2 <-  loadingspfa2$factors
cormat3$Comm <- communalities2
cormat3 <- filter(cormat3, Fakt2>0 & Comm>=.4)
itemsstep3 <- rownames(cormat3)
cormat3 <- cormat3[itemsstep3]
rm(itemsstep3)
rm(facparallel2)
rm(communalities2)
rm(parallel2)
rm(pfa2)

#zweite Runde Iterationen----
##um Items weiter zu reduzieren,  werden als Kriterien Faktorladungen >.4 herangezogen
###Parallelanalyse3
parallel3 <- fa.parallel(cormat3,n.obs=n.ger,fm="minres",fa="both",nfactors=1,
                         main="Parallel Analysis Scree Plots",
                         n.iter=200,error.bars=FALSE,se.bars=FALSE,SMC=FALSE,ylabel=NULL,show.legend=TRUE,
                         sim=TRUE,quant=.95,cor="cor",use="pairwise",plot=TRUE,correct=.5)
facparallel3 <- parallel3[["nfact"]]
pfa3 <- fa(cormat3, nfactors = facparallel3, n.obs=n.ger)
loadingspfa3 <- as.data.frame(unclass(pfa3$loadings))
communalities3 <- as.data.frame(unclass(pfa3$communalities))
loadingspfa3 <- loadingspfa3 %>%
  mutate(across(contains("MR"),
                ~ ifelse(.x >= 0.4, .x, NA)))
loadingspfa3$factors <- rowSums(!is.na(loadingspfa3))
cormat4 <- cormat3
cormat4$Fakt3 <-  loadingspfa3$factors
cormat4$Comm <- communalities3
cormat4 <- filter(cormat4, Fakt3>0 & Comm>=.4)
itemsstep4 <- rownames(cormat4)
cormat4 <- cormat4[itemsstep4]
rm(itemsstep4)
rm(facparallel3)
rm(communalities3)
rm(parallel3)
rm(pfa3)

###Parallelanalyse4
parallel4 <- fa.parallel(cormat4,n.obs=n.ger,fm="minres",fa="both",nfactors=1,
                         main="Parallel Analysis Scree Plots",
                         n.iter=200,error.bars=FALSE,se.bars=FALSE,SMC=FALSE,ylabel=NULL,show.legend=TRUE,
                         sim=TRUE,quant=.95,cor="cor",use="pairwise",plot=TRUE,correct=.5)
facparallel4 <- parallel4[["nfact"]]
pfa4 <- fa(cormat4, nfactors = facparallel4, n.obs=n.ger)
loadingspfa4 <- as.data.frame(unclass(pfa4$loadings))
communalities4 <- as.data.frame(unclass(pfa4$communalities))
loadingspfa4 <- loadingspfa4 %>%
  mutate(across(contains("MR"),
                ~ ifelse(.x >= 0.4, .x, NA)))
loadingspfa4$factors <- rowSums(!is.na(loadingspfa4))
cormat5 <- cormat4
cormat5$Fakt4 <-  loadingspfa4$factors
cormat5$Comm <- communalities4
cormat5 <- filter(cormat5, Fakt4>=1 & Comm>=.4)
itemsstep5 <- rownames(cormat5)
cormat5 <- cormat5[itemsstep5]
rm(itemsstep5)
rm(facparallel4)
rm(communalities4)
rm(parallel4)
rm(pfa4)

###Parallelanalyse5
parallel5 <- fa.parallel(cormat5,n.obs=n.ger,fm="minres",fa="both",nfactors=1,
                         main="Parallel Analysis Scree Plots",
                         n.iter=200,error.bars=FALSE,se.bars=FALSE,SMC=FALSE,ylabel=NULL,show.legend=TRUE,
                         sim=TRUE,quant=.95,cor="cor",use="pairwise",plot=TRUE,correct=.5)
facparallel5 <- parallel5[["nfact"]]
pfa5 <- fa(cormat5, nfactors = facparallel5, n.obs=n.ger)
loadingspfa5 <- as.data.frame(unclass(pfa5$loadings))
communalities5 <- as.data.frame(unclass(pfa5$communalities))
loadingspfa5 <- loadingspfa5 %>%
  mutate(across(contains("MR"),
                ~ ifelse(.x >= 0.4, .x, NA)))
loadingspfa5$factors <- rowSums(!is.na(loadingspfa5))
cormat6 <- cormat5
cormat6$Fakt5 <-  loadingspfa5$factors
cormat6$Comm <- communalities5
cormat6 <- filter(cormat6, Fakt5>=1 & Comm>=.4)
itemsstep6 <- rownames(cormat6)
cormat6 <- cormat6[itemsstep6]
nVars <- length(itemsstep6)
rm(itemsstep6)
rm(facparallel5)
rm(communalities5)
rm(parallel5)
rm(pfa5)


#Dritte Iterationsrunde ----
###nur noch Aufnahme von Items, die auf einzelnem Faktor laden
###Parallelanalyse6
parallel6 <- fa.parallel(cormat6,n.obs=n.ger,fm="minres",fa="both",nfactors=1,
                         main="Parallel Analysis Scree Plots",
                         n.iter=200,error.bars=FALSE,se.bars=FALSE,SMC=FALSE,ylabel=NULL,show.legend=TRUE,
                         sim=TRUE,quant=.95,cor="cor",use="pairwise",plot=TRUE,correct=.5)
facparallel6 <- parallel6[["nfact"]]
pfa6 <- fa(cormat6, nfactors = facparallel6, n.obs=n.ger)
loadingspfa6 <- as.data.frame(unclass(pfa6$loadings))
communalities6 <- as.data.frame(unclass(pfa6$communalities))
loadingspfa6 <- loadingspfa6 %>%
  mutate(across(contains("MR"),
                ~ ifelse(.x >= 0.4, .x, NA)))
loadingspfa6$factors <- rowSums(!is.na(loadingspfa6))
cormat7 <- cormat6
cormat7$Fakt6 <-  loadingspfa6$factors
cormat7$Comm <- communalities6
cormat7 <- filter(cormat7, Fakt6==1 & Comm>=.4)
itemsstep7 <- rownames(cormat7)
cormat7 <- cormat7[itemsstep7]
nVars <- length(itemsstep7)
rm(itemsstep7)
rm(facparallel6)
rm(communalities6)
rm(parallel6)
rm(pfa6)

###Parallelanalyse7
parallel7 <- fa.parallel(cormat7,n.obs=n.ger,fm="minres",fa="both",nfactors=1,
                         main="Parallel Analysis Scree Plots",
                         n.iter=200,error.bars=FALSE,se.bars=FALSE,SMC=FALSE,ylabel=NULL,show.legend=TRUE,
                         sim=TRUE,quant=.95,cor="cor",use="pairwise",plot=TRUE,correct=.5)
facparallel7 <- parallel7[["nfact"]]
pfa7 <- fa(cormat7, nfactors = facparallel7, n.obs=n.ger)
loadingspfa7 <- as.data.frame(unclass(pfa7$loadings))
communalities7 <- as.data.frame(unclass(pfa7$communalities))
loadingspfa7 <- loadingspfa7 %>%
  mutate(across(contains("MR"),
                ~ ifelse(.x >= 0.4, .x, NA)))
loadingspfa7$factors <- rowSums(!is.na(loadingspfa7))
cormat8 <- cormat7
cormat8$Fakt7 <-  loadingspfa7$factors
cormat8$Comm <- communalities7
cormat8 <- filter(cormat8,  Fakt7==1 & Comm>=.4)
itemsstep8 <- rownames(cormat8)
cormat8 <- cormat8[itemsstep8]
nVars <- length(itemsstep8)
rm(itemsstep8)
rm(facparallel7)
rm(communalities7)
rm(parallel7)
rm(pfa7)

###Parallelanalyse8
parallel8 <- fa.parallel(cormat8,n.obs=n.ger,fm="minres",fa="both",nfactors=1,
                         main="Parallel Analysis Scree Plots",
                         n.iter=200,error.bars=FALSE,se.bars=FALSE,SMC=FALSE,ylabel=NULL,show.legend=TRUE,
                         sim=TRUE,quant=.95,cor="cor",use="pairwise",plot=TRUE,correct=.5)
facparallel8 <- parallel8[["nfact"]]
pfa8 <- fa(cormat8, nfactors = facparallel8, n.obs=n.ger)
loadingspfa8 <- as.data.frame(unclass(pfa8$loadings))
communalities8 <- as.data.frame(unclass(pfa8$communalities))
loadingspfa8 <- loadingspfa8 %>%
  mutate(across(contains("MR"),
                ~ ifelse(.x >= 0.4, .x, NA)))
loadingspfa8$factors <- rowSums(!is.na(loadingspfa8))
cormat9 <- cormat8
cormat9$Fakt8 <-  loadingspfa8$factors
cormat9$Comm <- communalities8
cormat9 <- filter(cormat9, Fakt8==1 & Comm>=.4)
itemsstep9 <- rownames(cormat9)
cormat9 <- cormat9[itemsstep9]
nVars <- length(itemsstep9)
rm(itemsstep9)
rm(facparallel8)
rm(communalities8)
rm(parallel8)
rm(pfa8)

###Parallelanalyse9
parallel9 <- fa.parallel(cormat9,n.obs=n.ger,fm="minres",fa="both",nfactors=1,
                         main="Parallel Analysis Scree Plots",
                         n.iter=200,error.bars=FALSE,se.bars=FALSE,SMC=FALSE,ylabel=NULL,show.legend=TRUE,
                         sim=TRUE,quant=.95,cor="cor",use="pairwise",plot=TRUE,correct=.5)
facparallel9 <- parallel9[["nfact"]]
pfa9 <- fa(cormat9, nfactors = facparallel9, n.obs=n.ger)
loadingspfa9 <- as.data.frame(unclass(pfa9$loadings))
communalities9 <- as.data.frame(unclass(pfa9$communalities))
loadingspfa9 <- loadingspfa9 %>%
  mutate(across(contains("MR"),
                ~ ifelse(.x >= 0.4, .x, NA)))
loadingspfa9$factors <- rowSums(!is.na(loadingspfa9))
cormat10 <- cormat9
cormat10$Fakt9 <-  loadingspfa9$factors
cormat10$Comm <- communalities9
cormat10 <- filter(cormat10, Fakt9==1 & Comm>=.4)
itemsstep10 <- rownames(cormat10)
cormat10 <- cormat10[itemsstep10]
nVars <- length(itemsstep10)
rm(itemsstep10)
rm(facparallel9)
rm(communalities9)
rm(parallel9)
rm(pfa9)

###Parallelanalyse10
parallel10 <- fa.parallel(cormat10,n.obs=n.ger,fm="minres",fa="both",nfactors=1,
                          main="Parallel Analysis Scree Plots",
                          n.iter=200,error.bars=FALSE,se.bars=FALSE,SMC=FALSE,ylabel=NULL,show.legend=TRUE,
                          sim=TRUE,quant=.95,cor="cor",use="pairwise",plot=TRUE,correct=.5)
facparallel10 <- parallel10[["nfact"]]
pfa10 <- fa(cormat10, nfactors = facparallel10, n.obs=n.ger)
loadingspfa10 <- as.data.frame(unclass(pfa10$loadings))
communalities10 <- as.data.frame(unclass(pfa10$communalities))
loadingspfa10 <- loadingspfa10 %>%
  mutate(across(contains("MR"),
                ~ ifelse(.x >= 0.4, .x, NA)))
loadingspfa10$factors <- rowSums(!is.na(loadingspfa10))
cormat11 <- cormat10
cormat11$Fakt10 <-  loadingspfa10$factors
cormat11$Comm <- communalities10
cormat11 <- filter(cormat11, Fakt10==1 & Comm>=.4)
itemsstep11 <- rownames(cormat11)
cormat11 <- cormat11[itemsstep11]
nVars <- length(itemsstep11)
rm(itemsstep11)
rm(facparallel10)
rm(communalities10)
rm(parallel10)
rm(pfa10)

###Parallelanalyse11
parallel11 <- fa.parallel(cormat11,n.obs=n.ger,fm="minres",fa="both",nfactors=1,
                          main="Parallel Analysis Scree Plots",
                          n.iter=200,error.bars=FALSE,se.bars=FALSE,SMC=FALSE,ylabel=NULL,show.legend=TRUE,
                          sim=TRUE,quant=.95,cor="cor",use="pairwise",plot=TRUE,correct=.5)
facparallel11 <- parallel11[["nfact"]]
pfa11 <- fa(cormat11, nfactors = facparallel11, n.obs=n.ger)
loadingspfa11 <- as.data.frame(unclass(pfa11$loadings))
communalities11 <- as.data.frame(unclass(pfa11$communalities))
loadingspfa11 <- loadingspfa11 %>%
  mutate(across(contains("MR"),
                ~ ifelse(.x >= 0.4, .x, NA)))
loadingspfa11$factors <- rowSums(!is.na(loadingspfa11))
cormat12 <- cormat11
cormat12$Fakt11 <-  loadingspfa11$factors
cormat12$Comm <- communalities11
cormat12 <- filter(cormat12, Fakt11==1 & Comm>=.4)
itemsstep12 <- rownames(cormat12)
cormat12 <- cormat12[itemsstep12]
nVars <- length(itemsstep12)
rm(itemsstep12)
rm(facparallel11)
rm(communalities11)
rm(parallel11)
rm(pfa11)

###Parallelanalyse12
parallel12 <- fa.parallel(cormat12,n.obs=n.ger,fm="minres",fa="both",nfactors=1,
                          main="Parallel Analysis Scree Plots",
                          n.iter=200,error.bars=FALSE,se.bars=FALSE,SMC=FALSE,ylabel=NULL,show.legend=TRUE,
                          sim=TRUE,quant=.95,cor="cor",use="pairwise",plot=TRUE,correct=.5)
facparallel12 <- parallel12[["nfact"]]
pfa12 <- fa(cormat12, nfactors = facparallel12, n.obs=n.ger)
loadingspfa12 <- as.data.frame(unclass(pfa12$loadings))
communalities12 <- as.data.frame(unclass(pfa12$communalities))
loadingspfa12 <- loadingspfa12 %>%
  mutate(across(contains("MR"),
                ~ ifelse(.x >= 0.4, .x, NA)))
loadingspfa12$factors <- rowSums(!is.na(loadingspfa12))
cormat13 <- cormat12
cormat13$Fakt12 <-  loadingspfa12$factors
cormat13$Comm <- communalities12
cormat13 <- filter(cormat13, Fakt12==1 & Comm>=.4)
itemsstep13 <- rownames(cormat13)
cormat13 <- cormat13[itemsstep13]
nVars <- length(itemsstep13)
#rm(itemsstep13)
rm(facparallel12)
rm(communalities12)
rm(parallel12)
#rm(pfa12)
#ab diesem Punkt keine Veränderungen durch zusätzliche Parallelanalyse

#Beschreibung Endmodell----
data.scale2 <- data.scale[,itemsstep13]
Kennwerte2 <-  describe(data.scale2)
#Berechnung Itemschwierigkeit
Kennwerte2$difficulties <- (colMeans(data.scale2, na.rm=TRUE)/3)
data.scale2$sum <- rowMeans(data.scale2, na.rm=TRUE) * nVars
Trennschärfe <- cor(data.scale2, use="pairwise.complete.obs")
Trennschärfe <- Trennschärfe[-c(nVars),-c(1:(nVars-1))]
Kennwerte2$Trennschärfe <- Trennschärfe
rm("Trennschärfe")

itemsfact1 <-  filter(loadingspfa12, MR1>=.4)
itemsfact1 <- rownames(itemsfact1)
itemsfact2 <-  filter(loadingspfa12, MR2>=.4)
itemsfact2 <- rownames(itemsfact2)
itemsfact3 <-  filter(loadingspfa12, MR3>=.4)
itemsfact3 <- rownames(itemsfact3)


write.csv2(loadingspfa12, "Ladungen.csv")
