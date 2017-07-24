setwd("~/GitHub/Referendum-Costituzionale-2016")
referendum <- read.csv("resources/ScrutiniFI.csv",sep = ";", stringsAsFactors=FALSE, encoding="UTF-8")
head(referendum)

# add data thanks to http://ckan.ancitel.it/dataset/comuni-italiani-dati-territoriali-e-demografici

comuni <- read.csv("resources/comuniitaliani.csv",sep = ";", stringsAsFactors=FALSE, encoding="UTF-8")
comuni$Comune <- toupper(comuni$Comune)
comuni$Provincia <- toupper(comuni$Provincia)
comuni$Regione <- toupper(comuni$Regione)
head(comuni)
dati <- merge(x=referendum,
              y=comuni,
              by.x=c("DESCREGIONE", "DESCCOMUNE"),
              by.y=c("Regione", "Comune"),
              all.x=TRUE, all.y=TRUE)
View(dati[is.na(dati$ISTAT),])

# problema accenti

dati$DESCREGIONE <- as.factor(dati$DESCREGIONE)
dati$VOTANTI_F <- dati$VOTANTI - dati$VOTANTI_M

library(ggplot2)

