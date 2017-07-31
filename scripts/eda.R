library(plyr)
library(leaflet)
library(knitr)

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
              all.x=TRUE)
View(dati[is.na(dati$ISTAT),])

dati <- dati[!is.na(dati$Longitudine),]
dati <- dati[!is.na(dati$ELETTORI),]
dati <- dati[!is.na(dati$ELETTORI_M),]
dati <- dati[!is.na(dati$VOTANTI),]
dati <- dati[!is.na(dati$VOTANTI_M),]

# problema accenti

dati$DESCREGIONE <- as.factor(dati$DESCREGIONE)
dati$DESCPROVINCIA <- as.factor(dati$DESCPROVINCIA)
dati$DESCCOMUNE <- as.factor(dati$DESCCOMUNE)

dati$ELETTORI_F <- dati$ELETTORI - dati$ELETTORI_M
dati$VOTANTI_F <- dati$VOTANTI - dati$VOTANTI_M
dati$VOTI <- dati$NUMVOTISI + dati$NUMVOTINO
dati$PERC_SI <- dati$NUMVOTISI / dati$VOTI
dati$PERC_VOTANTI <- dati$VOTANTI / dati$ELETTORI
dati$PERC_VOTANTI_M <- dati$VOTANTI_M / dati$ELETTORI_M
dati$PERC_VOTANTI_F <- dati$VOTANTI_F / dati$ELETTORI_F
dati[is.na(dati$PERC_SI),"PERC_SI"] <- 0

dati$PERC_BIANCHI <- dati$NUMVOTIBIANCHI/dati$VOTANTI
dati[is.na(dati$PERC_BIANCHI),"PERC_BIANCHI"] <- 0
dati$PERC_NONVALIDI <- dati$NUMVOTINONVALIDI/dati$VOTANTI
dati[is.na(dati$PERC_NONVALIDI),"PERC_NONVALIDI"] <- 0
dati$PERC_CONTESTATI <- dati$NUMVOTICONTESTATI/dati$VOTANTI
dati[is.na(dati$PERC_CONTESTATI),"PERC_CONTESTATI"] <- 0

library(ggplot2)

View(dati)

# cart %sì rispetto variabili demografiche?

hist(dati$PERC_VOTANTI, nclass = round(sqrt(length(dati$PERC_VOTANTI))))
par(mfrow=c(2,1))
hist(dati$PERC_VOTANTI_M, nclass = round(sqrt(length(dati$PERC_VOTANTI_M))), xlim=c(0,1))
abline(v=mean(dati$PERC_VOTANTI_M), col=2, lwd=3)
abline(v=median(dati$PERC_VOTANTI_M), col=3, lwd=3)
hist(dati$PERC_VOTANTI_F, nclass = round(sqrt(length(dati$PERC_VOTANTI_F))), xlim=c(0,1))
abline(v=mean(dati$PERC_VOTANTI_F), col=2, lwd=3)
abline(v=median(dati$PERC_VOTANTI_F), col=3, lwd=3)
par(mfrow=c(1,1))

temp <- data.frame(PERC_VOTANTI=c(dati$PERC_VOTANTI_M,dati$PERC_VOTANTI_F), SEX=c(rep('M',length(dati$PERC_VOTANTI_M)),rep('F',length(dati$PERC_VOTANTI_F))))

ggplot(data=dati, aes(PERC_VOTANTI)) + geom_histogram(bins = round(sqrt(length(dati$PERC_VOTANTI)))) + xlim(0, 1)

ggplot(temp, aes(x=PERC_VOTANTI, fill=SEX)) +
  geom_histogram(alpha=.5, position="identity", bins = round(sqrt(length(dati$PERC_VOTANTI)))) +
  geom_vline(data=ddply(temp, "SEX", summarise, perc.mean=mean(PERC_VOTANTI)), aes(xintercept=perc.mean,  colour=SEX),
             linetype="dotted", size=1) + xlim(0, 1) + theme(legend.position="bottom")

# more than 100% because you can not vote in your polling station

t.test(x=(dati$VOTANTI_M / dati$ELETTORI_M), y=(dati$VOTANTI_F / dati$ELETTORI_F))

temp <- data.frame(PERC_VOTI = c(dati$PERC_BIANCHI, dati$PERC_CONTESTATI, dati$PERC_NONVALIDI),
                   TIPO = c(rep('bianchi', length(dati$PERC_BIANCHI)),
                            rep('contestati', length(dati$PERC_CONTESTATI)),
                            rep('non validi', length(dati$PERC_NONVALIDI))) )
ggplot(temp, aes(x=PERC_VOTI, fill=TIPO)) +
  geom_histogram(alpha=.5, position="identity", bins = round(sqrt(length(temp$PERC_VOTI)/3))) + xlim(-0.001, 0.03) + theme(legend.position="bottom")


View(dati[dati$PERC_NONVALIDI>.05,c("DESCREGIONE","DESCCOMUNE","ELETTORI","VOTANTI","NUMVOTISI", "NUMVOTINO","NUMVOTIBIANCHI","NUMVOTINONVALIDI", "NUMVOTICONTESTATI", "ClasseComune")]) # probably because low number of voters

dati_regione <- aggregate(.  ~ DESCREGIONE, dati[,c("ELETTORI","VOTANTI","NUMVOTISI","DESCREGIONE")], sum)
dati_regione$PERC_VOTANTI <- dati_regione$VOTANTI/dati_regione$ELETTORI
dati_regione$PERC_SI <- dati_regione$NUMVOTISI/dati_regione$VOTANTI


sum(dati$NUMVOTISI)
sum(dati$NUMVOTINO)
sum(dati$NUMVOTIBIANCHI) + sum(dati$NUMVOTINONVALIDI) + sum(dati$NUMVOTICONTESTATI)
sum(dati$ELETTORI) - (sum(dati$NUMVOTISI) + sum(dati$NUMVOTINO) + sum(dati$NUMVOTIBIANCHI) + sum(dati$NUMVOTINONVALIDI) + sum(dati$NUMVOTICONTESTATI))

totalone <- data.frame(tipo=c("sì","no","altri","non votato"), valore=c(sum(dati$NUMVOTISI), sum(dati$NUMVOTINO), sum(dati$NUMVOTIBIANCHI) + sum(dati$NUMVOTINONVALIDI) + sum(dati$NUMVOTICONTESTATI), sum(dati$ELETTORI) - (sum(dati$NUMVOTISI) + sum(dati$NUMVOTINO) + sum(dati$NUMVOTIBIANCHI) + sum(dati$NUMVOTINONVALIDI) + sum(dati$NUMVOTICONTESTATI))
))
barplot(totalone$valore, names.arg = totalone$tipo, col=c(3,2,"grey","grey"))

totalone <- data.frame(tipo=c("sì","no + non votato","altri"), valore=c(sum(dati$NUMVOTISI), sum(dati$ELETTORI) - (sum(dati$NUMVOTISI) + sum(dati$NUMVOTIBIANCHI) + sum(dati$NUMVOTINONVALIDI) + sum(dati$NUMVOTICONTESTATI)), sum(dati$NUMVOTIBIANCHI) + sum(dati$NUMVOTINONVALIDI) + sum(dati$NUMVOTICONTESTATI)))
barplot(totalone$valore, names.arg = totalone$tipo, col=c(3,2,"grey"))

m <- leaflet() %>%
      setView(lng = 12.567380, lat = 41.871940, zoom = 6) %>% 
      addProviderTiles('CartoDB.Positron')
pal <- colorNumeric(
  palette = c("red","yellow","green"),
  domain = dati$PERC_SI)

m  %>%  addCircleMarkers(data = dati,lat = dati$Latitudine, lng = dati$Longitudine,
  radius = sqrt(dati$VOTANTI/mean(dati$VOTANTI)),
  color = ~pal(PERC_SI),
  stroke = FALSE, fillOpacity = 0.5,
  popup = paste("comune: ", dati$DESCCOMUNE, "<br>",
    "votanti: ", dati$VOTANTI, "<br>",
    "percvotanti: ", round(100*dati$PERC_VOTANTI), "%<br>", 
    "num voti si: ", dati$NUMVOTISI, "<br>",
    "perc si: ", round(100*dati$PERC_SI), "%",
    sep="")
) %>% addLegend("bottomright", pal = pal, values = dati$PERC_SI,
            title = "PERC_SI",
            opacity = 1
)

write.csv(dati, "output/edited.csv", row.names = F)

