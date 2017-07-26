library(randomForest)
setwd("~/GitHub/Referendum-Costituzionale-2016")

dati <- read.csv("output/edited.csv")

# random forest

for (k in 1:10){
  train_index = sample(x = 1:dim(dati)[1], size = round(0.8*dim(dati)[1]))
  test_index = (1:dim(dati)[1])[! (1:dim(dati)[1]) %in% train_index]
  modello <- randomForest(PERC_SI ~ DESCREGIONE + ELETTORI + PopResidente +
                            PopStraniera + DensitaDemografica + SuperficieKmq,
                          data = dati[train_index,], importance = TRUE, ntree=200)
  print(paste("MAE:", mean(abs(predict(modello, dati[test_index,]) - dati[test_index,"PERC_SI"]))))
  print(paste("MSE:", mean((predict(modello, dati[test_index,]) - dati[test_index,"PERC_SI"])^2)))
}
plot(modello)


varImpPlot(modello,
           sort = T,
           main="Variable Importance") # descrregione the most important => it learned which region voted what (see Tableau viz)



# linear regression