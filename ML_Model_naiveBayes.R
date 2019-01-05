# Damit setzen wir das Working Directory auf den Ordner dieser Datei
if (!is.null(parent.frame(2)$ofile)) {
  this.dir <- dirname(parent.frame(2)$ofile)
  setwd(this.dir)
}

library(data.table)
library(ggplot2)
library(caret)

data <- fread("Data_for_UCI_named.csv")
data$stabf <- as.factor(data$stabf)


train.index <- createDataPartition(data$stabf, p = 0.75, list = FALSE)
train <- data[train.index, ]
test <- data[-train.index, ]


model <- naiveBayes(stabf ~ tau1 + tau2 + tau3 + tau4 +
                      p2 + p3 + p4 +
                      g1 + g2 + g3 + g4, train)

pred <- predict(model, test, type="class")
pred.acc <- confusionMatrix(table(pred, test$stabf))$overall["Accuracy"]
print(pred.acc)

save(model, file="model_naiveBayes.RData")
