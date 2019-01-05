# Damit setzen wir das Working Directory auf den Ordner dieser Datei
if (!is.null(parent.frame(2)$ofile)) {
  this.dir <- dirname(parent.frame(2)$ofile)
  setwd(this.dir)
}

library(data.table)
library(ggplot2)
library(caret)
library(e1071)

data <- fread("Data_for_UCI_named.csv")
data$stabf <- data$stabf == "stable"


train.index <- createDataPartition(data$stabf, p = 0.75, list = FALSE)
train <- data[train.index, ]
test <- data[-train.index, ]


model <- glm(stabf ~ tau1 + tau2 + tau3 + tau4 +
                      p2 + p3 + p4 +
                      g1 + g2 + g3 + g4, family = binomial(), data = train)

pred <- predict(model, test, type="response")
pred <- pred > 0.5
pred.acc <- confusionMatrix(table(pred, test$stabf))$overall["Accuracy"]
print(pred.acc)

save(model, file="model_logistische_regression.RData")
