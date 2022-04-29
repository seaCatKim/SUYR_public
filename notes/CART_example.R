## Regression tree example
## from http://www.sthda.com/english/articles/35-statistical-machine-learning-essentials/141-cart-model-decision-tree-essentials/


library(tidyverse)
library(caret)
library(rpart)
#library(AppliedPredictiveModeling)
library(mlbench)

# load Boston house price data from MASS package
data("Boston", package = 'MASS')
str(Boston)

# inspect the data
sample_n(Boston, 3)

# visualization
# from caret book topepo.github.io/caret/visualizations.html
# scatter plots
regVar <- c("age", "lstat", "tax")
str(Boston[, regVar])


theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(0.2, 0.2, 0.2, 0.4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, 0.7)
theme1$plot.line$lwd <- 2

featurePlot(x = Boston[, regVar],
            y = Boston$medv,
            plot = "scatter",
            layout = c(3,1))

# add smoother
featurePlot(x = Boston[, regVar],
            y = Boston$medv,
            plot = "scatter",
            type = c("p", "smooth"),
            span = 0.5,
            layout = c(3,1))

# split the data into training and test set
set.seed(123)

training.samples <- Boston$medv %>% 
  createDataPartition(p = 0.8, list = FALSE)

train.data <- Boston[training.samples, ]
test.data <- Boston[-training.samples, ]

# Fit the model on the training set
# the best complexity parameter (cp) value is the one that minimizes the prediction error RMSE room mean sq error
# RMSE = mean((observeds - predicteds)^2) %>% sqrt()
set.seed(123)
model <- train(medv ~ ., data = train.data, method = "rpart",
               trControl = trainControl("cv", number = 10),
               tuneLength = 10)

# plot model error vs different values of cp
plot(model)

# Print the best tuning parameter cp that minimizes the model RMSE
model$bestTune

# Plot the final tree model
par(xpd = NA)
plot(model$finalModel)
text(model$finalModel, digits = 3)

# Decision rules in the model
model$finalModel

# Make predictions on the test data
predictions <-  model %>% predict(test.data)
head(predictions)

# Compute the prediction error RMSE
RMSE(predictions, test.data$medv)

mean(predictions == test.data$medv)

# tree with rpart https://www.r-bloggers.com/2021/04/decision-trees-in-r/
set.seed(1234)

tree <- rpart(medv ~ ., data = train.data)
plot(tree)
printcp(tree)
plotcp(tree)
