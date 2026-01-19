library(mlbench)
library(boot)

# load data
data(BreastCancer)
BreastCancer$Id <- NULL
BreastCancer$Marg.adhesion <- NULL
BreastCancer <- na.omit(BreastCancer)
summary(BreastCancer)


# k- fold cv 
set.seed(123)

# build a logistic regression model using all variables
glm.fit <- glm(Class ~ ., data = BreastCancer, family = binomial)

cv.error <- cv.glm(BreastCancer, glm.fit, K=5)

cv.error$delta[1]



# Another method
set.seed(234)

k <- 5
n <- nrow(BreastCancer)

folds <- sample(rep(1:k, length.out= n))

cv.error <- numeric(k)

for (i in 1:k) {
  train_data <- BreastCancer[folds != i, ]
  test_data <- BreastCancer[folds == i, ]
  
  fit.glm <- glm(Class ~ ., data = train_data, family = binomial)
  
  prob <- predict(fit.glm, newdata=test_data, type = "response")
  
  pred_class <- ifelse(prob > 0.5, "malignant", "benign")
  
  cv.error[i] <- mean(pred_class != test_data$Class)
}

cv.error
