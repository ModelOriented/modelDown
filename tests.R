{
library("randomForest")
library("breakDown")
library("DALEX")
library("gbm")
library("e1071")

# lm

apartments_lm_model <- lm(m2.price ~ construction.year + surface + floor +
                            no.rooms + district, data = apartments)
explainer_lm <- explain(apartments_lm_model,
                        data = apartmentsTest[,2:6], y = apartmentsTest$m2.price)

# randomForest
set.seed(59)
apartments_rf_model <- randomForest(m2.price ~ construction.year + surface + floor +
                                      no.rooms + district, data = apartments)
explainer_rf <- explain(apartments_rf_model,
                        data = apartmentsTest[,2:6], y = apartmentsTest$m2.price)

# gbm
apartments_gbm_model <- gbm(m2.price ~ construction.year + surface + floor +
                              no.rooms + district, data = apartments, n.trees = 1000)
explainer_gbm <- explain(apartments_gbm_model,
                         data = apartmentsTest[,2:6], y = apartmentsTest$m2.price,
                         predict_function = function(m, d) predict(m, d, n.trees = 1000))

# SVM
apartments_svm_model <- svm(m2.price ~ construction.year + surface + floor +
                              no.rooms + district, data = apartments)

explainer_svm <- explain(apartments_svm_model,
                         data = apartmentsTest[,2:6], y = apartmentsTest$m2.price)

}
# Website generation
modelDown(explainer_lm, explainer_rf,
          explainer_gbm, explainer_svm, modules = c(),
          output_folder = "modelDown_example",
          pb.observations = c(161, 731, 2741, 4454),
          vr.type = "pdp",
          a.plot_width = 10)

library(DALEX2)
data("dragons")
head(dragons)
lm_model <- lm(life_length ~ ., data = dragons)
library("randomForest")
set.seed(59)
rf_model <- randomForest(life_length ~ ., data = dragons)

dragons
explainer_rf <- explain(rf_model,
                         data = dragons[,1:7], y = dragons$life_length)


if (typeof(explainer_rf$y) == "factor") {
  #classification
} else {
  #regression
}

lm_audit <- audit(lm_model, label = "lm", data = dragons, y = dragons$life_length)
rf_audit <- audit(rf_model, label = "rf", data = dragons, y = dragons$life_length)


plot(lm_audit, type = "LIFT")

l <- list(c("acf.svg", "ACF"), c("residuals.svg", "Residual"))
result <- list()
for(e in l){

print(e[1])
  result[e[2]] <- e[1]
}
result
library(auditor)
library(randomForest)
data(mtcars)

plot(model_performance(explainer_gbm))

# fitting models
model_lm <- lm(mpg ~ ., data = mtcars)
set.seed(123)
model_rf <- randomForest(mpg ~ ., data = mtcars)

# creating a modelAudit object which contains all necessary components required for further processing
au_lm <- audit(model_lm)
au_rf <- audit(model_rf, label = "rf")

# generating plots
plot(au_lm, type = "Residual")
plot(au_lm, au_rf, type = "Residual")

plot(au_lm, au_rf, variable = "wt", type = "Prediction")

plot(au_lm, au_rf, type = "ModelCorrelation")
plot(au_lm, au_rf, variable = "wt", type = "ModelCorrelation")
?audit
plot(au_rf)
au_lm
audits = list(au_lm, au_rf)
?plot
do.call(plot, c(audits, type = "ROC"))
?do.call
