context("Check modelDown() function")

test_that("Default arguments", {
  expect_true({

require("ranger")
require("breakDown")
require("DALEX")

# ranger
HR_ranger_model <- ranger(as.factor(left) ~ .,
                          data = HR_data, num.trees = 500, classification = TRUE, probability = TRUE)
explainer_ranger <- explain(HR_ranger_model,
                            data = HR_data, y = HR_data$left, function(model, data) {
                              return(predict(model, data)$prediction[,2])
                            }, na.rm=TRUE)

# glm
HR_glm_model <- glm(left~., HR_data, family = "binomial")
explainer_glm <- explain(HR_glm_model, data=HR_data, y = HR_data$left)

modelDown::modelDown(explainer_ranger, explainer_glm,
                     output_folder = "modelDown_tmp")

TRUE
  })
})

