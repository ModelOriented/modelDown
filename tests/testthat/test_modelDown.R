context("Check modelDown() function")

test_that("Default arguments", {
  expect_true({
    require("ranger")
    require("breakDown")
    require("DALEX")
    HR_data_subset = head(HR_data, 3000)
    HR_data_subset2 = head(HR_data, 4000)

    # ranger
    HR_ranger_model <- ranger(
      as.factor(left) ~ .,
      data = HR_data_subset,
      num.trees = 500,
      classification = TRUE,
      probability = TRUE
    )
    explainer_ranger <- explain(HR_ranger_model,
                                data = HR_data_subset, y = HR_data_subset$left, function(model, data) {
                                  return(predict(model, data)$prediction[, 2])
                                }, na.rm = TRUE)

    # glm
    HR_glm_model1 <- glm(left ~ ., HR_data_subset, family = "binomial")
    HR_glm_model2 <- glm(left ~ ., HR_data_subset2, family = "binomial")
    explainer_glm1 <-
      explain(HR_glm_model1, data = HR_data_subset, y = HR_data_subset$left)
    explainer_glm2 <-
      explain(HR_glm_model2, data = HR_data_subset2, y = HR_data_subset2$left)

    modelDown::modelDown(explainer_ranger, list(explainer_glm1, explainer_glm2),
                         output_folder = "modelDown_tmp")

    TRUE
  })
})

