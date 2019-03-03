library("DALEX")
library(ggplot2)

save_plot_image <- function(file_name, models, type, settings){
  pl <- do.call(plot, c(models, type = type))
  ggsave(file_name, pl, settings$device)
}

make_audit_plot_model <- function(explainers, img_folder, y, options) {

  models <- lapply(explainers, function(explainer) {
    auditor::audit(explainer)
  })

  plot_settings <- getPlotSettings(options, "a")
  extension <- plot_settings$device

  audit_plots <- list(c(paste("acf", extension, sep = '.'), "ACF"),
                      c(paste("rroc", extension, sep = '.'), "RROC"),
                      c(paste("scale_location", extension, sep = '.'), "ScaleLocation"),
                      c(paste("residuals", extension, sep = '.'), "Residual"),
                      c(paste("ranking", extension, sep = '.'), "ModelRanking"),
                      c(paste("rec", extension, sep = '.'), "REC"))
  # LIFT and ROC only for binary classification
  if (class(y) == "numeric" && length(levels(as.factor(y))) == 2) {
    audit_plots <- append(audit_plots, list(c(paste("roc", extension, sep = '.'), "ROC")))
    audit_plots <- append(audit_plots, list(c(paste("lift", extension, sep = '.'), "LIFT")))
  } else if (class(y) == "factor") {
    warning("ROC and LIFT charts are supported only for binary classification.")
  }
  result <- list()
  for(audit_plot in audit_plots) {
    img_filename <- audit_plot[1]
    img_path <- file.path(img_folder, img_filename)

    file.create(img_path)

    save_plot_image(img_path, models, audit_plot[2], plot_settings)
    result[audit_plot[2]] <- img_filename
  }

  result$link <- save_to_repository(models, options)

  return(result)
}

renderModelDriftTables <- function(data_set, factor_columns){
  factor_data <- vector()
  factor_data <- NULL

  if(length(factor_columns) > 0) {
    for(i in 1:length(factor_columns)){
      column_number <- factor_columns[[i]]
      column_name <- names(factor_columns)[i]
      temp_table <- kable_styling(kable(table(data_set[, column_number]), col.names = c(column_name, "Frequency")),bootstrap_options = c("responsive", "bordered", "hover"),full_width = FALSE)
      factor_data <- paste(factor_data, temp_table, sep="<br>")
    }
  }

  return(factor_data)
}

renderTable <- function(data_table) {
  return(kable_styling(kable(data_table), bootstrap_options = c("responsive", "bordered", "hover"), full_width = FALSE))
}

renderDrifterSection <- function(section_name, data_table) {
  section_header <- paste0("<h3 class='section-label'>", section_name, "</h3>")
  section_data <- renderTable(data_table)
  return(paste0(section_header, section_data, "</br>"))
}

generator <- function(explainer_pairs, options, img_folder) {

  drifter_data <- ""
  for(pair in explainer_pairs) {
    old_explainer <- pair[[1]]
    new_explainer <- pair[[2]]
    drifter_data <- paste0(drifter_data, "<h3 class='model-label'>", old_explainer$label, "</h3>")
    drift <- check_drift(old_explainer$model, new_explainer$model,
                         old_explainer$data, new_explainer$data,
                         old_explainer$y, new_explainer$y,
                         predict_function = old_explainer$predict_function)

    covariate_drift <- renderDrifterSection("Covariate Drift", data.frame(Variable = drift$covariate_drift$variables , Drift = drift$covariate_drift$drift))
    residual_drift <- renderDrifterSection("Residual Drift", drift$residual_drift)
    model_drift <- renderDrifterSection("Model Drift", drift$model_drift)

    drifter_data <- paste0(drifter_data, covariate_drift, residual_drift, model_drift, "</br>")
  }


  list(
    display_name='Drifter',
    name='drifter',
    data=list(drifter_data = drifter_data)
  )
}

