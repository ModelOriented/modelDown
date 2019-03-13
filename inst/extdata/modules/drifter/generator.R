library("DALEX")
library(drifter)

HELP_LINK <- 'https://modeloriented.github.io/drifter/'
DOCS_LINK <- 'https://modeloriented.github.io/drifter/reference/check_drift.html'

renderModelDriftTables <- function(data_set, factor_columns){
  factor_data <- vector()
  factor_data <- NULL

  if(length(factor_columns) > 0) {
    for(i in 1:length(factor_columns)){
      column_number <- factor_columns[[i]]
      column_name <- names(factor_columns)[i]
      temp_table <- kable_styling(kable(table(data_set[, column_number]), col.names = c(column_name, "Frequency")), bootstrap_options = c("responsive", "bordered", "hover"),full_width = FALSE)
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
  return(paste0("<div class='column'>", section_header, section_data, "</div>"))
}

generator <- function(explainer_pairs, options, img_folder) {

  drifter_data <- ""
  for(pair in explainer_pairs) {
    old_explainer <- pair[[1]]
    new_explainer <- pair[[2]]
    drifter_data <- paste0(drifter_data, "<div class='row'>", "<h3 class='model-label'>", old_explainer$label, "</h3>")
    drift <- check_drift(old_explainer$model, new_explainer$model,
                         old_explainer$data, new_explainer$data,
                         old_explainer$y, new_explainer$y,
                         predict_function = old_explainer$predict_function)

    archivist_link <- save_to_repository(drift, options)

    model_drift <- renderDrifterSection("Model Drift", drift$model_drift)
    covariate_drift <- renderDrifterSection("Covariate Drift", data.frame(Variable = drift$covariate_drift$variables , Drift = drift$covariate_drift$drift))
    residual_drift <- renderDrifterSection("Residual Drift", drift$residual_drift)
    help_section <- paste0("<help-panel help-url='", HELP_LINK, "' docs-url='", DOCS_LINK, "'></help-panel>")
    archivist_section <- paste0("<div class='archivist-code'><p>Get this object: <code>", archivist_link, "</code></p></div>")
    drifter_data <- paste0(drifter_data, model_drift, covariate_drift, residual_drift, help_section, "</div>", archivist_section)
  }

  list(
    display_name='Drifter',
    name='drifter',
    data=list(drifter_data = drifter_data)
  )
}

