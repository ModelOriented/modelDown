library("DALEX")
library(ggplot2)

HELP_LINK <- "https://pbiecek.github.io/ema/partialDependenceProfiles.html"
DOCS_LINK <- "https://modeloriented.github.io/DALEX/reference/model_profile.html"

save_plot_image <- function(file_name, models, settings){
  pl <- do.call(plot, models) + theme(text = element_text(size=settings$font_size))
  ggsave(file_name, pl, settings$device)
}

make_variable_plot <- function(variable_name, types, models, img_folder, options) {
  plot_settings <- getPlotSettings(options, "vr")
  img_filename <- paste('variable_response_', variable_name, '_', paste(types, collapse=''), '.', plot_settings$device, sep='')

  img_path <- file.path(img_folder, img_filename)

  file.create(img_path)
  save_plot_image(img_path, models, plot_settings)

  return(img_filename)
}

make_variable_plot_model <- function(variable_name, explainers, img_folder, options) {

  types <- options[["vr.type"]]
  if(is.null(types)) {
    types <- "partial"
  }

  models_per_type <- lapply(types, function(type) {
    lapply(explainers, function(explainer) { model_profile(explainer, variable_name, type=type)$agr_profiles })
  })

  models <- do.call(c, models_per_type)

  plot_filename <- make_variable_plot(variable_name, types, models, img_folder, options)

  link <- save_to_repository(models, options)

  list(
    variable_name=variable_name,
    img_filename=plot_filename,
    archivist_link = link
  )
}

sort_by_importance <- function(explainers, variables) {
  importance <- variable_importance(explainers[[1]])
  # only first permutation (from DALEX 1.0)
  importance <- importance[importance$permutation == 0,]
  variable_dropouts <- importance$dropout_loss[
    sapply(variables, function(var_name) {
      index <- which(importance$variable == var_name)
    })]
  variables_order <- order(variable_dropouts, decreasing = TRUE)
  sorted_variables <- variables[variables_order]
  return(sorted_variables)
}

generator <- function(explainers, options, img_folder, sort_by_importance = TRUE) {

  variables <- options[["vr.vars"]]
  if(is.null(variables)) {
    variables <- colnames(explainers[[1]]$data)
  }
  # how to sort variables?
  if (sort_by_importance) {
    variables <- sort_by_importance(explainers, variables)
  } else {
    variables <- sort(variables)
  }

  variable_models <- lapply(variables, make_variable_plot_model, explainers, img_folder, options)

  list(
    display_name='Variable Response',
    name='variable_response',
    data=list(
      variables=variable_models,
      HELP_LINK = HELP_LINK,
      DOCS_LINK = DOCS_LINK
    )
  )
}

