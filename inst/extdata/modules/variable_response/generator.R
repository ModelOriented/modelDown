library("DALEX")
library(ggplot2)

HELP_LINK <- "https://pbiecek.github.io/DALEX_docs/3-3-variableResponse.html#variableResponse"
DOCS_LINK <- "https://pbiecek.github.io/DALEX/reference/variable_response.html"

save_plot_image <- function(file_name, models, options){

  plot_settings <- getPlotSettings(options, "vr")

  pl <- do.call(plot, models) + theme(text = element_text(size=plot_settings$font_size))
  ggsave(file_name, pl, png, width = plot_settings$width, height = 500, limitsize = FALSE)
}

make_variable_plot <- function(variable_name, types, models, img_folder, options) {
  img_filename <- paste('variable_response_', variable_name, '_', paste(types, collapse=''), '.png', sep='')
  img_path <- file.path(img_folder, img_filename)

  file.create(img_path)
  save_plot_image(img_path, models, options)

  return(img_filename)
}

make_variable_plot_model <- function(variable_name, explainers, img_folder, options) {

  types <- options[["vr.type"]]
  if(is.null(types)) {
    types <- "pdp"
  }

  models_per_type <- lapply(types, function(type) {
    lapply(explainers, function(explainer) { variable_response(explainer, variable_name, type=type) })
  })

  models <- do.call(c, models_per_type)

  plot_filename <-make_variable_plot(variable_name, types, models, img_folder, options)

  link <- save_to_repository(models, options)

  list(
    variable_name=variable_name,
    img_filename=plot_filename,
    archivist_link = link
  )
}

generator <- function(explainers, options, img_folder) {

  variables <- options[["vr.vars"]]
  if(is.null(variables)) {
    variables <- colnames(explainers[[1]]$data)
  }
  variables <- sort(variables)

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

