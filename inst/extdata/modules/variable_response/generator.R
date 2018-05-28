library("DALEX")
library(ggplot2)

save_plot_image <- function(file_name, models, options){

  width <- getPlotWidth(options, "vr.plot_width")

  pl <- do.call(plot, models)
  ggsave(file_name, pl, png, width = width, height = 500, limitsize = FALSE)
}

make_variable_plot <- function(variable_name, types, explainers, img_folder, options) {
  img_filename <- paste('variable_response_', variable_name, '_', paste(types, collapse=''), '.png', sep='')
  img_path <- file.path(img_folder, img_filename)

  models_per_type <- lapply(types, function(type) {
    lapply(explainers, function(explainer) { variable_response(explainer, variable_name, type=type) })
  })

  models <- do.call(c, models_per_type)

  file.create(img_path)
  save_plot_image(img_path, models, options)

  return(img_filename)
}

make_variable_plot_model <- function(variable_name, explainers, img_folder, options) {

  types <- options[["vr.type"]]
  if(is.null(types)) {
    types <- c("pdp", "ale")
  }

  plot_filename <-make_variable_plot(variable_name, types, explainers, img_folder, options)

  list(
    variable_name=variable_name,
    img_filename=plot_filename
  )
}

generator <- function(explainers, options, img_folder) {

  variables <- options[["vr.vars"]]
  if(is.null(variables)) {
    variables <- colnames(explainers[[1]]$data)
  }

  variable_models <- lapply(variables, make_variable_plot_model, explainers, img_folder, options)

  list(
    display_name='Variable response',
    name='variable_response',
    data=list(
      variables=variable_models
    )
  )
}

