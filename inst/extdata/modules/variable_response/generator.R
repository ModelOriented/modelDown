library("DALEX")
library(ggplot2)

save_plot_image <- function(file_name, models){
  pl <- do.call(plot, models)
  ggsave(file_name, pl, png, width = 800, height = 500, limitsize = FALSE)
}

make_variable_plot <- function(variable_name, types, explainers, img_folder) {
  img_filename <- paste('variable_response_', variable_name, '_', paste(types, collapse=''), '.png', sep='')
  img_path <- file.path(img_folder, img_filename) 
  
  models_per_type <- lapply(types, function(type) {
    lapply(explainers, function(explainer) { variable_response(explainer, variable_name, type=type) })
  })
       
  models <- do.call(c, models_per_type)              
    
  file.create(img_path)
  save_plot_image(img_path, models)
  
  return(img_filename)
}

make_variable_plot_model <- function(variable_name, explainers, img_folder) {
  plot_filename <-make_variable_plot(variable_name, c("pdp", "ale"), explainers, img_folder)
  
  list(
    variable_name=variable_name,
    img_filename=plot_filename
  )
}

variable_response_generator <- function(explainers, img_folder) {
  
  variables <- colnames(explainers[[1]]$data)
  variable_models <- lapply(variables, make_variable_plot_model, explainers, img_folder)
  
  list(
    display_name='Variable response',
    name='variable_response',
    data=list(
      variables=variable_models
    )
  )
}

