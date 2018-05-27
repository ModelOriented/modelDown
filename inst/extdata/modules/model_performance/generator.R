library("DALEX")
library(ggplot2)

save_plot_image <- function(file_name, models){
  pl <- do.call(plot, models)
  ggsave(file_name, pl, png, width = 800, height = 500, limitsize = FALSE)
}

make_model_performance_plot_model <- function(explainers, img_folder) {
  img_filename <- 'model_performance.png'
  img_path <- file.path(img_folder, img_filename) 
  
  models <- lapply(explainers, function(explainer) {
    model_performance(explainer)
  })
  
  file.create(img_path)
  save_plot_image(img_path, models)
  
  img_filename
}

model_performance_generator <- function(explainers, img_folder) {
  
  img_filename <- make_model_performance_plot_model(explainers, img_folder)
  list(
    display_name='Model performance',
    name='model_performance',
    data=list(
      img_filename=img_filename
    )
  )
}