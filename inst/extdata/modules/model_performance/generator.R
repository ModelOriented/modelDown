library("DALEX")
library(ggplot2)

save_plot_image <- function(file_name, models, height, img_folder, options){

  path <- file.path(img_folder, file_name)
  file.create(path)

  plot_settings <- getPlotSettings(options, "mp")

  plot_obj <- do.call(plot, models) + theme(text = element_text(size=plot_settings$font_size))
  ggsave(path, plot_obj, png, width = plot_settings$width, height = height, limitsize = FALSE)
}

make_model_performance_plot_model <- function(explainers, img_folder, options) {
  img_filename <- 'model_performance.png'
  img_box_filename <- 'model_performance_box.png'

  models <- lapply(explainers, function(explainer) {
    model_performance(explainer)
  })

  save_plot_image(img_filename, models, 500, img_folder, options)
  models$geom <- "boxplot"
  save_plot_image(img_box_filename, models, 400, img_folder, options)

  list(img_filename = img_filename,
       img_box_filename = img_box_filename)
}

generator <- function(explainers, options, img_folder) {

  img_filename <- make_model_performance_plot_model(explainers, img_folder, options)
  list(
    display_name='Model Performance',
    name='model_performance',
    data=list(
      img_filename_mp1 = img_filename$img_filename,
      img_filename_mp2 = img_filename$img_box_filename
    )
  )
}
