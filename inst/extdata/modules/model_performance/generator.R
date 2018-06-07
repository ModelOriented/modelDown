library("DALEX")
library(ggplot2)

save_plot_image <- function(file_name, models, options){

  width <- getPlotWidth(options, "mp.plot_width")

  pl <- do.call(plot, models)
  ggsave(file_name, pl, png, width = width, height = 500, limitsize = FALSE)
}

make_model_performance_plot_model <- function(explainers, img_folder, options) {
  img_filename <- 'model_performance.png'
  img_box_filename <- 'model_performance_box.png'
  img_path <- file.path(img_folder, img_filename)
  img_box_path <- file.path(img_folder, img_box_filename)

  models <- lapply(explainers, function(explainer) {
    model_performance(explainer)
  })

  file.create(img_path)
  width <- getPlotWidth(options, "mp.plot_width")

  pl <- do.call(plot, models)
  ggsave(img_path, pl, png, width = width, height = 500, limitsize = FALSE)
  models$geom <- "boxplot"
  pl_box <- do.call(plot, models)
  ggsave(img_box_path, pl_box, png, width = width, height = 400, limitsize = FALSE)

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
