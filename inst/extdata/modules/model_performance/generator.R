library("DALEX")
library(ggplot2)

HELP_LINK <- "https://pbiecek.github.io/DALEX_docs/3-1-modelPerformance.html#modelPerformance"
DOCS_LINK <- "https://pbiecek.github.io/DALEX/reference/model_performance.html"

save_plot_image <- function(file_name, models, height, img_folder, settings){

  path <- file.path(img_folder, file_name)
  file.create(path)

  plot_obj <- do.call(plot, models) + theme(text = element_text(size=settings$font_size))
  ggsave(path, plot_obj, settings$device)
}

make_model_performance_plot_model <- function(explainers, img_folder, options) {
  plot_settings <- getPlotSettings(options, "mp")

  img_filename <- paste('model_performance', plot_settings$device, sep = '.')
  img_box_filename <- paste('model_performance_box', plot_settings$device, sep = '.')

  models <- lapply(explainers, function(explainer) {
    model_performance(explainer)
  })

  save_plot_image(img_filename, models, 5, img_folder, plot_settings)
  link_mp <- save_to_repository(models, options)

  models$geom <- "boxplot"
  save_plot_image(img_box_filename, models, 4, img_folder, plot_settings)
  link_mp_box <- save_to_repository(models, options)

  list(img_filename = img_filename,
       img_box_filename = img_box_filename,
       archivist_link_mp1 = link_mp,
       archivist_link_mp2 = link_mp_box)
}

generator <- function(explainers, options, img_folder) {

  img_filename <- make_model_performance_plot_model(explainers, img_folder, options)

  list(
    display_name='Model Performance',
    name='model_performance',
    data=list(
      img_filename_mp1 = img_filename$img_filename,
      img_filename_mp2 = img_filename$img_box_filename,
      HELP_LINK = HELP_LINK,
      DOCS_LINK = DOCS_LINK,
      archivist_link_mp1 = img_filename$archivist_link_mp1,
      archivist_link_mp2 = img_filename$archivist_link_mp2
    )
  )
}
