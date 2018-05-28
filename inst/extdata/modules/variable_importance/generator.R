library(DALEX)
library(ggplot2)
library(kableExtra)

save_plot_image <- function(file_name, models, options){

  width <- getPlotWidth(options, "vi.plot_width")

  pl <- do.call(plot, models)
  ggsave(file_name, pl, png, width = width, height = 500, limitsize = FALSE)
}

make_variable_importance_table <- function(explainer, img_folder) {
  label <- explainer$label

  model <- variable_importance(explainer, type="raw")

  html <- kable_styling(kable(model[,-3], row.names = FALSE), bootstrap_options = c("responsive", "bordered", "hover"), full_width = FALSE)

  list(
    name=label,
    dataframe=html
  )
}

create_plot_image <- function(explainers, img_folder, options){
  img_filename <- 'variable_importance.png'
  img_path <- file.path(img_folder, img_filename)

  models <- lapply(explainers, function(explainer) {
    variable_importance(explainer, type="raw")
  })

  file.create(img_path)
  save_plot_image(img_path, models, options)
}

generator <- function(explainers, options, img_folder) {

  variable_importance_models <- lapply(explainers, make_variable_importance_table, img_folder)

  create_plot_image(explainers, img_folder, options)

  list(
    display_name='Variable importance',
    name='variable_importance',
    data=list(
      img_filename='variable_importance.png',
      models=variable_importance_models
    )
  )
}
