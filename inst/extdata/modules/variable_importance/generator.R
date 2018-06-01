library(DALEX)
library(ggplot2)
library(kableExtra)

save_plot_image <- function(file_name, models, options){

  width <- getPlotWidth(options, "vi.plot_width")

  pl <- do.call(plot, models)
  ggsave(file_name, pl, png, width = width, height = 500, limitsize = FALSE)
}

make_variable_importance_table <- function(explainers) {
  importance_table<-vector()
  importance_table <- NULL

  for(explainer in explainers){
    model <- variable_importance(explainer, type="raw")
    importance_table <- rbind(importance_table, model[,-3])
  }

  html <- kable_styling(kable(importance_table, row.names = FALSE), bootstrap_options = c("responsive", "hover"), full_width = FALSE)
  i <- 1
  for(explainer in explainers){
    label <- explainer$label
    html <- html %>% group_rows(label, i, ncol(explainer$data) + 2)
    i <- i + ncol(explainer$data) + 2
  }

  html
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

  variable_importance_table <- make_variable_importance_table(explainers)

  create_plot_image(explainers, img_folder, options)

  list(
    display_name='Variable importance',
    name='variable_importance',
    data=list(
      img_filename='variable_importance.png',
      dataframe=variable_importance_table
    )
  )
}
