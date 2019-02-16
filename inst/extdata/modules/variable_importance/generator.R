library(DALEX)
library(ggplot2)
library(kableExtra)

save_plot_image <- function(file_name, models, options){

  width <- getPlotWidth(options, "vi.plot_width")

  pl <- do.call(plot, models)
  ggsave(file_name, pl, png, width = width, height = 500, limitsize = FALSE)
}

make_variable_importance_table <- function(explainers) {
  importance_table <- vector()
  importance_table <- NULL

  importance_table_list <- list()
  for (explainer_id in seq_along(explainers)) {
    explainer <- explainers[[explainer_id]]
    model <- variable_importance(explainer, type = "raw")
    importance_table_list[[explainer_id]] <- model[,-3]
  }
  importance_table <- do.call(rbind, importance_table_list)

  html <- kable_styling(kable(importance_table, row.names = FALSE),
                        bootstrap_options = c("responsive", "hover"),
                        full_width = FALSE)
  i <- 1
  for(explainer_id in seq_along(explainers)){
    explainer <- explainers[[explainer_id]]
    label <- explainer$label
    html <- group_rows(html, label, i, i + nrow(importance_table_list[[explainer_id]]) - 1,
                       hline_before = TRUE, hline_after = TRUE)
    i <- i + nrow(importance_table_list[[explainer_id]])
  }

  html
}

create_plot_image <- function(models, img_folder, options){
  img_filename <- 'variable_importance.png'
  img_path <- file.path(img_folder, img_filename)

  file.create(img_path)
  save_plot_image(img_path, models, options)
}

generate_models <- function(explainers){
  models <- lapply(explainers, function(explainer) {
    variable_importance(explainer, type="raw")
  })
  models
}

generator <- function(explainers, options, img_folder) {

  variable_importance_table <- make_variable_importance_table(explainers)

  models <- generate_models(explainers)

  create_plot_image(models, img_folder, options)

  link <- save_to_repository(models, options)

  list(
    display_name='Variable Importance',
    name='variable_importance',
    data=list(
      img_filename='variable_importance.png',
      dataframe=variable_importance_table,
      archivist_link = link
    )
  )
}
