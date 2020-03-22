library(DALEX)
library(ggplot2)
library(kableExtra)

HELP_LINK <- "https://pbiecek.github.io/ema/featureImportance.html"
DOCS_LINK <- "https://modeloriented.github.io/DALEX/reference/variable_importance.html"

save_plot_image <- function(file_name, models, settings){
  pl <- do.call(plot, models) + theme(text = element_text(size=settings$font_size))
  ggsave(file_name, pl, settings$device)
}

make_variable_importance_table <- function(explainers) {
  importance_table <- vector()
  importance_table <- NULL

  importance_table_list <- list()
  for (explainer_id in seq_along(explainers)) {
    explainer <- explainers[[explainer_id]]
    model <- variable_importance(explainer, type = "raw")
    # only first permutation (from DALEX 1.0)
    model <- model[model$permutation == 0,]
    importance_table_list[[explainer_id]] <- model[,c(1,3)]
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
  plot_settings <- getPlotSettings(options, "vi")

  img_filename <- paste('variable_importance', plot_settings$device, sep='.')
  img_path <- file.path(img_folder, img_filename)

  file.create(img_path)
  save_plot_image(img_path, models, plot_settings)

  img_filename
}

generate_models <- function(explainers){
  models <- lapply(explainers, function(explainer) {
    model <- variable_importance(explainer, type="raw")
    # only first permutation (from DALEX 1.0)
    model[model$permutation == 0,]
  })
  models
}

generator <- function(explainers, options, img_folder) {

  variable_importance_table <- make_variable_importance_table(explainers)

  models <- generate_models(explainers)

  filename <- create_plot_image(models, img_folder, options)

  link <- save_to_repository(models, options)

  list(
    display_name='Variable Importance',
    name='variable_importance',
    data=list(
      img_filename=filename,
      dataframe=variable_importance_table,
      archivist_link = link,
      HELP_LINK = HELP_LINK,
      DOCS_LINK = DOCS_LINK
    )
  )
}
