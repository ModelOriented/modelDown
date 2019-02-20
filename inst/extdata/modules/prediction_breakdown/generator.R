library(DALEX)
library(ggplot2)
library(DT)
library(kableExtra)

HELP_LINK <- "https://pbiecek.github.io/DALEX_docs/4-2-predictionBreakdown.html#predictionBreakdown"
DOCS_LINK <- "https://pbiecek.github.io/DALEX/reference/prediction_breakdown.html"

save_plot_image <- function(file_name, data, options){

  plot_settings <- getPlotSettings(options, "pb")

  pl <- do.call(plot, data) + theme(text = element_text(size=plot_settings$font_size))
  ggsave(file_name, pl, png, width = plot_settings$width, height = 500, limitsize = FALSE)
}

generate_prediction_breakdown_data <- function(explainer, observation_number){
  observation_to_present <- explainer$data[observation_number,]
  prediction_breakdown(explainer, observation = observation_to_present)
}

create_prediction_breakdown_file <- function(observation_number, explainers, img_folder, label, is_worst, options){
  breakdown_data <- lapply(explainers, generate_prediction_breakdown_data, observation_number)
  img_filename <- paste('prediction_breakdown_', observation_number, '_', label,'.png', sep='')
  img_path <- file.path(img_folder, img_filename)

  file.create(img_path)
  save_plot_image(img_path, breakdown_data, options)

  html_table <- kable_styling(kable(explainers[[1]]$data[observation_number,], row.names = FALSE), bootstrap_options = c("responsive", "bordered", "hover"))

  if(is_worst) {
    name_prefix <- "Worst Prediction"
  }
  else
  {
    name_prefix <- "Prediction"
  }

  list(
    name=paste(name_prefix, " for ", label, " - observation #", observation_number, sep = ""),
    img_filename=img_filename,
    observation=html_table
    )
}

find_worst_observations <- function(explainer){
  perf <- model_performance(explainer)
  observation_numbers <- head(order(perf$diff, decreasing = TRUE), 1)
  observation_numbers
}

create_prediction_breakdown_for_explainer <- function(explainer, explainers, img_folder, options, observations_numbers=NULL){

  if(is.null(observations_numbers)) {
    observations_numbers <- find_worst_observations(explainer)
    is_worst <- TRUE
  }
  else {
    is_worst <- FALSE
  }

  breakdown_models <- lapply(observations_numbers, create_prediction_breakdown_file, explainers, img_folder, explainer$label, is_worst, options)
  breakdown_models
}

generator <- function(explainers, options, img_folder) {
  user_defined_observations <- options[["pb.observations"]]
  breakdown_models <- lapply(explainers, create_prediction_breakdown_for_explainer, explainers, img_folder, options, user_defined_observations)

  models <- unlist(breakdown_models, recursive = FALSE)
  len <- length(models)
  new_models <- list()
  for(i in 1:len) {
    new_models[[i]] <- models[[i]]
  }

  list(
    display_name='Prediction BreakDown',
    name='prediction_breakdown',
    data=list(
      models=new_models,
      HELP_LINK = HELP_LINK,
      DOCS_LINK = DOCS_LINK
    )
  )
}
