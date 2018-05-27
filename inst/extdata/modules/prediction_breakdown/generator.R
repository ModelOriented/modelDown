library("DALEX")
library(ggplot2)
library(DT)
library(kableExtra)

save_plot_image <- function(file_name, data){
  pl <- do.call(plot, data)
  ggsave(file_name, pl, png, width = 800, height = 500, limitsize = FALSE)
}

generate_prediction_breakdown_data <- function(explainer, observation_number){
  observation_to_present <- explainer$data[observation_number,]
  prediction_breakdown(explainer, observation = observation_to_present) 
}

create_prediction_breakdown_file <- function(observation_number, explainers, img_folder, label){
  breakdown_data <- lapply(explainers, generate_prediction_breakdown_data, observation_number)
  img_filename <- paste('prediction_breakdown_', observation_number, '_', label,'.png', sep='')
  img_path <- file.path(img_folder, img_filename) 
  
  file.create(img_path)
  save_plot_image(img_path, breakdown_data)
  
  html_table <- kable_styling(kable(explainers[[1]]$data[observation_number,], row.names = FALSE), bootstrap_options = c("responsive", "bordered", "hover"))

  list(
    name=paste("Worst prediction for ", label, " - observation #", observation_number, sep = ""),
    img_filename=img_filename,
    observation=html_table
    )
}

find_observations <- function(explainer){
  perf <- model_performance(explainer)
  observation_numbers <- head(order(perf$diff, decreasing = TRUE), 1)
  observation_numbers
}

create_prediction_breakdown_for_explainer <- function(explainer, explainers, img_folder){
  observations_numbers <- find_observations(explainer)
  breakdown_models <- lapply(observations_numbers, create_prediction_breakdown_file, explainers, img_folder, explainer$label)
  breakdown_models
}

prediction_breakdown_generator <- function(explainers, img_folder) {
  breakdown_models <- lapply(explainers, create_prediction_breakdown_for_explainer, explainers, img_folder)

  for(i in 1:length(breakdown_models)){
    breakdown_models[[i]] <- breakdown_models[[i]][[1]]
  }
  
  list(
    display_name='Prediction breakdown',
    name='prediction_breakdown',
    data=list(
        models=breakdown_models
    )
  )
}
