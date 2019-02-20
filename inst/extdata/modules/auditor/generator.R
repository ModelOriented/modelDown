library("DALEX")
library(ggplot2)

save_plot_image <- function(file_name, models, type, settings){
  pl <- do.call(plot, c(models, type = type))
  ggsave(file_name, pl, settings$device)
}

make_audit_plot_model <- function(explainers, img_folder, y, options) {

  models <- lapply(explainers, function(explainer) {
    auditor::audit(explainer)
  })

  plot_settings <- getPlotSettings(options, "a")
  extension <- plot_settings$device

  audit_plots <- list(c(paste("acf", extension, sep = '.'), "ACF"),
                      c(paste("rroc", extension, sep = '.'), "RROC"),
                      c(paste("scale_location", extension, sep = '.'), "ScaleLocation"),
                      c(paste("residuals", extension, sep = '.'), "Residual"),
                      c(paste("ranking", extension, sep = '.'), "ModelRanking"),
                      c(paste("rec", extension, sep = '.'), "REC"))
  # LIFT and ROC only for binary classification
  if (class(y) == "numeric" && length(levels(as.factor(y))) == 2) {
    audit_plots <- append(audit_plots, list(c(paste("roc", extension, sep = '.'), "ROC")))
    audit_plots <- append(audit_plots, list(c(paste("lift", extension, sep = '.'), "LIFT")))
  } else if (class(y) == "factor") {
    warning("ROC and LIFT charts are supported only for binary classification.")
  }
  result <- list()
  for(audit_plot in audit_plots) {
    img_filename <- audit_plot[1]
    img_path <- file.path(img_folder, img_filename)

    file.create(img_path)

    save_plot_image(img_path, models, audit_plot[2], plot_settings)
    result[audit_plot[2]] <- img_filename
  }

  result$link <- save_to_repository(models, options)

  return(result)
}


generator <- function(explainers, options, img_folder) {

    y = explainers[[1]]$y
    audit_img_filename <-
      make_audit_plot_model(explainers, img_folder, y, options)
    auditor_plots <- list(
      roc_img_filename = audit_img_filename$ROC,
      lift_img_filename = audit_img_filename$LIFT,
      acf_img_filename = audit_img_filename$ACF,
      ranking_img_filename = audit_img_filename$ModelRanking,
      residuals_img_filename = audit_img_filename$Residual,
      rec_img_filename = audit_img_filename$REC,
      rroc_img_filename = audit_img_filename$RROC,
      scale_location_img_filename = audit_img_filename$ScaleLocation,
      archivist_link=audit_img_filename$link
    )


  list(
    display_name='Auditor',
    name='auditor',
    data=auditor_plots
  )
}

