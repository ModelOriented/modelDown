library("DALEX")
library(ggplot2)

save_plot_image <- function(file_name, models, type, options){

  plot_settings <- getPlotSettings(options, "a")

  pl <- do.call(plot, c(models, type = type))
  ggsave(file_name, pl, svg, width = plot_settings$width, height = 5, limitsize = FALSE)
}

make_audit_plot_model <- function(explainers, img_folder, y, options) {

  models <- lapply(explainers, function(explainer) {
    auditor::audit(explainer)
  })

  audit_plots <- list(c("acf.svg", "ACF"), c("rroc.svg", "RROC"),
                      c("scale_location.svg", "ScaleLocation"), c("residuals.svg", "Residual"),
                      c("ranking.svg", "ModelRanking"), c("rec.svg", "REC"))
  # LIFT and ROC only for binary classification
  if (class(y) == "numeric" && length(levels(as.factor(y))) == 2) {
    audit_plots <- append(audit_plots, list(c("roc.svg", "ROC")))
    audit_plots <- append(audit_plots, list(c("lift.svg", "LIFT")))
  } else if (class(y) == "factor") {
    warning("ROC and LIFT charts are supported only for binary classification.")
  }
  result <- list()
  for(audit_plot in audit_plots) {
    img_filename <- audit_plot[1]
    img_path <- file.path(img_folder, img_filename)

    file.create(img_path)

    save_plot_image(img_path, models, audit_plot[2], options)
    result[audit_plot[2]] <- img_filename
  }

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
      scale_location_img_filename = audit_img_filename$ScaleLocation
    )


  list(
    display_name='Auditor',
    name='auditor',
    data=auditor_plots
  )
}

