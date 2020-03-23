library("DALEX")
library(ggplot2)

ROC_HELP_LINK <- "https://modeloriented.github.io/auditor/articles/model_evaluation_audit.html#receiver-operating-characteristic-roc"
ROC_DOCS_LINK <- "https://modeloriented.github.io/auditor/reference/plotROC.html"
# LIFT
LIFT_HELP_LINK <- "https://modeloriented.github.io/auditor/articles/model_evaluation_audit.html#lift-chart"
LIFT_DOCS_LINK <- "https://modeloriented.github.io/auditor/reference/plotLIFT.html"
#ACF
ACF_HELP_LINK <- "https://modeloriented.github.io/auditor/articles/model_residuals_audit.html#plotacf---autocorrelation-function-of-residuals"
ACF_DOCS_LINK <- "https://modeloriented.github.io/auditor/reference/plotACF.html"
# RANKING
RANKING_HELP_LINK <- "https://modeloriented.github.io/auditor/articles/model_performance_audit.html#model-ranking-radar-plot"
RANKING_DOCS_LINK <- "https://modeloriented.github.io/auditor/reference/plotModelRanking.html"
# RESIDUALS
RESIDUALS_HELP_LINK <- "https://modeloriented.github.io/auditor/articles/model_residuals_audit.html#plotresidual---plot-residuals-vs-observed-fitted-or-variable-values"
RESIDUALS_DOCS_LINK <- "https://modeloriented.github.io/auditor/reference/plotResidual.html"
# REC
REC_HELP_LINK <- "https://modeloriented.github.io/auditor/articles/model_residuals_audit.html#plotrec---regression-error-characteristic-rec-curve"
REC_DOCS_LINK <- "https://modeloriented.github.io/auditor/reference/plotREC.html"
# RROC
RROC_HELP_LINK <- "https://modeloriented.github.io/auditor/articles/model_residuals_audit.html#plotrroc---regression-receiver-operating-characteristic-rroc"
RROC_DOCS_LINK <- "https://modeloriented.github.io/auditor/reference/plotRROC.html"
# SCALE
SCALE_HELP_LINK <- "https://modeloriented.github.io/auditor/articles/model_residuals_audit.html#plotscalelocation---scale-location-plot"
SCALE_DOCS_LINK <- "https://modeloriented.github.io/auditor/reference/plotScaleLocation.html"


save_plot_image <- function(file_name, models, type, settings){
  pl <- do.call(plot, c(models, type = type))
  ggsave(file_name, pl, settings$device)
}

make_audit_plot_model <- function(explainers, img_folder, y, options) {

  models <- lapply(explainers, function(explainer) {
    explainer
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

    data <- list(
      # ROC
      roc_img_filename = audit_img_filename$ROC,
      ROC_HELP_LINK = ROC_HELP_LINK,
      ROC_DOCS_LINK = ROC_DOCS_LINK,
      # LIFT
      lift_img_filename = audit_img_filename$LIFT,
      LIFT_HELP_LINK = LIFT_HELP_LINK,
      LIFT_DOCS_LINK = LIFT_DOCS_LINK,
      #ACF
      acf_img_filename = audit_img_filename$ACF,
      ACF_HELP_LINK = ACF_HELP_LINK,
      ACF_DOCS_LINK = ACF_DOCS_LINK,
      # RANKING
      ranking_img_filename = audit_img_filename$ModelRanking,
      RANKING_HELP_LINK = RANKING_HELP_LINK,
      RANKING_DOCS_LINK = RANKING_DOCS_LINK,
      # RESIDUALS
      residuals_img_filename = audit_img_filename$Residual,
      RESIDUALS_HELP_LINK = RESIDUALS_HELP_LINK,
      RESIDUALS_DOCS_LINK = RESIDUALS_DOCS_LINK,
      # REC
      rec_img_filename = audit_img_filename$REC,
      REC_HELP_LINK = REC_HELP_LINK,
      REC_DOCS_LINK = REC_DOCS_LINK,
      # RROC
      rroc_img_filename = audit_img_filename$RROC,
      RROC_HELP_LINK = RROC_HELP_LINK,
      RROC_DOCS_LINK = RROC_DOCS_LINK,
      # SCALE
      scale_location_img_filename = audit_img_filename$ScaleLocation,
      SCALE_HELP_LINK = SCALE_HELP_LINK,
      SCALE_DOCS_LINK = SCALE_DOCS_LINK,
      archivist_link=audit_img_filename$link
    )

  list(
    display_name='Auditor',
    name='auditor',
    data=data
  )
}

