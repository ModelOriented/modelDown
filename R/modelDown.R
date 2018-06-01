#' Create page with summary for your models
#'
#' @param ... explainers
#' @param modules selected modules to generate
#' @param output_folder folder where generated page will be stored
#' @param vr.vars variables which will be examined in Variable Response module. Defaults to all variables. Example vr.vars = c("var1", "var2")
#' @param pb.observations observations which will be examined in Prediction Breakdown module. When not given it selects worst predicted observations for each model. Example pb.observations = c(1,2,3) where 1,2,3 are observation numbers.
#' @param vr.type types of examinations which will be conducteed in Variable Response module. Defaults to c("ale", "pdb"). Example vr.type = "ale"
#' @param plot_width default width for plots. Defaults to 800. Example plot_width = 750
#' @param vr.plot_width Override plot width for Variable Response module. Defaults to plot_width. Example vr.plot_width = 750
#' @param mp.plot_width Override plot width for Model Performance module. Defaults to plot_width. Example mp.plot_width = 750
#' @param pb.plot_width Override plot width for Prediction Breakdown module. Defaults to plot_width. Example pb.plot_width = 750
#' @param vi.plot_width Override plot width for Variable Importance module. Defaults to plot_width. Example vi.plot_width = 750
#'
#' @export
#' @import kableExtra
#' @import whisker
#' @author Magda Tatarynowicz, Kamil Romaszko, Mateusz Urabński
#' @examples
#' \dontrun{
#' require(ranger)
#' require(breakDown)
#' require(DALEX)
#'
#' # ranger
#' HR_ranger_model <- ranger(as.factor(left) ~ .,
#'                       data = HR_data, num.trees = 500, classification = TRUE, probability = TRUE)
#' explainer_ranger <- explain(HR_ranger_model,
#'                       data = HR_data, y = HR_data$left, function(model, data) {
#'  return(predict(model, data)$prediction[,2])
#' }, na.rm=TRUE)
#'
#' # glm
#' HR_glm_model <- glm(left~., HR_data, family = "binomial")
#' explainer_glm <- explain(HR_glm_model, data=HR_data, y = HR_data$left)
#'
#' modelDown::modelDown(explainer_ranger, explainer_glm) #all defaults
#'
#' modelDown::modelDown(explainer_glm,
#'   modules = c("model_performance", "variable_importance", "variable_response", "prediction_breakdown"),
#'   output_folder = "modelDown_output",
#'   vr.vars= c("average_montly_hours", "time_spend_company"),
#'   pb.observations = c(1,2,3),
#'   vr.type = "ale",
#'   plot_width=700,
#'   pb.plot_width=810,
#'   mp.plot_width=820,
#'   vi.plot_width=830,
#'   vr.plot_width=840)
#' }
modelDown <- function(..., modules = c("model_performance", "variable_importance", "variable_response", "prediction_breakdown"),
                      output_folder="output") {

  args <- list(..., version=1.0 )
  #named arguments are options (except those specified after ... in function definition)
  options <- args[names(args) != ""]
  #unnamed arguments are explainers
  explainers <- args[names(args) == ""]

  ensureOutputFolderStructureExist(output_folder);
  do.call(file.remove, list(list.files(output_folder, full.names = TRUE, recursive = TRUE)))
  copyAssets(system.file("extdata", "template", package = "modelDown"), output_folder)

  generated_modules <- generateModules(modules, output_folder, explainers, options)

  renderModules(generated_modules, output_folder)
  renderMainPage(generated_modules, output_folder, explainers)
  utils::browseURL(file.path(output_folder, "index.html"))
}


getPlotWidth <- function(options, plot_with_variable = NULL, default_width = 800) {
  if(!is.null(plot_with_variable)) {
    width <- options[[plot_with_variable]]
  }
  if(is.null(width)) {
    width <- options[["plot_width"]]
  }
  if(is.null(width)) {
    width <- default_width
  }

  width
}

makeGeneratorEnvironment <- function() {
  e <- new.env()
  e$getPlotWidth <- getPlotWidth
  e
}

copyAssets <- function(from, to) {
  asset_files <- list.files(from)
  css_files <- asset_files[grepl(".*.css", asset_files)]
  css_files_paths <- unlist(lapply(css_files, function(name) {file.path(from, name)}))
  file.copy(css_files_paths, to, recursive=TRUE, overwrite = TRUE)
}

generateModules <- function(modules_names, output_folder, explainers, options) {
  return(lapply(modules_names, function(module_name) {
    print(paste("Generating ", module_name, "...", sep = ""))
    generator_path <- system.file("extdata", "modules", module_name, "generator.R", package = "modelDown")
    generator_env <- makeGeneratorEnvironment()
    source(generator_path, local=generator_env)
    data <- generator_env$generator(explainers, options, file.path(output_folder, "img"))
    return(data)
  }))
}

ensureOutputFolderStructureExist <- function(output_folder) {
  if(!dir.exists(output_folder)) {
    dir.create(output_folder)
  }

  img_folder_path <- file.path(output_folder, "img")
  if(!dir.exists(img_folder_path)) {
    dir.create(img_folder_path)
  }
}

renderPage <- function(content, modules, output_path) {

  menu_links <- lapply(modules, function(module) {
    return(list(
      name=module[['display_name']],
      link=paste(module[['name']],'.html', sep="")
    ))
  })

  data <- list(
    content = content,
    menu_links = menu_links,
    datetime = Sys.time()
  )

  iteratelist(data[['menu_links']], name='menu_links')

  base_template_path <- system.file("extdata", "template", "base_template.html", package = "modelDown")
  base_template <- readLines(base_template_path)
  page <- whisker.render(base_template, data)
  file.create(output_path)
  writeLines(page, output_path)
}

renderModules <- function(modules, output_folder) {
  lapply(modules, function(module) {
    module_path <- file.path("modules", module[['name']])
    content_template <- readLines(system.file("extdata", module_path, "template.html", package = "modelDown"))

    content <- whisker.render(content_template, module[['data']])
    output_path <- file.path(output_folder, paste(module[['name']], ".html", sep=""))
    renderPage(content, modules, output_path)
  })
}

renderMainPage <- function(modules, output_folder, explainers) {
  variables_data <- kable_styling(kable(psych::describe(explainers[[1]]$data)), bootstrap_options = c("responsive", "bordered", "hover"))

  main_page_data <- list(
    explainers = renderExplainersList(explainers),
    data_summary = variables_data,
    observations_count = nrow(explainers[[1]]$data),
    columns_count = ncol(explainers[[1]]$data)
  )

  content_template <- readLines(system.file("extdata", "template", "index_template.html", package = "modelDown"))
  content <- whisker.render(content_template, main_page_data)
  output_path <- file.path(output_folder, "index.html")
  renderPage(content, modules, output_path)
}

renderExplainersList <- function(explainers){
  explainers_ul <- "<ul>"
  for(explainer in explainers){
    explainers_ul <- paste(explainers_ul, "<li>", explainer$label, "</li>", sep = "")
  }
  explainers_ul <- paste(explainers_ul, "</ul>", sep = "")
  explainers_ul
}

