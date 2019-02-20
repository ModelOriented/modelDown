#' Generates a website with HTML summaries for predictive models
#'
#' @param ... one or more explainers createdwith \code{DALEX::explain()} function
#' @param modules modules that should be included in the website
#' @param output_folder folder where the website will be saved
#'
#' @details
#' Additional arguments that could by passed by name:
#' \itemize{
#'   \item{vr.vars} {variables which will be examined in Variable Response module. Defaults to all variables. Example vr.vars = c("var1", "var2")}
#'   \item{pb.observations} {observations which will be examined in Prediction Breakdown module. When not given it selects worst predicted observations for each model. Example pb.observations = c(1,2,3) where 1,2,3 are observation numbers.}
#'   \item{vr.type} {types of examinations which will be conducteed in Variable Response module. Defaults to "pdp". Example vr.type = c("ale", "pdp")}
#'   \item{plot_width default} {width for plots (in inches). Defaults to 8. Example plot_width = 9.5}
#'   \item{a.plot_width} {Override plot width for Auditor plots. Defaults to plot_width. Example vi.plot_width = 10}
#'   \item{vr.plot_width} {Override plot width for Variable Response module. Defaults to plot_width. Example vr.plot_width = 10}
#'   \item{mp.plot_width} {Override plot width for Model Performance module. Defaults to plot_width. Example mp.plot_width = 10}
#'   \item{pb.plot_width} {Override plot width for Prediction Breakdown module. Defaults to plot_width. Example pb.plot_width = 10}
#'   \item{vi.plot_width} {Override plot width for Variable Importance module. Defaults to plot_width. Example vi.plot_width = 10}
#' }
#'
#' @export
#' @import kableExtra
#' @import whisker
#' @import ggplot2
#' @importFrom grDevices svg
#' @importFrom graphics plot
#' @author Magda Tatarynowicz, Kamil Romaszko, Mateusz Urabński
#' @examples
#' \dontrun{
#' require("ranger")
#' require("breakDown")
#' require("DALEX")
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
#'   modules = c("auditor", "model_performance", "variable_importance",
#'               "variable_response", "prediction_breakdown"),
#'   output_folder = "modelDown_output",
#'   vr.vars= c("average_montly_hours", "time_spend_company"),
#'   pb.observations = c(1,2,3),
#'   vr.type = "ale",
#'   plot_width=7,
#'   pb.plot_width=8.1,
#'   mp.plot_width=8.2,
#'   vi.plot_width=8.3,
#'   vr.plot_width=8.4,
#'   a.plot_width=8.5)
#' }
modelDown <- function(...,
                      modules = c("auditor", "model_performance", "variable_importance", "variable_response", "prediction_breakdown"),
                      output_folder="output",
                      repository_name="repository") {

  source(system.file("extdata", "config.R", package = "modelDown"))
  args <- list(..., version=1.0 )
  #named arguments are options (except those specified after ... in function definition)
  options <- args[names(args) != ""]
  options[["output_folder"]] <- output_folder
  options[["repository_name"]] <- repository_name

  #unnamed arguments are explainers
  explainers <- args[names(args) == ""]

  ensureOutputFolderStructureExist(output_folder);
  do.call(file.remove, list(list.files(output_folder, full.names = TRUE, recursive = TRUE)))

  # create local repository
  repository <- file.path(output_folder, options[["repository_name"]])
  archivist::createLocalRepo(repoDir = repository)

  # save explainers
  for(explainer in explainers){
    saveRDS(explainer, file = paste0(output_folder,"/explainers/", explainer$label, ".rda"))
  }
  copyAssets(system.file("extdata", "template", package = "modelDown"), output_folder)

  generated_modules <- generateModules(modules, output_folder, explainers, options)

  renderModules(generated_modules, output_folder)
  renderMainPage(generated_modules, output_folder, explainers, options)
  utils::browseURL(file.path(output_folder, "index.html"))
}


getPlotSettings <- function(options, options_prefix = NULL, default_width = DEFAULT_WIDTH) {

  if(!is.null(options_prefix)) {
    plot_width_variable <- paste(options_prefix, ".plot_width", sep = "")
    font_size_variable <- paste(options_prefix, ".font_size", sep = "")
  }

  width <- getVarOrDefault(options, plot_width_variable, "plot_width", default_value = default_width)

  default_font_size <- round(width/DEFAULT_WIDTH_TO_FONT_SIZE_DIVIDER)
  font_size <- getVarOrDefault(options, font_size_variable, "font_size", default_value = default_font_size)

  return(list(
    width = width,
    font_size = font_size
  ))
}

getVarOrDefault <- function(options, var1, var2, default_value) {
  if(!is.null(var1)) {
    value <- options[[var1]]
  }
  if(is.null(value)) {
    value <- options[[var2]]
  }
  if(is.null(value)) {
    value <- default_value
  }
  return(value)
}

save_to_repository <- function(artifact, options){
  # todo - repository name from options
  repository <- file.path(options[["output_folder"]], options[["repository_name"]])
  hash <- archivist::saveToLocalRepo(artifact, repoDir=repository)

  remote_path <- options[["remote_repository_path"]]
  link <- ''
  if(is.null(remote_path)) {
    link <- paste('archivist::loadFromLocalRepo(md5hash = "', hash, '", ', 'repoDir = "', repository,'")', sep = '')
  }
  else {
    link <- paste('archivist::aread("', remote_path, '/', repository, '/', hash, '")', sep = '')
  }
  return(link)
}

makeGeneratorEnvironment <- function() {
  e <- new.env()
  e$getPlotSettings <- getPlotSettings
  e$save_to_repository <- save_to_repository
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
    generator_path <-
      system.file("extdata", "modules", module_name, "generator.R", package = "modelDown")
    generator_env <- makeGeneratorEnvironment()
    source(generator_path, local = generator_env)
    data <- generator_env$generator(explainers, options, file.path(output_folder, "img"))
    return(data)
  }))
}

ensureOutputFolderStructureExist <- function(output_folder) {
  if(!dir.exists(output_folder)) {
    dir.create(output_folder)
  }

  img_folder_path <- file.path(output_folder, "explainers")
  if(!dir.exists(img_folder_path)) {
    dir.create(img_folder_path)
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

  base_template_path <-
    system.file("extdata", "template", "base_template.html", package = "modelDown")
  base_template <- readLines(base_template_path)
  page <- whisker.render(base_template, data)
  file.create(output_path)
  writeLines(page, output_path)
}

renderModules <- function(modules, output_folder) {
  lapply(modules, function(module) {
    module_path <- file.path("modules", module[['name']])
    content_template <-
      readLines(system.file("extdata", module_path, "template.html", package = "modelDown"))

    content <- whisker.render(content_template, module[['data']])
    output_path <- file.path(output_folder, paste(module[['name']], ".html", sep=""))
    renderPage(content, modules, output_path)
  })
}

renderMainPage <- function(modules, output_folder, explainers, options) {
  data_set <- explainers[[1]]$data
  numeric_columns <- which(sapply(data_set, class) != "factor")
  factor_columns <- which(sapply(data_set, class) == "factor")
  variables_data <- kable_styling(kable(psych::describe(data_set[,numeric_columns])), bootstrap_options = c("responsive", "bordered", "hover"))

  main_page_data <- list(
    explainers = renderExplainersList(explainers),
    data_summary = variables_data,
    factor_summary = renderFactorTables(data_set, factor_columns),
    observations_count = nrow(explainers[[1]]$data),
    columns_count = ncol(explainers[[1]]$data)
  )

  content_template <-
    readLines(system.file("extdata", "template", "index_template.html", package = "modelDown"))
  content <- whisker.render(content_template, main_page_data)
  output_path <- file.path(output_folder, "index.html")
  renderPage(content, modules, output_path)
}

renderExplainersList <- function(explainers){
  explainers_ul <- "<ul>"
  for(explainer in explainers){
    explainers_ul <-
      paste(
        explainers_ul,
        "<li>",
        explainer$label,
        " <a href='explainers/",
        explainer$label,
        ".rda'>(download)</a></li>",
        sep = ""
      )
  }
  explainers_ul <- paste(explainers_ul, "</ul>", sep = "")
  explainers_ul
}

renderFactorTables <- function(data_set, factor_columns){
  factor_data <- vector()
  factor_data <- NULL

  if(length(factor_columns) > 0) {
    for(i in 1:length(factor_columns)){
      column_number <- factor_columns[[i]]
      column_name <- names(factor_columns)[i]
      temp_table <- kable_styling(kable(table(data_set[, column_number]), col.names = c(column_name, "Frequency")),bootstrap_options = c("responsive", "bordered", "hover"),full_width = FALSE)
      factor_data <- paste(factor_data, temp_table, sep="<br>")
    }
  }

  return(factor_data)
}

