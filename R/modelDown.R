#' Generates a website with HTML summaries for predictive models
#'
#' @param ... one or more explainers createdwith \code{DALEX::explain()} function
#' @param modules modules that should be included in the website
#' @param output_folder folder where the website will be saved
#' @param repository_name name of local archivist repository that will be created
#' @param should_open_website should generated website be automatically opened in default browser
#'
#' @details
#' Additional arguments that could by passed by name:
#' \itemize{
#'   \item{remote_repository_path} {Path to remote repository that stores folder with archivist repository. If not provided, links to local repository will be shown.}
#'   \item{device} {Device to use. Tested for "png" and "svg", but values from \code{ggplot2::ggsave} function should be working fine. Defaults to "png".}
#'   \item{vr.vars} {variables which will be examined in Variable Response module. Defaults to all variables. Example vr.vars = c("var1", "var2")}
#'   \item{vr.type} {types of examinations which will be conducteed in Variable Response module. Defaults to "pdp". Example vr.type = c("ale", "pdp")}
#' }
#'
#' @export
#' @import kableExtra
#' @import whisker
#' @import ggplot2
#' @importFrom grDevices svg
#' @importFrom graphics plot
#' @author Magda Tatarynowicz, Kamil Romaszko, Mateusz Urbański
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
#'               "variable_response"),
#'   output_folder = "modelDown_output",
#'   repository_name = "HR",
#'   remote_repository_path = "some_user/remote_repo_name",
#'   device = "png",
#'   vr.vars= c("average_montly_hours", "time_spend_company"),
#'   vr.type = "ale")
#' }

modelDown <- function(...,
                      modules = c("auditor", "model_performance", "variable_importance", "variable_response"),
                      output_folder="output",
                      repository_name="repository",
                      should_open_website=TRUE) {

  source(system.file("extdata", "config.R", package = "modelDown"))
  args <- list(..., version=1.0 )
  #named arguments are options (except those specified after ... in function definition)
  options <- args[names(args) != ""]
  options[["output_folder"]] <- output_folder
  options[["repository_name"]] <- repository_name

  #unnamed arguments are explainers
  explainers <- args[names(args) == ""]

  # arguments validation
  validateParameters(explainers, options, modules, output_folder, repository_name, should_open_website)

  ensureOutputFolderStructureExist(output_folder);
  do.call(file.remove, list(list.files(output_folder, full.names = TRUE, recursive = TRUE)))

  # create local repository
  repository <- file.path(output_folder, options[["repository_name"]])
  archivist::createLocalRepo(repoDir = repository)

  # save explainers
  for(explainer in explainers){
    saveRDS(explainer, file = paste0(output_folder,"/explainers/", explainer$label, ".rda"))
  }

  #save session info
  session_info <- devtools::session_info()
  writeLines(capture.output(session_info), paste0(output_folder,"/session_info/session_info.txt"))
  save(session_info, file = paste0(output_folder,"/session_info/session_info.rda"))

  copyAssets(system.file("extdata", "template", package = "modelDown"), output_folder)

  generated_modules <- generateModules(modules, output_folder, explainers, options)

  renderModules(generated_modules, output_folder)
  renderMainPage(generated_modules, output_folder, explainers, options)
  if(should_open_website){
    utils::browseURL(file.path(output_folder, "index.html"))
  }
}

validateParameters <- function(explainers, options, modules, output_folder, repository_name, should_open_website){
  validateExplainers(explainers)
  validateOptions(options, explainers)

  if(!is.logical((should_open_website))){
    stop("Parameter 'should_open_website' has to be logical")
  }

  # todo - put modules names into one variable
  correct_modules <- c("auditor", "model_performance", "variable_importance", "variable_response")
  if(!all(modules %in% correct_modules)){
    stop("Parameter 'modules' contains invalid names. Please refer to documentation.")
  }
}

validateExplainers <- function(explainers){
  if(length(explainers) == 0){
    stop("At least one explainer must be provided")
  }

  for(explainer in explainers){
    if(class(explainer) != "explainer"){
      stop("All explainers must be of class 'explainer' (generated by explain() from DALEX package)")
    }
  }

  # Check if explainers have the same columns
  # Comparing with first explainer
  explainer1 <- explainers[1]

  for(explainer2 in tail(explainers,-1)){
    for (i in names(explainer1)) {
      if (!(i %in% names(explainer2))) {
        stop("Explainers data variables must be identical")
      }
    }
    for (i in names(explainer2)) {
      if (!(i %in% names(explainer1))) {
        stop("Explainers data variables must be identical")
      }
    }
  }
}

validateOptions <- function(options, explainers){
  vr.type <- options[["vr.type"]]
  if(!is.null(vr.type) && !vr.type %in% c("pdp", "ale")){
    stop("Invalid 'vr.type' value. Must be 'pdp' or 'ale'.")
  }

  vr.vars <- options[["vr.vars"]]
  if(!is.null(vr.vars)){
    explainer <- explainers[1]
    if(!all(vr.vars %in% colnames(explainer$data))) {
      stop("All variables in 'vr.vars' must be contained in data frame columns.")
    }
  }

  device <- options[["device"]]
  if(!is.null(device)){
    suggested_devices <- c("png", "svg")
    # available_devices from ggplot2::ggsave() documentation
    available_devices <- c("eps", "ps", "tex", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf")
    if(!device %in% available_devices){
      stop('Device parameter is incorrect. Please provide one of "eps", "ps", "tex", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"')
    }
    if(!device %in% suggested_devices){
      warning('Package not tested for specified device. Displaying and saving plot images may not work as expected.')
      warning("It is recommended to use 'png' or 'svg' as device.")
    }
  }
}

getPlotSettings <- function(options, options_prefix = NULL, default_font_size = DEFAULT_FONT_SIZE, default_device = DEFAULT_DEVICE) {

  if(!is.null(options_prefix)) {
    font_size_variable <- paste(options_prefix, ".font_size", sep = "")
    device_variable <- paste(options_prefix, ".device", sep = "")
  }

  font_size <- getVarOrDefault(options, font_size_variable, "font_size", default_value = default_font_size)
  device <- getVarOrDefault(options, device_variable, "device", default_value = default_device)

  return(list(
    font_size = font_size,
    device = device
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
  return(e)
}

copyAssets <- function(from, to) {
  asset_files <- list.files(from)
  css_files <- asset_files[grepl(".*.css", asset_files)]
  css_files_paths <- unlist(lapply(css_files, function(name) {file.path(from, name)}))
  file.copy(css_files_paths, to, recursive=TRUE, overwrite = TRUE)
  return(css_files)
}

generateModules <- function(modules_names, output_folder, explainers, options) {
  return(lapply(modules_names, function(module_name) {
    print(paste("Generating ", module_name, "...", sep = ""))
    generator_path <-
      system.file("extdata", "modules", module_name, "generator.R", package = "modelDown")
    generator_env <- makeGeneratorEnvironment()
    source(generator_path, local = generator_env)

    module_folder <- file.path(output_folder, module_name)
    img_folder <- file.path(module_folder, "img")
    createDirIfNotExists(module_folder)
    createDirIfNotExists(img_folder)

    data <- generator_env$generator(explainers, options, img_folder)
    return(data)
  }))
}

ensureOutputFolderStructureExist <- function(output_folder) {
  createDirIfNotExists(output_folder)

  img_folder_path <- file.path(output_folder, "explainers")
  createDirIfNotExists(img_folder_path)

  img_folder_path <- file.path(output_folder, "img")
  createDirIfNotExists(img_folder_path)

  session_folder_path <- file.path(output_folder, "session_info")
  createDirIfNotExists(session_folder_path)
}

renderPage <- function(content, modules, output_path, root_path, extra_css = c()) {

  menu_links <- lapply(modules, function(module) {
    return(list(
      name=module[['display_name']],
      link=paste(module[['name']], '/index.html', sep="")
    ))
  })

  data <- list(
    content = content,
    menu_links = menu_links,
    datetime = Sys.time(),
    root_path = root_path,
    extra_css = extra_css
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
    module_path <- file.path("extdata", "modules", module[['name']])
    content_template <-
      readLines(system.file(module_path, "template.html", package = "modelDown"))

    output_module_folder <- file.path(output_folder, module[['name']])
    createDirIfNotExists(output_module_folder)

    copied_assets <- copyAssets(system.file(module_path, package = "modelDown"), output_module_folder)
    content <- whisker.render(content_template, module[['data']])
    output_path <- file.path(output_module_folder, "index.html")
    renderPage(content, modules, output_path, "../", copied_assets)
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
  renderPage(content, modules, output_path, "./")
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

createDirIfNotExists <- function(path) {
  if(!dir.exists(path)) {
    dir.create(path)
  }
}

