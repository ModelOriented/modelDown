library(stringi)

.getPlotWidth <- function(options, plot_with_variable = NULL, default_width = 800) {
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

.makeGeneratorEnviroment <- function() {
  e <- new.env()
  e$getPlotWidth <- .getPlotWidth
  e
}

.copyAssets <- function(from, to) {
  asset_files <- list.files(from)
  css_files <- asset_files[grepl(".*.css", asset_files)]
  css_files_paths <- lapply(css_files, function(name) {file.path(from, name)})
  file.copy(css_files_paths, to, recursive=TRUE, overwrite = TRUE)
}

.generateModules <- function(modules_names, output_folder, explainers, options) {
  return(lapply(modules_names, function(module_name) {
    print(paste("Generating ", module_name, "...", sep = ""))
    generator_path <- system.file("extdata", "modules", module_name, "generator.R", package = "modelDown")
    generator_env <- .makeGeneratorEnviroment()
    source(generator_path, local=generator_env)
    data <- generator_env$generator(explainers, options, file.path(output_folder, "img"))
    return(data)
  }))
}

.ensureOutputFolderStructureExist <- function(output_folder) {
  if(!dir.exists(output_folder)) {
    dir.create(output_folder)
  }

  img_folder_path <- file.path(output_folder, "img")
  if(!dir.exists(img_folder_path)) {
    dir.create(img_folder_path)
  }
}

.renderPage <- function(content, modules, output_path) {

  menu_links <- lapply(modules, function(module) {
    return(list(
      name=module[['display_name']],
      link=paste(module[['name']],'.html', sep="")
    ))
  })

  data <- list(
    content = content,
    menu_links = menu_links
  )

  whisker::iteratelist(data[['menu_links']], name='menu_links')

  base_template_path <- system.file("extdata", "template", "base_template.html", package = "modelDown")
  base_template <- readLines(base_template_path)
  page <- whisker::whisker.render(base_template, data)
  file.create(output_path)
  writeLines(page, output_path)
}

.renderModules <- function(modules, output_folder) {
  lapply(modules, function(module) {
    module_path <- file.path("modules", module[['name']])
    content_template <- readLines(system.file("extdata", module_path, "template.html", package = "modelDown"))

    content <- whisker::whisker.render(content_template, module[['data']])
    output_path <- file.path(output_folder, paste(module[['name']], ".html", sep=""))
    .renderPage(content, modules, output_path)
  })
}

.renderMainPage <- function(modules, output_folder, explainers) {
  library(kableExtra)

  explainers_data <- lapply(explainers, function(explainer){
    list(explainer_name = explainer$label)
  })

  variables_data <- kable_styling(kable(psych::describe(explainers[[1]]$data)), bootstrap_options = c("responsive", "bordered", "hover"))

  main_page_data <- list(
    explainers = explainers_data,
    data_summary = variables_data,
    observations_count = nrow(explainers[[1]]$data),
    columns_count = ncol(explainers[[1]]$data)
  )

  content_template <- readLines(system.file("extdata", "template", "index_template.html", package = "modelDown"))
  content <- whisker::whisker.render(content_template, main_page_data)
  output_path <- file.path(output_folder, "index.html")
  .renderPage(content, modules, output_path)
}

modelDown <- function(..., modules = c("model_performance", "variable_importance", "variable_response", "prediction_breakdown"),
                      output_folder="output") {

  args <- list(..., version=1.0 )
  #named arguments are options (except those specified after ... in function definition)
  options <- args[names(args) != ""]
  #unnamed arguments are explainers
  explainers <- args[names(args) == ""]

  .ensureOutputFolderStructureExist(output_folder);
  do.call(file.remove, list(list.files(output_folder, full.names = TRUE, recursive = TRUE)))
  .copyAssets(system.file("extdata", "template", package = "modelDown"), output_folder)

  generated_modules <- .generateModules(modules, output_folder, explainers, options)

  .renderModules(generated_modules, output_folder)
  .renderMainPage(generated_modules, output_folder, explainers)
  browseURL(file.path(output_folder, "index.html"))
}

