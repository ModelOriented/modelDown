library(stringi)

.copyAssets <- function(from, to) {
  asset_files <- list.files(from)
  css_files <- asset_files[grepl(".*.css", asset_files)]
  css_files_paths <- lapply(css_files, function(name) {file.path(from, name)})
  file.copy(css_files_paths, to, recursive=TRUE, overwrite = TRUE)
}

.generateModules <- function(modules_names, output_folder, explainer) {
  return(lapply(modules_names, function(module_name) {
    print(module_name)
    generator_path <- system.file("extdata", "modules", module_name, "generator.R", package = "modelDown")
    source(generator_path)
    generator <- get(paste(module_name, "_generator", sep=""))
    data <- generator(explainer, file.path(output_folder, "img"))
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

.renderMainPage <- function(modules, output_folder) {
  content_template <- readLines(system.file("extdata", "template", "index_template.html", package = "modelDown"))
  content <- whisker::whisker.render(content_template)
  output_path <- file.path(output_folder, "index.html")
  .renderPage(content, modules, output_path)
}

modelDown <- function(..., modules_names = c("model_performance", "variable_importance", "variable_response", "prediction_breakdown"),
                      output_folder="output") {
  
  .ensureOutputFolderStructureExist(output_folder);
  do.call(file.remove, list(list.files(output_folder, full.names = TRUE, recursive = TRUE)))
  .copyAssets(system.file("extdata", "template", package = "modelDown"), output_folder)
  
  explainers <- list(...)
  modules <- .generateModules(modules_names, output_folder, explainers)
  
  .renderModules(modules, output_folder)
  .renderMainPage(modules, output_folder)
  browseURL(file.path(output_folder, "index.html"))
}

