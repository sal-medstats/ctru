#' Setup a new project directory
#'
#' @description Setup a project directory for a CTRU study.
#'
#' @details
#'
#' Creates a default project directory.
#'
#'
#' @param path Absolute or relative path to create the directory structure.
#'
#' @export
new_project<- function(path = '',
                         ...){
    ## Detect what system we are on (determines how to create directories)
    os <- Sys.info()[1]
    if(os == 'Linux')        mkdir <- 'mkdir -p '
    else if(os == 'Windows') mkdir <- 'mkdir'
    ## Make a series of system calls to create directories
    mkdir.lib <- paste0(mkdir, path, '/lib/')
    system(paste0(mkdir.lib, 'data-raw'))
    system(paste0(mkdir.lib, 'data'))
    system(paste0(mkdir.lib, 'R'))
    system(paste0(mkdir.lib, 'vignette'))
    system(paste0(mkdir.lib, '/inst/shiny/', path))
    mkdir.doc <- paste0(mkdir, path, '/doc/')
    system(paste0(mkdir.doc, 'word/sap'))
    system(paste0(mkdir.doc, 'excel'))
    system(paste0(mkdir, path, 'tmp'))
    system(paste0(mkdir, path, 'ref'))
    system(paste0(mkdir, path, 'doc'))
    ## Create/copy template files
    system(paste0('touch ', path, ' README.md'))
    system(paste0('touch ', path, ' lib/DESCRIPTION'))
}
