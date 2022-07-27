#' @title Prostar
#' @return A new window in the default internet browser
#' @author Samuel Wieczorek
#' @examples
#' if (interactive()) {
#'     Prostar()
#' }
#'
#' @export
#'

#' @import DT
#' @import highcharter
#' @import shinyBS
#' @import shinyjqui
#' @import later
#' @import RColorBrewer
#' @import htmlwidgets
#' @import shinyWidgets
#' @import shinycssloaders
#' @import shinyAce
#' @import shinyjs
#' @import shiny
#' @import data.table
#' @import MSnbase
#' @import DAPAR
#' @import rhandsontable
#' @import MSnbase

Prostar <- function() {
    options(
        shiny.maxRequestSize = 1024^3,
        port = 3838,
        host = "0.0.0.0"
    )
    
    
    # Load packages via require instead of @import directive.
    if (!requireNamespace("BiocManager", quietly = TRUE)) {
        stop("Please install BiocManager: install.packages('BiocManager')")
    }
    
    if (!requireNamespace("gplots", quietly = TRUE)) {
        stop("Please install gplots: BiocManager::install('gplots')")
    }
    
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Please install ggplot2: BiocManager::install('ggplot2')")
    }
    
    if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
        stop("Please install RColorBrewer: BiocManager::install('RColorBrewer')")
    }
    
    if (!requireNamespace("sass", quietly = TRUE)) {
        stop("Please install sass: BiocManager::install('sass')")
    }
    
    if (!requireNamespace("later", quietly = TRUE)) {
        stop("Please install later: BiocManager::install('later')")
    }
    
    if (!requireNamespace("R.utils", quietly = TRUE)) {
        stop("Please install R.utils: BiocManager::install('R.utils')")
    }
    
    if (!requireNamespace("future", quietly = TRUE)) {
        stop("Please install future: BiocManager::install('future')")
    }
    
    if (!requireNamespace("colourpicker", quietly = TRUE)) {
        stop("Please install colourpicker: BiocManager::install('colourpicker')")
    }
    
    if (!requireNamespace("promises", quietly = TRUE)) {
        stop("Please install promises: BiocManager::install('promises')")
    }
    
    if (!requireNamespace("vioplot", quietly = TRUE)) {
        stop("Please install vioplot: BiocManager::install('vioplot')")
    }
    
    if (!requireNamespace("DAPARdata", quietly = TRUE)) {
        stop("Please install DAPARdata: BiocManager::install('DAPARdata')")
    }
    
    
    if (!requireNamespace("webshot", quietly = TRUE)) {
        stop("Please install webshot: BiocManager::install('webshot')")
    }
    
    if (!requireNamespace("shinythemes", quietly = TRUE)) {
        stop("Please install shinythemes: BiocManager::install('shinythemes')")
    }
    
    if (!requireNamespace("XML", quietly = TRUE)) {
        stop("Please install XML: BiocManager::install('XML')")
    }
    
    if (!requireNamespace("gtools", quietly = TRUE)) {
        stop("Please install gtools: BiocManager::install('gtools')")
    }
    

    if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
        stop("Please install htmlwidgets: BiocManager::install('htmlwidgets')")
    }
    
    
    if (!requireNamespace("shinycssloaders", quietly = TRUE)) {
        stop("Please install shinycssloaders: BiocManager::install('shinycssloaders')")
    }
    
    if (!requireNamespace("shinyjqui", quietly = TRUE)) {
        stop("Please install shinyjqui: BiocManager::install('shinyjqui')")
    }
    
    if (!requireNamespace("shinyWidgets", quietly = TRUE)) {
        stop("Please install shinyWidgets: BiocManager::install('shinyWidgets')")
    }
    
    if (!requireNamespace("tibble", quietly = TRUE)) {
        stop("Please install tibble: BiocManager::install('tibble')")
    }
    
    
    shiny::runApp(system.file("ProstarApp", package = "Prostar"),
        launch.browser = TRUE
    )
}
