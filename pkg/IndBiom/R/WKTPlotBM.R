#' Plot Biomass WKT Format function
#'
#' This function generates a WKTfile using PlotBio function output.
#' It can be imported into QGIS
#' @param
#' PlotBio_output - PlotBio Output using PlotIDfile (Should have Lat and Long Columns).
#' @keywords Biomass
#' @details
#' It generates a WKTfile for easy loading the biomass files on any GIS platform (e.g. QGIS).
#'
#' @export
#'
#' @examples
#' ##Sample data is in data folder
#' field_data = read.csv("data/sample_tree_data.csv") #read a csv file to a dataframe
#' pidfile = "data/sample_plotid_file.csv" # path to plotid locations file
#' samplewktfile = "data/sample_output.wkt"
#'
#' output = PlotBio(field_data,PlotIDfile=pidfile) # attaches lat and long information
#'
#' WKTPlotBM(output,wktfile=samplewktfile)
#'

WKTPlotBM <- function(PlotBio_output,wktfile)
{

  if (!("Lat" %in% colnames(PlotBio_output)))
  {
    stop("No Column - Lat Detected in Input")
  }
  else if (!("Long" %in% colnames(PlotBio_output)))
  {
    stop("No Column - Long Detected in Input")
  }

  PlotBio_output$Wkt = paste("POINT (",PlotBio_output$Long," ",PlotBio_output$Lat,")",sep="")
  write.csv(file=wktfile,PlotBio_output,row.names = FALSE)
  print("WKT file has been created")

}
