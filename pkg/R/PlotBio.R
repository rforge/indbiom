#' Plot Level Biomass Computation
#'
#' This function calculates Biomass values of all trees on PlotID basis.
#'
#' @param
#' field_data: Dataframe of SpeciesName, StateName, Girth (cm)
#' @param
#' outfile: Path to output file where the outputs have to be generated.
#' @param
#' PlotIDfile: File containing PlotID and Lat and Long for joining with Biomass outputs.
#'
#' @details
#' Biomass is computed on a tree basis using specieswise allometric equations with Diameter > 10cm.
#' An incremental biomass approach is used for trees of diameter < 10cm.
#'
#' The function takes minimum of one input to calculate the biomass (field_data)
#' Additionally it take PlotIDfile to join Lat and Long information from a seperate file based on PlotID column
#' The output of the function can be directly saved into a csv file using outfile argument (See examples).
#'
#'
#' @keywords Plot Biomass
#' @export
#' @examples
#' ##Sample data is in data folder
#' field_data = read.csv("data/sample_tree_data.csv") #read a csv file to a dataframe
#' pidfile = "data/sample_plotid_file.csv" # path to plotid locations file
#' outputfile = "data/sample_output.csv" # path to output file.
#'
#' ## only using field_data
#' PlotBio(field_data) # shows the summary of the plot biomass
#' output = PlotBio(field_data) # can be stored in a data frame
#'
#' ## Using PlotIDfile
#' output = PlotBio(field_data,PlotIDfile=pidfile) # attaches lat and long information
#'
#' ## save output to a separate file
#' PlotBio(field_data,outfile = outputfile,PlotIDfile=pidfile)
#' # output is summarised along with lat and long information
#'
#' ## or
#' PlotBio(field_data,outfile = outputfile) #no lat and long file.


PlotBio <- function(field_data,outfile,PlotIDfile)
{
  VolEq_DB = readRDS("data/IndBiom.rdat") #database file of volume equations
  #data(VolEq_DB, envir=environment()) # load volume equation database - to speed up
  column_names = colnames(field_data)
  if (!("SpeciesName" %in% colnames(field_data)))
  {
    stop("No Column - SpeciesName Detected in database")
  }
  else if (!("StateName" %in% colnames(field_data)))
  {
    stop("No Column - StateName Detected in database")
  }
  else if (!("Girth" %in% colnames(field_data)))
  {
    stop("No Column - Girth Detected in database")
  }
  else if (!("PlotID" %in% colnames(field_data)))
  {
    stop("No Column - PlotID Detected in database")
  }

  print("Input File Ok. Proceeding with Biomass Estimation")
  no_samples = nrow(field_data)
  field_data$DBH=rep(NA,no_samples)
  field_data$BA=rep(NA,no_samples)
  field_data$BM=rep(NA,no_samples)
  field_data$Vol = rep(NA,no_samples)

  ## Progress bar
  print("Biomass Estimation using Trees with Diameter > 10 cm")
  pb <- txtProgressBar(min = 0, max = no_samples, style = 3)

  for (i in 1:no_samples)
  {
    setTxtProgressBar(pb, i)
    sub_data = field_data[i,]
    sample_spname = as.character(sub_data$SpeciesName)
    sample_statename = as.character(sub_data$StateName)
    sample_girth = as.double(sub_data$Girth) # in cm
    sample_dbh = 0.01*(sample_girth/pi) # conversion to m
    field_data$DBH[i] = sample_dbh
    # DBH > 10 cm
    if (sample_dbh>0.1) # Error here means -- empty girth Record!
    {
      output = TreeBio(sample_spname,sample_statename,sample_girth,VolEq_DB=VolEq_DB)
      field_data$BA[i] = as.double(output[1])
      field_data$Vol[i] = as.double(output[2])
      field_data$BM[i] = as.double(output[3])
    }
    else {
      field_data$BA[i] = pi*(sample_dbh^2)/4
    }
  }
  close(pb)
  print("Calculating Incremental Biomass of Trees with Diameter < 10 cm")
  DBH_flg = field_data$DBH>0.1

  if (sum(DBH_flg)==0)
  {
    print("No Measurements of Trees with Diameter < 10 cm")
  }
  else
  {

    field_data$BAGT0.1 = field_data$BA*DBH_flg
    field_data$BMGT0.1 = field_data$BM*DBH_flg
    field_data$BALT0.1 = field_data$BA*(!DBH_flg)


    output2 = setNames(aggregate(cbind(field_data$BAGT0.1,field_data$BMGT0.1,field_data$BALT0.1),
                                 by=list(field_data$PlotID),FUN="sum",na.rm=TRUE),
                       c("PlotID","BAGT0.1","BMGT0.1","BALT0.1"))
    # fit linear regression
    model1 = lm(BMGT0.1 ~ BAGT0.1, data=output2)
    output2$BMpred1 = predict(model1,newdata=setNames(data.frame(output2$BAGT0.1),c("BAGT0.1")))
    output2$BMpred2 = predict(model1,newdata=setNames(data.frame(output2$BALT0.1 + output2$BAGT0.1),c("BAGT0.1")))

    output2$BMincr=output2$BMpred2-output2$BMpred1
    output2$totBM = output2$BMincr+output2$BMGT0.1
    output2$totBA = output2$BAGT0.1+output2$BALT0.1

  }
  final_output = data.frame(PlotID=output2$PlotID,BA=output2$totBA,BM=output2$totBM)
  print("Done...")

  if (hasArg(PlotIDfile))
  {
    PlotIDdata = read.csv(PlotIDfile)
    if (!("Lat" %in% colnames(PlotIDdata)))
    {
      stop("No Column - Lat Detected in PlotIDfile")
    }
    else if (!("Long" %in% colnames(PlotIDdata)))
    {
      stop("No Column - Long Detected in PlotIDfile")
    }

    final_output = merge(PlotIDdata,final_output, by =intersect(names(PlotIDdata),names(final_output)))
  }


  if (hasArg(outfile))
  {
    write.csv(final_output,file=outfile,row.names=FALSE)

  } else
  {
    return(final_output)
  }
}
