#' Tree Biomass Function
#'
#' This function calculates Biomass values at every tree level using species level allometric equations
#' of different states of India and wood density. (Equations Source : Forest Survey of India (FSI)).
#' @param
#' sample_spname - Species Name of the Tree.
#' @param
#' sample_statename - State Name.
#' @param
#' sample_girth - Measured Girth value in centimeter (cm).
#' @param
#' VolEq_DB - Inbuilt database for volume equations and wood density (no need to mention)
#'            Automatically updates from Data folder of the package.
#' @details
#' This function estimates the biomass of individual tree using regional allometric equations of FSI.
#' It doesn't take empty inputs of girth value and State Name.
#' If the species wise equation is not found a general species equation is used for different states.
#'
#' @keywords Biomass
#' @export
#' @examples
#' TreeBio("Tectona grandis","Karnataka",120)
#' ## Outputs
#' ## BA - Basal Area of the tree
#' ## vol - Volume of the tree and
#' ## biomass - Biomass of the tree



TreeBio <- function(sample_spname,sample_statename,sample_girth,VolEq_DB)
{
  if (!(hasArg(VolEq_DB))) {
    VolEq_DB = readRDS("data/IndBiom.rdat")
    #data(VolEq_DB, envir=environment()) #if not existing from previous function!!
  }
  sample_dbh = 0.01*(sample_girth/pi) # conversion to m
  # Turn off case sensitivity - convert everything to uppercase
  VolEq_DB$SpeciesName = toupper(VolEq_DB$SpeciesName)
  VolEq_DB$StateName = toupper(VolEq_DB$StateName)
  sample_spname = toupper(sample_spname)
  sample_statename = toupper(sample_statename)

  # State Wise Matching
  match1 = which( (VolEq_DB$StateName == sample_statename))
  VolEq_DB_sub = VolEq_DB[match1,]

  # Species wise match
  match2 = which( (VolEq_DB_sub$SpeciesName == sample_spname))

  # if species wise equation doesnot exits - Search Family Equation.
  if (length(match2)==0)
  {
    split_spname = strsplit(sample_spname," ")
    familyname = paste(split_spname[1],"SPECIES", sep = " ")
    match2 = which((VolEq_DB_sub$SpeciesName == familyname))
  }


  # if species wise equation doesnot exits - Statewise General Equation.
  if (length(match2)==0)
  {
    misc_name = "MISCELLANEOUS"
    match2 = which((VolEq_DB_sub$SpeciesName == misc_name))
  }

  VolEq_DB_sub = VolEq_DB_sub[match2,]
  if (nrow(VolEq_DB_sub)>1)
  {
    print ("Warning: Multiple Volume equations in database -- Check Database")
    print ("Proceeding With the First Match Record...")
    VolEq_DB_sub = VolEq_DB_sub[1,]
    #stop()
  }

  a0 = VolEq_DB_sub$a0
  a0.5 = VolEq_DB_sub$a0.5
  a1 = VolEq_DB_sub$a1
  a2 = VolEq_DB_sub$a2
  a3 = VolEq_DB_sub$a3
  a4 = VolEq_DB_sub$a4
  a5 = VolEq_DB_sub$a5
  squareflag = VolEq_DB_sub$SquareFlag
  wood_density = VolEq_DB_sub$WD

  sample_volume = a0 + a1 * (sample_dbh) + a2 * (sample_dbh)^2 +
    a3 * (sample_dbh)^3 + a4 * (sample_dbh)^4 + a5 * (sample_dbh)^5 + a0.5 *(sqrt(sample_dbh))

  if (squareflag == 1)
  {
    sample_volume = sample_volume^2
  }

  sample_BA = pi*(sample_dbh^2)/4
  sample_biomass = sample_volume * wood_density
  outputs = NULL
  outputs$BA = sample_BA
  outputs$vol = sample_volume
  outputs$biomass = sample_biomass
  #= c(sample_BA,sample_volume,sample_biomass)

  return(outputs)
}
