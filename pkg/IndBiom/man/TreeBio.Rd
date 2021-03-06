% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TreeBio.R
\name{TreeBio}
\alias{TreeBio}
\title{Tree Biomass Function}
\usage{
TreeBio(sample_spname, sample_statename, sample_girth, VolEq_DB)
}
\arguments{
\item{sample_spname}{- Species Name of the Tree.}

\item{sample_statename}{- State Name.}

\item{sample_girth}{- Measured Girth value in centimeter (cm).}

\item{VolEq_DB}{- Inbuilt database for volume equations and wood density (no need to mention)
Automatically updates from Data folder of the package.}
}
\description{
This function calculates Biomass values at every tree level using species level allometric equations
of different states of India and wood density. (Equations Source : Forest Survey of India (FSI)).
}
\details{
This function estimates the biomass of individual tree using regional allometric equations of FSI.
It doesn't take empty inputs of girth value and State Name.
If the species wise equation is not found a general species equation is used for different states.
}
\examples{
TreeBio("Tectona grandis","Karnataka",120)
## Outputs
## BA - Basal Area of the tree
## vol - Volume of the tree and
## biomass - Biomass of the tree
}
\keyword{Biomass}

