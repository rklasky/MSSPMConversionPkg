#
# Atlantis_ObsBM_to_MSSPM ######################################################
#

## Brief Description ############################################################
# Script to convert Atlantic survey index file to MSSPM observed biomass file
#

## Detailed Description ########################################################
#
# This function takes the input Atlantis survey biomass data frame,
# converts it to an MSSPM formatted data frame, and writes it out
# to the passed CSV file.
#

## Example Usage ################################################################
#
# speciesMappingDataFrame <- mskeyrun::focalSpecies # needed to map SVSPP value to modelName
# observedBMDataFrame <- mskeyrun::surveyIndexA4
# surveySeason <- 'SPRING' # Use 'FALL' or "SPRING"
# outputFile <- paste('/home/rklasky/test/testObsBM_',surveySeason,'.csv',sep="")
# convertAtlantisObsBMtoMSSPM(speciesMappingDataFrame,observedBMDataFrame,surveySeason,outputFile)

## roxygen help #############################################################
#' Atlantis to MSSPM Biomass Conversion
#'
#' Conversion of Atlantis Survey Index file to MSSPM Observed Biomass csv file
#' @param speciesMappingDataFrame The Atlantis data frame that contains a mapping of the SVSPP numeric values to corresponding species names (i.e., modelName)
#' @param observedBMDataFrame The Atlantis input survey index (i.e. biomass) data frame
#' @param surveySeason The survey season data to convert (Atlantis file currently has FALL and SPRING)
#' @param outputfile The output csv data file containing the MSSPM observed biomass formatted data
#' @return n/a
#' @examples
#'
#' speciesMappingDataFrame <- mskeyrun::focalSpecies # needed to map SVSPP value to modelName
#' observedBMDataFrame <- mskeyrun::surveyIndexA4
#' surveySeason <- 'SPRING' # Use 'FALL' or "SPRING"
#' outputFile <- paste('/home/rklasky/test/testObsBM_',surveySeason,'.csv',sep="")
#'
#' convertAtlantisObsBMtoMSSPM(speciesMappingDataFrame,observedBMDataFrame,surveySeason,outputFile)
#'
#' @export
convertAtlantisObsBMtoMSSPM <- function(speciesMappingDataFrame,observedBMDataFrame,surveySeason,outputFile) {

  # Grab the rows for the desired season and data type using the desired survey index data frame
  seasonData <- subset(observedBMDataFrame, SEASON==surveySeason & variable=='strat.biomass')

  # Grab just the columns needed for MSSPM
  seasonDataColumns <- as.data.frame(select(seasonData, YEAR, SVSPP, value))
  # Q1. Why do I need the as.data.frame here?

  # Using the SVSPP-Name lookup data frame, replace the numeric SVSPP values with their modelName values
  seasonDataColumns$SVSPP <- with(speciesMappingDataFrame, modelName[match(seasonDataColumns$SVSPP,SVSPP)])

  # Rename the column header from SVSPP to Name
  names(seasonDataColumns)[names(seasonDataColumns) == "SVSPP"] <- "Name"

  # Create columns as species
  msspmObsBM <- tidyr::spread(seasonDataColumns, Name, value)
  # Q2. There is missing data, what to do? (i.e., Fall, SVSPP=32, 1979)

  # Write out final table
  readr::write_csv(msspmObsBM,outputFile)
}
