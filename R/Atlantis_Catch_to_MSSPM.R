#
# Atlantis_Catch_to_MSSPM ######################################################
#

## Brief Description ############################################################
# Script to convert Atlantic sim index file to MSSPM catch file
#

## Detailed Description #########################################################
#
# This function takes the input Atlantis data frame, converts it
# to an MSSPM formatted data frame, and writes it out to the passed
# CSV file.
#

## Example Usage ################################################################
#
# inputDataFrame <- mskeyrun::simCatchIndex
# startYear <- 55
# outputFile <- '/home/rklasky/test/testCatch.csv'
# convertAtlantisCatchToMSSPM(inputDataFrame,startYear,outputFile)

## roxygen help #############################################################
#' Atlantis to MSSPM Catch Conversion
#'
#' Conversion of Atlantis Simulated Catch Index file to MSSPM Catch csv file
#' @param inputDataFrame The Atlantis input simulated catch data frame
#' @param startYear The year to start the conversion (earlier years will be skipped)
#' @param outputfile The output csv data file containing the MSSPM catch formatted data
#' @return n/a
#' @examples
#' inputDataFrame <- mskeyrun::simCatchIndex
#' startYear <- 55
#' outputFile <- '/home/rklasky/test/testCatch.csv'
#'
#' convertAtlantisCatchToMSSPM(inputDataFrame,startYear,outputFile)
#'

#' @export
convertAtlantisCatchToMSSPM <- function(inputDataFrame,startYear,outputFile) {

  # Get only the catch rows (i.e., no cv) and years >= the startYear
  simCatchData <- subset(inputDataFrame,year>=startYear & variable=='catch')

  # Keep only the columns MSSPM will need
  msspmColumns <- dplyr::select(simCatchData, year, Name, value)

  # Convert rows into columns
  msspmCatch <- tidyr::spread(msspmColumns, Name, value)

  # Write out final table
  readr::write_csv(msspmCatch,outputFile)

}

