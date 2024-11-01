#Title: Spatial/Temporal Changes in Genetic Variation among the NW Atlantic Population of Roseate Terns
#Date: 2020/06/09
#Authors: Jacob Dayton, jacob.dayton@tufts.edu

#Purpose: Read in data (Input: infile_format_GenAlex.csv, Output: infile.genind )

#R Environment, v.3.6.3 (2020-02-29)

readInData <- function(infileName) {

  genInd <- read.genalex(infileName, genclone = FALSE)
  genPop <- genind2genpop(genInd)
  
  popNames <- popNames(genInd)
  locNames <- locNames(genInd)
  
  output <- list(genInd, genPop, popNames, locNames)
 
  rm(genInd)
  rm(genPop)
  rm(popNames)
  rm(locNames)
  
  return(output)
}
