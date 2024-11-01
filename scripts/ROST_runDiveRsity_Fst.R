#Title: Spatial/Temporal Changes in Genetic Variation among the NW Atlantic Population of Roseate Terns
#Date: 2020/06/10
#Authors: Jacob Dayton, jacob.dayton@tufts.edu

#Purpose: Run diveRsity_Fst, calculate W-C Fst, calculate basic diversity statistics w/ 95% boostrap resampling confidence intervals (across loci)

#R Environment, v.3.6.3 (2020-02-29)

runDiveRsity_Fst <- function(infile_genepop, outFileName) {
  
  output <- diffCalc(infile = infile_genepop, outfile = NULL,
                     pairwise = TRUE, fst = TRUE, bs_locus = TRUE,
                     bs_pairwise = TRUE, boots = 1000, ci_type = "individuals", alpha = 0.05, para = FALSE)

    output.fis <- output$bs_locus$Fis
    colnames(output.fis) <-  c("Locus", "Fis", "Lower.Fis", "Upper.Fis")
    
    output.fst <- output$bs_locus$Fst
    colnames(output.fst) <- c("Locus", "Fst", "Lower.Fst", "Upper.Fst")
    
    output.fstats <- data.frame(c(output.fis, output.fst))
    output.fstats.global <- data.frame(c(output$global_bs[7,], output$global_bs[5,]))
    colnames(output.fstats.global) <- colnames(output.fstats)
    
    output.fstats <- rbind(output.fstats, output.fstats.global)
    output.pairwise.fstats <- output$bs_pairwise$Fst
    
    write.xlsx(output.fstats, col.names = TRUE, row.names = TRUE, append = TRUE, sheetName = "Fstats_Locus",  file = paste0(outFileName,".xlsx"))
    write.xlsx(output.pairwise.fstats, col.names = TRUE, row.names = TRUE, append = TRUE, sheetName = "Fst_Pairwise",  file = paste0(outFileName,".xlsx"))
    
    
  return(output)
}
