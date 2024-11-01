#Title: Spatial/Temporal Changes in Genetic Variation among the NW Atlantic Population of Roseate Terns
#Date: 2020/06/09
#Authors: Jacob Dayton, jacob.dayton@tufts.edu

#Purpose: Analyze loci for deviations from HWE (Input: outputList_readinData.R)

#R Environment, v.3.6.3 (2020-02-29)

testHWE <- function(infileList, outFileName) {
  
  #Tests for deviations from HWE using pegas
  #(Col1 = Loci, Col2 = df, Col3 = chi^2 associated p-value, Col3 = P-value from MCMC which matches genepop exact p-value)
  #Chi^2 test based on expected genotype frequencies calculated from allelic frequencies, followed by exact test on MCMC permutations of alleles
  #B specifies # of permutations
  
  hwe.bypop <- seppop(infileList[[1]]) %>% lapply(hw.test, B = 10000)
  hwe.matrix <- sapply(hwe.bypop, "[", i = TRUE, j = 4)
  colnames(hwe.matrix) <- str_sub(colnames(hwe.matrix), 1, -4)
  
  hwe.p.adjust <- p.adjust( p = as.vector(hwe.matrix),
                            method = "BY",
                            n = length(hwe.matrix))
  
  hwe.matrix_p.adjust <- matrix(hwe.p.adjust, byrow = FALSE, nrow=length(infileList[[4]]))
  rownames(hwe.matrix_p.adjust) <- rownames(hwe.matrix)
  colnames(hwe.matrix_p.adjust) <- colnames(hwe.matrix)
  
  
  hwe_raw <- levelplot(t(hwe.matrix), xlab = "Year", ylab = "Locus", cex.lab=0.5,
                       main = "Raw P-Values, HWE Tests")
  hwe_adj <- levelplot(t(hwe.matrix_p.adjust), xlab = "Year", ylab = "Locus", cex=0.5,
                       main = "B-Y Adjusted P-Values, HWE Tests")
  
  write.xlsx(hwe.matrix, col.names = TRUE, row.names = TRUE, append = TRUE, sheetName = "HWE_Raw_Pvalues",  file = paste0(outFileName,".xlsx"))
  write.xlsx(hwe.matrix_p.adjust, col.names = TRUE, row.names = TRUE, append = TRUE, sheetName = "HWE_Adj_Pvalues",  file = paste0(outFileName,".xlsx"))
  
  hweplot_alltimes <- grid.arrange(hwe_raw,hwe_adj, ncol=2)
  
  pdf(paste0(outFileName, "_HWE.pdf"), width = 10, height = 8, paper = "USr")
  # 2. Create the plot
  grid.arrange(hwe_raw,hwe_adj, ncol=2)
  
  # 3. Close the file
  dev.off()

}


