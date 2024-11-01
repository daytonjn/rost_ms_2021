#Title: Spatial/Temporal Changes in Genetic Variation among the NW Atlantic Population of Roseate Terns
#Date: 2020/06/10
#Authors: Jacob Dayton, jacob.dayton@tufts.edu

#Purpose: Run diveRsity, calculate basic diversity statistics w/ 95% boostrap resampling confidence intervals (across loci)

#R Environment, v.3.6.3 (2020-02-29)

runDiveRsity <- function(infile_genepop, outFileName) {
  
   no_cores <- detectCores() - 4
  cl <- makeCluster(no_cores)
  clusterExport(cl, "basicStats", envir = environment())
  
  #If you want output, change NULL to outfile path
 divstats <- basicStats(infile = infile_genepop, outfile = NULL,
                         fis_ci = TRUE, ar_ci = FALSE, fis_boots = 1000, ar_boots = 1000,
                         mc_reps = 100, rarefaction = TRUE, ar_alpha = 0.05, fis_alpha = 0.05 )
    stopCluster(cl)

divStats_list <- vector(mode = 'list', length = length(divstats$main_tab))
i=1
m.param <- c(1, 3, 4, 6)

for (i in 1:length(divstats$main_tab)){
  m = 1
  k = 2
  divStats_list[[i]] <- divstats$main_tab[[i]]$overall[c(2,1,3:4,6)] %>% round(digits = 2)
  
  for (m in 1:length(m.param)){
    temp <- as.numeric(divstats$main_tab[[i]][m.param[m],1:(ncol(divstats$main_tab[[i]])-1)])
    nboots <- 1000
    boot.result <- numeric(nboots)
    
    j=1
    
    for(j in 1:nboots){
      boot.samp <- sample(temp, length(temp), replace=TRUE)
      boot.result[j] <- mean(boot.samp, na.rm = TRUE)
      j=j+1
    }
    param.conf.intervals <- boot.result %>% quantile(c(0.025, 0.975)) %>% round(digits = 2)
    divStats_list[[i]][k]<- paste0(divStats_list[[i]][k], " (", param.conf.intervals[1], "-", param.conf.intervals[2], ")")
    
    k=k+1
    m=m+1
  }
  
  i = i+1
}


names(divStats_list) <- names(divstats$main_tab)
diversity.table <- data.frame(divStats_list, check.names = FALSE)
colnames(diversity.table) <- names(divStats_list)
rownames(diversity.table) <- c("N", "AR", "Ho", "He", "F")
print(diversity.table)

diversity.table <- diversity.table %>% t()

#Saves a .csv file with this information in working directory
write.xlsx(diversity.table, col.names = TRUE, row.names = TRUE, append = TRUE, sheetName = "DiveRsityTable_95percCI",  file = paste0(outFileName,".xlsx"))

}

