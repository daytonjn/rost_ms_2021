#Title: Spatial/Temporal Changes in Genetic Variation among the NW Atlantic Population of Roseate Terns
#Date: 2020/07/08
#Authors: Jacob Dayton, jacob.dayton@tufts.edu

#Purpose: Parse Rmetasim output across simulation iterations 
#Input: Output from Rmetasim (Function: runForwardSim_1iter in ROST_runRmetaParallel.R), Output: .xslx summary )

#R Environment, v.3.6.3 (2020-02-29)

combine.rmeta.output.sims <- function(sim.output, outFileName){
  
  res.Na <- as.data.frame(do.call(cbind, lapply(1:length(sim.output), function(i) cbind(sim.output[[i]][,1]))),
                          row.names = paste0("Year_", 1:nrow(sim.output[[1]]))) %>% setNames(paste0("Sim_", 1:length(sim.output))) 
  res.Ho <- as.data.frame(do.call(cbind, lapply(1:length(sim.output), function(i) cbind(sim.output[[i]][,2]))),
                          row.names = paste0("Year_", 1:nrow(sim.output[[1]]))) %>% setNames(paste0("Sim_", 1:length(sim.output))) 
  res.He <- as.data.frame(do.call(cbind, lapply(1:length(sim.output), function(i) cbind(sim.output[[i]][,3]))),
                          row.names = paste0("Year_", 1:nrow(sim.output[[1]]))) %>% setNames(paste0("Sim_", 1:length(sim.output)))  
  res.F <-  as.data.frame(do.call(cbind, lapply(1:length(sim.output), function(i) cbind(sim.output[[i]][,4]))),
                          row.names = paste0("Year_", 1:nrow(sim.output[[1]]))) %>% setNames(paste0("Sim_", 1:length(sim.output))) 
  res.NB <- as.data.frame(do.call(cbind, lapply(1:length(sim.output), function(i) cbind(sim.output[[i]][,5]))),
                          row.names = paste0("Year_", 1:nrow(sim.output[[1]]))) %>% setNames(paste0("Sim_", 1:length(sim.output))) 
  res.FM <- as.data.frame(do.call(cbind, lapply(1:length(sim.output), function(i) cbind(sim.output[[i]][,6]))),
                          row.names = paste0("Year_", 1:nrow(sim.output[[1]]))) %>% setNames(paste0("Sim_", 1:length(sim.output)))
  
  write.xlsx(res.Na, col.names = TRUE, row.names = TRUE, append = TRUE, sheetName = "Na_mu",  file = paste0(outFileName,".xlsx"))
  write.xlsx(res.Ho, col.names = TRUE, row.names = TRUE, append = TRUE, sheetName = "Ho_mu",  file = paste0(outFileName,".xlsx"))
  write.xlsx(res.He, col.names = TRUE, row.names = TRUE, append = TRUE, sheetName = "He_mu",  file = paste0(outFileName,".xlsx"))
  write.xlsx(res.F, col.names = TRUE, row.names = TRUE, append = TRUE, sheetName = "Fis_mu",  file = paste0(outFileName,".xlsx"))
  write.xlsx(res.NB, col.names = TRUE, row.names = TRUE, append = TRUE, sheetName = "NB",  file = paste0(outFileName,".xlsx"))
  write.xlsx(res.FM, col.names = TRUE, row.names = TRUE, append = TRUE, sheetName = "FSexBias",  file = paste0(outFileName,".xlsx"))
  
  #Empty dataframe appended to end of sim.output list; this will store our bootstrap 95% confidence intervals
  sim.output[[length(sim.output)+1]] <- as.data.frame(matrix(ncol = 6, nrow = nrow(sim.output[[1]]),
                                                             dimnames = list(paste0("Year_",
                                                                                    1:nrow(sim.output[[1]])), c("Na", "Ho", "He", "F", "NB", "F%"))))
  
  sim.output[[length(sim.output)]]$Na <- boot_stats(res.Na)
  sim.output[[length(sim.output)]]$Ho <- boot_stats(res.Ho)
  sim.output[[length(sim.output)]]$He <- boot_stats(res.He)
  sim.output[[length(sim.output)]]$F <- boot_stats(res.F)
  sim.output[[length(sim.output)]]$NB <- boot_stats(res.NB)
  sim.output[[length(sim.output)]]$'F%' <- boot_stats(res.FM)
  
  write.xlsx(sim.output[[length(sim.output)]], col.names = TRUE, row.names = TRUE,
             append = TRUE, sheetName = "mu_95CI",  file = paste0(outFileName,".xlsx"))
  
  
}
