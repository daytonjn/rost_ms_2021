#Title: Spatial/Temporal Changes in Genetic Variation among the NW Atlantic Population of Roseate Terns
#Date: 2020/06/09
#Authors: Jacob Dayton, jacob.dayton@tufts.edu

#Purpose: Data analyses for project

#R Environment, v.3.6.3 (2020-02-29)

#setwd("C:\\Users\\dayto\\Documents\\OtherLaptop\\Roseate Terns\\Manuscript\\PostReview\\FinalRostData")
library('pbapply')

source("ROST_requiredPackages.R") #Required packges
source("ROST_readInData.R") #Reads in data for analysis
source("ROST_testHWE.R") #Tests for deviations from HWE
source("ROST_runDiveRsity.R") #Calculates basic diversity stats with 95% bootstrap resampled CIs
source("ROST_runDiveRsity_Fst.R") #Calculates WC's theta (Fst)


source("ROST_bootSims.R")
source("ROST_runRmetaParallel.R")
source("ROST_combineRmetaSims.R")
source("ROST_createLand_10stages_MF.R")
source("ROST_createLand_10stages_Nisbet_MF.R")

getwd()

temp = list.files(pattern="*.csv")
print(temp)
######
#Stores data in a list: [1] genInd, [2] genPop, [3] popNames, [4] locusNames
#Access slots in lists by [[]]; ex: list[[1]]@tab 
ROST_1870sto2016 <- readInData(temp[1]) 
ROST_1997to2018 <- readInData(temp[2])

missing_1870sto2016 <- info_table(ROST_1870sto2016[[1]], plot = TRUE, scale = TRUE)
missing_1997to2018 <- info_table(ROST_1997to2018[[1]], plot = TRUE, scale = TRUE)

######
#Tests for deviations from HWE and provides output files
testHWE(ROST_1870sto2016, "res_1870sto2016")
testHWE(ROST_1997to2018, "res_1997to2018")

######
#Tests for deviations from LD and provides output files
temp = list.files(pattern="Genepop_*")

test_LD(inputFile = 'Genepop_ROST_1870sto2016.txt', outputFile = 'res_1870sto2016_LD.txt',
        dememorization = 1000, batches = 100, iterations = 10000)
test_LD(inputFile = 'Genepop_ROST_1997sto2018.txt', outputFile = 'res_1997to2018_LD.txt',
        dememorization = 1000, batches = 100, iterations = 10000)

#Manually open Genepop LD output files, delete top rows (leave header), remove "---" under column names,
#find and replace contingency tables with "NA", delete bottom rows containing global LD estimates

#Read in file and manually remove rows that contain "no continency table" because locus was monomorphic in one population
res_1870sto2016_LD <- read_table2("res_1870sto2016_LD.txt") %>% drop_na()

res_1870sto2016_LD$S.E. <- p.adjust(res_1870sto2016_LD$`P-Value`, method = "BY", n = nrow(res_1870sto2016_LD))
colnames(res_1870sto2016_LD)[5] <- "Adj_P-Value"

res_1997to2018_LD <- read_table2("res_1997to2018_LD.txt")  %>% drop_na()
res_1997to2018_LD$S.E. <- p.adjust(res_1997to2018_LD$`P-Value`, method = "BY", n = nrow(res_1997to2018_LD))
colnames(res_1997to2018_LD)[5] <- "Adj_P-Value"

signif_1870sto2016_LD <- data.frame(filter(res_1870sto2016_LD, `P-Value`<0.05))
signif_1997to2018_LD <- data.frame(filter(res_1997to2018_LD, `P-Value`<0.05))

nrow(signif_1870sto2016_LD)/nrow(res_1870sto2016_LD)
nrow(signif_1997to2018_LD)/nrow(res_1997to2018_LD)

write.xlsx(signif_1870sto2016_LD, col.names = TRUE, row.names = TRUE, append = TRUE, sheetName = "LD_Signif1",  file = paste0("res_1870sto2016",".xlsx"))
write.xlsx(signif_1997to2018_LD, col.names = TRUE, row.names = TRUE, append = TRUE, sheetName = "LD_Signif1",  file = paste0("res_1997to2018",".xlsx"))
######
#Run diveRsity to calculate basic diversity stats and WC Fstats

infile_genepop <- "Genepop_ROST_1870sto2016.txt"
outFileName <- "res_1870sto2016"

runDiveRsity(infile_genepop, outFileName)
res_1870sto2016_fst <- runDiveRsity_Fst(infile_genepop, outFileName)

infile_genepop <- "Genepop_ROST_1997sto2018.txt"
outFileName <- "res_1997to2018"
runDiveRsity(infile_genepop, outFileName)
res_1997to2018_fst <- runDiveRsity_Fst(infile_genepop, outFileName)


######
#Run separate scripts (adegenet) to conduct xvalDAPC analyses; provided in folder

#ROST_xvalDAPC_1870sto2016
#ROST_xvalDAPC_1870sto2016_locprior
#ROST_xvalDAPC_1997to2018
#ROST_xvalDAPC_1997to2017_locprior

#To combine plot outputs from xvalDAPC
#ROST_xvalDAPC_combiningPlots_1997to2018


#####
#Run STRUCTURE externally, use clumpak to generate admixture files for major cluster runs, upload STRUCTURE output and visualize plots in ggplot2

#ROST_compoplot_STRUCTURE_1870sto2016
#ROST_compoplot_STRUCTURE_1997to2018

#Example admix file from a Structure run:
#"admix_ROST_1870sto2016_k2"

#####
#Forward Genetic Simulations in RMetaSim
#NOTE!!! Rmetasim uses 0-based indexing
#####
#Prepare list; each element contains same simulation parameters; simulations run in parallel (not serial) to maximize efficiency
run_A <- list()
num_replications <- 100
mut_rate <- 0.005
num_simYears <- 20
sampled_stages <- c(0,5) #male juvenile, female juvenile

params <- c(mut_rate, num_simYears, sampled_stages)

for(i in 1:num_replications){
  run_A[[i]] <- c(params)
}
run_A  #mut_rate, num_simYears, sampled_stages, iteration : 0.005, 5, c(0,5), iteration

#Parallelize lapply loops
no_cores <- detectCores(logical = FALSE) - 2
cl <- makeCluster(no_cores)
clusterExport(cl, c("runForwardSim_1iter", "run_A", "create.land.10stages.MF"), envir = environment())
clusterEvalQ(cl, c(library(rmetasim), library(magrittr)))

sim.output_10stages_MF_1and5_005 <- parLapply(cl = cl, run_A, runForwardSim_1iter)
stopCluster(cl)

combine.rmeta.output.sims(sim.output_10stages_MF_1and5_005, "ROST_rMetaSim_10stages_MF_1and5sample_005mut")

#########################
#Prepare list; each element contains same simulation parameters; simulations run in parallel (not serial) to maximize efficiency
run_B <- list()
num_replications <- 100
mut_rate <- 0.0005
num_simYears <- 20
sampled_stages <- c(0,5) #male juvenile, female juvenile

params <- c(mut_rate, num_simYears, sampled_stages)

for(i in 1:num_replications){
  run_B[[i]] <- c(params)
}
run_B  #mut_rate, num_simYears, sampled_stages, iteration : 0.005, 5, c(0,5), iteration

#Parallelize lapply loops
no_cores <- detectCores(logical = FALSE) - 2
cl <- makeCluster(no_cores)
clusterExport(cl, c("runForwardSim_1iter", "run_B", "create.land.10stages.MF"), envir = environment())
clusterEvalQ(cl, c(library(rmetasim), library(magrittr)))

sim.output_10stages_MF_1and5_0005 <- parLapply(cl = cl, run_B, runForwardSim_1iter)
stopCluster(cl)

combine.rmeta.output.sims(sim.output_10stages_MF_1and5_0005, "ROST_rMetaSim_10stages_MF_1and5sample_0005mut")

#########################
#Prepare list; each element contains same simulation parameters; simulations run in parallel (not serial) to maximize efficiency
run_C <- list()
num_replications <- 100
mut_rate <- 0.005
num_simYears <- 20
sampled_stages <- c(0,5) #male juvenile, female juvenile

params <- c(mut_rate, num_simYears, sampled_stages)

for(i in 1:num_replications){
  run_C[[i]] <- c(params)
}
run_C  #mut_rate, num_simYears, sampled_stages, iteration : 0.005, 5, c(0,5), iteration

#Parallelize lapply loops
no_cores <- detectCores(logical = FALSE) - 3
cl <- makeCluster(no_cores)
clusterExport(cl, c("runForwardSim_1iter_Nisbet", "run_C", "create.land.10stages_Nisbet.MF"), envir = environment())
clusterEvalQ(cl, c(library(rmetasim), library(magrittr)))

sim.output_10stages_MF_1and5_nisbet <- pblapply(cl = cl, run_C, runForwardSim_1iter_Nisbet)
#sim.output_10stages_MF_1and5_nisbet <- parLapply(cl = cl, run_C, runForwardSim_1iter_Nisbet)

stopCluster(cl)

combine.rmeta.output.sims(sim.output_10stages_MF_1and5_nisbet, "ROST_rMetaSim_10stages_MF_1and5sample_LebretonSzczys_005")


####################################

#"ROST_rMetaSim_10stages_MF_1and5sample_005mut"


na_005.df <- read.xlsx2("ROST_rMetaSim_10stages_MF_1and5sample_LebretonSzczys_005.xlsx", 1, header = TRUE)

na_005.df[,-1] <-  apply(na_005.df[-1], 2, function(x) as.numeric(as.character(x)))

# na_005.df$Sim.mu <- apply(na_005.df[,-1], 1, mean)
# na_005.df$Sim.low <- apply(na_005.df[,-1], 1, function(x)(quantile(x, 0.025)))
# na_005.df$Sim.high <- apply(na_005.df[,-1], 1, function(x)(quantile(x, 0.975)))
# 
# ggplot(na_005.df, aes(x = Year, y = Sim.mu))  + geom_point()  +
#   scale_y_continuous(name="Sim.mu Number of Alleles, NA") +
#   geom_errorbar(aes(ymin=Sim.low, ymax=Sim.high), width=.2)


###
ho_005.df <- read.xlsx2("ROST_rMetaSim_10stages_MF_1and5sample_LebretonSzczys_005.xlsx", 2, header = TRUE)
ho_005.df[,-1] <-  apply(ho_005.df[-1], 2, function(x) as.numeric(as.character(x)))

###
he_005.df <- read.xlsx2("ROST_rMetaSim_10stages_MF_1and5sample_LebretonSzczys_005.xlsx", 3, header = TRUE)
he_005.df[,-1] <-  apply(he_005.df[-1], 2, function(x) as.numeric(as.character(x)))

###
f_005.df <- read.xlsx2("ROST_rMetaSim_10stages_MF_1and5sample_LebretonSzczys_005.xlsx", 4, header = TRUE)
f_005.df[,-1] <-  apply(f_005.df[-1], 2, function(x) as.numeric(as.character(x)))
