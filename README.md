# Roseate tern genetics manuscript
scripts for [Dayton &amp; Szczys (2021)](https://doi.org/10.1093/ornithapp/duab037); Metapopulation connectivity retains genetic diversity following a historical bottleneck in a federally endangered seabird


#Date: 2020/06/09
#Authors: Jacob Dayton, jacob.dayton@tufts.edu

- Data files provided in "Data" folder
- Scripts for analyses provided in "Scripts" folder

Scripts to reproduce analyses:
 1) Open "ROST_analyzeData_MasterFile.R". This file calls "functions" encapsulated by the other R.files present within this folder. File contains descriptions of steps.
 2) Make sure that path to data files is set for your system. Best to change path, or save all data in same file as scripts. Example:
Example: setwd("..\\Data")
 3) Running the masterFile will output tabulated results containing metrics for sample-specific diversity  (Ar, Na, etc..), differentiation (Fst), etc.. metrics.
 	- Output ex: "res_1997to2018.xlsx"
 	- Output ex: "xvaldapc_ROST_1870sto2016_k2"
 4) Follow steps in master, will direct you to separate files to run xvalDAPC and the forward genetic simulations
	
Data Files:

-.xslx file with raw genotypes and sample IDs:
	-20200718_ROST_msData_Final.xlsx 
-.CSV  files in GenAlEx format for upload into R for analyses:
	-20200605_ROST_1870sto2016.csv: Temporal Dataset, 8 loci
	-20200605_ROST_1997to2018.csv: Spatial/Temporal Dataset, 16 loci
-.txt files in GenePop format for upload into R for analyses, BOTTLENECK for het. excess test, and NeEstimator for LDNE estimation:
	-Genepop_ROST_1870sto2016.txt
	-Genepop_ROST_1997sto2018.txt
-.txt files in Admixture format (from STRUCTURE output) for upload into R for plotting STRUCTURE runs; see ...MasterFile.R for instructions.