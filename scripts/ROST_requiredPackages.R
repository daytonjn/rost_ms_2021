#Title: Spatial/Temporal Changes in Genetic Variation among the NW Atlantic Population of Roseate Terns
#Date: 2020/06/09
#Authors: Jacob Dayton, jacob.dayton@tufts.edu

#Purpose: Source packages required for analysis:

#R Environment, v.3.6.3 (2020-02-29)

#Read/Write and Manipulate data
require("xlsx")
require('dplyr')
require('stringr')
require('readr')
require('tidyr')
require('gridExtra')
require('cowplot')
require('reshape2')
library('magrittr')

#Visualization
require("plotrix") #v.3.7.8
require("ggplot2") #v.3.3.1
require("lattice") #v.0.20.41
require("gridExtra") #v.2.3
library('khroma')

#Analysis
require("adegenet") #v.2.1.3
require("pegas") #0.13
require("poppr") #2.8.6
require("hierfstat") #0.4.22
require('PopGenKit') #1.0
require('diveRsity') #1.9.90
require('genepop') #v.1.1.7
require('rmetasim') #v.3.1.15
require('boot') #v.1.3.24

#Running in parallel
require('parallel')
require("doParallel")
require("foreach")

#Color Palette, CB-friendly and pretty
#Blue, Red, Green, Yellow, Cyan, Purple, Grey
colors.bright <- c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377", "#BBBBBB")
#Cyan, Blue, Red, Green, Yellow, Purple, Grey
colors.bright.alt <- c( "#66CCEE", "#4477AA", "#EE6677", "#228833", "#CCBB44", "#AA3377", "#BBBBBB")

