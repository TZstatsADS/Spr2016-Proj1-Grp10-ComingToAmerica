########################################################################
# Team 10 Code
#
#
#
#
#
#
########################################################################

# Load libraries
library(data.table)
library(dplyr)
library(googleVis)
library(survey)
library(ggplot2)
library(plyr)
detach("package:plyr", unload=TRUE)
library(dplyr)
library(reshape)
library(RColorBrewer)

source("lib/jmd2228_functions.R")
population <- prepare_data()
had_child_t_test(population)
had_child_bar_chart(population)
childs_parents_nativity()

source("lib/sri2116_functions.R")
population <- prepare_data()
states(population)
