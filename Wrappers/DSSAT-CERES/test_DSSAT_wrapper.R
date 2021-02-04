#
# This script test the DSSAT wrapper following the advice given at https://sticsrpacks.github.io/CroptimizR/articles/Designing_a_model_wrapper.html
# Printed sums of differences should be different from 0. 
# IT IS JUST A TEMPLATE : some information must be filled before running it (see below)
# 

if(!require("DSSAT")){
  install.packages("DSSAT")
  library("DSSAT")
}

source("DSSAT_wrapper.R")


########## TO BE ADAPTED TO YOUR CASE ....

model_options <- vector("list")
model_options$DSSAT_path <- 'C:\\DSSAT47'
model_options <- vector("list")
model_options$DSSAT_path <-    # e.g. 'C:\\DSSAT47'
model_options$DSSAT_exe <-     # e.g. 'DSCSM047.EXE'
model_options$Crop <- "Wheat"
model_options$Genotype <- "Genotype" 
model_options$ecotype_filename <-  # e.g. "WHCER047.ECO"
model_options$cultivar_filename <- # e.g. "WHCER047.CUL"

# Adapt to Australian or French dataset case ...
model_options$project_file <-  # e.g. 'CSIR1066.WHX'
model_options$ecotype <-       # e.g. "AUWH01"
model_options$cultivar <-      # e.g. "CSIR01"


param_names <- c("P1","P3")    # set the name of one or several model input parameters in a vector
param_lb<-c(100,100)       # set the lower bounds of these parameters in a vector (no Inf or -Inf ...)
param_ub<-c(500,500)       # set the upper bounds of these parameters in a vector (no Inf or -Inf ...)
var_name<-"GSTD"       # give the name of an output variable sensitive to this (or these) parameter(s)

situation_name<- as.character (seq(1, 2, by=1)) # give the name of the situation to simulate 

############


wrapper<-DSSAT_wrapper        # give the name of your wrapper

param_values_min <- setNames(param_lb, param_names)
param_values_max <- setNames(param_ub, param_names)
sim_min       <- wrapper(param_values = param_values_min, model_options = model_options, 
                         sit_names=situation_name)
sim_max       <- wrapper(param_values = param_values_max, model_options = model_options, 
                         sit_names=situation_name)


for (sit in situation_name) {
  
  min_nrow <- min(nrow(sim_min$sim_list[[sit]]), nrow(sim_max$sim_list[[sit]]))
  
  print(paste("Sum of differences, variable",var_name,", situation",sit," = ",
              
              sum(abs(sim_max$sim_list[[sit]][1:min_nrow,var_name] - sim_min$sim_list[[sit]][1:min_nrow,var_name])
                  
                  ,na.rm=TRUE)))
}


options(tibble.print_max = Inf)
sim_min$sim_list
sim_max$sim_list
