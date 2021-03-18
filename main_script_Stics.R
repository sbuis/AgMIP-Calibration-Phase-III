# Install / Load needed libraries and functions
if(!require("CroptimizR")){
  devtools::install_github("SticsRPacks/CroptimizR@*release")
  library("CroptimizR")
}
if(!require("CroPlotR")){
  devtools::install_github("SticsRPacks/CroPlotR@*release")
  library("CroPlotR")
}
library(SticsOnR)
library(SticsRFiles)
library(dplyr)

source("R/AICc.R")
source("R/BIC.R")
source("R/select_param_FwdReg.R")
source("R/main_function_guidelines.R")
source("R/create_AgMIP_outputs.R")
source("R/complete_init_values.R")

# Define your model and associated options
# i.e. model_options argument given to estim_param that depends on your model wrapper 
model_function <- stics_wrapper
javastics_path <- "D:\\Home\\sbuis\\Documents\\WORK\\STICS\\JavaSTICS-1.40-stics-8.50"
stics_path <- file.path(javastics_path,"bin/stics_modulo")
data_dir <- "D:\\Home\\sbuis\\Documents\\WORK\\AgMIP\\CalibrationProtocol\\2ndExercise_Guidelines_2020\\PhaseIIIProtocol\\AustralianDataset\\SticsData"
stics_inputs_path <- file.path(data_dir,"TxtFiles")
model_options <- stics_wrapper_options(javastics_path=javastics_path,data_dir=stics_inputs_path,parallel=TRUE)

  
# Select the observations for the parameter estimation
# i.e. set obs_list here
# sit_names <- c("Erad-2010-TOS1","Erad-2010-TOS2","Erad-2010-TOS3","Minn-2010-TOS1",
#                "Minn-2010-TOS2","Minn-2010-TOS3","Lake-2010-TOS1","Lake-2010-TOS2",
#                "Lake-2010-TOS3","Erad-2011-TOS1","Erad-2011-TOS2","Erad-2011-TOS3",
#                "Minn-2011-TOS1","Minn-2011-TOS2","Minn-2011-TOS3","Spri-2010-TOS1",
#                "Spri-2010-TOS2","Spri-2010-TOS3","Spri-2011-TOS1","Spri-2011-TOS2",
#                "Spri-2011-TOS3","Lake-2011-TOS1","Lake-2011-TOS2","Lake-2011-TOS3")
# obs_list <- get_obs(file.path(data_dir,"XmlFiles"))
# obs_list <- filter_obs(obs_list, sit_names = sit_names,include=TRUE)
# obs_list <- sapply(obs_list, function(x)  x %>% dplyr::select(-ian, -mo, -jo, -jul, -Plant), simplify = FALSE)

source("D:\\Home\\sbuis\\Documents\\WORK\\AgMIP\\CalibrationProtocol\\2ndExercise_Guidelines_2020\\PhaseIIIProtocol\\AustralianDataset\\R\\generate_obs.R")
obs_list <- generate_obs("D:\\Home\\sbuis\\Documents\\WORK\\AgMIP\\CalibrationProtocol\\2ndExercise_Guidelines_2020\\PhaseIIIProtocol\\AustralianDataset\\BugObs")


# Give information on the parameters to estimate : 

## Names of obligatory parameters
oblig_param_list <- c("stlevamf","stamflax", "stflodrp", "stlevdrp", "stdrpmat")

## Names of additional candidate parameters
add_param_list <- c("sensrsec", "belong", "stressdev", "tdmax", "sensiphot", "jvc", "phobase", "phosat")
add_param_list <- c("sensrsec", "belong")

param_names <- c(oblig_param_list,add_param_list)

## Bound constraints
param_info_tot <- list(lb=c(stlevamf=1, stamflax=1, stflodrp=0, stlevdrp=1, stdrpmat=1, 
                            jvc=0, stressdev=0, tdmax=15, sensiphot=0, sensrsec=0, belong=0.005, phobase=0, phosat=6.3),
                       ub=c(stlevamf=Inf, stamflax=Inf, stflodrp=Inf, stlevdrp=Inf, stdrpmat=Inf, 
                            jvc=Inf, stressdev=1, tdmax=44, sensiphot=1, sensrsec=1, belong=Inf, phobase=20, phosat=24),
                       init_values=c(stlevamf=678, stamflax=400, stflodrp=296, stlevdrp=1293, stdrpmat=679,
                                     jvc=0, stressdev=0.2, tdmax=30, sensiphot=1, sensrsec= 0.5, belong= 0.012, phobase=6.3, phosat=20))

## Lower and upper bounds for initial values sampling 
lb_initV <- c(stlevamf=150, stamflax=150, stflodrp=0, stlevdrp=450, stdrpmat=500, 
              jvc=0, stressdev=0, tdmax=25, sensiphot=0, sensrsec= 0, belong= 0.005, phobase=0, phosat=0)
ub_initV <- c(stlevamf=800, stamflax=600, stflodrp=300, stlevdrp=1500, stdrpmat=1200, 
              jvc=20, stressdev=1, tdmax=35, sensiphot=1, sensrsec= 1, belong= 0.03, phobase=24, phosat=24)


# Define parameter estimation algorithm options
optim_options=list(path_results = getwd(), # path where to store the results (graph and Rdata)
                   #nb_rep = 10,             # Number of repetitions of the minimization -> NO MORE USED, , USE nb_rep variable instead (see here-after)
                   ranseed = 1234)         # set random seed so that each execution give the same results
                                           # If you want randomization, don't set it.
optim_options$maxeval=2

# Number of repetitions: if nb_rep includes only one value, all steps will use the same number of repetitions
#                        if nb_rep includes 2 values, the first step will use the first value as repetition number and ALL the others will use the second values
#                        otherwise, nb_rep can include as many values as number of steps if you want to define different values for each step
nb_rep <- c(20,5) 

##########################################

main_function_guidelines(optim_options, oblig_param_list, add_param_list, 
                         param_info_tot, lb_initV, ub_initV, obs_list, 
                         model_function, model_options, info_crit_name="AICc", nb_rep=nb_rep)

main_function_guidelines(optim_options, oblig_param_list, add_param_list, 
                         param_info_tot, lb_initV, ub_initV, obs_list, 
                         model_function, model_options, info_crit_name="BIC", nb_rep=nb_rep)
						 
print(paste0("Results saved in ",optim_options$path_results))