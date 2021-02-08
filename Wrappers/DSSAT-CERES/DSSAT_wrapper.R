#' @title DSSAT wrapper for CroptimizR, a simple version for AgMIP Calibration Protocol
#'
#' @description This function runs DSSAT with given ecotype and/or cultivar parameter 
#' values and returns its results as required by CroptimizR package.
#'
#' @param param_values (optional, NULL by default) a named vector that contains values
#' of DSSAT input parameters to use in the simulations. Should have one named column per
#' parameter. If param_values is not provided or set to NULL, the simulations will
#' be performed using the parameters values defined in the DSSAT input files referenced
#' in model_options argument.
#'
#' @param sit_names Vector of situations (TRTNO) names for which results
#' must be returned.
#'
#' @param model_options List containing the information needed by the model.
#'   `DSSAT_path`: path to DSSAT directory
#'   `DSSAT_exe`: name of the model executable
#'   `Crop`: name of the crop (i.e. of subdirectory in DSSAT path)
#'   `project_file`: name of the .WHX project file
#'   `ecotype`: name of the ecotype
#'   `cultivar`: name of the cultivar
#'   `ecotype_filename`: name of the ecotype filename
#'   `cultivar_filename`: name of the cultivar filename
#'
#' @return A list containing simulated values. It include 2 elements:
#'   - `sim_list`: a named list of tibbles, one tibble per situation, each tibble contains a 
#'                 column Date storing the dates in Date or POSIXct format, plus one column 
#'                 per simulated variable storing the simulated values for each date.
#'                 The names of the elements of the list are the situation names (TRTNO)
#'   - `error`: an error code indicating if at least one simulation ended 
#'              with an error (TRUE) or if all simulations went OK (FALSE).
#' 
#' @details This wapper simulates situations (TRTNO, treatment number) included 
#' in a single project which filename is given in model_options$project_file and
#' path in model_options$project_path.
#' ecotype and cultivar could be retrieved from project_file and sit_names but seems 
#' that reading project_file can be time consuming ...
#' 
#' 
DSSAT_wrapper <- function( param_values=NULL, sit_names, model_options, ...) {
  
  # Initializations
  param_names <- names(param_values)
  results <- list(sim_list = setNames(vector("list",length(sit_names)), nm = sit_names), error=FALSE)
  
  options(DSSAT.CSM = file.path(model_options$DSSAT_path,model_options$DSSAT_exe))
  project_path <- file.path(model_options$DSSAT_path,model_options$Crop)
  Genotype_path <- file.path(model_options$DSSAT_path,model_options$Genotype)
  project_file <- model_options$project_file
  ecotype_filename <- model_options$ecotype_filename
  cultivar_filename <- model_options$cultivar_filename
  ecotype <- model_options$ecotype
  
  # Force ecotype parameters if provided in param_values
  if (!is.null(param_values)) {
    eco <- read_eco(file.path(Genotype_path,ecotype_filename))	  # read ecotype DSSAT file => put results in eco data.frame
    if (any (param_names %in% names(eco))) {   # if some parameters in param_values are ecotype parameters
      eco_paramNames <- intersect(param_names, names(eco))
      idx <- which(eco$`ECO#`==ecotype)
      for (param in eco_paramNames) {   # modify their values in the eco data.frame
        eco[idx,param] <- param_values[param] 
      }      
      write_eco(eco,file.path(Genotype_path,ecotype_filename))  # write the ecotype DSSAT file from the modified eco data.frame
    }
  }
  
  # Force cultivar parameters if provided in param_values, same as for ecotype parameters but for cultivar ones
  if (!is.null(param_values)) {
    cul <- read_cul(file.path(Genotype_path,cultivar_filename)) 
    if (any (param_names %in% names(cul))) {
      cul_paramNames <- intersect(param_names, names(cul))
      idx <- which(cul$`VAR#`==cultivar)
      for (param in cul_paramNames) {
        cul[idx,param] <- param_values[param]  
      }
      write_cul(cul,file.path(Genotype_path,cultivar_filename))
    }
  }
  
  # Run the model
  setwd(project_path)
  write_dssbatch(x=project_file,trtno=as.integer(sit_names)) # Generate a DSSAT batch file with function arguments
  run_dssat() # Run DSSAT-CSM
  
  # Read its outputs and store them in CroptimizR format
  pgro <- read_output("PlantGro.OUT") %>% mutate(Date=DATE) %>% select(-DATE)
  for (situation in sit_names) {
    results$sim_list[[situation]] <- filter(pgro, TRNO==as.integer(situation))
  }
  attr(results$sim_list, "class")= "cropr_simulation"

  return(results)
  
}