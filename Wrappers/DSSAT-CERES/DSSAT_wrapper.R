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
#' @importFrom wrapr seqi
#' @importFrom lubridate year
#' 
DSSAT_wrapper <- function( param_values=NULL, sit_names, model_options, ...) {
  
  on.exit({
    # set the parameters file to their original values
    if (flag_eco_param) {
      res <- file.rename(from=file.path(Genotype_path,paste0(ecotype_filename,"_tmp")),to=file.path(Genotype_path,ecotype_filename))
      if (!res) {
        stop(paste("Unable to rename ",file.path(Genotype_path,paste0(ecotype_filename,"_tmp")),"in ",file.path(Genotype_path,ecotype_filename),
                   ".\n This may alter the following results. Please allow write permissions on",file.path(Genotype_path,ecotype_filename)))
      }
    }
    if (flag_cul_param) {
      res <- file.rename(from=file.path(Genotype_path,paste0(cultivar_filename,"_tmp")),to=file.path(Genotype_path,cultivar_filename))
      if (!res) {
        stop(paste("Unable to rename ",file.path(Genotype_path,paste0(cultivar_filename,"_tmp")),"in ",file.path(Genotype_path,cultivar_filename),
                   ".\n This may alter the following results. Please allow write permissions on",file.path(Genotype_path,cultivar_filename)))
      }
    }
  })
  # Initializations
  flag_eco_param = FALSE; flag_cul_param = FALSE
  if (!is.null(model_options$add_forced_param)) {
    param_values <- model_options$add_forced_param(param_values) # user function to force some parameters from the values of those provided in param_values
  }
  param_names <- names(param_values)
  results <- list(sim_list = setNames(vector("list",length(sit_names)), nm = sit_names), error=FALSE)
  
  options(DSSAT.CSM = file.path(model_options$DSSAT_path,model_options$DSSAT_exe))
  project_path <- file.path(model_options$DSSAT_path,model_options$Crop)
  Genotype_path <- file.path(model_options$DSSAT_path,model_options$Genotype)
  project_file <- model_options$project_file
  ecotype_filename <- model_options$ecotype_filename
  cultivar_filename <- model_options$cultivar_filename
  ecotype <- model_options$ecotype
  cultivar <- model_options$cultivar
  
  # Force ecotype parameters if provided in param_values
  if (!is.null(param_values)) {
    eco <- read_eco(file.path(Genotype_path,ecotype_filename))	  # read ecotype DSSAT file => put results in eco data.frame
    if (any (param_names %in% names(eco))) {   # if some parameters in param_values are ecotype parameters
      res <- file.copy(from=file.path(Genotype_path,ecotype_filename),to=file.path(Genotype_path,paste0(ecotype_filename,"_tmp")),overwrite = TRUE)
      if (!res) {
        stop(paste("Unable to copy ",file.path(Genotype_path,ecotype_filename)))
      }
      flag_eco_param = TRUE
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
      res <- file.copy(from=file.path(Genotype_path,cultivar_filename),to=file.path(Genotype_path,paste0(cultivar_filename,"_tmp")),overwrite = TRUE)
      if (!res) {
        stop(paste("Unable to copy ",file.path(Genotype_path,cultivar_filename)))
      }
      flag_cul_param = TRUE
      cul_paramNames <- intersect(param_names, names(cul))
      idx <- which(cul$`VAR#`==cultivar)
      for (param in cul_paramNames) {
        if (is.character(cul[[param]])) {
          cul[idx,param] <- as.character(round(param_values[[param]],digits = 2)) 
        } else {
          cul[idx,param] <- param_values[param] 
        }
      }
      write_cul(cul,file.path(Genotype_path,cultivar_filename))
    }
  }
  
  # Run the model
  setwd(project_path)
  write_dssbatch(x=project_file,trtno=as.integer(sit_names)) # Generate a DSSAT batch file with function arguments

  count=1
  while (!file.exists("PlantGro.OUT") && count<10) {
    run_dssat() # Run DSSAT-CSM
    count <- count+1
  }
  
  # Read its outputs and store them in CroptimizR format
  if (file.exists("PlantGro.OUT")) {
    pgroTot <- read_output("PlantGro.OUT") %>% mutate(Date=DATE) %>% select(-DATE)
    if (!all(as.integer(sit_names) %in% pgroTot$TRNO)) {
      run_dssat() # Run DSSAT-CSM run again
      if (file.exists("PlantGro.OUT")) {
        pgroTot <- read_output("PlantGro.OUT") %>% mutate(Date=DATE) %>% select(-DATE)
      
      if (!all(as.integer(sit_names) %in% pgroTot$TRNO)) {
       results$error=TRUE
      warning(paste("Treatment(s) number",paste0(setdiff(as.integer(sit_names), pgroTot$TRNO), collapse=",")," were missing in the DSSAT output file PlantGro.OUT."))
    }
      }
    }
    for (situation in sit_names) {
      
      pgro <- filter(pgroTot, TRNO==as.integer(situation))
      pgro <- pgro[!duplicated(pgro$Date),]  # DSSAT sometimes include replicated lines in the .out file ...
      results$sim_list[[situation]] <- pgro
      
      # Create variables that contain Zadok phenological stages in julian days (computed from the 1st of Jan of the sowing Year)
      zadok_df <- getIntGSTD(pgro$GSTD)
      zadok_df$dates <- pgro$Date[zadok_df$firstIndex]
      zadok_df$julDay <- julian(zadok_df$dates,origin=as.Date(paste(lubridate::year(zadok_df$dates[1]),"01","01",sep = "-")))
      
      ZlistNonFloored <- data.frame(Zadok=pgro$GSTD, julDay=julian(pgro$Date,origin=as.Date(paste(lubridate::year(zadok_df$dates[1]),"01","01",sep = "-"))))
      
      # interpolate julian days for non simulated zadok stages between first and last zadok simulated
      missingZadok <- setdiff(zadok_df$Zadok[1]:zadok_df$Zadok[length(zadok_df$Zadok)],zadok_df$Zadok)
      interpJulDays <- sapply(missingZadok, function(x) interp(x,ZlistNonFloored))
      zadok_df$firstIndex <- NULL
      tmp <- data.frame(Zadok=missingZadok, julDay=interpJulDays, dates=NA)
      
      zadok_df <- rbind(zadok_df,tmp)
      zadok_df <- zadok_df[order(zadok_df$Zadok),]
      
      # Extrapolate julian days for Zadok stages posterior to these simulated (to take them into account if they are observed but not simulated ...)
      missingZadok <- wrapr::seqi(max(zadok_df$Zadok)+1,100)
      endDate <- as.POSIXct(paste(lubridate::year(pgro$Date[nrow(pgro)]),"12","31",sep = "-"),format="%Y-%m-%d", tz="UTC")
      
      if (length(missingZadok)>0) {
        julianEndOfYear <- julian(as.Date(endDate), 
                                  origin=as.Date(paste(lubridate::year(zadok_df$dates[1]),"01","01",sep = "-")))
        
        dftmp <- data.frame(Zadok=missingZadok, julDay=julianEndOfYear[1], dates=NA)
        zadok_df <- rbind(zadok_df,dftmp)
      }
      
      df=as.data.frame(as.list(setNames(as.numeric(zadok_df$julDay),paste0("Zadok",zadok_df$Zadok))))
      
      if (!(endDate %in% results$sim_list[[situation]]$Date)) {
        results$sim_list[[situation]] <- 
          dplyr::bind_rows(results$sim_list[[situation]],data.frame(Date=endDate))
      }
      
      results$sim_list[[situation]] <- 
        dplyr::bind_cols(results$sim_list[[situation]],df)
      
    }
    attr(results$sim_list, "class")= "cropr_simulation"
    
    # Remove PlantGro.OUT file to be able to check if it is created for next model runs.
    file.remove("PlantGro.OUT")
    
  } else {
    results$error=TRUE
    warning("DSSAT output file PlantGro.OUT has not been generated.")
  }

  return(results)
  
}


# Extract rounded values for GSTD and their index in the GSTD list
getIntGSTD <- function(GSTD) { u <- unique(floor(GSTD)) ; data.frame(Zadok=u, firstIndex=match(u, floor(GSTD)))}

interp <- function(ZadokStage,ZList) {
  # Interpolate the julian day of a Zadok stage from julian days of other Zadok stages
  # ZadokStage is the ZadokSateg to interpolate
  # ZList is a list containing a vector named Zadok and a vector named julDay, containing the Zadok stages and their associated julian days
  inf <- which(ZadokStage > ZList$Zadok)
  sup <- which(ZadokStage < ZList$Zadok)
  tmp <- (ZList$julDay[sup[1]]-ZList$julDay[inf[length(inf)]]) * (ZadokStage - ZList$Zadok[inf[length(inf)]]) /
    (ZList$Zadok[sup[1]]-ZList$Zadok[inf[length(inf)]])+ZList$julDay[inf[length(inf)]]
  return(tmp)
}

