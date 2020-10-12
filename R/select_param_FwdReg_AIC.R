#' @title Provide a list of candidate parameters to estimate based on Forward Selection with AIC
#'
#' @param oblig_param_list Vector of names of parameters that must be estimated 
#' (list of obligatory parameters)
#' @param add_param_list Vector of names of additional parameters candidate to the 
#' estimation
#' @param crt_list Vector of names of candidate parameters that have been previously 
#' used in parameter estimation
#' @param crt_AIC AIC value obtained for the estimation performed using the candidate 
#' parameters listed in crt_list
#' @param prev_AIC Vector of AIC values obtained so far for all lists of candidate 
#' parameters except the current one (crt_list)
#'
#' @details This function implements the Froward Selection based on AIC algorithm, 
#' as described in AgMIP Calibration protocol phase III
#' (Crop model calibration using phenology data)
#'   If current AIC value is superior to the previous one, we replace the previously 
#'   added candidate parameter in crt_list by the next one in add_param_list.
#'   If current AIC value is inferior to the previous one, we add a new candidate 
#'   parameter to crt_list
#'   
#'   A first estimation using oblig_param_list as candidate parameters must be performed 
#'   before calling this function
#'
#' @return Names of the candidate parameters for next parameter estimation
#' Return NULL when all parameters have been tested.
#'
select_param_FwdReg_AIC <- function(oblig_param_list, add_param_list, crt_list, crt_AIC, prev_AIC) {

  if (crt_list[length(crt_list)]==add_param_list[length(add_param_list)]) { 
    
    # we tested all parameters
    return(NULL)
    
  } else if (length(crt_list)==length(oblig_param_list)) { 
    
    # we only tested so far the obligatory parameters
    next_list <- c(oblig_param_list,add_param_list[1])
    
  } else {
    
    if (crt_AIC<min(prev_AIC)) {
      # Add the next candidate to the list
      next_list <- c(crt_list, add_param_list[which(add_param_list==crt_list[length(crt_list)])+1])
      
    } else {
      
      # Replace the last candidate parameter by the next candidate
      new_candidate <- add_param_list[which(add_param_list==crt_list[length(crt_list)])+1]
      next_list <- c(crt_list[-length(crt_list)], new_candidate)
    }
  }

  return(next_list)
}