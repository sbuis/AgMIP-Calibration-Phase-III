#' @title Complete given initial values by sampling values from bounds taking into account inequality constraints between parameters
#'
#' @param init_values A data.frame containing some initial
#' values for the parameters (or NULL)
#' 
#' @param nb_rep Number of repetition of the minimization process
#'
#' @param lb Lower bounds of the parameters
#' 
#' @param ub Upper bounds of the parameters
#' 
#' @param ranseed Set random seed so that each execution give the same results.
#' If you want randomization, set it to NULL
#' 
#' @param satisfy_par_const Function that defines inequality constraints between parameters
#' 
#' @return A data.frame containing initial values for all parameters and repetitions 
#' of the minimization process that includes the initial values given in `init_values` 
#' plus some randomly chosen ones in bounds `lb` and `ub`
#' 
complete_init_values <- function(init_values, nb_rep, lb, ub, ranseed, satisfy_par_const=NULL) {
  
  if (!is.null(lb) && !is.null(ub)) {
    param_names <- names(lb)
    init_values <- CroptimizR:::get_params_init_values(list(lb=lb, ub=ub, init_values=init_values))
    sampled_values <- as.data.frame(CroptimizR:::sample_params(list(lb=lb, ub=ub),nb_rep,ranseed))
    if (!is.null(satisfy_par_const)) {
      idx <- which(sapply(1:nrow(sampled_values),function(x) {satisfy_par_const(sampled_values[x,])}))
      sampled_values <- sampled_values[idx,]
      count <- 1
      while (nrow(sampled_values)<nb_rep && count<1000) {
        seed <- sample(1000,1,replace=FALSE)
        sampled_tmp <- as.data.frame(CroptimizR:::sample_params(list(lb=lb, ub=ub),nb_rep, 
                                                                seed=seed))
        idx <- which(sapply(1:nrow(sampled_tmp),function(x) {satisfy_par_const(sampled_tmp[x,])}))
        sampled_values <- bind_rows(sampled_values,sampled_tmp[idx,])	
        count <- count+1
      }
      if (count>=1000) stop("Error, number of sampling of init values reached maximum allowed value.")
      sampled_values <- sampled_values[1:nb_rep,]
    }
    for (param in param_names) {
      idx <- which(!is.na(init_values[,param]))
      if (length(idx)>0) {
        sampled_values[idx[1:min(nb_rep,length(idx))],param] <- init_values[idx[1:min(nb_rep,length(idx))],param]
      }
    }
  } else {
    if (nrow(init_values)<nb_rep || any(is.na(init_values))) {
      stop("Init_values must contain initial values for all parameters and repetitions if ub and lb are not provided.")
    }
    sampled_values <- init_values
  }
  
  return(sampled_values)
}
