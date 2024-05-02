# Helpers -----------------------------------------------------------------
mcse_prop <- function(x, n_rep){
    sqrt(x * (1 - x) /n_rep)
}

mcse_gen <- function(empirical_var, n_rep){
    sqrt(empirical_var / n_rep)
}



# Bias --------------------------------------------------------------------
# As an example of how we could move forward
bias_mcse <- function(...){
    bias(..., MCSE = TRUE)
}

# TODO how to handle absolute but not relative bias
bias <- function(estimate, parameter = NULL, type = 'bias', abs = FALSE,
                 percent = FALSE, unname = FALSE, mcse = FALSE){
    if(isList(estimate)){
        return(unwind_apply_wind.list(
            lst=estimate, mat=parameter, fun=bias,
            type=type, abs=abs, percent=percent, unname=unname))
    }

    if(is.data.frame(estimate)) estimate <- as.matrix(estimate)
    if(is.vector(estimate)){
        nms <- names(estimate)
        estimate <- matrix(estimate)
        colnames(estimate) <- nms
    }
    stopifnot(is.matrix(estimate))
    stopifnot(type %in% c('bias', 'standardized', 'relative', 'abs_relative'))
    n_col <- ncol(estimate)
    if(type == "relative") stopifnot(!is.null(parameter))
    if(is.null(parameter)) parameter <- 0
    if(is.data.frame(parameter)) parameter <- unlist(parameter)
    stopifnot(is.vector(parameter))
    if(length(parameter) == 1L) parameter <- rep(parameter, n_col)
    equal_len <- length(estimate) == length(parameter)
    if(!equal_len)
        stopifnot(ncol(estimate) == length(parameter))
    diff <- t(t(estimate) - parameter)
    ret <- if(type == 'relative') colMeans(diff / parameter)
    else if(type == 'abs_relative') colMeans(diff / abs(parameter))
    else if(type == 'standardized') colMeans(diff) / colSDs(estimate)
    else colMeans(diff)
    if(abs) ret <- abs(ret)
    if(percent){
        ret <- ret * 100
        if(!(type %in% c('relative', 'abs_relative')))
            warning('Percentage only make sense for relative measures')
    }
    if(mcse){
        if(is.null(names(ret))) stop('Please provide named estimates as input')
        emp_sd <- colSDs(estimate)
        nsim <- nrow(estimate)
        if(type == 'relative'){
            ret <- emp_sd / sqrt(nsim) / abs(parameter)
        }
        else if(type == 'abs_relative'){
            # TODO
        }
        else if(type == 'standardized'){
            # TODO
        }
        else {  # "normal" bias
            ret <- emp_sd / sqrt(nsim)
        }
        if(percent){
            ret <- ret * 100
        }
    }
    if(unname) ret <- unname(ret)
    ret
}
