#--------- TODOs
#- Decide on best way to return values
#- decide on what to do with other estimates used in the plan* functions
#- add input checks/useful error messages
#- add documentation
#- add tests
#- decide if we use n-1 or n for variances etc.

# Helper for calculating point estimate and confidence interval for empirical variance
ci_var <- function(data,
                   alpha = 0.05){
    n <- length(data)
    var <- stats::var(data)
    df <- n - 1
    ci <- c((n - 1) * var / qchisq(1 - alpha / 2, df),
            var,
            (n - 1) * var / qchisq(alpha / 2, df))
    ci
}

plan_bias <-
    function(target_mcse,
             empirical_var = NULL,
             data = NULL,
             alpha = 0.05) {
        if (is.null(empirical_var)) {
            empirical_var <- stats::var(data)
        }

        n_rep <- empirical_var / target_mcse ^ 2

        if (!is.null(data)) {
            ci <- ci_var(data, alpha)
            lower_bound <- ci[1] / target_mcse ^ 2
            upper_bound <- ci[3] / target_mcse ^ 2
        } else {
            lower_bound <- NA
            upper_bound <- NA
        }

        result <-
            structure(n_rep, lower_bound = lower_bound, upper_bound = upper_bound)
        class(result) <- "plan_sim"
        result
    }

# TODO what's the cleanest way to obtain the MSE_hat?
# that will also be associated with uncertainty
plan_RMSE <-
    function(target_mcse,
             empirical_var = NULL,
             data = NULL,
             mse_hat,
             alpha = 0.05) {
        if (is.null(empirical_var)) {
            empirical_var <- stats::var(data)
        }

        n_rep <- empirical_var / (4 * mse_hat * target_mcse ^ 2)

        if(!is.null(data)){
            ci <- ci_var(data, alpha)
            lower_bound <- ci[1] / (4 * mse_hat * target_mcse ^ 2)
            upper_bound <- ci[3] / (4 * mse_hat * target_mcse ^ 2)
        } else {
            lower_bound <- NA
            upper_bound <- NA
        }
        result <-
            structure(n_rep, lower_bound = lower_bound, upper_bound = upper_bound)
        class(result) <- "plan_sim"
        result
}

# TODO not fully clear to me, not sure how relevant
plan_IRMSE <- function(){

}

plan_MAE <- function(){

}

plan_RE <- function(){

}

plan_RSE <- function(){

}


# Standard error of ratio:
# we need to calculate the correlation between the two estimates
# and the variance of the ratio
# and the individiual variances of the estimates
# wonder if there is too much uncertainty in there
plan_RAB <- function(){

}



plan_RD <- function(){

}

plan_EDR <- function(target_mcse,
                     target_edr = 0.5){
    n_rep <- target_edr * (1 - target_edr) / target_mcse^2
    n_rep
}


# ECR also includes the average width of the CIs
# should accept "coverage" and width
plan_ECR <- function(target_mcse,
                     target_ecr = 0.5,
                     target_statistic = "coverage",
                     alpha = 0.05){
    if(target_statistic == "coverage"){
        n_rep <- target_edr * (1 - target_edr) / target_mcse^2

    } else if(target_statistic == "width"){
        n_rep <- empirical_var / target_mcse ^ 2
        if(!is.null(data)){
            ci <- ci_var(data, alpha)
            lower_bound <- ci[1] / target_mcse ^ 2
            upper_bound <- ci[3] / target_mcse ^ 2
        } else {
            lower_bound <- NA
            upper_bound <- NA
        }
    }
    result <-
        structure(n_rep, lower_bound = lower_bound, upper_bound = upper_bound)
    class(result) <- "plan_sim"
    result

}


# Congruence Coefficient
plan_CC <- function(target_mcse,
                    empirical_var = NULL,
                    data = NULL,
                    alpha = 0.05){



}


