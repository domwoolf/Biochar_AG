#' Calculate Relative Present Value (RPV)
#'
#' @param results_list List of result objects from calculate_bes, calculate_beccs, calculate_bebcs.
#' @return Data frame with comparison.
#' @export
calculate_rpv <- function(results_list) {
    # Extract NPVs
    npvs <- sapply(results_list, function(x) x$net_value)
    names(npvs) <- sapply(results_list, function(x) x$technology)

    # Find best alternative for each
    rpv_res <- list()
    for (tech in names(npvs)) {
        others <- npvs[names(npvs) != tech]
        oc <- max(others) # Opportunity Cost is the max of alternatives
        rpv <- npvs[tech] - oc
        rpv_res[[tech]] <- rpv
    }

    data.frame(
        Technology = names(npvs),
        NPV = npvs,
        Best_Alternative_NPV = sapply(names(npvs), function(x) max(npvs[names(npvs) != x])),
        RPV = unlist(rpv_res)
    )
}
