#----------------------------------------------------------------------------#

#' Perform a Hochberg correction of p-values to account for multiple hypothesis testing [Hochberg (1988) - Step Up].
#' 
#' @export
#' @param p Vector of raw p-values [numeric].
#' @return
#' @examples

hochberg_adj <- function(p) {

    ## Note: code taken from the p.adjust function ['stats' package] 
    
    method <- "hochberg"
    n      <- length(p)
    p      <- as.numeric(p)

    i      <- n:1L
    o      <- order(p, decreasing = TRUE)
    ro     <- order(o)
    p_mod  <- pmin(1, cummin((n - i + 1L) * p[o]))[ro]

    return(p_mod)
}

#----------------------------------------------------------------------------#


