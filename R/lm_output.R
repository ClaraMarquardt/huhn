#----------------------------------------------------------------------------#

#' @title Extract and format the output of a linear regression object.
#'
#' @description Extract the coeffients, standard  errors and p-values from a linear regression object (p-values based on t-dist (t-score under H0 - mean:0 & SE) with residual DF (# of clusters - # of coefficients)
#'
#' @export
#' @import data.table
#' @param TBA
#' @return TBA
#' @examples

lm_output <- function(lm, var_index=2, res_df=lm$df.residual, digit=3) {

  out        <- c(lm$coefficients[var_index],SE(lm)[var_index], 2*(1-pt(as.numeric(abs(lm$coefficients[var_index]/SE(lm)[var_index])),
                res_df)))
  out        <- round(out, digits=digit)
  sign       <- sign_star(out[3])

  out[1]     <-paste0(out[1]," ",sign)
  out[2]     <-paste0("[",out[2],"]")

  out        <- data.frame(c(out[1:2]),c(out[3],""))
  names(out) <- c(".coeff",".p")
    
  return(out)

}

#----------------------------------------------------------------------------#
