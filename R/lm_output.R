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

lm_output <- function(model, var_index=2, res_df=model$df.residual, digit=3, mode=NULL,
	cluster=NULL) {

  if (is.null(mode)) {
  	
  	out        <- c(model$coefficients[var_index],SE(model)[var_index], 
  					       2*(1-pt(as.numeric(abs(model$coefficients[var_index]/SE(model)[var_index])),
                	res_df)))
  
  } else if (mode=="cluster") {

  	var_cov_cluster <-  cluster.vcov(model, c(cluster))
  	model_cluster   <-  coeftest(model, var_cov_cluster)


  	out        <- c(model_cluster[var_index,][1], model_cluster[var_index,][2], 
  					       2*(1-pt(as.numeric(abs(model_cluster[var_index,][1]/model_cluster[var_index,][2])),
                	 res_df)))

  } else if (mode=="robust") {

  	var_cov_cluster <-  hccm(model, "hc1")
  	model_cluster   <-  coeftest(model, var_cov_cluster)


  	out        <- c(model_cluster[var_index,][1], model_cluster[var_index,][2], 
  					       2*(1-pt(as.numeric(abs(model_cluster[var_index,][1]/model_cluster[var_index,][2])),
                	 res_df)))

  }

  out        <- round(out, digits=digit)
  sign       <- sign_star(out[3])

  out[1]     <-paste0(out[1]," ",sign)
  out[2]     <-paste0("[",out[2],"]")

  out        <- data.frame(c(out[1:2]),c(out[3],""))
  names(out) <- c(".coeff",".p")
    
  return(out)

}

#----------------------------------------------------------------------------#
