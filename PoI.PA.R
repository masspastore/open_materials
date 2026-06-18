# +++++++++++++++++++++++++++++++++++
#' @name PoI.PA 
#' @description
#' Proportion of affected individuals and Interaction
#' @param Z = dataset with variables Y, X and D
PoI.PA <- function( Z, yvar = Y, xvar = X, dvar = D ){
  
  fit <- lm( formula( paste0(yvar," ~ ",xvar,"*",dvar) )
             , data = Z )
  BE <- coef(fit)
  int.point <- BE[3]*(-1) / BE[4]
  PA <- sum( fit$model[,xvar] > int.point ) / 
    length(fit$model[,xvar])
  
  PoI <- ( 1 + ( BE[4]*min(Z[,xvar]) + BE[3] )^2 / 
             ( BE[4]*max(Z[,xvar]) + BE[3] )^2 ) ^ (-1)
  
  return( list( PA = PA, PoI = as.numeric(PoI), 
                int.point = as.numeric(int.point) ))
}
