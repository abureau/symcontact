contact.1mat = function(theta,Nj=1,alpha=0.05,scaling=FALSE)
#' @description Reconstruction of contact matrix and estimation of confidence bounds from coefficients when there is a single contact matrix
#' @param theta Vector of parameters
#' @param Nj Matrix of sample sizes
#' @param alpha Significance level
#' @param scaling Boolean: if scaling=TRUE, divide mean contacts by Nj
#' @return List with elements estim (contact matrix estimate), lb (lower confidence bound) and ub (upper confidence bound)
  {
	nn = sqrt(length(theta)-1)
	b = matrix(theta[-length(theta)],nn,nn)
	a = theta[length(theta)]
	if (scaling==TRUE) n = as.vector(Nj)
	else n = 1
	mu = exp(b)/n
	# On divise par racine de 2 les termes hors diagnonale obtenus en prenant la moyenne
	mult = matrix(1/sqrt(2),nn,nn)
	diag(mult) = 1
# On construit les bornes de confiance sur l'Ã©chelle log
	l = mult*qnorm(1-alpha/2)*sqrt((1/mu+1/a)/Nj)
	lb = exp(b-l)/n
	ub = exp(b+l)/n
	list(estim=mu,lb=lb,ub=ub)
}
