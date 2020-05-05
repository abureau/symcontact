contact.1mat = function(theta,Nj=1,alpha=0.05)
#' @description Reconstruction of contact matrix and estimation of confidence bounds from coefficients when there is a single contact matrix
#' @param theta Vector of parameters
#' @param alpha Significance level
{
	nn = sqrt(length(theta)-1)
	b = matrix(theta[-length(theta)],nn,nn)
	a = theta[length(theta)]
	mu = exp(b)
	# On divise par racine de 2 les termes hors diagnonale obtenus en prenant la moyenne
	mult = matrix(1/sqrt(2),nn,nn)
	diag(mult) = 1
# On construit les bornes de confiance sur l'échelle log
	l = mult*qnorm(1-alpha/2)*sqrt((1/mu+1/a)/Nj)
	lb = exp(b-l)
	ub = exp(b+l)
	list(estim=mu,lb=lb,ub=ub)
}