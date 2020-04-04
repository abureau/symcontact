# Maximisation de la vraisemblance pour matrices de contact par groupe 
# sous contrainte de symétrie de la somme des matrices

# Fichier des fonctions

fit.matrices = function(dat,wi,X,count.names,agecut,ipremy,theta0)
#' @description Fitting contact matrices for multiple locations under the constraint that the matrix of the total of the contacts is symmetrical.
#' @param dat Matrix containing the contacts counts for all age slices and all locations, in wide format
#' @param wi Vector of individual weights
#' @param X Design matrix for the cross-tabulated data
#' @param count.names Vector of names of the columns of dat containing the contacts counts
#' @param agecut Vector of breakpoints of the age slices for the participants (should match the age slices for the contacts)
#' @param ipremy Matrix of the first age slice for each combination of location and type of household
#' @param theta0 Vector of initial parameter values (log counts and dispersion parameters) to be passed to the optimizer
{
	nn = length(agecut)-1
	
	tab = t(apply(dat[,count.names],2,function(vec) tapply(vec,list(cut(dat$age,breaks=agecut),dat$menage_avec017),sum,na.rm=T)))
	wj = t(tapply(wi,list(cut(dat$age,breaks=agecut),dat$menage_avec017),sum,na.rm=T))
	wj[is.na(wj)] = 0

	# Création du vecteur de comptes y, du vecteur de poids w et du vecteur d'indices de début de chaque matrice iniv
	y = w = NULL
	# Boucle sur les types de matrices (5 types)
	for (k in 1:(nrow(tab)/nn)) 
	{
		# Boucle sur le statut avec017 (non/oui)
		for (j in 1:(ncol(tab)/nn))
		{
		vec = as.vector(tab[(k-1)*nn+1:nn,(j-1)*nn+ipremy[j,k]:nn])
		y = c(y,vec)
		w = c(w,rep(wj[j,ipremy[j,k]:nn],rep(nn,nn-ipremy[j,k]+1)))				
		}
	}
	
	# Estimation des matrices
	obj = ROI:::nlminb2(start=theta0,objective=nlognb,eqFun=contrc.fonc)
	return(obj$par)
}

nlognb = function(theta)
#' @param theta Vector of parameters
#' @details Computes minus the log-likelihood of a negative binomial distribution. There must be a vector y of contact counts, a design matrix X and a vector of weights w in the environment. The last parameter is the scale parameter, all others are regression coefficients.
#' @return minus the log-likelihood
{
    # On sépare le paramètre d'échelle (dernier) des coefficients
	b = theta[-length(theta)]
	a = theta[length(theta)]
	mu = exp(X%*%b)
	ll = 0
	for (i in 1:length(y)) ll = ll + w[i]*dnbinom(x= y[i], size=a, mu=mu[i], log = TRUE)
	return (-ll)
}

nlognb.counts = function(theta)
#' @param theta Vector of parameters
#' @details Computes minus the log-likelihood of a negative binomial distribution. There must be a vector y of contact counts and a vector of weights w in the environment. The last parameter is the scale parameter, all others are log mean counts.
#' @return minus the log-likelihood
{
    # On sépare le paramètre d'échelle (dernier) des coefficients
	mu = exp(theta[-length(theta)])
	a = theta[length(theta)]
	ll = 0
	for (i in 1:length(y)) ll = ll + w[i]*dnbinom(x= y[i], size=a, mu=mu[i], log = TRUE)
	return (-ll)
}

contr.fonc = function(theta) 
#' @param theta Vector of parameters 
#' @details Evaluates contrasts to inforce symetry of the sum of two matrices. There must be a vector y of contact counts, a design matrix X and a vector of weights w in the environment. Parameters are regression coefficients (except the last one).
#' @return Vector of constraint values
{
b = theta[-length(theta)]
nn = sqrt(nrow(X)/2)
contr.vec = numeric(0)
for(k in 1:(nn-1))
{
	tmp = wj[k]*(exp(X[(k-1)*nn+(k+1):nn,]%*%b)+exp(X[nn^2+(k-1)*nn+(k+1):nn,]%*%b))-wj[(k+1):nn]*(exp(X[(k:(nn-1))*nn+k,]%*%b)+exp(X[nn^2+(k:(nn-1))*nn+k,]%*%b))
	contr.vec = c(contr.vec,tmp)
}
return(contr.vec)
}

contrc2.fonc = function(theta) 
#' @param theta Vector of parameters
#' @details Evaluates contrasts to inforce symetry of the sum of two matrices. There must be a vector y of contact counts and a vector of weights w in the environment. Parameters are log mean counts (except the last one).
#' @return Vector of constraint values
{
b = theta[-length(theta)]
nn = sqrt(length(b)/2)
contr.vec = numeric(0)
for(k in 1:(nn-1))
{
	tmp = wj[k]*(exp(b[(k-1)*nn+(k+1):nn])+exp(b[nn^2+(k-1)*nn+(k+1):nn]))-wj[(k+1):nn]*(exp(b[(k:(nn-1))*nn+k])+exp(b[nn^2+(k:(nn-1))*nn+k]))
	contr.vec = c(contr.vec,tmp)
}
return(contr.vec)
}


contrc.fonc = function(theta) 
#' @param theta Vector of parameters
#' @details Evaluates contrasts to inforce symetry of the sum of length(iniv) matrices. In the environment there must be a vector y of contact counts, a vector w of weights, a vector iniv of start indices for each contact matrix, a vector iprem of initial age slice for each contact matrix and a list imat of boolean matrices indicating which contact matrices apply to each age slice for each combination of matrices applicable to the age slice. Parameters are log mean counts (except the last one).
#' @return Vector of constraint values
{
b = theta[-length(theta)]
contr.vec = numeric(0)
# Boucle sur les tranches d'âge
for(k in 1:(nn-1))
{
	# Indice initial pour ce groupe d'âge dans le vecteur de coefficients
	tmp = numeric(nn-k)
	# Boucle sur les combinaisons possibles de matrices pour un groupe d'âge
	for(h in 1:nrow(imat[[k]]))
	{
		# Si on a atteint la 1re tranche d'âge pour la matrice
		mc = matrix(exp(b[outer((k+1):nn, iniv[imat[[k]][h,]] + (k-iprem[imat[[k]][h,]])*nn,"+")]),nn-k,sum(imat[[k]][h,]))
		ml = matrix(exp(b[matrix(iniv[imat[[k]][h,]],nn-k,sum(imat[[k]][h,]),byrow=T) + outer(k:(nn-1),iprem[imat[[k]][h,]],"-")*nn + k]),nn-k,sum(imat[[k]][h,]))
		tmp = tmp + wj[h,k]*apply(mc,1,sum) - wj[h,(k+1):nn]*apply(ml,1,sum)
	}
	contr.vec = c(contr.vec,tmp)
}
return(contr.vec)
}


contact.square.mat = function(b,X,nn)
#' @description Reconstruction of contact matrices from coefficients
#' @param b Vector of regression coefficients
#' @param X Design matrix (optional)
#' @param nn Dimension of the contact matrix
#' @return List of length(mu)/nn^2 contact matrices
{
	if (missing(X)) mu = exp(b)
	else mu = exp(X%*%b)
	m = list()
	for (g in 1:(length(mu)/nn^2))
	m[[g]] = matrix(mu[(g-1)*nn^2+(1:nn^2)],nn,nn)
	return(m)
}

contact.mat = function(b,X,iniv,iprem)
#' @description Reconstruction of contact matrices from coefficients
#' @param b Vector of regression coefficients
#' @param X Design matrix (optional)
#' @param nn Dimension of the contact matrix
#' @return List of length(mu)/nn^2 contact matrices
{
	if (missing(X)) mu = exp(b)
	else mu = exp(X%*%b)
	inivl = c(iniv,length(mu))
	m = list()
	for (g in 1:length(iniv))
	m[[g]] = matrix(c(rep(0,(iprem[g]-1)*nn),mu[(inivl[g]+1):inivl[g+1]]),nn,nn)
	return(m)
}
