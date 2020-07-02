# Maximisation de la vraisemblance pour matrices de contact par groupe 
# sous contrainte de symétrie de la somme des matrices

# Fichier des fonctions

fit.matrices = function(dat,wi,X,duration,count.names,agecut,iprem,idern,ipremy,iderny,imat,theta0,iniv,var.kid,vd,boot=F)
#' @description Fitting contact matrices for multiple locations under the constraint that the matrix of the total of the contacts is symmetrical.
#' @param dat Matrix containing the contacts counts for all age slices and all locations, in wide format
#' @param wi Vector of individual weights
#' @param X Design matrix for the cross-tabulated data (optionnal)
#' @param duration Vector of durations of observations of contacts (optionnal)
#' @param count.names Vector of names of the columns of dat containing the contacts counts
#' @param agecut Vector of breakpoints of the age slices for the participants (should match the age slices for the contacts)
#' @param iprem Vector of the first age slice for each contact matrix
#' @param idern Vector of the last age slice for each contact matrix
#' @param ipremy Matrix of the first age slice for each combination of location and type of household
#' @param iderny Matrix of the last age slice for each combination of location and type of household
#' @param imat List of boolean matrices where each row indicates the contact matrices applicable to a different subset of subjects
#' @param theta0 Vector of initial parameter values (log counts and dispersion parameters) to be passed to nlminb2
#' @param iniv Vector of the indices preceeding the first parameter of each contact matrix in the vector theta0
#' @param vd Vector of the indices of the matrices with a specific denominator. The names of this vector must be the boolean variables defining whether a subject is included in the denominator or not
#' @param boot Boolean indicating whether to return only a vector of statistics. The default is FALSE, which implies the object produced by nlminb2 will be returned
{
	if (missing(X)) objective = nlognb.counts
	else
	{
		if (ncol(X) != (length(theta0)-1)) stop ("Number of columns of X ",ncol(X)," does not equal the length of theta0 minus one",length(theta0)-1)
		assign("X",X,env = parent.frame())
		objective = nlognb
	}

	if (!missing(duration)) d = duration else d = rep(1,length(wi))
	nn = length(agecut)-1
	
	if (missing(var.kid))
	{
		tab = t(apply(dat[,count.names],2,function(vec) tapply(vec,cut(dat$age,breaks=agecut),sum,na.rm=T)))
		wt = matrix(tapply(wi,cut(dat$age,breaks=agecut),mean,na.rm=T),1,nn)		
	}
	else 
	{
		tab = t(apply(dat[,count.names],2,function(vec) tapply(vec,list(cut(dat$age,breaks=agecut),dat[,var.kid]),sum,na.rm=T)))
		wt = tapply(wi,list(dat[,var.kid],cut(dat$age,breaks=agecut)),mean,na.rm=T)		
	}			
	if (missing(vd))
	{
		wj = wtt = wte = wt
		wj[is.na(wj)] = 0
		vd = numeric(0)
	}
	else
	{
	  # Création de la variable catégorielle « lieud » à partir du croisement des niveaux 
	  # des variables de dénominateurs incluses dans « vd » encodées 0/1 pour la 1re variable, 0/2 pour la 2e, ainsi de suite.
	  lieud = rep(0,nrow(dat))
	  for (v in 1:length(vd)) lieud = lieud + unlist(dat[names(vd)[v]])*2^(v-1)

		# Ici on somme les poids par catégorie d'occupation
		if (missing(var.kid)) wj = tapply(d*wi,list(lieud,cut(dat$age,breaks=agecut)),sum,na.rm=T)
		else 
		{
			tmp = tapply(d*wi,list(lieud,dat[,var.kid],cut(dat$age,breaks=agecut)),sum,na.rm=T)
			wj = apply(tmp,3,function(mat) stack(data.frame(mat))$values)
		}
		# Ensuite on divise les poids par les effectifs totaux par tranche d'âge
		if (!missing(duration)) n.par.age = tapply(duration,cut(dat$age,breaks=agecut),sum,na.rm=T)	
		else n.par.age = tapply(wi,cut(dat$age,breaks=agecut),function(vec) sum(!is.na(vec)))
		wj = wj/matrix(rep(n.par.age,nrow(wj)),nrow=nrow(wj),byrow=T)
		wj[is.na(wj)] = 0

		# Poids et effectifs pour les matrices par lieux (on somme les lignes de wj correspondantes)
		
		wl = list()
		for (v in 1:length(vd))
		{
		  # Recul de la dernière rangée pour une matrice
		  ir = (length(unique(lieud))-v)*(1:0)
		  if (missing(var.kid))	
		    wl[[vd[v]]] = matrix(apply(wj[nrow(nj)-ir,],2,sum),1,nn)
		  else
		    wl[[vd[v]]] = rbind(apply(wj[nrow(nj)/2-ir,],2,sum),apply(wj[nrow(nj)-ir,],2,sum))
		}
  }
	# Création du vecteur de comptes y, du vecteur de poids w et du vecteur d'indices de début de chaque matrice iniv
	y = w = NULL
	# Boucle sur les types de matrices
	for (k in 1:(nrow(tab)/nn)) 
	{
		# Boucle sur le statut avec017 (non/oui)
		for (j in 1:(ncol(tab)/nn))
		{
		vec = as.vector(tab[(k-1)*nn+1:nn,(j-1)*nn+ipremy[j,k]:iderny[j,k]])
		y = c(y,vec)
		# Si la matrice a un dénominateur spécifique
		if (k %in% vd)
		  w = c(w,rep(wl[[k]][j,ipremy[j,k]:iderny[j,k]],rep(nn,iderny[j,k]-ipremy[j,k]+1)))				
		else
		  w = c(w,rep(wt[j,ipremy[j,k]:iderny[j,k]],rep(nn,iderny[j,k]-ipremy[j,k]+1)))			
		}
	}
	
	# Assignation des objets requis par nlognb et contrc.fonc dans l'environnement parent
	assign("nn",nn,env = parent.frame())
	assign("y",y,env = parent.frame())
	assign("w",w,env = parent.frame())
	assign("wj",wj,env = parent.frame())
	assign("iniv",iniv,env = parent.frame())
	assign("iprem",iprem,env = parent.frame())	
	assign("idern",idern,env = parent.frame())	
	assign("imat",imat,env = parent.frame())
	# Estimation des matrices
	obj = ROI:::nlminb2(start=theta0,objective=objective,eqFun=contrc.fonc)
	if (boot) return(c(obj$par,as.vector(wj)))
	else
	{
		obj$wj = wj
		return(obj)
	}
}

fit.rates.matrices = function(dat,wi,X,duration,count.names,agecut,iprem,idern,ipremy,iderny,imat,theta0,iniv,var.kid,vd,boot=F)
#' @description Fitting contact matrices for multiple locations under the constraint that the matrix of the total of the contacts is symmetrical.
#' @param dat Matrix containing the contacts counts for all age slices and all locations, in wide format
#' @param wi Vector of individual weights
#' @param X Design matrix for the cross-tabulated data (optionnal)
#' @param duration Vector of durations of observations of contacts (optionnal)
#' @param count.names Vector of names of the columns of dat containing the contacts counts
#' @param agecut Vector of breakpoints of the age slices for the participants (should match the age slices for the contacts)
#' @param iprem Vector of the first age slice for each contact matrix
#' @param idern Vector of the last age slice for each contact matrix
#' @param ipremy Matrix of the first age slice for each combination of location and type of household
#' @param iderny Matrix of the last age slice for each combination of location and type of household
#' @param imat List of boolean matrices where each row indicates the contact matrices applicable to a different subset of subjects
#' @param theta0 Vector of initial parameter values (log counts and dispersion parameters) to be passed to nlminb2
#' @param iniv Vector of the indices preceeding the first parameter of each contact matrix in the vector theta0
#' @param vd Vector of the indices of the matrices with a specific denominator. The names of this vector must be the boolean variables defining whether a subject is included in the denominator or not
#' @param boot Boolean indicating whether to return only a vector of statistics. The default is FALSE, which implies the object produced by nlminb2 will be returned
{
	if (missing(X)) objective = nlognb.counts.rates
	else
	{
		if (ncol(X) != (length(theta0)-1)) stop ("Number of columns of X ",ncol(X)," does not equal the length of theta0 minus one",length(theta0)-1)
		objective = nlognb.rates
	}

	if (!missing(duration)) d = duration else d = rep(1,length(wi))
	nn = length(agecut)-1
	
	if (missing(var.kid))
	{
		tab = t(apply(dat[,count.names],2,function(vec) tapply(vec,cut(dat$age,breaks=agecut),sum,na.rm=T)))
		wt = matrix(tapply(wi*d,cut(dat$age,breaks=agecut),sum,na.rm=T),1,nn)
		n.par.age = matrix(tapply(d,cut(dat$age,breaks=agecut),sum,na.rm=T),1,nn)	
}
	else 
	{
		tab = t(apply(dat[,count.names],2,function(vec) tapply(vec,list(cut(dat$age,breaks=agecut),dat[,var.kid]),sum,na.rm=T)))
		wt = tapply(wi*d,list(dat[,var.kid],cut(dat$age,breaks=agecut)),sum,na.rm=T)		
		n.par.age = tapply(d,list(dat[,var.kid],cut(dat$age,breaks=agecut)),sum,na.rm=T)
	}			
	if (missing(vd))
	{
		wj = wt
		nj = n.par.age
		wj[is.na(wj)] = 0
		nj[is.na(nj)] = 0
		vd = numeric(0)
	}
	else
	{
	  # Création de la variable catégorielle « lieud » à partir du croisement des niveaux 
	  # des variables de dénominateurs incluses dans « vd » encodées 0/1 pour la 1re variable, 0/2 pour la 2e, ainsi de suite.
	  lieud = rep(0,nrow(dat))
	  for (v in 1:length(vd)) lieud = lieud + unlist(dat[names(vd)[v]])*2^(v-1)
	  
		# Ici on somme les poids par catégorie d'occupation
		if (missing(var.kid)) 
		{
			wj = tapply(wi*d,list(lieud,cut(dat$age,breaks=agecut)),sum,na.rm=T)
			nj = tapply(d,list(lieud,cut(dat$age,breaks=agecut)),sum,na.rm=T)	
		}
		else 
		{
			tmp = tapply(wi*d,list(lieud,dat[,var.kid],cut(dat$age,breaks=agecut)),sum,na.rm=T)
			wj = apply(tmp,3,function(mat) stack(data.frame(mat))$values)
			tmp = tapply(d,list(lieud,dat[,var.kid],cut(dat$age,breaks=agecut)),sum,na.rm=T)
			nj = apply(tmp,3,function(mat) stack(data.frame(mat))$values)
		}
		wj[is.na(wj)] = 0
		nj[is.na(nj)] = 0

		# Poids et effectifs pour les matrices par lieux (on somme les lignes de wj correspondantes)

		wl = nl = list()
		cobs = sort(unique(lieud))
		for (v in 1:length(vd))
		{
		  # Recul de la dernière rangée pour une matrice
		  suite = rep(seq(0,2^length(vd)-2^v,by=2^v),rep(2^(v-1),2^(length(vd)-v))) + rep((2^(v-1)+1):2^v,2^(length(vd)-v)) - 1
		  ir = which(cobs%in%suite)
		  if (missing(var.kid))
		  {
			  nl[[vd[v]]] = matrix(apply(nj[ir,],2,sum),1,nn)
			  wl[[vd[v]]] = matrix(apply(wj[ir,],2,sum),1,nn)
		  }
		  else
		  {
			  nl[[vd[v]]] = rbind(apply(nj[ir,],2,sum),apply(nj[nrow(nj)/2+ir,],2,sum))
			  wl[[vd[v]]] = rbind(apply(wj[ir,],2,sum),apply(wj[nrow(nj)/2+ir,],2,sum))
		  }
		}
	}

	# Création du vecteur de comptes y, du vecteur d'effectifs nvec et du vecteur de poids w et du vecteur d'indices de début de chaque matrice iniv
	y = w = nvec = NULL
	# Boucle sur les types de matrices
	for (k in 1:(nrow(tab)/nn)) 
	{
		# Boucle sur le statut avec017 (non/oui)
		for (j in 1:(ncol(tab)/nn))
		{
		  vec = as.vector(tab[(k-1)*nn+1:nn,(j-1)*nn+ipremy[j,k]:iderny[j,k]])
		  y = c(y,vec)
		  # Si la matrice a un dénominateur spécifique
		  if (k %in% vd)
		  {
			  w = c(w,rep(wl[[k]][j,ipremy[j,k]:iderny[j,k]],rep(nn,iderny[j,k]-ipremy[j,k]+1)))				
			  nvec = c(nvec,rep(nl[[k]][j,ipremy[j,k]:iderny[j,k]],rep(nn,iderny[j,k]-ipremy[j,k]+1)))							
		  }
			else
			{
				w = c(w,rep(wt[j,ipremy[j,k]:iderny[j,k]],rep(nn,iderny[j,k]-ipremy[j,k]+1)))			
				nvec = c(nvec,rep(n.par.age[j,ipremy[j,k]:iderny[j,k]],rep(nn,iderny[j,k]-ipremy[j,k]+1)))							
			}			
		}
	}
	# On garde seulement les combinaisons dont les effectifs sont non-nuls
	nonnul = nvec>0
	nvec = nvec[nonnul]
	y=y[nonnul]
	w=w[nonnul]
	if (!missing(X)) X=X[nonnul,]
	assign("X",X,env = parent.frame())
	# normalisation des poids
	w = w/nvec
	
	# Assignation des objets requis par nlognb.rates et contrc.fonc dans l'environnement parent
	assign("nn",nn,env = parent.frame())
	assign("y",y,env = parent.frame())
	assign("w",w,env = parent.frame())
	assign("wj",wj,env = parent.frame())
	assign("nvec",nvec,env = parent.frame())
	assign("iniv",iniv,env = parent.frame())
	assign("iprem",iprem,env = parent.frame())	
	assign("idern",idern,env = parent.frame())	
	assign("imat",imat,env = parent.frame())
	# Estimation des matrices
	obj = ROI:::nlminb2(start=theta0,objective=objective,eqFun=contrc.fonc)
	if (boot) return(c(obj$par,as.vector(wj)))
	else
	{
		obj$wj = wj
		return(obj)
	}
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

nlognb.rates = function(theta)
  #' @param theta Vector of parameters
  #' @details Computes minus the log-likelihood of a negative binomial distribution. There must be a vector y of contact counts, a design matrix X and a vector of weights w in the environment. The last parameter is the scale parameter, all others are regression coefficients.
  #' @return minus the log-likelihood
{
  # On sépare le paramètre d'échelle (dernier) des coefficients
  b = theta[-length(theta)]
  a = theta[length(theta)]
  mu = nvec*exp(X%*%b)
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

nlognb.counts.rates = function(theta)
#' @param theta Vector of parameters
#' @details Computes minus the log-likelihood of a negative binomial distribution. There must be a vector y of contact counts and a vector of weights w in the environment. The last parameter is the scale parameter, all others are log mean counts.
#' @return minus the log-likelihood
{
    # On sépare le paramètre d'échelle (dernier) des coefficients
	mu = nvec*exp(theta[-length(theta)])
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
# Boucle sur les tranches d'âge des participants
for(k in 1:(nn-1))
{
	# Indice initial pour ce groupe d'âge dans le vecteur de coefficients
	tmp = numeric(nn-k)
	# Boucle sur les combinaisons possibles de matrices pour un groupe d'âge
	for(h in 1:nrow(imat[[k]]))
	{
		# imat[[k]][h,] s'assure qu'on a atteint la 1re tranche d'âge pour la matrice
		# Pour la matrice mc des colonnes dans les contraintes, on fait le calcul complet
		mc = matrix(exp(b[outer((k+1):nn, iniv[imat[[k]][h,]] + (k-iprem[imat[[k]][h,]])*nn,"+")]),nn-k,sum(imat[[k]][h,]))
		# Matrice des indices de la ligne des contacts de la tranche d'âge k pour les participants de la 1re tranche d'âge des participants
		mil = matrix(iniv[imat[[nn+1]][h,]],nn-k,sum(imat[[nn+1]][h,]),byrow=T) + k
#		ml = matrix(0,nn-k,sum(imat[[k]][h,]))
# On utilise la dernière entrée de imat donnant toutes les matrices qui peuvent être impliquées
		ml = matrix(0,nn-k,sum(imat[[nn+1]][h,]))
		for (l in 1:ncol(ml))
			# Pour la matrice ml des lignes dans les contraintes, la tranche d'âge k contribue seulement si les participants de cette tranche sont inclus dans la matrice impliquée		
			if (k<idern[imat[[nn+1]][h,]][l])
			{
				# On ajoute à l'indice de base dans mil le décalage pour la tranche d'âge k des participants
				ml[max(1,iprem[imat[[nn+1]][h,]][l]-k)+0:(idern[imat[[nn+1]][h,]][l]-max(k+1,iprem[imat[[nn+1]][h,]][l])),l] = exp(b[mil[1:(idern[imat[[nn+1]][h,]][l]-max(k,iprem[imat[[nn+1]][h,]][l]-1)),l] + ((max(k+1,iprem[imat[[nn+1]][h,]][l]):idern[imat[[nn+1]][h,]][l])-iprem[imat[[nn+1]][h,]][l])*nn ])
			}
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

contact.mat = function(b,X,iniv,iprem,idern,nn,Nj,scaling=FALSE)
#' @description Reconstruction of contact matrices from coefficients
#' @param b Vector of regression coefficients
#' @param X Design matrix (optional)
#' @param iniv Vector of the index preceeding the first parameter of each contact matrix in the vector theta0
#' @param iprem Vector of the first age slice for each contact matrix
#' @param idern Vector of the last age slice for each contact matrix
#' @param nn Dimension of the contact matrix
#' @param Nj List of matrices of sample sizes
#' @param scaling Boolean: if scaling=TRUE, divide mean contacts by Nj
#' @return List of length(mu)/nn^2 contact matrices
{
	if (scaling==TRUE) n = unlist(Nj)
	else n = 1
	if (missing(X)) mu = exp(b)/n
	else mu = exp(X%*%b)/n
	inivl = c(iniv,length(mu))
	m = list()
	for (g in 1:length(iniv))
	m[[g]] = matrix(c(rep(0,(iprem[g]-1)*nn),mu[(inivl[g]+1):inivl[g+1]],rep(0,(nn-idern[g])*nn)),nn,nn)
	return(m)
}
