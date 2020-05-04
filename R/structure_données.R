# Création des objets décrivant la structure des données 

# Matrices à 10 tranches d'âge
#nn=10
#iniv.10cat = cumsum(c(0,7,nn,8,rep(nn,2))*nn)
iprem = c(4,1,3,1,1,1)
idern.10cat = c(10,9,9,8,8,10)
#idern.10cat = c(10,9,10,8,9,10)
#np.10cat = sum(c(7,nn,8,rep(nn,3))*nn)
#np.10cat = sum((idern.10cat-iprem+1)*nn)
# Matrices à 8 tranches d'âge
nn=8
idern.8cat = idern.10cat - 2
iniv.8cat = cumsum(c(0,(idern.8cat-iprem+1)[-length(iprem)])*nn)
np.8cat = sum((idern.8cat-iprem+1)*nn)
iprem.5mat = iprem[2:6]
idern.5mat.8cat = idern.8cat[c(1,3:6)]
iniv.5mat.8cat = cumsum(c(0,(idern.5mat.8cat-iprem.5mat+1)[-length(iprem.5mat)])*nn)
np.5mat.8cat = sum((idern.5mat.8cat-iprem.5mat+1)*nn)

# Structures en supposant que tous les ado et adultes peuvent étudier et travailler
# Pour enfant.struct et ado.struct, la 1re ligne est arbitraire car les effectifs sont 0
enfant.struct = matrix(rep(c(F,T,F,rep(T,3)),2),2,6,byrow=T)
ado.struct = matrix(rep(c(F,rep(T,5)),2),2,6,byrow=T)
adulte.struct = matrix(c(T,F,F,T,rep(T,8)),2,6)
retraite.struct = matrix(c(T,F,T,F,F,T),1,6)
concensus.struct = adulte.struct

enfant.5mat.struct = matrix(c(T,F,rep(T,3)),1,5)
adulte.5mat.struct = matrix(c(rep(T,5)),1,5)
preretraite.5mat.struct = matrix(c(T,T,F,F,T),1,5)
retraite.5mat.struct = matrix(c(T,F,F,F,T),1,5)
concensus.5mat.struct = adulte.5mat.struct


# Structures stratifiant pour les étudiants et les travailleurs
# Ici on suppose que tous les enfants de 6 ans et plus et les ados vont à l'école
petits.strat.struct = matrix(F,8,6)
petits.strat.struct[5,] = c(F,T,F,F,rep(T,2))
petits.strat.struct[6,] = c(F,T,F,rep(T,3))
enfant.strat.struct = matrix(F,8,6)
enfant.strat.struct[6,] = c(F,T,F,rep(T,3))
ado.strat.struct = matrix(F,8,6)
ado.strat.struct[6,] = c(F,T,F,rep(T,3))
ado.strat.struct[8,] = c(F,rep(T,5))
adulte.strat.struct = matrix(F,8,6)
adulte.strat.struct[1,] = c(T,F,F,F,T,T)
adulte.strat.struct[2,] = c(T,F,F,T,T,T)
adulte.strat.struct[3,] = c(T,F,T,F,T,T)
adulte.strat.struct[4,] = c(T,F,T,T,T,T)
adulte.strat.struct[5,] = c(F,T,F,F,T,T)
adulte.strat.struct[6,] = c(F,T,F,T,T,T)
adulte.strat.struct[7,] = c(F,T,T,F,T,T)
adulte.strat.struct[8,] = c(F,T,T,T,T,T)
concensus.strat.struct = adulte.strat.struct

enfant.strat.5mat.struct = matrix(F,4,5)
enfant.strat.5mat.struct[2,] = c(T,F,rep(T,3))
ado.strat.5mat.struct = matrix(F,4,5)
ado.strat.5mat.struct[2,] = c(T,F,rep(T,3))
ado.strat.5mat.struct[4,] = c(rep(T,5))
adulte.strat.5mat.struct = matrix(F,4,5)
adulte.strat.5mat.struct[1,] = c(T,F,F,T,T)
adulte.strat.5mat.struct[2,] = c(T,F,T,T,T)
adulte.strat.5mat.struct[3,] = c(T,T,F,T,T)
adulte.strat.5mat.struct[4,] = c(T,T,T,T,T)
concensus.strat.5mat.struct = adulte.strat.5mat.struct


# Note: le dernier élément de imat ne sert pas. Les contraintes sur les matrices applicables au dernier groupe
# doivent être imposées dans idern

# Matrices à 10 tranches d'âge
# Sans limite supérieure
#imat = list(enfant.struct,enfant.struct,ado.struct,adulte.struct,adulte.struct,adulte.struct,adulte.struct,adulte.struct,adulte.struct,adulte.struct,concensus.struct)
# Avec limite supérieure pour éliminer tranches d'âges avancées sans effectifs
# imat = list(enfant.struct,enfant.struct,ado.struct,adulte.struct,adulte.struct,adulte.struct,adulte.struct,adulte.struct,adulte.struct,retraite.struct,concensus.struct)

# Matrices à 8 tranches d'âge
# Avec limite supérieure pour éliminer tranches d'âges avancées sans effectifs
imat.8cat = list(enfant.struct,enfant.struct,ado.struct,adulte.struct,adulte.struct,adulte.struct,adulte.struct,retraite.struct,concensus.struct)

imat.5mat.8cat = list(enfant.5mat.struct,enfant.5mat.struct,adulte.5mat.struct,adulte.5mat.struct,adulte.5mat.struct,adulte.5mat.struct,preretraite.5mat.struct,retraite.5mat.struct,concensus.5mat.struct)

imat.strat.8cat = list(petits.strat.struct,enfant.strat.struct,ado.strat.struct,adulte.strat.struct,adulte.strat.struct,adulte.strat.struct,adulte.strat.struct,retraite.struct,concensus.strat.struct)

imat.strat.5mat.8cat = list(enfant.strat.5mat.struct,enfant.strat.5mat.struct,ado.strat.5mat.struct,adulte.strat.5mat.struct,adulte.strat.5mat.struct,adulte.strat.5mat.struct,adulte.strat.5mat.struct,retraite.struct,concensus.strat.5mat.struct)

# Une seule matrice
bidon = matrix(TRUE,1,1)
imat1.8cat = list(bidon,bidon,bidon,bidon,bidon,bidon,bidon,bidon,bidon)

