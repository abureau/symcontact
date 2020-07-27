# Création des objets décrivant la structure des données

# Matrices à 10 tranches d'âge
# (On les garde car on les utilise pour la construction des structures pour les matrices à 8 tranches d'âge)
idern.10cat = c(10,9,9,8,8,10)
idern.10cat.Can = c(10,10,10,10,10,10)

# Matrices à 8 tranches d'âge
nn=8
iprem.8 = c(4,1,3,1,1,1)
idern.8cat = idern.10cat - 2
idern.8cat.Can = idern.10cat.Can - 2
iniv.8cat = cumsum(c(0,(idern.8cat-iprem.8+1)[-length(iprem.8)])*nn)
iniv.8cat.Can = cumsum(c(0,(idern.8cat.Can-iprem.8+1)[-length(iprem.8)])*nn)
np.8cat = sum((idern.8cat-iprem.8+1)*nn)
np.8cat.Can = sum((idern.8cat.Can-iprem.8+1)*nn)
iprem.5mat = iprem.8[2:6]
idern.5mat.8cat = idern.8cat[c(1,3:6)]
idern.5mat.8cat.Can = idern.8cat.Can[c(1,3:6)]
iniv.5mat.8cat = cumsum(c(0,(idern.5mat.8cat-iprem.5mat+1)[-length(iprem.5mat)])*nn)
iniv.5mat.8cat.Can = cumsum(c(0,(idern.5mat.8cat.Can-iprem.5mat+1)[-length(iprem.5mat)])*nn)
np.5mat.8cat = sum((idern.5mat.8cat-iprem.5mat+1)*nn)
np.5mat.8cat.Can = sum((idern.5mat.8cat.Can-iprem.5mat+1)*nn)

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
preretraite.strat.struct = matrix(F,8,6)
preretraite.strat.struct[1,] = c(T,F,F,F,F,T)
preretraite.strat.struct[2,] = c(T,F,F,T,F,T)
preretraite.strat.struct[3,] = c(T,F,T,F,F,T)
preretraite.strat.struct[4,] = c(T,F,T,T,F,T)
preretraite.strat.struct[5,] = c(F,T,F,F,F,T)
preretraite.strat.struct[6,] = c(F,T,F,T,F,T)
preretraite.strat.struct[7,] = c(F,T,T,F,F,T)
preretraite.strat.struct[8,] = c(F,T,T,T,F,T)
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

# Matrices à 8 tranches d'âge
# Avec limite supérieure pour éliminer tranches d'âges avancées sans effectifs
imat.8cat = list(enfant.struct,enfant.struct,ado.struct,adulte.struct,adulte.struct,adulte.struct,adulte.struct,retraite.struct,concensus.struct)

imat.5mat.8cat = list(enfant.5mat.struct,enfant.5mat.struct,adulte.5mat.struct,adulte.5mat.struct,adulte.5mat.struct,adulte.5mat.struct,preretraite.5mat.struct,retraite.5mat.struct,concensus.5mat.struct)
imat.5mat.8cat.Can = list(enfant.5mat.struct,enfant.5mat.struct,adulte.5mat.struct,adulte.5mat.struct,adulte.5mat.struct,adulte.5mat.struct,adulte.5mat.struct,retraite.5mat.struct,concensus.5mat.struct)


imat.strat.8cat = list(petits.strat.struct,enfant.strat.struct,ado.strat.struct,adulte.strat.struct,adulte.strat.struct,adulte.strat.struct,preretraite.strat.struct,retraite.struct,concensus.strat.struct)
imat.strat.8cat.Can = list(petits.strat.struct,enfant.strat.struct,ado.strat.struct,adulte.strat.struct,adulte.strat.struct,adulte.strat.struct,adulte.strat.struct,retraite.struct,concensus.strat.struct)


imat.strat.5mat.8cat = list(enfant.strat.5mat.struct,enfant.strat.5mat.struct,ado.strat.5mat.struct,adulte.strat.5mat.struct,adulte.strat.5mat.struct,adulte.strat.5mat.struct,adulte.strat.5mat.struct,retraite.struct,concensus.strat.5mat.struct)

# Une seule matrice
bidon = matrix(TRUE,1,1)
imat1.8cat = list(bidon,bidon,bidon,bidon,bidon,bidon,bidon,bidon,bidon)

## Analyse pour les 4 régions du Canada : ( matrices avec denominateurs populationnels)

### Construction des structures :

# Methode utilisée pour la construction :


#######       Analyse pour la région du Québec : ########


# On a 8 catégories d'âge et 13 lieux : (en ordre)

# maison : ménage sans 0-17 ans, ménage avec 0-17 ans
# travail : santé, vente&service, autres
# école : préscolaire,  primaire, secondaire,  post-secondaire, indéterminé
# transport, loisirs, autres

# Explication des notation :
# enfant : [0,11]
# ado : [12,17]
# jeune adulte : [18:25]
# adulte : [26:65]
# retraite : [66:75]
# retraite seniors : [76:100]

# Construction de l'objet imat.13mat.8cat.Qc :

# Methode utilisée pour la construction :

#vec=c("denom_trav_sante","denom_trav_ventserv","denom_trav_autre","denom_ecol_gard","denom_ecol_prim","denom_ecol_second","denom_ecol_postsec","denom_ecol_indeterm","denom_tcom","denom_lois","denom_autr")

#any(dat.Can[c(between(x = dat.Can$age,left = 76,right =100)),"menage_avec017"])

#for (element in vec){
#  print(c(any(dat.Can[c(between(x = dat.Can$age,left = 76,right = 100)),element]),element))
#}

nn=8

enfant.struct.Qc = matrix(c(F,T, F,F,F, T,T,F,F,F, T,T,T),1,13,byrow=T)

ado.struct.Qc = matrix(c(F,T, F,T,T, F,T,T,T,F, T,T,T),1,13,byrow=T)

jeune.adulte.struct.Qc = matrix(c(T,T, T,T,T, T,F,T,T,T, T,T,T),1,13,byrow=T)

adulte.struct.Qc = matrix(c(T,T,T,T,T,T,T,T,T,T,T,T,T),1,13,byrow=T)

retraite.struct.Qc = matrix(c(T,T, T,T,T, F,F,F,F,T, T,T,T),1,13,byrow=T)

retraite.seniors.struct.Qc = matrix(c(T,F, F,F,T, F,F,F,F,F, T,T,T),1,13,byrow=T)

concensus.struct.Qc = matrix(c(T,T,T,T,T,T,T,T,T,T,T,T,T),1,13,byrow=T)

imat.13mat.8cat.Qc = list(enfant.struct.Qc,enfant.struct.Qc,ado.struct.Qc,jeune.adulte.struct.Qc,adulte.struct.Qc,adulte.struct.Qc,retraite.struct.Qc,retraite.seniors.struct.Qc,concensus.struct.Qc)

# Construction de l'objet iprem.13mat.8cat et idern.13mat.8cat:

iprem.13mat.8cat.Qc = c(4,1,4,3,3,1,1,3,3,4,1,1,1)

idern.13mat.8cat.Qc = c(8,7,7,7,8,6,6,6,6,6,6,8,8)

ipremy.13mat.8cat.Qc = matrix(iprem.13mat.8cat.Qc,1,length(iprem.13mat.8cat.Qc))
iderny.13mat.8cat.Qc = matrix(idern.13mat.8cat.Qc,1,length(idern.13mat.8cat.Qc))

#Construction de l'objet iniv.13mat.8cat
iniv.13mat.8cat.Qc = cumsum(c(0,(idern.13mat.8cat.Qc-iprem.13mat.8cat.Qc+1)[-length(iprem.13mat.8cat.Qc)])*nn)

np.8cat.13mat.Qc = sum((idern.13mat.8cat.Qc-iprem.13mat.8cat.Qc+1)*nn)


# Construction des structures pour les matrices avec denominateurs populationnel totale symétrique:

## Matrice symétrique totale maison :

nn=8

enfant.struct.maison.Qc = matrix(c(F,T),1,2,byrow=T)

ado.struct.maison.Qc = matrix(c(F,T),1,2,byrow=T)

jeune.adulte.struct.maison.Qc = matrix(c(T,T),1,2,byrow=T)

adulte.struct.maison.Qc = matrix(c(T,T),1,2,byrow=T)

retraite.struct.maison.Qc = matrix(c(T,T),1,2,byrow=T)

retraite.seniors.struct.maison.Qc = matrix(c(T,F),1,2,byrow=T)

concensus.struct.maison.Qc = matrix(c(T,T),1,2,byrow=T)

imat.mat.maison.8cat.Qc = list(enfant.struct.maison.Qc,enfant.struct.maison.Qc,ado.struct.maison.Qc,jeune.adulte.struct.maison.Qc,adulte.struct.maison.Qc,adulte.struct.maison.Qc,retraite.struct.maison.Qc,retraite.seniors.struct.maison.Qc,concensus.struct.maison.Qc)


# Construction de l'objet iprem.mat.maison.8cat.Qc et idern.mat.maison.8cat.Qc:

iprem.mat.maison.8cat.Qc = c(4,1)

idern.mat.maison.8cat.Qc = c(8,7)

ipremy.mat.maison.8cat.Qc = matrix(iprem.mat.maison.8cat.Qc,1,length(iprem.mat.maison.8cat.Qc))
iderny.mat.maison.8cat.Qc = matrix(idern.mat.maison.8cat.Qc,1,length(idern.mat.maison.8cat.Qc))

#Construction de l'objet iniv.mat.maison.8cat.Qc

iniv.mat.maison.8cat.Qc = cumsum(c(0,(idern.mat.maison.8cat.Qc-iprem.mat.maison.8cat.Qc+1)[-length(iprem.mat.maison.8cat.Qc)])*nn)


## Matrice symétrique totale ecole : (matrice 6x6, on n'inclut pas retraité et retraité séniors)

nn=6

enfant.struct.ecole.Qc = matrix(c(T,T,F,F,F),1,5,byrow=T)

ado.struct.ecole.Qc = matrix(c(F,T,T,T,F),1,5,byrow=T)

jeune.adulte.struct.ecole.Qc = matrix(c(T,F,T,T,T),1,5,byrow=T)

adulte.struct.ecole.Qc = matrix(c(T,T,T,T,T),1,5,byrow=T)

retraite.struct.ecole.Qc = matrix(c(F,F,F,F,T),1,5,byrow=T)

retraite.seniors.struct.ecole.Qc = matrix(c(F,F,F,F,F),1,5,byrow=T)

concensus.struct.ecole.Qc = matrix(c(T,T,T,T,T),1,5,byrow=T)

imat.mat.ecole.Qc.6cat.Qc = list(enfant.struct.ecole.Qc,enfant.struct.ecole.Qc,ado.struct.ecole.Qc,jeune.adulte.struct.ecole.Qc,adulte.struct.ecole.Qc,adulte.struct.ecole.Qc,concensus.struct.ecole.Qc)

# Construction de l'objet iprem.mat.ecole.6cat.Qc et idern.mat.ecole.6cat.Qc:

iprem.mat.ecole.6cat.Qc = c(1,1,3,3,4)

idern.mat.ecole.6cat.Qc = c(6,6,6,6,6)

ipremy.mat.ecole.6cat.Qc = matrix(iprem.mat.ecole.6cat.Qc,1,length(iprem.mat.ecole.6cat.Qc))
iderny.mat.ecole.6cat.Qc = matrix(idern.mat.ecole.6cat.Qc,1,length(idern.mat.ecole.6cat.Qc))

#Construction de l'objet iniv.mat.ecole.6cat.Qc

iniv.mat.ecole.6cat.Qc = cumsum(c(0,(idern.mat.ecole.6cat.Qc-iprem.mat.ecole.6cat.Qc+1)[-length(iprem.mat.ecole.6cat.Qc)])*nn)



#######       Analyse pour la région Atlantic : ########


# On a 8 catégories d'âge et 13 lieux : (en ordre)

# maison : ménage sans 0-17 ans, ménage avec 0-17 ans
# travail : santé, vente&service, autres
# école : garderie,  primaire, secondaire,  post-secondaire, indéterminé
# transport, loisirs, autres

# Explication des notation :
# petit enfant : [0:5]
# enfant : [6,11]
# ado : [12,17]
# jeune adulte : [18:25]
# adulte : [26:65]
# retraite : [66:75]
# retraite seniors : [76:100]


nn=8

petit.enfant.struct.Atc = matrix(c(F,T, F,F,F, T,T,F,F,F, T,T,T),1,13,byrow=T)

enfant.struct.Atc = matrix(c(F,T, F,F,F, F,T,F,F,T, T,T,T),1,13,byrow=T)

ado.struct.Atc = matrix(c(F,T,F,T,T,F,F,T,F,T,T,T,T),1,13,byrow=T)

jeune.adulte.struct.Atc = matrix(c(T,T,T,T,T,T,T,T,T,F,T,T,T),1,13,byrow=T)

adulte.struct.Atc = matrix(c(T,T,T,T,T,T,T,T,T,T,T,T,T),1,13,byrow=T)

retraite.struct.Atc = matrix(c(T,T,T,T,T,T,F,F,T,T,T,T,T),1,13,byrow=T)

retraite.seniors.struct.Atc = matrix(c(T,F,T,F,T,F,F,F,F,T,T,T,T),1,13,byrow=T)

concensus.struct = matrix(c(T,T,T,T,T,T,T,T,T,T,T,T,T),1,13,byrow=T)

imat.13mat.8cat.Atc = list(petit.enfant.struct.Atc,enfant.struct.Atc,ado.struct.Atc,jeune.adulte.struct.Atc,adulte.struct.Atc,adulte.struct.Atc,retraite.struct.Atc,retraite.seniors.struct.Atc,concensus.struct)

# Construction de l'objet iprem.13mat.8cat et idern.13mat.8cat.Atc:

iprem.13mat.8cat.Atc = c(4,1,4,3,3,1,1,3,4,2,1,1,1)

idern.13mat.8cat.Atc = c(8,7,8,7,8,7,6,6,7,8,8,8,8)

ipremy.13mat.8cat.Atc = matrix(iprem.13mat.8cat.Atc,1,length(iprem.13mat.8cat.Atc))
iderny.13mat.8cat.Atc = matrix(idern.13mat.8cat.Atc,1,length(idern.13mat.8cat.Atc))

#Construction de l'objet iniv.13mat.8cat
iniv.13mat.8cat.Atc = cumsum(c(0,(idern.13mat.8cat.Atc-iprem.13mat.8cat.Atc+1)[-length(iprem.13mat.8cat.Atc)])*nn)

np.8cat.13mat.Atc = sum((idern.13mat.8cat.Atc-iprem.13mat.8cat.Atc+1)*nn)

#######       Analyse pour la région d'Ontario : ########

nn=8


# On a 8 catégories d'âge et 13 lieux : (en ordre)

# maison : ménage sans 0-17 ans, ménage avec 0-17 ans
# travail : santé, vente&service, autres
# école : garderie,  primaire, secondaire,  post-secondaire, indéterminé
# transport, loisirs, autres

# Explication des notation :
# petit enfant : [0:5]
# enfant : [6,11]
# ado : [12,17]
# jeune adulte : [18:25]
# adulte : [26:65]
# retraite : [66:75]
# retraite seniors : [76:100]


petit.enfant.struct.Ont = matrix(c(F,T,F,F,F,T,T,F,F,T,T,T,T),1,13,byrow=T)

enfant.struct.Ont = matrix(c(F,T,F,F,F,T,T,F,F,F,T,T,T),1,13,byrow=T)

ado.struct.Ont = matrix(c(F,T, F,T,T,T,F,T,T,T,T,T,T),1,13,byrow=T)

jeune.adulte.struct.Ont = matrix(c(T,T,T,T,T,T,T,T,T,F,T,T,T),1,13,byrow=T)

adulte.struct.Ont = matrix(c(T,T,T,T,T,T,T,T,T,T,T,T,T),1,13,byrow=T)

retraite.struct.Ont = matrix(c(T,T,T,F,T,F,T,F,T,T,T,T,T),1,13,byrow=T)

retraite.seniors.struct.Ont = matrix(c(T,T,F,T,T,F,T,T,F,T,T,T,T),1,13,byrow=T)

concensus.struct = matrix(c(T,T,T,T,T,T,T,T,T,T,T,T,T),1,13,byrow=T)

imat.13mat.8cat.Ont = list(petit.enfant.struct.Ont,enfant.struct.Ont,ado.struct.Ont,jeune.adulte.struct.Ont,adulte.struct.Ont,adulte.struct.Ont,retraite.struct.Ont,retraite.seniors.struct.Ont,concensus.struct)

# Construction de l'objet iprem.13mat.8cat.Ont et idern.13mat.8cat.Ont:

iprem.13mat.8cat.Ont = c(4,1,4,3,3,1,1,3,3,1,1,1,1)

idern.13mat.8cat.Ont = c(8,8,7,8,8,6,8,8,7,8,8,8,8)

ipremy.13mat.8cat.Ont = matrix(iprem.13mat.8cat.Ont,1,length(iprem.13mat.8cat.Ont))
iderny.13mat.8cat.Ont = matrix(idern.13mat.8cat.Ont,1,length(idern.13mat.8cat.Ont))

#Construction de l'objet iniv.13mat.8cat.Ont
iniv.13mat.8cat.Ont = cumsum(c(0,(idern.13mat.8cat.Ont-iprem.13mat.8cat.Ont+1)[-length(iprem.13mat.8cat.Ont)])*nn)

np.8cat.13mat.Ont = sum((idern.13mat.8cat.Ont-iprem.13mat.8cat.Ont+1)*nn)

#######       Analyse pour la région Ouest : ########


# Construction des structures :

# On a 8 catégories d'âge et 13 lieux : (en ordre)

# maison : ménage sans 0-17 ans, ménage avec 0-17 ans
# travail : santé, vente&service, autres
# école : garderie,  primaire, secondaire,  post-secondaire, indéterminé
# transport, loisirs, autres

# Explication des notation :
# petit enfant : [0:5]
# enfant : [6,11]
# ado : [12,17]
# jeune adulte : [18:25]
# adulte : [26:65]
# retraite : [66:75]
# retraite seniors : [76:100]

nn=8

petit.enfant.struct.Ost = matrix(c(F,T, F,F,F, T,F,F,F,T, T,T,T),1,13,byrow=T)

enfant.struct.Ost = matrix(c(F,T, F,F,F, T,T,F,F,F, T,T,T),1,13,byrow=T)

ado.struct.Ost = matrix(c(F,T, F,T,F, F,T,T,T,T, T,T,T),1,13,byrow=T)

jeune.adulte.struct.Ost = matrix(c(T,T, T,T,T, T,T,T,T,F, T,T,T),1,13,byrow=T)

adulte.struct.Ost = matrix(c(T,T,T,T,T,T,T,T,T,T,T,T,T),1,13,byrow=T)

retraite.struct.Ost = matrix(c(T,T, F,T,T, T,F,F,F,T, T,T,T),1,13,byrow=T)

retraite.seniors.struct.Ost = matrix(c(T,T, F,F,T, F,F,F,F,T, T,T,T),1,13,byrow=T)

concensus.struct = matrix(c(T,T,T,T,T,T,T,T,T,T,T,T,T),1,13,byrow=T)

imat.13mat.8cat.Ost = list(petit.enfant.struct.Ost,enfant.struct.Ost,ado.struct.Ost,jeune.adulte.struct.Ost,adulte.struct.Ost,adulte.struct.Ost,retraite.struct.Ost,retraite.seniors.struct.Ost,concensus.struct)

# Construction de l'objet iprem.13mat.8cat.Ost et idern.13mat.8cat.Ost:

iprem.13mat.8cat.Ost = c(4,1,4,3,4,1,2,3,3,1,1,1,1)

idern.13mat.8cat.Ost = c(8,8,6,7,8,7,6,6,6,8,8,8,8)

ipremy.13mat.8cat.Ost = matrix(iprem.13mat.8cat.Ost,1,length(iprem.13mat.8cat.Ost))
iderny.13mat.8cat.Ost = matrix(idern.13mat.8cat.Ost,1,length(idern.13mat.8cat.Ost))

#Construction de l'objet iniv.13mat.8cat.Ost
iniv.13mat.8cat.Ost = cumsum(c(0,(idern.13mat.8cat.Ost-iprem.13mat.8cat.Ost+1)[-length(iprem.13mat.8cat.Ost)])*nn)

np.8cat.13mat.Ost = sum((idern.13mat.8cat.Ost-iprem.13mat.8cat.Ost+1)*nn)


#######       Analyse pour tout le Canada : ########

# Construction des objets iprem et idern pour le Canada

nn=8

iprem.13mat.8cat.matrice.Can = matrix(data =c(iprem.13mat.8cat.Atc,iprem.13mat.8cat.Qc,iprem.13mat.8cat.Ont,iprem.13mat.8cat.Ost),nrow = 4,byrow = T )

idern.13mat.8cat.matrice.Can = matrix(data =c(idern.13mat.8cat.Atc,idern.13mat.8cat.Qc,idern.13mat.8cat.Ont,idern.13mat.8cat.Ost),nrow = 4,byrow = T )

iprem.13mat.8cat.Can = apply(X =iprem.13mat.8cat.matrice.Can,MARGIN = 2,FUN = min )

idern.13mat.8cat.Can = apply(X =idern.13mat.8cat.matrice.Can,MARGIN = 2,FUN = max )

iniv.13mat.8cat.Can = cumsum(c(0,(idern.13mat.8cat.Can-iprem.13mat.8cat.Can+1)[-length(iprem.13mat.8cat.Can)])*nn)

np.8cat.13mat.Can = sum((idern.13mat.8cat.Can-iprem.13mat.8cat.Can+1)*nn)


# Construction de l'objet imat.13.8cat.Can pour le Canada :


petit.enfant.struct.Can = rbind(petit.enfant.struct.Atc,enfant.struct.Qc,petit.enfant.struct.Ont,petit.enfant.struct.Ost)

enfant.struct.Can = rbind(enfant.struct.Atc,enfant.struct.Qc,enfant.struct.Ont,enfant.struct.Ost)

ado.struct.Can = rbind (ado.struct.Atc,ado.struct.Qc,ado.struct.Ont,ado.struct.Ost)

jeune.adulte.struct.Can = rbind (jeune.adulte.struct.Atc,jeune.adulte.struct.Qc,jeune.adulte.struct.Ont,jeune.adulte.struct.Ost)

adulte.struct.Can = rbind(adulte.struct.Atc,adulte.struct.Qc,adulte.struct.Ont,adulte.struct.Ost)

retraite.struct.Can = rbind (retraite.struct.Atc,retraite.struct.Qc, retraite.struct.Ont,retraite.struct.Ost)

retraite.seniors.struct.Can = rbind (retraite.seniors.struct.Atc, retraite.seniors.struct.Qc,retraite.seniors.struct.Ont,retraite.seniors.struct.Ost)

concensus.struct.Can = matrix(rep(T,13),nrow = 4,ncol = 13)

imat.13mat.8cat.Can = list(petit.enfant.struct.Can,enfant.struct.Can,ado.struct.Can,jeune.adulte.struct.Can,adulte.struct.Can,adulte.struct.Can,retraite.struct.Can,retraite.seniors.struct.Can,concensus.struct.Can)


# Construction des structures pour les matrices avec denominateurs populationnel totale symétrique pour le Canada

## Matrice symétrique totale maison :


# Construction des structures :

# On a 8 catégories d'âge et 2 lieux : (en ordre)

# maison : ménage sans 0-17 ans, ménage avec 0-17 ans

# Explication des notation :

# enfant : [0,17]

# adulte : [18:75]

# retraite seniors : [76:100]

nn=8


enfant.struct.maison.Can = matrix(rep(c(F,T),4),nrow = 4,ncol = 2,byrow = T)


adulte.struct.maison.Can = matrix(rep(T,8),nrow = 4,ncol = 2,byrow = T)


retraite.seniors.struct.maison.Can = matrix(data = c(rep(T,4),F,F,T,T),nrow = 4,ncol = 2)


concensus.struct.maison.Can = matrix(rep(T,8),nrow = 4,ncol = 2,byrow = T)


imat.mat.maison.8cat.Can = list(enfant.struct.maison.Can,enfant.struct.maison.Can,enfant.struct.maison.Can,adulte.struct.maison.Can,adulte.struct.maison.Can,adulte.struct.maison.Can,adulte.struct.maison.Can,retraite.seniors.struct.maison.Can,concensus.struct.maison.Can)


# Construction de l'objet iprem.mat.8cat.maison.Can et idern.mat.8cat.maison.Can:


iprem.mat.8cat.matrice.maison.Can = matrix(data =c(iprem.13mat.8cat.Atc,iprem.13mat.8cat.Qc,iprem.13mat.8cat.Ont,iprem.13mat.8cat.Ost),nrow = 4,byrow = T )[,c(1,2)]

idern.mat.8cat.matrice.maison.Can = matrix(data =c(idern.13mat.8cat.Atc,idern.13mat.8cat.Qc,idern.13mat.8cat.Ont,idern.13mat.8cat.Ost),nrow = 4,byrow = T )[,c(1,2)]

iprem.mat.8cat.maison.Can = apply(X =iprem.mat.8cat.matrice.maison.Can,MARGIN = 2,FUN = min )

idern.mat.8cat.maison.Can = apply(X =idern.mat.8cat.matrice.maison.Can,MARGIN = 2,FUN = max )


#Construction de l'objet iniv.mat.8cat.maison.Can:

iniv.mat.8cat.maison.Can = cumsum(c(0,(idern.mat.8cat.maison.Can-iprem.mat.8cat.maison.Can+1)[-length(iprem.mat.8cat.maison.Can)])*nn)

# Construction de l'objet np.8cat.mat.maison.Can :

np.8cat.mat.maison.Can = sum((idern.mat.8cat.maison.Can-iprem.mat.8cat.maison.Can+1)*nn)


## Matrice symétrique totale ecole :


# Construction des structures :

# On a 8 catégories d'âge et 5 lieux : (en ordre)

# garderie - primaire - secondaire - post secondaire - indeterminé


nn=8

petit.enfant.struct.ecole.Can = petit.enfant.struct.Can[,c(6:10)]

enfant.struct.ecole.Can = enfant.struct.Can[,c(6:10)]

ado.struct.ecole.Can = ado.struct.Can[,c(6:10)]

jeune.adulte.struct.ecole.Can = jeune.adulte.struct.Can[,c(6:10)]

adulte.struct.ecole.Can = adulte.struct.Can[,c(6:10)]

retraite.struct.ecole.Can = retraite.struct.Can[,c(6:10)]

retraite.seniors.struct.ecole.Can = retraite.seniors.struct.Can[,c(6:10)]

concensus.struct.ecole.Can = concensus.struct.Can[,c(6:10)]

imat.mat.ecole.8cat.Can = list(petit.enfant.struct.ecole.Can,enfant.struct.ecole.Can,ado.struct.ecole.Can,jeune.adulte.struct.ecole.Can,adulte.struct.ecole.Can,adulte.struct.ecole.Can,retraite.struct.ecole.Can,retraite.seniors.struct.ecole.Can,concensus.struct.ecole.Can)


# Construction de l'objet iprem.mat.8cat.ecole.Can et idern.mat.8cat.ecole.Can:


iprem.mat.8cat.matrice.ecole.Can = matrix(data =c(iprem.13mat.8cat.Atc,iprem.13mat.8cat.Qc,iprem.13mat.8cat.Ont,iprem.13mat.8cat.Ost),nrow = 4,byrow = T )[,c(6:10)]

idern.mat.8cat.matrice.ecole.Can = matrix(data =c(idern.13mat.8cat.Atc,idern.13mat.8cat.Qc,idern.13mat.8cat.Ont,idern.13mat.8cat.Ost),nrow = 4,byrow = T )[,c(6:10)]

iprem.mat.8cat.ecole.Can = apply(X =iprem.mat.8cat.matrice.ecole.Can,MARGIN = 2,FUN = min )

idern.mat.8cat.ecole.Can = apply(X =idern.mat.8cat.matrice.ecole.Can,MARGIN = 2,FUN = max )


#Construction de l'objet iniv.mat.8cat.ecole.Can:

iniv.mat.8cat.ecole.Can = cumsum(c(0,(idern.mat.8cat.ecole.Can-iprem.mat.8cat.ecole.Can+1)[-length(iprem.mat.8cat.ecole.Can)])*nn)

# Construction de l'objet np.8cat.mat.ecole.Can :

np.8cat.mat.ecole.Can = sum((idern.mat.8cat.ecole.Can-iprem.mat.8cat.ecole.Can+1)*nn)
