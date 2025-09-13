setwd("C:\\Users\\valen\\Documents\\BRESSON Ecodev\\Data Rapport Pauvrete")
#packages à installer pour charger les libraries
#install.packages("ggplot2")
#install.packages("tidyverse")
#install.packages('naijR')
#install.packages("gridExtra")

## téléchargement de librairies :

library(foreign) # lire fichier stata
library(ggplot2) # graphiques
library(tidyverse) #pour manipulation avec les graphiques
library(naijR) # pour modélisations carte du nigéria
library(dplyr) #manipulation des données (pipe sequences)
library(gridExtra) #creation de pdf pour les tableaux (dataframes)

## Importation de données
  # données aggrégées
d101 <- read.dta("10cons_agg_wave1_visit1.dta")  # visit 1 = post planting = apres semis = avant récolte # 2 = post harvest = après récolte
d102 <- read.dta("10cons_agg_wave1_visit2.dta")
d121 <- read.dta("12cons_agg_wave2_visit1.dta")
d122 <- read.dta("12cons_agg_wave2_visit2.dta")
d151 <- read.dta("15cons_agg_wave3_visit1.dta")
d152 <- read.dta("15cons_agg_wave3_visit2.dta")
d18 <- read.dta("18totcons_final.dta")
dr18<-read.dta("18sect6_harvestw4.dta") #données pour les transferts monétaires uniquement disponible en 2018 pour notre niveau de détail
dh101 <-read.dta("10sect1_plantingw1.dta")
dh102<-read.dta("10sect1_harvestw1.dta")
dh182<-read.dta("18sect1_harvestw4.dta")
#import données éducation
de10<-read.dta("10sect2_plantingw1.dta")
de12<-read.dta("12sect2_plantingw2.dta")
de15<-read.dta("15sect2_harvestw3.dta")
de18<-read.dta("18sect2_harvestw4.dta")
ds102<-read.dta("10sect3a_harvestw1.dta") #s3aq11 labor activity

#aligner la codification de d18 avec les autres dataframes
names(d18)[c(3,7,62)] <- c("rururb","hhweight","totcons") #remplace les noms de colonnes de 2018 comme ceux des années précédentes
d18$zone <- str_replace_all(d18$zone, c("4. South East" = "4. SOUTH EAST","2. North East" = "2. NORTH EAST","5. South South" = "5. SOUTH SOUTH","1. North Central" = "1. NORTH CENTRAL","6. South West" = "6. SOUTH WEST","3. North West" = "3. NORTH WEST"))
d18$rururb <- str_replace_all(d18$rururb, c("2. Rural"= "Rural","1. Urban" = "Urban"))

#consommations alimentaires
d102$foodcons <- rowSums(d102[,34:76]) #additionne les valeurs des colonnes 34 à 76 -> dépense totale alimentaire annuelle
d101$foodcons <- rowSums(d101[,34:76])
d121$foodcons <- rowSums(d121[,34:76])
d122$foodcons <- rowSums(d122[,34:76])
d151$foodcons <- rowSums(d151[,34:76])
d152$foodcons <- rowSums(d152[,34:76])
d18$foodcons <- rowSums(d18[,8:46])

d102$foodcons<- d102$foodcons*2.401
d101$foodcons<- d101$foodcons*2.401
d122$foodcons <- d122$foodcons*1.93
d121$foodcons <- d121$foodcons*1.93
d151$foodcons <- d151$foodcons*1.51
d152$foodcons <- d152$foodcons*1.51
  ## conso monetaire ajustée
d102$conspond <- d102$totcons*2.401  #consommation ajusté au niveau prix 2018
d101$conspond <- d101$totcons*2.401   #IPC 2018/IPC 2010 = 240.1/100 = 2.401
d122$conspond <- d122$totcons*1.93    # IPC 2018/IPC 2012 = 240.1/124.4 =1.93
d121$conspond <- d121$totcons*1.93      #etc  .
d152$conspond <- d152$totcons*1.51
d151$conspond <- d151$totcons*1.51
d18$conspond <- d18$totcons
##lignes de pauvreté
  #ligne de pauvreté monétaire annuelle nationale Nigéria
z <- 137430

  ## ligne de pauvreté alimentaire régionale
  #assignation d'une ligne de pauvreté alimentaire par zone (différence de niveau des prix alimentaires) voir calcul en annexe ...'
za <- data.frame(zone=c("4. SOUTH EAST","2. NORTH EAST","5. SOUTH SOUTH","1. NORTH CENTRAL","6. SOUTH WEST","3. NORTH WEST"),za=c(87605.59,56818.16,117410.69,69480.44,78466.84,62880.88))

noms <- gsub("_", " ", colnames(za)) #enleve les "_" dans les noms de colonne pour faire joli
colnames(za) <- noms
pdf("C:\\Users\\valen\\Documents\\BRESSON Ecodev\\za.pdf", width = 14, height = 6)
grid.table(za,rows = NULL)
#dev.off()

d102 <- merge(d102, za, by.x = "zone", by.y = "zone", all.x = TRUE) #assignation d'une ligne de pauvreté alimentaire par zone '
d101 <- merge(d101, za, by.x = "zone", by.y = "zone", all.x = TRUE)
d121 <- merge(d121, za, by.x = "zone", by.y = "zone", all.x = TRUE)
d122 <- merge(d122, za, by.x = "zone", by.y = "zone", all.x = TRUE)
d151 <- merge(d151, za, by.x = "zone", by.y = "zone", all.x = TRUE)
d152 <- merge(d152, za, by.x = "zone", by.y = "zone", all.x = TRUE)
d18 <- merge(d18, za, by.x = "zone", by.y = "zone", all.x = TRUE)

##découpage des données éducation

ed10<-merge(de10,dh102, by=c("hhid","indiv"), all=TRUE)%>% #de10 contient données éducation par individu et dh10 contient la position de l'individu dans le ménage (chef de ménage) et religion '
filter(s1q3 == "head")%>% #s1q3 = position de l'individu dans le ménage
filter(complete.cases(s2q8))%>%
merge(.,d102, by="hhid",all.x=TRUE)%>%
filter(complete.cases(conspond))%>%
filter(s2q8 != "other (specify)")%>% #j'exclue ici le "diplome" other(specify) il s'agit généralement d'une éducation coranique'
mutate(nived=as.factor(ifelse(s2q8 == "none", "none", ifelse(s2q8 == "fslc", "prim", ifelse(s2q8 %in% c("jss", "mslc", "sss 'o level'","a level"),"sec","ter")))))

ed10<-split(ed10,ed10$nived)

## données religion du chef du ménage

r10<-dh101%>%   #nous prenons dh101 car les données sur la religion du chef du ménages sont plus fournies acvec un échantillon significatif de "traditionnel"
filter(s1q3 == "head")%>% #s1q3 = position de l'individu dans le ménage
filter(s1q12 != "others") %>%#j'exclue ici la religion "autre" qui ne représente que 7 indiv
droplevels(unique(.$s1q12))%>%  #pour cela je dois également retirer autre de "levels" qui représente les valeurs que ce vecteur peut prendre mais qui s'affiche dans mon tableau si je ne filtre pas ici'
filter(complete.cases(s1q12))%>% #filtrer NA sur la religion de l'individu'
merge(.,d102, by="hhid",all.x=TRUE)%>%
filter(complete.cases(conspond)) #au cas où je filtre NA conspond

r10<-split(r10,r10$s1q12)


ng_zone <-c("4. SOUTH EAST","2. NORTH EAST","5. SOUTH SOUTH","1. NORTH CENTRAL", "6. SOUTH WEST", "3. NORTH WEST") #chaque zone geopolitique du Nigeria
zone_sep <- function(zo,d){
u<- c()
for (i in zo) {
u[[i]] <- subset(d, zone == i)
}
return(u)
}

zo2010_2 <- zone_sep(ng_zone,d102) #je ne connaissais pas l'existence de la fonction split, liste de df des données de 2010_2 par zone géopolitique (graphique 3)
zo2010_1 <- zone_sep(ng_zone,d101)
zo2012_2 <- zone_sep(ng_zone,d122)
zo2012_1 <- zone_sep(ng_zone,d121)
zo2015_2 <- zone_sep(ng_zone,d152)
zo2015_1 <- zone_sep(ng_zone,d151)
zo2018 <- zone_sep(ng_zone,d18)

  #séparation entre échantillon rural et urbain

#fonction split pour séparer une liste de df par les valeurs d'une colonne'
splitlist <- function(ldf,sep){  lapply(ldf, function(df) split(df, df[[sep]]))
}
urb10 <- subset(d102, rururb == "Urban")
urb12 <- subset(d122, rururb == "Urban")
urb15 <- subset(d152, rururb == "Urban")
urb18 <- subset(d18, rururb == "Urban")

rur10 <- subset(d102, rururb == "Rural")
rur12 <- subset(d122, rururb == "Rural")
rur15 <- subset(d152, rururb == "Rural")
rur18 <- subset(d18, rururb == "Rural")

## Calcul ***indices pauvreté monétaire FGTa***

FGTa <- function(y,z,wi,a) #y= conso annuelle, z = ligne de pauvreté , wi=poid individu dans la pop.,a=0,1,2 -> incidence, profondeur, sévérité'
{
nac <- na.omit(data.frame(y,wi)) #retirer les valeurs 'NA' pour les revenus eq. et les pondérations
y <- nac[,1]  #extraire le vecteur de revenu et de poids depuis les colonne du vecteur nac
wi <- nac[,2]

m <- round(sum(((z-y)/z)^a*(y<=z)*wi)/sum(wi),2)  #formule FGTa (cf p35 cours, formules detaillees en annexe)  = moyenne empirique pondérée de la pauvreté (Weighted mean)
return(m)
}

#fonction pour calculer FGTa alimentaire ou monétaire ("type") al=alimentaire, m=monétaire, ldf= liste de dataframes à traiter ,   pour tous les éléments d'une liste/vecteur à un ordre choisi a=0,1,2'

FGTasup <- function(ldf,type,a){
  if (type=="al") {
    u <- c()
    for (i in ldf) {
    u[[length(u) + 1]] <- FGTa(i$foodcons, i$za[1], i$hhweight * sqrt(i$hhsize), a)#calcule indice FGTa mais pour les dépenses alimentaires et la ligne de pauvreté alimentaire précédemment associée à chaque zone
    }
    }
  else if(type=="m") {
  u <- c()
  for (i in ldf) {
  u[[length(u) + 1]] <- FGTa(i$conspond, 137430, i$hhweight * sqrt(i$hhsize), a)#calcule indice FGTa mais pour les dépenses alimentaires et la ligne de pauvreté alimentaire précédemment associée à chaque zone
  }
  }

  return(u)
}


#fonction pour calculer FGTa alimentaire ou monétaire de tous les éléments d'une liste/vecteur à tous les ordre '  ldf= liste de dataframes à traiter
FGTasupsup <- function(ldf,type) {
if (type=="al") {
lapply(0:2, function(a) FGTasup(ldf,"al",a))}
else if(type=="m") {
lapply(0:2, function(a) FGTasup(ldf,"m",a))
}
}

# Fonction Intervalles de confiance : FGTa -> IC et ET compatible  à tous les ordres (Kakwani,1993)

ic<- function(y,z,wi,a) {
et<-round(sqrt((FGTa(y,z,wi,2*a)-FGTa(y,z,wi,a)^2)/length(y)),4)# calcul de l'écart type
cil<-c(round(FGTa(y,z,wi,a)-1.96*et,2))
ciu<-c(round(FGTa(y,z,wi,a)+1.96*et,2))
ci<-paste(cil,ciu,sep = "-")
return(list(et,ci))
}

icsup <- function(ldf,type,a){
  if (type=="al") {
    u <- c()
    for (i in ldf) {
    u[[length(u) + 1]] <- ic(i$foodcons, i$za[1], i$hhweight * sqrt(i$hhsize), a)#calcule indice FGTa mais pour les dépenses alimentaires et la ligne de pauvreté alimentaire précédemment associée à chaque zone
    }
    }
  else if(type=="m") {
  u <- c()
  for (i in ldf) {
  u[[length(u) + 1]] <- ic(i$conspond, 137430, i$hhweight * sqrt(i$hhsize), a)#calcule indice FGTa mais pour les dépenses alimentaires et la ligne de pauvreté alimentaire précédemment associée à chaque zone
  }
  }

  return(u)
}

icsupsup <- function(ldf,type) {
if (type=="al") {
lapply(0:2, function(a) icsup(ldf,"al",a))}
else if(type=="m") {
lapply(0:2, function(a) icsup(ldf,"m",a))
}
}


    #vecteur FGT national
FGTa_10_2<-sapply(0:2,function(a) FGTa(d102$conspond,z,d102$hhweight*sqrt(d102$hhsize),a))
FGTa_12_2<-sapply(0:2,function(a) FGTa(d122$conspond,z,d122$hhweight*sqrt(d122$hhsize),a))
FGTa_15_2<-sapply(0:2,function(a) FGTa(d152$conspond,z,d152$hhweight*sqrt(d152$hhsize),a))
FGTa_18<-sapply(0:2,function(a) FGTa(d18$conspond,z,d18$hhweight*sqrt(d18$hhsize),a))

    #vecteur FGT urbain
FGTa_urb_10_2<-sapply(0:2,function(a) FGTa(urb10$conspond,z,urb10$hhweight*sqrt(urb10$hhsize),a))
FGTa_urb_12_2<-sapply(0:2,function(a) FGTa(urb12$conspond,z,urb12$hhweight*sqrt(urb12$hhsize),a))
FGTa_urb_15_2<-sapply(0:2,function(a) FGTa(urb15$conspond,z,urb15$hhweight*sqrt(urb15$hhsize),a))
FGTa_urb_18<-sapply(0:2,function(a) FGTa(urb18$conspond,z,urb18$hhweight*sqrt(urb18$hhsize),a))

    #vecteur FGT rural
FGTa_rur_10_2<-sapply(0:2,function(a) FGTa(rur10$conspond,z,rur10$hhweight*sqrt(rur10$hhsize),a))
FGTa_rur_12_2<-sapply(0:2,function(a) FGTa(rur12$conspond,z,rur12$hhweight*sqrt(rur12$hhsize),a))
FGTa_rur_15_2<-sapply(0:2,function(a) FGTa(rur15$conspond,z,rur15$hhweight*sqrt(rur15$hhsize),a))
FGTa_rur_18<-sapply(0:2,function(a) FGTa(rur18$conspond,z,rur18$hhweight*sqrt(rur18$hhsize),a))

    # dominance stochastique
#zpalier <- sort(unique(c(d102$conspond, d122$conspond, d152$conspond, d18$conspond)))
#zpalier <- zpalier[zpalier <= 300000]  # sur mon ordinateur, cette méthode plus rigoureuse donne une différence négligeable de précision et est plus lente
zpalier <- seq(30000, 300000, by = 500) #pour gagner du temps sur le calcul

domz10 <- sapply(zpalier,function(zval) FGTa(d102$conspond,zval,d102$hhweight*sqrt(d102$hhsize),0))
domz12 <- sapply(zpalier,function(zval) FGTa(d122$conspond,zval,d122$hhweight*sqrt(d122$hhsize),0))
domz15 <- sapply(zpalier,function(zval) FGTa(d152$conspond,zval,d152$hhweight*sqrt(d152$hhsize),0))
domz18 <- sapply(zpalier,function(zval) FGTa(d18$conspond,zval,d18$hhweight*sqrt(d18$hhsize),0))


##Figures :

    #Figure 1 : pauvreté monétaire nationale (FGT)

FGTdata<- data.frame(annee=rep(c("2010","2012","2015","2018"),each=3),index=rep(c("FGT0","FGT1","FGT2"), times=4),incidence=c(FGTa_10_2,FGTa_12_2,FGTa_15_2,FGTa_18))
FGTgraph <- ggplot() + geom_col(data=FGTdata, aes(x=annee,y=incidence,fill=index),position="dodge") + scale_y_continuous(breaks = seq(0,0.65, by = 0.05)) + labs(x="Année", y="valeur de l'indice",fill="Indicateur")+ theme_minimal()+ theme(axis.text = element_text(color = "black", size = 12)) + theme(axis.text.x = element_text(margin = margin(t = 10)),axis.text.y = element_text(margin = margin(r = 10)),axis.title.x = element_text(margin = margin(t = 10)),axis.title.y = element_text(margin = margin(r = 10)))+ geom_segment(aes(x = 0, xend = 0, y = 0, yend = 0.7), color = "black", arrow = arrow(type = "closed",length=unit(0.3,"cm")))

 #ggplot créée le graphique et geom_col correspond aux barres du diagramme
dev.new() #ouvre une nouvelle fenêtre avant pour y afficher le graphique
FGTgraph



  #Graphique 2 : pauvreté par type de région  (FGT)

        # extraction des données graphique région urbain rural
FGTdata_r<- data.frame(annee_r=rep(c("2010 urbain","2010 rural","2012 urbain","2012 rural","2015 urbain","2015 rural","2018 urbain","2018 rural"),each=3),index_r=rep(c("FGT0","FGT1","FGT2"), times=8),incidence_r=c(FGTa_urb_10_2,FGTa_rur_10_2,FGTa_urb_12_2,FGTa_rur_12_2,FGTa_urb_15_2,FGTa_rur_15_2,FGTa_urb_18,FGTa_rur_18))
FGTgraph_r <- ggplot(data=FGTdata_r, aes(x=annee_r,y=incidence_r,fill=index_r)) + geom_col(width = 0.4,position="dodge")+theme_minimal()+labs(x="",y="", fill="Type d'indice FGT")+ geom_segment(aes(x = 0, xend = 0, y = 0, yend = 0.7), color = "black", arrow = arrow(type = "closed",length=unit(0.3,"cm")))+ theme(axis.text = element_text(color = "black", size = 16)) + scale_y_continuous(breaks = seq(0,0.7,by = 0.05))

dev.new()
FGTgraph_r

    #Graphique 3 : carte FGT0-2 par zone 2010-18

 #calcul FGTa pour totcons pour tous les éléments d'une liste'
FGTz<-FGTasup(split(d102,d102$zone),"m",0)
interv <- seq(0.1, 0.6, by = 0.1)
Incidence<-c(FGTz[[4]][1],FGTz[[2]][1],FGTz[[5]][1],FGTz[[4]][1],FGTz[[2]][1],FGTz[[5]][1],FGTz[[1]][1],FGTz[[2]][1],FGTz[[5]][1],FGTz[[5]][1],FGTz[[4]][1],FGTz[[5]][1],FGTz[[6]][1],FGTz[[4]][1],FGTz[[1]][1],FGTz[[2]][1],FGTz[[4]][1],FGTz[[3]][1],FGTz[[3]][1],FGTz[[3]][1],FGTz[[3]][1],FGTz[[3]][1],FGTz[[1]][1],FGTz[[1]][1],FGTz[[6]][1],FGTz[[1]][1],FGTz[[1]][1],FGTz[[6]][1],FGTz[[6]][1],FGTz[[6]][1],FGTz[[6]][1],FGTz[[1]][1],FGTz[[5]][1],FGTz[[3]][1],FGTz[[2]][1],FGTz[[2]][1],FGTz[[3]][1])
dev.new()
map_ng(region = states(), x =Incidence,categories = c("10-20%", "20-30%", "30-40%","40-50%","50-60%","t"), breaks = interv, col = 'YlOrRd', show.text = TRUE)

    #Graphique 4 : pauvreté par religion
rel10 <- FGTasup(r10,"m",0)
FGTrel<- data.frame(rel=c("chrétien","Islam","Tradionnel"),incid=c(rel10[[1]][1],rel10[[2]][1],rel10[[3]][1]))
FGTgraph_rel <- ggplot()+ geom_col(data=FGTrel, aes(x=rel,y=incid),fill="cadetblue2",width=0.4) +labs(x="Religion du chef du ménage", y="Incidence de pauvreté des ménages (%)") +
 theme_minimal()+ theme(axis.text = element_text(color = "black", size = 12)) + scale_y_continuous(breaks = seq(0,0.5,by = 0.05))+
  theme(axis.text.x = element_text(margin = margin(t = 10)),axis.text.y = element_text(margin = margin(r = 10)),axis.title.x = element_text(margin = margin(t = 10)),axis.title.y = element_text(margin = margin(r = 10)))+
   geom_segment(aes(x = 0, xend = 0, y = 0, yend = 0.5), color = "black", arrow = arrow(type = "closed",length=unit(0.3,"cm")))
dev.new()
FGTgraph_rel

    #Graphique 5: pauvreté par niveau d'éducation du chef de ménage
educ10 <- FGTasup(ed10,"m",0)
FGTed<- data.frame(qual=c("sans diplome","primaire","secondaire","supérieur"),incid=c(educ10[[1]][1],educ10[[2]][1],educ10[[3]][1],educ10[[4]][1]))
FGTgraph_ed <- ggplot()+ geom_col(data=FGTed, aes(x=qual,y=incid),fill="darkolivegreen2",width=0.4) +labs(x="Niveau d'éducation obtenu (chef du ménage)", y="Incidence de pauvreté des ménages (%)") + theme_minimal()+ theme(axis.text = element_text(color = "black", size = 12)) + scale_y_continuous(breaks = seq(0,0.5,by = 0.05))+ theme(axis.text.x = element_text(margin = margin(t = 10)),axis.text.y = element_text(margin = margin(r = 10)),axis.title.x = element_text(margin = margin(t = 10)),axis.title.y = element_text(margin = margin(r = 10)))+ geom_segment(aes(x = 0, xend = 0, y = 0, yend = 0.5), color = "black", arrow = arrow(type = "closed",length=unit(0.3,"cm")))

dev.new()
FGTgraph_ed

    #Graphique 6: Dominance stochastique FGT0

domzgdat<- data.frame(ligne_pauvrete= zpalier,incidence=domz10,domz12,domz15,domz18)
domgraph <- ggplot() + geom_line(data=domzgdat, aes(x=ligne_pauvrete,y=incidence,color="a")) + geom_line(data=domzgdat, aes(x=ligne_pauvrete,y=domz12,color="b"))+
 geom_line(data=domzgdat, aes(x=ligne_pauvrete,y=domz15,color="c"))+ geom_line(data=domzgdat, aes(x=ligne_pauvrete,y=domz18,color="d"))+geom_vline(xintercept = 137430, linetype = "dotted", color = "red")+scale_color_manual(name="année",labels=c("2010","2012","2015","2018"),values=c("blue","green","red","orange"))+labs(x="Seuil de pauvreté (en Nairas)", y="Incidence de la pauvreté", fill="Année") + theme_minimal() + scale_x_continuous(breaks = seq(30000,300000, by = 50000))+ theme(axis.text = element_text(color = "black", size = 12))+
  geom_segment(aes(x = 0, xend = 300000, y = 0, yend = 0), color = "black", arrow = arrow(type = "closed",length=unit(0.3,"cm"))) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 1), color = "black", arrow = arrow(type = "closed",length=unit(0.3,"cm")))

dev.new()
domgraph

  #Figure 7 : Tableau(x) de pauvreté alimentaire avant et après récolte par zone
L102 <- FGTasupsup(zo2010_2,"al") #pourrait être remplacé par un split mais par manque de temps je ne peux pas
L101 <- FGTasupsup(zo2010_1,"al")
L122 <-FGTasupsup(zo2012_2,"al")
L121 <- FGTasupsup(zo2012_1,"al")
L152 <-FGTasupsup(zo2015_2,"al")
L151 <- FGTasupsup(zo2015_1,"al")
L18 <-FGTasupsup(zo2018,"al")

l1<-c("FGT0 Sud Est","FGT0 Nord Est","FGT0 Sud Sud","FGT0 Nord Central", "FGT0 Sud-Ouest", "FGT0 Nord-Ouest","FGT1 Sud Est","FGT1 Nord Est","FGT1 Sud Sud","FGT1 Nord Central", "FGT1 Sud-Ouest", "FGT1 Nord-Ouest","FGT2 Sud Est","FGT2 Nord Est","FGT2 Sud Sud","FGT2 Nord Central", "FGT2 Sud-Ouest", "FGT2 Nord-Ouest")
F7<-data.frame(Indicateur_et_Zone=l1,Avant_recolte_2010=unlist(L101),Apres_recolte_2010=unlist(L102),Avant_recolte_2012=unlist(L121),Apres_recolte_2012=unlist(L122),Avant_recolte_2015=unlist(L151),Apres_recolte_2015=unlist(L152),Annee_2018=unlist(L18))
F7moy<-data.frame(Saison_Annee=colnames(F7[2:8]),FGT0=round(colMeans(F7[1:6,2:8]),2),FGT1=round(colMeans(F7[7:12,2:8]),2),FGT2=round(colMeans(F7[13:18,2:8]),2))
F7zm <- data.frame(Indicateur_et_Zone=l1,Moyenne_region=round(rowMeans(F7[,-1]),2))
noms <- gsub("_", " ", colnames(F7)) #enleve les "_" dans les noms de colonne pour faire joli
colnames(F7) <- noms
pdf("C:\\Users\\valen\\Documents\\BRESSON Ecodev\\Figure7.pdf", width = 14, height = 6)
grid.table(F7,rows = NULL)
dev.off()
pdf("C:\\Users\\valen\\Documents\\BRESSON Ecodev\\Figure7moy.pdf", width = 8, height = 4)
grid.table(F7moy,rows = NULL)
dev.off()
pdf("C:\\Users\\valen\\Documents\\BRESSON Ecodev\\F7zm.pdf", width = 4, height = 6)
grid.table(F7zm,rows = NULL)
dev.off()

#Graphique 8 : Partie 2 : taille moyenne des ménages ruraux vs urbains

#distinction ménages agricoles/non-agricole
ds102 <- ds102[c("hhid", "s3aq11")] %>% #dans le df ds102 on garde le hhid de chaque membre et s3aq11 contenant le secteur des membres du ménage. le pipe %>% = "then" (package dplyr) effectue des modifications par étapes à partir de la variable précédente'
filter(complete.cases(s3aq11)) %>% #je retire les membres du ménages ayant un secteur d'activité "NA"
group_by(hhid) %>% #ds102 est groupé en fonction de l'identifiant du ménage, à partir de maintenant les opérations se feront sur les ménages et non sur les membres du ménage eux-meme.
mutate(hhsector = ifelse("agriculture" %in% s3aq11, "agricole","non-agricole")) %>% #créé une colonne hhsector, où pour chaque groupe(chaque menage), le code inspecte chaque ligne et "si" s3aq11 = "agriculture" pour un membre il assigne " agricultural" (pour le ménage) dans hhsector sinon il y assigne "non-agricultural"
ungroup() %>% #maintenant on ne raisonne plus en groupe mais en membre du ménage
filter(!duplicated(hhid))  # on ne garde qu'un membre du ménage pour merge ds102 et d102 l'information étant contenue dans hhsector étant par ménage pas de problème

ds102<-merge(ds102,d102, by = "hhid", all.y = TRUE)

#analyse taille du ménage ruraux / urbains
hh10 <-split(ds102,ds102$rururb)
hhu10<-hh10[["Urban"]]
hhr10<-hh10[["Rural"]]
hhs10 <- data.frame(type_de_menage = c("Rural 2010", "Urbain 2010"),taille_moyenne_menage = c(round(mean(hhr10$hhsize),2),round(mean(hhu10$hhsize),2)))
hhsgraph <- ggplot(data = hhs10, aes(x = type_de_menage, y = taille_moyenne_menage)) + geom_col(fill = "brown1", width = 0.2) + geom_text(aes(label = taille_moyenne_menage), vjust = -0.5, position = position_dodge(width = 0.2)) +
 scale_y_continuous(breaks = seq(0, 8, by = 1))+ labs(x="Type de ménage",y="Nombre d'individus moyen du ménage")+ theme(plot.title = element_text(hjust = 0.5)) +theme_minimal()+
 geom_segment(aes(x = 0, xend = 0, y = 0, yend = 6), color = "black", arrow = arrow(type = "closed",length=unit(0.3,"cm")))+ theme(axis.text = element_text(color = "black", size = 12)) +
  theme(axis.text.x = element_text(margin = margin(t = 10)),axis.text.y = element_text(margin = margin(r = 10)),axis.title.x = element_text(margin = margin(t = 10)),axis.title.y = element_text(margin = margin(r = 10)))

dev.new()
hhsgraph

#Graphique 9 : Partie 2 : taille moyenne des ménages ruraux agricole / non-agricole

#analyse taille du ménage agricole/non-agricole parmis les ruraux
mr <-split(hhr10,hhr10$hhsector)
mra <- mr[["agricole"]]
mrna <- mr[["non-agricole"]]
mr10 <- data.frame(type_de_menage = c("ménage agricole", "ménage non-agricole"),taille_moyenne_menage = c(round(mean(mra$hhsize),2),round(mean(mrna$hhsize),2)))
mrgraph <- ggplot(data = mr10, aes(x = type_de_menage, y = taille_moyenne_menage)) + geom_col(fill = "darkseagreen3", width = 0.2) + geom_text(aes(label = taille_moyenne_menage), vjust = -0.5, position = position_dodge(width = 0.2)) +
 labs(x="Type de ménage",y="Nombre d'individus moyen du ménage")+ theme(plot.title = element_text(hjust = 0.5)) +
 theme_minimal()+ geom_segment(aes(x = 0, xend = 0, y = 0, yend = 6), color = "black", arrow = arrow(type = "closed",length=unit(0.3,"cm")))+ theme(axis.text = element_text(color = "black", size = 12))+
  theme(axis.text.x = element_text(margin = margin(t = 10)),axis.text.y = element_text(margin = margin(r = 10)),axis.title.x = element_text(margin = margin(t = 10)),axis.title.y = element_text(margin = margin(r = 10)))

dev.new()
mrgraph

chi <- dh102[c("hhid","s1q3")] %>%
group_by(hhid) %>%
mutate(nombre_enfant = sum(s1q3 %in% c("own child", "adopted child"))) %>%
ungroup() %>%
filter(!duplicated(hhid))

hsc <- merge(chi, d102, by = "hhid", all.x=TRUE)
inter <- function() {
tra <- c(seq(0, 80, by = 10), seq(80, 120, by = 20), 463)
tra <- unique(tra)  # Ensure unique breaks
return(tra)
}

hsc <- hsc %>%
arrange(conspond) %>%
mutate(conspond = conspond / 10000) %>%
filter(complete.cases(conspond)) %>%
mutate(revenu = cut(conspond, breaks = inter())) %>%
group_by(revenu) %>%
summarise(nombre_enfant_moyen = mean(nombre_enfant)) %>%
ungroup()

#Graphique 10 : Partie 2 : nombre d'enfants moyen par niveau de revenu'

hscgraph <- ggplot(data = hsc, aes(x = revenu, y = nombre_enfant_moyen)) + geom_bar(stat = "identity",fill = "darkseagreen3", width = 0.7,position="dodge")+ labs(x="Niveau de consommation par tête annuel(en dizaines de milliers de Nairas)", y="Nombre d'enfants moyen")+
 theme_minimal()+ geom_segment(aes(x = 0, xend = 0, y = 0, yend = 5.2), color = "black", arrow = arrow(type = "closed",length=unit(0.3,"cm")))+ theme(axis.text = element_text(color = "black", size = 12))+
  theme(axis.text.x = element_text(margin = margin(t = 10)),axis.text.y = element_text(margin = margin(r = 10)),axis.title.x = element_text(margin = margin(t = 10)),axis.title.y = element_text(margin = margin(r = 10)))

dev.new()
hscgraph

# Graphique 11 : Partie 2 : composition des ménages receveurs de transferts
dr18 <- dr18[c("hhid","indiv","s6q1__1","s6q1__2","s6q1__3","s6q1__4")] %>%
group_by(hhid) %>%
mutate(tr = ifelse(any(c(s6q1__1, s6q1__2, s6q1__3, s6q1__4) != 0), 1, 0))%>%
ungroup()%>%
filter(!duplicated(hhid))

dh18<- dh182[c("hhid","indiv","s1q2","s1q3","s1q4","s1q7")] %>%
filter(complete.cases(s1q3))%>%
group_by(hhid)%>%
summarize(neld = sum(s1q4 > 65),chi = sum(s1q3 %in% c("3. OWN CHILD", "5. ADOPTED CHILD")),fd=sum(s1q2 == "2. FEMALE" & s1q7 == "6. WIDOWED"),nnb = sum(s1q4 < 2)) %>% #cette fonction est faite pour résumer des statistiques par groupe, elle m'a permi de compter le nombre d'enfant rapidement
ungroup()%>%
mutate(fd = ifelse(is.na(fd),0,fd))%>%
filter(!duplicated(hhid))

hhtr<- merge(dr18,dh18,by = "hhid", all.y=TRUE)%>%
filter(complete.cases(.)) #la fonction group ne fonctionnera pas si je ne filtre pas
hhtr <- hhtr %>%  #ici si je retire cette ligne le code ne fonctionne plus, d'où cette étape "redondante" '
group_by(tr) %>%
summarize(meld = mean(neld), mchi = mean(chi), mfd = mean(fd), mnb = mean(nnb))


hhtrdf<-data.frame(x=rep(c("nombre de personnes agées","nombre d'enfants","nombre de femmes divorcées","nombre de nouveaux nés"),each=2),
data=round(c(hhtr[[1, "meld"]], hhtr[[2, "meld"]], hhtr[[1, "mchi"]], hhtr[[2, "mchi"]],hhtr[[1, "mfd"]], hhtr[[2, "mfd"]], hhtr[[1, "mnb"]], hhtr[[2, "mnb"]]),2),
tr=c("Ménages ne recevants pas de transferts","Ménages recevants des transferts","Ménages ne recevants pas de transferts","Ménages recevants des transferts","Ménages ne recevants pas de transferts","Ménages recevants des transferts","Ménages ne recevants pas de transferts","Ménages recevants des transferts"))

hhtrgraph <- FGTgraph <- ggplot() + geom_col(data=hhtrdf, aes(x=x,y=data,fill=tr),position="dodge",width = 0.5) +
labs(x="",y="Nombre moyen d'individus par ménage",fill="Réception de transferts")+
theme(plot.title = element_text(hjust = 0.5))+ scale_y_continuous(breaks = seq(0,3.4, by = 0.5))+ theme_minimal()+ geom_segment(aes(x = 0, xend = 0, y = 0, yend = 3.4), color = "black", arrow = arrow(type = "closed",length=unit(0.3,"cm")))+ theme(axis.text = element_text(color = "black", size = 14))+
  theme(axis.text.x = element_text(margin = margin(t = 10)),axis.text.y = element_text(margin = margin(r = 10)),axis.title.x = element_text(margin = margin(t = 10)),axis.title.y = element_text(margin = margin(r = 10)))

dev.new()
hhtrgraph


#ECART-TYPES et Intervalles de confiances
##ecart-types généraux
F1etci<-data.frame(annee=rep(c("2010 apres semis","2010 apres recolte","2012 apres semis","2012 apres recolte","2015 apres semis","2015 apres recolte","2018"),each=2,times=3),index=rep(c("FGT0","FGT1","FGT2"),each=14),index=rep(c("écart-type","intervalle de confiance"),times=21),resultat=unlist(icsupsup(list(d101,d102,d121,d122,d151,d152,d18),"m")))
pdf("C:\\Users\\valen\\Documents\\BRESSON Ecodev\\ET&CIGeneral_monétaire.pdf", width = 6, height = 14)
grid.table(F1etci,rows = NULL)
dev.off()
F1etci<-data.frame(annee=rep(c("2010 apres semis","2010 apres recolte","2012 apres semis","2012 apres recolte","2015 apres semis","2015 apres recolte","2018"),each=2,times=3),index=rep(c("FGT0","FGT1","FGT2"),each=14),index=rep(c("écart-type","intervalle de confiance"),times=21),resultat=unlist(icsupsup(list(d101,d102,d121,d122,d151,d152,d18),"al")))
pdf("C:\\Users\\valen\\Documents\\BRESSON Ecodev\\ET&CIGeneral_alimentaire.pdf", width = 6, height = 14)
grid.table(F1etci,rows = NULL)
dev.off()

## écarts types par type de régions

F2etci<-data.frame(annee=rep(c("2010 apres recolte","2012 après récolte","2015 après récolte","2018 après récolte"),each=4,times=3),region=rep(c("rural","urbain"),each=2,times=12),index=rep(c("FGT0","FGT1","FGT2"),each=16),index=rep(c("écart-type","intervalle de confiance"),times=24),resultat=unlist(icsupsup(c(split(d102,d102$rururb),split(d122,d122$rururb),split(d152,d152$rururb),split(d18,d18$rururb)),"m")))
pdf("C:\\Users\\valen\\Documents\\BRESSON Ecodev\\ET&CIrururb.pdf", width = 6, height = 14)
grid.table(F2etci,rows = NULL)
dev.off()

##écarts-types par zones :
etcizone_m<-data.frame(annee=rep(c("2010 apres recolte","2012 après récolte","2015 après récolte","2018 après récolte"),each=12),zone=rep(c("1. NORTH CENTRAL","2. NORTH EAST","3. NORTH WEST","4. SOUTH EAST","5. SOUTH SOUTH","6. SOUTH WEST"),each=2,times=4),index=rep(c("FGT0"),times=48),index=rep(c("écart-type","intervalle de confiance"),times=24),result=unlist(icsup(c(split(d102,d102$zone),split(d122,d122$zone),split(d152,d152$zone),split(d18,d18$zone)),"m",0)))
pdf("C:\\Users\\valen\\Documents\\BRESSON Ecodev\\ET&CI_ZONE_monetaire.pdf", width = 7, height = 16)
grid.table(etcizone_m,rows = NULL)
dev.off()
etcizone_a<-data.frame(annee=rep(c("2010 apres recolte","2012 après récolte","2015 après récolte","2018 après récolte"),each=12),zone=rep(c("1. NORTH CENTRAL","2. NORTH EAST","3. NORTH WEST","4. SOUTH EAST","5. SOUTH SOUTH","6. SOUTH WEST"),each=2,times=4),index=rep(c("FGT0"),times=48),index=rep(c("écart-type","intervalle de confiance"),times=24),result=unlist(icsup(c(split(d102,d102$zone),split(d122,d122$zone),split(d152,d152$zone),split(d18,d18$zone)),"al",0)))
pdf("C:\\Users\\valen\\Documents\\BRESSON Ecodev\\ET&CI_ZONE_alimentaire.pdf", width = 7, height = 16)
grid.table(etcizone_a,rows = NULL)
dev.off()
#par religion
etcirel<-data.frame(annee=rep(c("2010 apres recolte"),times=6),religion=rep(c("Chrétien","Islam","traditionnel"),each=2),index=rep(c("FGT0"),times=6),index=rep(c("écart-type","intervalle de confiance"),times=3),result=unlist(icsup(r10,"m",0)))
pdf("C:\\Users\\valen\\Documents\\BRESSON Ecodev\\ET&CI_religion.pdf", width = 7, height = 5)
grid.table(etcirel,rows = NULL)
dev.off()
#par niveau d'education'
etci_ed<-data.frame(annee=rep(c("2010 apres recolte"),times=8),education=rep(c("sans diplome","primaire","secondaire","supérieur"),each=2),index=rep(c("FGT0"),times=8),index=rep(c("écart-type","intervalle de confiance"),times=4),result=unlist(icsup(ed10,"m",0)))
pdf("C:\\Users\\valen\\Documents\\BRESSON Ecodev\\ET&CI_education.pdf", width = 7, height = 5)
grid.table(etci_ed,rows = NULL)
dev.off()
