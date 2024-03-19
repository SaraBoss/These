
#Document pour récupérer les données nécessaires pour optimiser :

library(zoo)
# library(annmatrix)
library(readxl)
library(base)
library(ggplot2)
library(reshape2)
library(matrixStats)
library(dplyr)
library(tidyr)
library(ggpubr)
library(RColorBrewer)
library(wesanderson)
library(grid)
library(lubridate)
library(bbmle)
library(lme4)
library(deSolve)






##############################################FONCTIONS NECESSAIRES  ############################################################################################
#Mise en place de fonctions pour permettre d'analyser les données

## Fonctions utiles
#Fonctions pour calculer les moyennes (par date, distance, localisation, rang) : Mean_type


################Mean_type#####################################################
#Descriptif : fonction qui permet de calculer différents types de moyennes de hauteurs d'herbe sur les parcelles.
#La fonction Mean_type prend un tableau dataHeight en entrée et le type de moyenne à réaliser
#dataHeight est un dataframe avec les colonnes : Date, Distance, IRa1, IRa2, IRa3, Rb1, Rb2, Rb3, IRc1, IRc2, IRc3 (les 9 colonnes de mesures de hauteurs d'herbes)
#Type peut prendre les arguments suivants : 
#         -> "date" si on calcule la moyenne sur toute la parcelle par date (100-120 points par moyenne)
#         -> "distance" si on calcule la moyenne par date et à une distance donnée (9 points par moyenne)
#         -> "localisation" si on calcule la moyenne par date, à une distance et une localisation donnée (3 points par moyenne)
#         -> "rang" si on calcule la moyenne par date et rang (30-40 points par moyenne)


Mean_type<-function(dataHeight,type){
  #MOYENNE PAR DATE
  if (type=="date"){
    #moyenne et écart type dans la parcelle selon la date
    result<-dataHeight%>%
      pivot_longer(cols = c(paste0("IRa",1:3),paste0("Rb",1:3),paste0("IRc",1:3)),names_to="localisation",values_to="Height")%>%
      group_by(Date,.groups="keep") %>%     summarise(Mean_height=mean(Height,na.rm=TRUE),Sd_height=sd(Height,na.rm=T)/sqrt(length(Height)))
  }
  
  if (type=="smallplot"){
    #moyenne et écart type dans la parcelle selon date et smallplots
    result<-dataHeight %>%
      pivot_longer(cols = c(paste0("IRa",1:3),paste0("Rb",1:3),paste0("IRc",1:3)),names_to="localisation",values_to="Height")%>%
      group_by(Smallplot,Date) %>%   summarise(Mean_height=mean(Height,na.rm=TRUE),Sd_height=sd(Height,na.rm=T)/sqrt(length(Height)))}
  
  
  #MOYENNE PAR DATE ET DISTANCE (sans tenir compte de la localisation)
  if (type=="distance"){
    result<-dataHeight%>%
      rowwise() %>%
      summarise(Date=Date,Distance=Distance,Mean_height= mean(c(IRa1, IRa2,IRa3,Rb1,Rb2,Rb3,IRc1,IRc2,IRc3),na.rm=T),Sd_height=sd(c(IRa1, IRa2,IRa3,Rb1,Rb2,Rb3,IRc1,IRc2,IRc3),na.rm=T))}
  
  #moyenne par localisation, date et distance (sur 3 points)
  
  if (type=="localisation"){
    result<-dataHeight%>%
      pivot_longer(cols = c(contains("IRa"),contains("Rb"),contains("IRc")),names_to=c("Rang","Point"),names_pattern="([A-Za-z]+)(\\d+)",values_to="Height")%>%
      group_by(Date,Distance,Rang)%>%
      summarise(Mean_height=mean(Height,na.rm=TRUE),Sd_height=sd(Height,na.rm=T))}
  
  if (type=="rang"){
    result<-dataHeight %>%
      pivot_longer(cols = c(contains("IRa"),contains("Rb"),contains("IRc")),names_to=c("Rang","Point"),names_pattern="([A-Za-z]+)(\\d+)",values_to="Height") %>%
      group_by(Date,Rang) %>%
      summarise(Mean_height=mean(Height,na.rm=T),Sd_height=sd(Height,na.rm=T))}
  
  return(result)
}



################growth_vector#####################################################
#####Fonction qui calcule un taux de croissance : dH/dt
#####Entrée : m un dataframe issu de la fonction Mean_type qui comprend une colonne Date et une colonne Mean_height 
#####Sortie : renvoie le vecteur des taux de croissance

growth_vector<-function(m){
  res<-m %>%
    #on décale les vecteurs pour calculer à t-1
    mutate(Datebis=c(Date[-1],NA), Mean_heightbis=c(Mean_height[-1],NA),.keep="all") %>%
    #on calcule la différence entre les deux vecteurs à t et t-1
    mutate(deltat=difftime(Datebis,Date,units = "days"),deltah=(Mean_heightbis-Mean_height),.keep="all") %>%
    #on calcule le rapport dH/dt
    mutate(V_growth=deltah/as.numeric(deltat),.keep="unused") %>%
    #on arrange le tableau pour ne sélectionner que ce qui nous intéresse
    select(-Datebis,-Mean_heightbis)
  return(res)
}


################growth_rate#####################################################
####Fonction qui permet de calculer taux de croissance à partir d'un df comprenant des données de hauteurs et des dates, fonction qui utilise la fonction growth_vector

#Entrées : 
#     df comprend : une colonne avec les dates (Date), une colonne avec les hauteurs (Mean_height)
#     /!\ df ne doit pas contenir des données de plusieurs parcelle !
#     typeofmean : "date", "distance", "localisation"
#     
#Sorties : 
#     un dataframe avec

growth_rate<-function(df,typeofmean){
  dataframe<-df%>%ungroup()
  
  #si on groupe les données par date (100-120 points/moyenne)
  if (typeofmean=="date"){
    result <- dataframe %>%
      ungroup()%>%
      group_by(Plot) %>%
      growth_vector(.)
    
  }
  
  
  if (typeofmean=="smallplot"){
    result <- dataframe %>%
      group_by(Smallplot) %>%
      growth_vector(.)
    
  }
  #si on groupe les données par distance et date (9 points/moyenne)
  if (typeofmean=="distance"){
    result<-dataframe %>%
      group_by(Plot, Distance)%>% 
      growth_vector(.)
  }
  #si on groupe les données par localisation et date (3 points/moyenne)
  if (typeofmean=="localisation"){
    result<-dataframe %>% 
      ungroup() %>%
      mutate(loc=paste0(Distance,Rang,Plot),.keep="all")%>%
      group_by(loc) %>% 
      growth_vector(.)
    
  }
  #si on groupe les données par rang et date (46 points/moyenne)
  if (typeofmean=="rang"){
    result<-dataframe %>%
      ungroup() %>%
      mutate(rangplot=paste0(Rang,Plot),.keep="all") %>%
      group_by(rangplot) %>%
      growth_vector(.)
    
  }
  return(result)
}


##################################################################DONNEES##################################################################################
# Mise en place des données

#Importation des données depuis les données brutes 

sp<-read_xlsx( path="D:/home/Sara/Experimentations/1-expe_patafix/Suivi_herbe/Donnees/Donnees_hauteurs/Soleou/Soleou-poules/Data/Suivi_herbe_sol_poules.xlsx" , sheet=2)
st<-read_xlsx(path = "D:/home/Sara/Experimentations/1-expe_patafix/Suivi_herbe/Donnees/Donnees_hauteurs/Soleou/Soleou-temoin/Data/Suivi_herbe_sol_temoin.xlsx", sheet=2)
mp<-read_xlsx(path="D:/home/Sara/Experimentations/1-expe_patafix/Suivi_herbe/Donnees/Donnees_hauteurs/Mistral/Mistral-poules/Data/Suivi_herbe_mis_poules.xlsx", sheet=2)
mt<-read_xlsx(path="D:/home/Sara/Experimentations/1-expe_patafix/Suivi_herbe/Donnees/Donnees_hauteurs/Mistral/Mistral-temoin/Data/Suivi_herbe_mis_temoin.xlsx", sheet=2)

#On change le nom de la colonne distance

names(sp)[2]<-"Distance"
names(st)[2]<-"Distance"
names(mp)[2]<-"Distance"
names(mt)[2]<-"Distance"


## Analyses de données à la parcelle

#Dans cette analyse, on calcule les moyennes de hauteurs d'herbe à la parcelle. On regarde l'évolution au cours du temps des hauteurs d'herbes à la parcelle, dans 2 x 2 modalités.

#on analyse les données de hauteur en calculant les moyennes de hauteurs d'herbe par parcelle en fonction de la date
#calcul pour les 2 x 2 modalités
meanMp<-Mean_type(mp,"date")
meanMt<-Mean_type(mt,"date")
meanSt<-Mean_type(st,"date")
meanSp<-Mean_type(sp,"date")

#mise en forme des résultats
meanMp$Plot<-"Mp"
meanMp$Hens<-"with"
meanMt$Plot<-"Mt"
meanMt$Hens<-"without"
meanSp$Plot<-"Sp"
meanSp$Hens<-"with"
meanSt$Plot<-"St"
meanSt$Hens<-"without"

###DATAFRAME : meanAll_date : MOYENNE DE HAUTEURS D'HERBE EN FONCTION DU TEMPS PAR PARCELLE POUR LES 4 PARCELLES
meanAll_date<-bind_rows(meanMp,meanMt,meanSp,meanSt)

print(head(meanAll_date))



#Si on prend en compte les fauches dans les parcelles témoins du 22/02 et 06/04 


#/!\ ATTENTION NUANCE : il faut tenir compte de certaines interventions sur la parcelle aussi : fauches réalisées le 22/02/2022 et le 06/04/2022 !!!!
#on peut donc séparer selon la distance 
#définition des distances fauchées pour mistral : 2m, 6m, 10m etc
distancefauchemistral<-c(2,6,10,14,18,22)
#définition des distances fauchées pour soleou : 0m, 4m, 8m etc
distancefauchesoleou<-c(0,4,8,12,16,20)



#on sépare les moyennes des parties fauchées ou non fauchées pour mistral
meanMt_fauche<-Mean_type(subset(mt,mt$Distance %in% distancefauchemistral),"date")
meanMt_sansfauche<-Mean_type(subset(mt,!(mt$Distance %in% distancefauchemistral)),"date")
meanMt_fauche$Plot<-"Mt_cut"
meanMt_sansfauche$Plot<-"Mt_wcut"


#on sépare les moyennes des parties fauchées ou non fauchées pour soleou
meanSt_fauche<-Mean_type(subset(st,st$Distance %in% distancefauchesoleou),"date")
meanSt_sansfauche<-Mean_type(subset(st,!(st$Distance %in% distancefauchesoleou)),"date")
meanSt_fauche$Plot<-"St_cut"
meanSt_sansfauche$Plot<-"St_wcut"



### meanTemoin_date_fauche : MOYENNE DE HAUTEURS D'HERBE EN FONCTION DU TEMPS PAR PARCELLE (TEMOIN) ENTRE FAUCHE ET NON FAUCHE
meanTemoin_date_fauche<-bind_rows(meanMt_fauche,meanMt_sansfauche,meanSt_fauche,meanSt_sansfauche)



print(head(meanTemoin_date_fauche))






###########################DONNEES UTILISEES############################################
#données générales témoin en retirant les zones fauchées

mt_wcut<-mt %>%
  subset(.,!((Distance%in%distancefauchemistral)&(as.Date("2022-02-22")<as.Date(Date))))%>%
  summarise(Mean_type(.,"date"))


st_wcut<-st %>%
  subset(.,!((Distance%in%distancefauchesoleou)&(as.Date("2022-02-22")<as.Date(Date))))%>%
  summarise(Mean_type(.,"date"))


#données poules en retirant les zones fauchées

#si on retire de manière à avoir des données équidistantes pour la calibration
mp_equi<-mp%>%
  subset(Distance%in%c(-4,-2,0,2,4,6,8,10,12,14,16,18,20))
meanMp_equi<-Mean_type(mp_equi,"date")

sp_equi<-sp%>%
  subset(Distance%in%c(-2.5,0,2,4,6,8,10,12,14,16,18,20,22))
meanSp_equi<-Mean_type(sp_equi,"date")






####DATA AND PARAMETERS NOT TO CALIBRATE #####
#Real data#
datatotal=rbind(st_wcut,mt_wcut,meanSp_equi,meanMp_equi)


#on supprime les données de manière à n'avoir qu'une année de données
dataoneyear<-datatotal %>%
  ungroup%>%
  filter(Date<as.Date("2022-07-28 UTC")) 

#on donne  
data<-dataoneyear$Mean_height


###Time data####
#vecteur de données de temps
time1=subset(st_wcut,Date<as.Date("2022-07-28 UTC"),select=c(Date))
#on convertit les données de date en données de nombre de jours depuis le début des mesures
timedays1=(as.numeric(time1$Date)-1626739200)/86400
#vecteur de données de temps
time2=subset(mt_wcut,Date<as.Date("2022-07-28 UTC"),select=c(Date))
#on convertit les données de date en données de nombre de jours depuis le début des mesures
timedays2=(as.numeric(time2$Date)-1626739200)/86400
#vecteur de données de temps
time3=subset(meanSp_equi,Date<as.Date("2022-07-28 UTC"),select=c(Date))
#on convertit les données de date en données de nombre de jours depuis le début des mesures
timedays3=(as.numeric(time3$Date)-1626739200)/86400
#vecteur de données de temps
time4=subset(meanMp_equi,Date<as.Date("2022-07-28 UTC"),select=c(Date))
#on convertit les données de date en données de nombre de jours depuis le début des mesures
timedays4=(as.numeric(time4$Date)-1626739200)/86400

time=c(timedays1,timedays2+366,timedays3+732,timedays4+1098)

tmax=365

###Presence of poultry####



#parametres définis en amont
#par rapport à l'expérimentation : 
#importation des données

sp_poules<-read_xlsx( path="D://home/Sara/Experimentations/1-expe_patafix/Suivi_herbe/Protocoles/2021_10_21_interventions_parcelles.xlsx", sheet=3)
mp_poules<-sp_poules<-read_xlsx( path="D://home/Sara/Experimentations/1-expe_patafix/Suivi_herbe/Protocoles/2021_10_21_interventions_parcelles.xlsx", sheet=5)
p_sp<-subset(sp_poules,Date>="2021-07-20"&Date<=as.Date("2022-07-20 UTC"))
p_mp<-subset(mp_poules,Date>="2021-07-20"&Date<=as.Date("2022-07-20 UTC"))

p_zerosp<-p_sp%>%
  mutate(Date=Date,Np=rep(0,length(Date)))


p_zeromp<-p_mp%>%
  mutate(Date=Date,Np=rep(0,length(Date)))


#Hauteurs initiales
h1=st_wcut$Mean_height[1]
h2=mt_wcut$Mean_height[1]
h3=meanSp_equi$Mean_height[1]
h4=meanMp_equi$Mean_height[1]



