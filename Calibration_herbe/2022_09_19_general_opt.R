#script pour executer la calibration de données avec des nombre de poules


#définition des données sur lesquels on calibre le modèle

run("2022_09_19_def_data.R")


#Test de différentes formes fonctionnelles de prélèvement

#forme fonctionnelle 1 : prélèvement proportionnel à la hauteur d'herbe et par partie :

run("2022_09_19_opt_propo_partsv0.R")


run("2022_09_19_opt_propo_squarev0.R")




###########################CALIBRATION AVEC DES POULES/HA######################################################
run("2022_11_24_def_data_ha.R")


run("2022_09_19_opt_propo_lin.R")




#analyse de sensibilité

#jeu de paramètres utilisés 

#données de base
  source("D:/home/Sara/Modelo/2-Herbe/Code/1-Analyse_donnees/2022_11_24_def_data_ha.R")
  source("D:/home/Sara/Modelo/2-Herbe/Code/2-Calibration/2022_11_23_opt_propo_lin.R")

#paramètres calibrés au 24/11/2022 : on prend les paramètres qui correspondent à la simulation du 24/11/2022 avec un jeu initial : meilleur RMSE

r1=1.159668e-02
r2=1.522184e-03
r3=7.804576e-02
r4=8.885467e-03
Hmax1=3.513651e+01
Hmax2=2.837988e+00
Hmax3=8.118482e+01
Hmax4=2.345212e+01
t1=8.200000e+01
t2=2.420014e+02
t3=3.202903e+02
a=1.486718e-04
b=-9.734020e-04
hmin=3.757924e+00




parametres<-c(r1,r2,r3,r4,Hmax1,Hmax2,Hmax3,Hmax4,t1,t2,t3,a,b,hmin)


#RMSE/RRMSE initiaux
initial=general_model_propo_lin(parametres)


#analyse de sensibilité 
#pour r1
r1_sens<-sensibility_test("r1",parametres,p_variation =10)

#pour r2
r2_sens<-sensibility_test("r2",parametres,p_variation =10)

#pour r3
r3_sens<-sensibility_test("r3",parametres,p_variation =10)

#pour r4
r4_sens<-sensibility_test("r4",parametres,p_variation =10)


#pour Hmax1
Hmax1_sens<-sensibility_test("Hmax1",parametres,p_variation =10)

#pour Hmax2
Hmax2_sens<-sensibility_test("Hmax2",parametres,p_variation =10)

#pour Hmax3
Hmax3_sens<-sensibility_test("Hmax3",parametres,p_variation =10)

#pour Hmax4
Hmax4_sens<-sensibility_test("Hmax4",parametres,p_variation =10)


#pour t1
t1_sens<-sensibility_test("t1",parametres,p_variation =10)

#pour t2
t2_sens<-sensibility_test("t2",parametres,p_variation =10)

#pour t3
t3_sens<-sensibility_test("t3",parametres,p_variation =10)

#pour Imax
a_sens<-sensibility_test("a",parametres,p_variation =10)


#pour b
b_sens<-sensibility_test("b",parametres,p_variation =10)

#pour hmin
hmin_sens<-sensibility_test("hmin",parametres,p_variation =10)



result_sensibility_RMSE<-data.frame(rbind(r1_sens,r2_sens,r3_sens,r4_sens,Hmax1_sens,Hmax2_sens,Hmax3_sens,Hmax4_sens,t1_sens,t2_sens,t3_sens,a_sens,b_sens,hmin_sens))

result_sensibility_long_RMSE<-result_sensibility%>%
  pivot_longer(c(RMSE_down, RMSE_up,RRMSE_down,RRMSE_up))


#paramètres calibrés au 24/11/2022 : on prend les paramètres qui correspondent à la simulation du 24/11/2022 avec un jeu initial : meilleur RRMSE

r1=8.431958e-03
r2=1.384284e-03
r3=8.307207e-02
r4=1.098945e-02
Hmax1=35.51880
Hmax2=292.011
Hmax3=80.79
Hmax4=23.572
t1=82.00
t2=242.00
t3=320.004
a=1.501091e-04
b=-6.241792e-04
hmin=3.913957




parametres<-c(r1,r2,r3,r4,Hmax1,Hmax2,Hmax3,Hmax4,t1,t2,t3,a,b,hmin)


#RMSE/RRMSE initiaux
initial=general_model_propo_lin(parametres)


#analyse de sensibilité 
#pour r1
r1_sens<-sensibility_test("r1",parametres,p_variation =10)

#pour r2
r2_sens<-sensibility_test("r2",parametres,p_variation =10)

#pour r3
r3_sens<-sensibility_test("r3",parametres,p_variation =10)

#pour r4
r4_sens<-sensibility_test("r4",parametres,p_variation =10)


#pour Hmax1
Hmax1_sens<-sensibility_test("Hmax1",parametres,p_variation =10)

#pour Hmax2
Hmax2_sens<-sensibility_test("Hmax2",parametres,p_variation =10)

#pour Hmax3
Hmax3_sens<-sensibility_test("Hmax3",parametres,p_variation =10)

#pour Hmax4
Hmax4_sens<-sensibility_test("Hmax4",parametres,p_variation =10)


#pour t1
t1_sens<-sensibility_test("t1",parametres,p_variation =10)

#pour t2
t2_sens<-sensibility_test("t2",parametres,p_variation =10)

#pour t3
t3_sens<-sensibility_test("t3",parametres,p_variation =10)

#pour Imax
a_sens<-sensibility_test("a",parametres,p_variation =10)


#pour b
b_sens<-sensibility_test("b",parametres,p_variation =10)

#pour hmin
hmin_sens<-sensibility_test("hmin",parametres,p_variation =10)



result_sensibility_RRMSE<-data.frame(rbind(r1_sens,r2_sens,r3_sens,r4_sens,Hmax1_sens,Hmax2_sens,Hmax3_sens,Hmax4_sens,t1_sens,t2_sens,t3_sens,a_sens,b_sens,hmin_sens))

result_sensibility_long_RRMSE<-result_sensibility%>%
  pivot_longer(c(RMSE_down, RMSE_up,RRMSE_down,RRMSE_up))



#on trace le dotplot du test de sensibilité

RMSE_index<- result_sensibility_long_RMSE$name %in% grep("^RMSE",result_sensibility_long_RMSE$name,value=TRUE)
RRMSE_index<- result_sensibility_long_RRMSE$name %in% grep("RRMSE",result_sensibility_long_RRMSE$name,value=TRUE)

sens_RMSE<-ggplot(subset(result_sensibility_long_RMSE,subset=RMSE_index)) + 
  geom_dotplot(binaxis='y', stackdir='center',stackratio=1.5, dotsize=1 )+theme_bw()+theme(text = element_text(size=20))+ylab("RMSE")+ coord_flip()+scale_fill_discrete(name="Variations",labels = c("-10%","+10%"))+geom_point(y=initial[[1]])

sens_RRMSE<-ggplot(subset(result_sensibility_long_RRMSE,subset=RRMSE_index),aes(x=Parameter, y=value,fill=name)) + 
  geom_dotplot(binaxis='y', stackdir='center',stackratio=1.5, dotsize=1 )+theme_bw()+theme(text = element_text(size=20))+ylab("RRMSE")+ coord_flip()+scale_fill_discrete(name="Variations",labels = c("-10%","+10%"))


sens_RMSE1<-ggplot(subset(result_sensibility_long_RMSE,subset=RMSE_index))+geom_point(aes(x=Parameter,y=value,shape=name,fill=name),size=7)+scale_shape_manual(values=c(25,24))+theme_bw()+theme(text = element_text(size=25))+ylab("RMSE")+geom_point(aes(x=Parameter,y=initial[[1]]),color="red")

sens_RRMSE1<-ggplot(subset(result_sensibility_long_RRMSE,subset=RRMSE_index))+geom_point(aes(x=Parameter,y=value,shape=name,fill=name),size=7)+scale_shape_manual(values=c(25,24))+theme_bw()+theme(text = element_text(size=25))+ylab("RRMSE")+geom_point(aes(x=Parameter,y=initial[[2]]),color="red")





#paramètres calibrés au 29/11/2022 : on prend les paramètres qui correspondent à la simulation du 29/11/2022 avec un jeu initial : meilleur RMSE


resultat_f4_RMSE<-read.csv(file="Z://modelo_Sara/Code/Simul/2022_11_29_sim_propo_linRMSEone.csv")
source("D:/home/Sara/Modelo/2-Herbe/Code/1-Analyse_donnees/2022_09_19_def_data.R")
source("D:/home/Sara/Modelo/2-Herbe/Code/3-Plot/2022_09_19_plot_type.R")
source("D:/home/Sara/Modelo/2-Herbe/Code/4-Analyse_sensibilite/2022_10_18_sensibility-analysis.R")

parametersRMSE<-as.numeric(unlist(resultat_f4_RMSE$x[2:15]))
plotRMSE_f4<-plot_type(parametersRMSE,"lin")

plotRMSE_f4_intake_height<-plot_type_intake(parametersRMSE,"lin","Height")
plotRMSE_f4_intake_time<-plot_type_intake(parametersRMSE,"lin","time")

#RMSE/RRMSE initiaux
initial=general_model_propo_lin(parametersRMSE)


#analyse de sensibilité 
#pour r1
r1_sens<-sensibility_test("r1",parametersRMSE,p_variation =10)

#pour r2
r2_sens<-sensibility_test("r2",parametersRMSE,p_variation =10)

#pour r3
r3_sens<-sensibility_test("r3",parametersRMSE,p_variation =10)

#pour r4
r4_sens<-sensibility_test("r4",parametersRMSE,p_variation =10)


#pour Hmax1
Hmax1_sens<-sensibility_test("Hmax1",parametersRMSE,p_variation =10)

#pour Hmax2
Hmax2_sens<-sensibility_test("Hmax2",parametersRMSE,p_variation =10)

#pour Hmax3
Hmax3_sens<-sensibility_test("Hmax3",parametersRMSE,p_variation =10)

#pour Hmax4
Hmax4_sens<-sensibility_test("Hmax4",parametersRMSE,p_variation =10)


#pour t1
t1_sens<-sensibility_test("t1",parametersRMSE,p_variation =10)

#pour t2
t2_sens<-sensibility_test("t2",parametersRMSE,p_variation =10)

#pour t3
t3_sens<-sensibility_test("t3",parametersRMSE,p_variation =10)

#pour Imax
a_sens<-sensibility_test("a",parametersRMSE,p_variation =10)


#pour b
b_sens<-sensibility_test("b",parametersRMSE,p_variation =10)

#pour hmin
hmin_sens<-sensibility_test("hmin",parametersRMSE,p_variation =10)



result_sensibility_RMSE<-data.frame(rbind(r1_sens,r2_sens,r3_sens,r4_sens,Hmax1_sens,Hmax2_sens,Hmax3_sens,Hmax4_sens,t1_sens,t2_sens,t3_sens,a_sens,b_sens,hmin_sens))

result_sensibility_long_RMSE<-result_sensibility_RMSE%>%
  pivot_longer(c(RMSE_down, RMSE_up,RRMSE_down,RRMSE_up))



#on trace le dotplot du test de sensibilité

RMSE_index<- result_sensibility_long_RMSE$name %in% grep("^RMSE",result_sensibility_long_RMSE$name,value=TRUE)
#RRMSE_index<- result_sensibility_long_RRMSE$name %in% grep("RRMSE",result_sensibility_long_RRMSE$name,value=TRUE)

sens_RMSE<-ggplot(subset(result_sensibility_long_RMSE,subset=RMSE_index)) + 
  geom_dotplot(binaxis='y', stackdir='center',stackratio=1.5, dotsize=1 )+theme_bw()+theme(text = element_text(size=20))+ylab("RMSE")+ coord_flip()+scale_fill_discrete(name="Variations",labels = c("-10%","+10%"))+geom_point(y=initial[[1]])

#sens_RRMSE<-ggplot(subset(result_sensibility_long_RRMSE,subset=RRMSE_index),aes(x=Parameter, y=value,fill=name)) + 
  #geom_dotplot(binaxis='y', stackdir='center',stackratio=1.5, dotsize=1 )+theme_bw()+theme(text = element_text(size=20))+ylab("RRMSE")+ coord_flip()+scale_fill_discrete(name="Variations",labels = c("-10%","+10%"))


sens_RMSE1<-ggplot(subset(result_sensibility_long_RMSE,subset=RMSE_index))+geom_point(aes(x=Parameter,y=value,shape=name,fill=name),size=7)+scale_shape_manual(values=c(25,24))+theme_bw()+theme(text = element_text(size=25))+ylab("RMSE")+geom_point(aes(x=Parameter,y=initial[[1]]),color="red")

#sens_RRMSE1<-ggplot(subset(result_sensibility_long_RRMSE,subset=RRMSE_index))+geom_point(aes(x=Parameter,y=value,shape=name,fill=name),size=7)+scale_shape_manual(values=c(25,24))+theme_bw()+theme(text = element_text(size=25))+ylab("RRMSE")+geom_point(aes(x=Parameter,y=initial[[2]]),color="red")






#paramètres calibrés au 29/11/2022 : on prend les paramètres qui correspondent à la simulation du 29/11/2022 avec un jeu initial : meilleur RRMSE

resultat_f4_RRMSE<-read.csv(file ="Z://modelo_Sara/Code/Simul/2022_11_29_sim_propo_linRRMSEone.csv" )
indiceminRRMSE=which.min(resultat_f4_RRMSE$RRMSE)
parametersRRMSE<-as.numeric(unlist(resultat_f4_RRMSE$x[2:15]))
plotRRMSE_f4<-plot_type(parametersRRMSE,"lin")

plotRRMSE_f4_intake_height<-plot_type_intake(parametersRRMSE,"lin","Height")
plotRRMSE_f4_intake_time<-plot_type_intake(parametersRRMSE,"lin","time")

#RMSE/RRMSE initiaux
initial=general_model_propo_lin(parametersRRMSE)


#analyse de sensibilité 
#pour r1
r1_sens<-sensibility_test("r1",parametersRRMSE,p_variation =10)

#pour r2
r2_sens<-sensibility_test("r2",parametersRRMSE,p_variation =10)

#pour r3
r3_sens<-sensibility_test("r3",parametersRRMSE,p_variation =10)

#pour r4
r4_sens<-sensibility_test("r4",parametersRRMSE,p_variation =10)


#pour Hmax1
Hmax1_sens<-sensibility_test("Hmax1",parametersRRMSE,p_variation =10)

#pour Hmax2
Hmax2_sens<-sensibility_test("Hmax2",parametersRRMSE,p_variation =10)

#pour Hmax3
Hmax3_sens<-sensibility_test("Hmax3",parametersRRMSE,p_variation =10)

#pour Hmax4
Hmax4_sens<-sensibility_test("Hmax4",parametersRRMSE,p_variation =10)


#pour t1
t1_sens<-sensibility_test("t1",parametersRRMSE,p_variation =10)

#pour t2
t2_sens<-sensibility_test("t2",parametersRRMSE,p_variation =10)

#pour t3
t3_sens<-sensibility_test("t3",parametersRRMSE,p_variation =10)

#pour Imax
a_sens<-sensibility_test("a",parametersRRMSE,p_variation =10)


#pour b
b_sens<-sensibility_test("b",parametersRRMSE,p_variation =10)

#pour hmin
hmin_sens<-sensibility_test("hmin",parametersRRMSE,p_variation =10)



result_sensibility_RRMSE<-data.frame(rbind(r1_sens,r2_sens,r3_sens,r4_sens,Hmax1_sens,Hmax2_sens,Hmax3_sens,Hmax4_sens,t1_sens,t2_sens,t3_sens,a_sens,b_sens,hmin_sens))

result_sensibility_long_RRMSE<-result_sensibility_RRMSE%>%
  pivot_longer(c(RMSE_down, RMSE_up,RRMSE_down,RRMSE_up))



#on trace le dotplot du test de sensibilité

#RMSE_index<- result_sensibility_long_RMSE$name %in% grep("^RMSE",result_sensibility_long_RMSE$name,value=TRUE)
RRMSE_index<- result_sensibility_long_RRMSE$name %in% grep("RRMSE",result_sensibility_long_RRMSE$name,value=TRUE)

#sens_RMSE<-ggplot(subset(result_sensibility_long_RMSE,subset=RMSE_index)) + 
  #geom_dotplot(binaxis='y', stackdir='center',stackratio=1.5, dotsize=1 )+theme_bw()+theme(text = element_text(size=20))+ylab("RMSE")+ coord_flip()+scale_fill_discrete(name="Variations",labels = c("-10%","+10%"))+geom_point(y=initial[[1]])

sens_RRMSE<-ggplot(subset(result_sensibility_long_RRMSE,subset=RRMSE_index),aes(x=Parameter, y=value,fill=name)) + 
geom_dotplot(binaxis='y', stackdir='center',stackratio=1.5, dotsize=1 )+theme_bw()+theme(text = element_text(size=20))+ylab("RRMSE")+ coord_flip()+scale_fill_discrete(name="Variations",labels = c("-10%","+10%"))


#sens_RMSE1<-ggplot(subset(result_sensibility_long_RMSE,subset=RMSE_index))+geom_point(aes(x=Parameter,y=value,shape=name,fill=name),size=7)+scale_shape_manual(values=c(25,24))+theme_bw()+theme(text = element_text(size=25))+ylab("RMSE")+geom_point(aes(x=Parameter,y=initial[[1]]),color="red")

sens_RRMSE1<-ggplot(subset(result_sensibility_long_RRMSE,subset=RRMSE_index))+geom_point(aes(x=Parameter,y=value,shape=name,fill=name),size=7)+scale_shape_manual(values=c(25,24))+theme_bw()+theme(text = element_text(size=25))+ylab("RRMSE")+geom_point(aes(x=Parameter,y=initial[[2]]),color="red")



#plot des différences relatives entre RRMSE initial et les RRMSE +/-10%

result_sensibility_long_RRMSE_percent<-subset(result_sensibility_long_RRMSE,subset=RRMSE_index)%>%
  mutate(Percent=(value-initial[[2]])/initial[[2]]*100)


sens_RRMSE2<-ggplot(result_sensibility_long_RRMSE_percent)+geom_point(aes(x=Parameter,y=Percent,shape=name,fill=name),size=7)+scale_shape_manual(values=c(25,24))+theme_bw()+theme(text = element_text(size=25))+ylab("Différence par rapport au RMSRE initial (en %)")+xlab("Paramètres")
