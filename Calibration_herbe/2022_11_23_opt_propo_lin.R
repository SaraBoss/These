#code pour optimisation modèle prélèvement proportionnel à la hauteur de manière linéaire : surtout utilisé pour optimisation en poules/ha 




#DEFINITION DES FONCTIONS DE CROISSANCE D'HERBE ET DE PRELEVEMENT D'HERBE

#    A) CODAGE DU MODELE DE CROISSANCE D'HERBE 


####################################################INDICATRICE FUNCTION ##############################################################################
#indicatrice function : indicates if the value is in the intervalle
#input : a time t, tmin and tsup, two time values defining an intervalle
#output : return 1 if tmin<t<tsup, return 0 else


indicatrice<-function(t,tmin,tsup){
  if((tmin<=t)&(t<tsup))
  {value=1}
  else{value=0}
  return(value)
  
}





#######                                               2) En temps discret

####################################################GENERAL LOGISTIC DISCRETE ##############################################################################
#on code la fonction de croissance logistique de l'herbe sous forme discrete (depend du pas précédent), pas de temps journalier
#input : H : height (or something else) at time t, r : growth rate, Hmax : maximal height
#output : H_tplusun : height (or something else) at time t+1

logistic_step<-function(x,K,r){
  y<-x*(r*(1-x/K)+1)
  return(y)
}

####################################################GENERAL LOGISTIC DISCRETE DELTA TIME#######################################################################
#on code la fonction de croissance logistique de l'herbe sous forme discrete (depend du pas précédent espacé de deltat) : approximation 
#input : (H : height (or something else) at time t), (r : growth rate), (Hmax : maximal height), (deltat : number of days between H and H_tplusun)
#output : H_tplusun : height (or something else) at time t+1

logistic_delta<-function(H,r,Hmax,deltat){
  H_tplusun<-H*(r*(1-H/Hmax)*deltat+1)
  return(H_tplusun)
}


K_et_b<-function(b){
  K=sqrt(1/((b*((1-2*b)/3)+1)*16/3*(b+1)^2))
  return(K)  
}



#     B) CODAGE DES MODELES DE PRELEVEMENT


#################################################################FONCTION INTAKE#############################################################
###N.B :intake functions are always discrete time function

############                                            2) Intake proportional linear to height



hen_effet_propo_lin<-function(H,p,a,b,hmin){
  if (H<hmin){
    consum=0
  } else {
    consum=(a*H+b)*p
  }
  return(consum)
}



hen_effet_lin<-function(h0,r1,r2,r3,r4,Hmax1,Hmax2,Hmax3,Hmax4,t1,t2,t3,p_df,a,b,hmin,tmax){
  H=c(h0)
  I=c()
  I_indiv=c()
  
  p_dform<-p_df%>%
    mutate(datedays=(as.numeric(as.Date(Date))-1626739200)/86400)
  
  for (t in 1:tmax){
    p=p_dform$Np[t]
    index=ifelse((t>0&t<=t1),1,ifelse((t>t1&t<=t2),2,ifelse((t>t2&t<=t3),3,ifelse((t>t3&t<=tmax),4,"ERROR"))))
    r=cbind(r1,r2,r3,r4)[index]
    Hmax=cbind(Hmax1,Hmax2,Hmax3,Hmax4)[index]
    intake=hen_effet_propo_lin(H[t],p,a,b,hmin)
    
    if (p!=0){
      intake_indiv=intake/p}
    else intake_indiv=0
    
    hplusun=logistic_step(H[t],Hmax,r)-intake
    if (hplusun<=0|is.na(hplusun)|is.nan(hplusun)){
      hplusun=0
    }
    
    H=c(H,hplusun)
    I=c(I,intake)
    I_indiv=c(I_indiv,intake_indiv)
    
  }
  df_tout=data.frame("Height"=H,"I"=c(I,0),"Intake_indiv"=c(I_indiv,0))
  return(df_tout)
}








simul_hen_effet_propo_lin<-function(h0,r1,r2,r3,r4,Hmax1,Hmax2,Hmax3,Hmax4,t1,t2,t3,p_df,a,b,hmin,tmax){
  H=c(h0)
  I=c(0)
  p_dform<-p_df%>%
    mutate(datedays=(as.numeric(as.Date(Date))-1626739200)/86400)
  
  for (t in 1:tmax){
    p=p_dform$Np[t]
    index=ifelse((t>0&t<=t1),1,ifelse((t>t1&t<=t2),2,ifelse((t>t2&t<=t3),3,ifelse((t>t3&t<=tmax),4,"ERROR"))))
    r=cbind(r1,r2,r3,r4)[index]
    Hmax=cbind(Hmax1,Hmax2,Hmax3,Hmax4)[index]
    intake=hen_effet_propo_lin(H[t],p,a,b,hmin)
    
    hplusun=logistic_step(H[t],Hmax,r)-intake
    if (hplusun<=0|is.na(hplusun)|is.nan(hplusun)){
      hplusun=0
    }
    H=c(H,hplusun)
    
  }
  
  return(H)
}





#######MODELE #############
# input : a vector of parameters to estimate
# output : a vector of simulated value
# Needed functions : (grass growth function : logistic_step), (intake function : hen_effet_propo_parts)
general_model_propo_lin<-function(parameters){
  #parameters for the growth function
  
  r1=parameters[1]
  r2=parameters[2]
  r3=parameters[3]
  r4=parameters[4]
  Hmax1=parameters[5]
  Hmax2=parameters[6]
  Hmax3=parameters[7]
  Hmax4=parameters[8]
  t1=parameters[9]
  t2=parameters[10]
  t3=parameters[11]
  
  #parameters for the intake function
  a=parameters[12]
  b=parameters[13]
  hmin=parameters[14]
  
  
  #simulation first dataset : soleou temoin
  
  simul1=simul_hen_effet_propo_lin(h1,r1,r2,r3,r4,Hmax1,Hmax2,Hmax3,Hmax4,t1,t2,t3,p_zerosp,a,b,hmin,tmax)
  #simulation second dataset : mistral temoin
  
  simul2=simul_hen_effet_propo_lin(h2,r1,r2,r3,r4,Hmax1,Hmax2,Hmax3,Hmax4,t1,t2,t3,p_zeromp,a,b,hmin,tmax)
  #simulation third dataset : soleou poules
  
  simul3=simul_hen_effet_propo_lin(h3,r1,r2,r3,r4,Hmax1,Hmax2,Hmax3,Hmax4,t1,t2,t3,p_sp,a,b,hmin,tmax)
  #simulation fourth dataset : mistral poules
  
  simul4=simul_hen_effet_propo_lin(h4,r1,r2,r3,r4,Hmax1,Hmax2,Hmax3,Hmax4,t1,t2,t3,p_mp,a,b,hmin,tmax)
  
  #general simulation, for which we select days at which we have real data (timedays)
  simul_general<-c(simul1[timedays1+1],simul2[timedays2+1],simul3[timedays3+1],simul4[timedays4+1])
  
  #calculation of RMSE and RRMSE
  RMSE=sqrt(1/length(data)*sum((simul_general-data)^2))
  RRMSE=sqrt(1/length(data)*sum(((simul_general-data)/data)^2))
  
  return(c("RMSE"=RMSE,"RRMSE"=RRMSE))
  
}


#fonction qui permet de calculer une simulation à partir des paramètres calibrés 

modele<-function(h0,r1,r2,r3,r4,Hmax1,Hmax2,Hmax3,Hmax4,t1,t2,t3,p_df,a,b,hmin,tmax){
  
  #simulation 
  
  simul=simul_hen_effet_propo_lin(h0,r1,r2,r3,r4,Hmax1,Hmax2,Hmax3,Hmax4,t1,t2,t3,p_df,a,b,hmin,tmax)
  
  return(simul)
  
}








############OPTIMISATION DE LA FONCTION AVEC ALGORITHME NELDER-MEAD EN PARTANT D'UN JEU DE DONNEES INITIALE ###########################

#optimisation de la fonction avec Nelder-Mead : 


#définition du vecteur initial de paramètres
#h0,h0bis,r1,r2,r3,r4,Hmax1,Hmax2,Hmax3,Hmax4,t1,t2,t3,c1,c2,c3,c4
#partir de plein de paramètres initiaux


par_ini=c(0.0132,0.00181,0.08053,0.008902,35.06,2.86,81.17,23.55,82,242,320.22,0.000135249,-0.00047,3.77)

#on borne l'exploration des paramètres par un vecteur minimal et un vecteur maximal
#valeurs minimales de paramètres
par_low=c(-1,-1,-1,-1,0,0,0,0,60,150,200,0,-0.005,0)
#valeurs maximales de paramètres
par_up=c(1,1,1,1,100,100,100,100,120,250,340,0.0002,0.005,80)



#on optimise ensuite la fonction obtenue avec un algorithme de Nelder-Mead
#fonction de calcul
opt_general_nm_RMSE<-function(val_ini){
  resultat<-Nelder_Mead(fn=function(x){return(general_model_propo_lin(x)[1])},par=val_ini,lower = par_low,upper=par_up,control=list(verbose=1))
  nm_result_RMSE<-as.vector(unlist(resultat))
  return(nm_result_RMSE)

  }

#control=list(MinfMax=1,verbose=0,FtolAbs=1e-15,XtolAbs=1e-15,FtolRel=1e-25)

#Calcul RRMSE
opt_general_nm_RRMSE<-function(val_ini){
  resultat<-Nelder_Mead(fn=function(x){return(general_model_propo_lin(x)[2])},par=val_ini,lower = par_low,upper=par_up,control=list(maxfun=1000000,MinfMax=0.00001,verbose=0,FtolAbs=1e-15,XtolAbs
                                                                                                                                    =1e-15,FtolRel=1e-25,verbose=1))
  nm_result_RRMSE<-as.vector(unlist(resultat))
  return(nm_result_RRMSE)}



#OPTIMISATION A PARTIR UN JEU DE VALEURS INITIALES
#on fait tourner la fonction d'optimisation sur un jeu de paramètres initiaux

result_oneRMSE<-opt_general_nm_RMSE(par_ini)
result_oneRRMSE<-opt_general_nm_RRMSE(par_ini)

#vecteur de paramètres obtenus RMSE : les paramètres obtenus qui minimisent la fonction f 
parametersRMSEone<-as.numeric(result_oneRMSE[2:(length(par_ini)+1)])
print("Les valeurs de paramètres obtenues pour RMSE sont ")
print(parametersRMSEone)



#vecteur de paramètres obtenus RRMSE
parametersRRMSEone<-as.numeric(result_oneRRMSE[2:(length(par_ini)+1)])
print("Les valeurs de paramètres obtenues pour RRMSE sont ")
print(parametersRRMSEone)







write.csv(result_oneRMSE,"Z://modelo_Sara/Code/Simul/2022_11_29_sim_propo_linRMSEone.csv")
write.csv(result_oneRRMSE,"Z://modelo_Sara/Code/Simul/2022_11_29_sim_propo_linRRMSEone.csv")






############OPTIMISATION DE LA FONCTION AVEC ALGORITHME NELDER-MEAD EN PARTANT D'UNE GRILLE DE DONNEES INITIALES###########################


#on préfère une grille d'exploration des paramètres qui soit aléatoire que déterministe
#on tire 200 jeux de paramètres initiaux

df_ini=as.data.frame(cbind("r1"=runif(200, min=0, max=1),"r2"=runif(200, min=0, max=1)))
df_ini$r3=runif(200, min=0, max=1)
df_ini$r4=runif(200, min=0, max=1) 
df_ini$Hmax1=runif(200, min=0, max=100)
df_ini$Hmax2=runif(200, min=0, max=100)
df_ini$Hmax3=runif(200, min=0, max=100)
df_ini$Hmax4=runif(200, min=0, max=100)
df_ini$t1=runif(200, min=60, max=120)
df_ini$t2=runif(200, min=150, max=250)
df_ini$t3=runif(200, min=200, max=340)
df_ini$a=runif(200, min=0, max=0.002)
df_ini$b=runif(200, min=-0.005, max=0.005)
df_ini$hmin=runif(200, min=0, max=10)





#simulation et obtention des résultats
resultRMSE0<-apply(X = df_ini,MARGIN=1,FUN=function(x){
  print("début") 
  return(opt_general_nm_RMSE(x))})


resultRRMSE0<-apply(X = df_ini,MARGIN=1,FUN=function(x){
  print("début") 
  return(opt_general_nm_RRMSE(x))})


#mise en forme du tableau de resultats

resultRMSE<-data.frame(t(resultRMSE0))
resultRRMSE<-data.frame(t(resultRRMSE0))


resultRMSEone<-data.frame(t(result_oneRMSE))
resultRRMSEone<-data.frame(t(result_oneRRMSE))



#nom de colonnes simulation
namesnmRMSE<-c("RMSE",names(df_ini),"convergence","NM.result","message","iprint","maxfun","FtolAbs","FtolRel","XtolRel","MinfMax","warnOnly","xst1","xst2","xst3","xst4","xst5","xst6","xst7","xst8","xst9","xst10","xst11","xst12","xst13","xst14","xt1","xt2","xt3","xt4","xt5","xt6","xt7","xt8","xt9","xt10","xt11","xt12","xt13","xt14","feval")
namesnmRRMSE<-c("RRMSE",names(df_ini),"convergence","NM.result","message","iprint","maxfun","FtolAbs","FtolRel","XtolRel","MinfMax","warnOnly","xst1","xst2","xst3","xst4","xst5","xst6","xst7","xst8","xst9","xst10","xst11","xst12","xst13","xst14","xt1","xt2","xt3","xt4","xt5","xt6","xt7","xt8","xt9","xt10","xt11","xt12","xt13","xt14","feval")




colnames(resultRMSE)<-namesnmRMSE
colnames(resultRRMSE)<-namesnmRRMSE
colnames(resultRMSEone)<-namesnmRMSE
colnames(resultRRMSEone)<-namesnmRRMSE


resultatRMSE<-resultRMSE%>%
  mutate_at(.vars=vars(-message,-warnOnly),.funs=as.numeric)


resultatRRMSE<-resultRRMSE%>%
  mutate_at(.vars=vars(-message,-warnOnly),.funs=as.numeric)


resultatRMSEone<-resultRMSEone%>%
  mutate_at(.vars=vars(-message,-warnOnly),.funs=as.numeric)


resultatRRMSEone<-resultRRMSEone%>%
  mutate_at(.vars=vars(-message,-warnOnly),.funs=as.numeric)




#RESULT OBTAINED  RMSE
indiceminRMSE=which.min(resultRMSE$RMSE)

parametersRMSE<-resultRMSE[indiceminRMSE,2:length(df_ini)]

#RESULT OBTAINED  RRMSE
indiceminRRMSE=which.min(resultRRMSE$RRMSE)

parametersRRMSE<-resultRRMSE[indiceminRRMSE,2:length(df_ini)]


#export of data


write.csv(resultatRMSE,"Simul/2022_11_23_sim_propo_linRMSE.csv")
write.csv(resultatRRMSE,"Simul/2022_11_23_sim_propo_linRRMSE.csv")





write.csv(resultatRMSEone,"Z://modelo_Sara/Code/Simul/2022_11_23_sim_propo_linRMSEone.csv")
write.csv(resultatRRMSEone,"Z://modelo_Sara/Code/Simul/2022_11_23_sim_propo_linRRMSEone.csv")




#on réoptimise seulement sur a


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
b=-9.734020e-04
hmin=3.757924e+00

  
#######MODELE #############
# input : parameter a to optimize
# output : a vector of simulated value
# Needed functions : (grass growth function : logistic_step), (intake function : hen_effet_propo_parts)
general_model_propo_lin_a<-function(par_a){
  #parameters for the growth function
  
  a=par_a
  
  #simulation first dataset : soleou temoin
  
  simul1=simul_hen_effet_propo_lin(h1,r1,r2,r3,r4,Hmax1,Hmax2,Hmax3,Hmax4,t1,t2,t3,p_zerosp,a,b,hmin,tmax)
  #simulation second dataset : mistral temoin
  
  simul2=simul_hen_effet_propo_lin(h2,r1,r2,r3,r4,Hmax1,Hmax2,Hmax3,Hmax4,t1,t2,t3,p_zeromp,a,b,hmin,tmax)
  #simulation third dataset : soleou poules
  
  simul3=simul_hen_effet_propo_lin(h3,r1,r2,r3,r4,Hmax1,Hmax2,Hmax3,Hmax4,t1,t2,t3,p_sp,a,b,hmin,tmax)
  #simulation fourth dataset : mistral poules
  
  simul4=simul_hen_effet_propo_lin(h4,r1,r2,r3,r4,Hmax1,Hmax2,Hmax3,Hmax4,t1,t2,t3,p_mp,a,b,hmin,tmax)
  
  #general simulation, for which we select days at which we have real data (timedays)
  simul_general<-c(simul1[timedays1+1],simul2[timedays2+1],simul3[timedays3+1],simul4[timedays4+1])
  
  #calculation of RMSE and RRMSE
  RMSE=sqrt(1/length(data)*sum((simul_general-data)^2))
  RRMSE=sqrt(1/length(data)*sum(((simul_general-data)/data)^2))
  
  return(c("RMSE"=RMSE,"RRMSE"=RRMSE))
  
}


par_ini_a=1.501091e-04
par_low_a=0
par_up_a=0.002

#on optimise ensuite la fonction obtenue avec un algorithme de Nelder-Mead
#fonction de calcul
opt_general_nm_RMSE_a<-function(val_ini){
  resultat<-Nelder_Mead(fn=function(x){return(general_model_propo_lin_a(x)[1])},par=val_ini,lower = par_low_a,upper=par_up_a,control=list(verbose=1))
  nm_result_RMSE<-as.vector(unlist(resultat))
  return(nm_result_RMSE)
  
}

#control=list(MinfMax=1,verbose=0,FtolAbs=1e-15,XtolAbs=1e-15,FtolRel=1e-25)

#Calcul RRMSE
opt_general_nm_RRMSE_a<-function(val_ini){
  resultat<-Nelder_Mead(fn=function(x){return(general_model_propo_lin_a(x)[2])},par=val_ini,lower = par_low_a,upper=par_up_a,control=list(maxfun=1000000,MinfMax=0.00001,verbose=0,FtolAbs=1e-15,XtolAbs
                                                                                                                                    =1e-15,FtolRel=1e-25,verbose=1))
  nm_result_RRMSE<-as.vector(unlist(resultat))
  return(nm_result_RRMSE)}



#OPTIMISATION A PARTIR UN JEU DE VALEURS INITIALES
#on fait tourner la fonction d'optimisation sur un jeu de paramètres initiaux

result_oneRMSE_a<-opt_general_nm_RMSE_a(par_ini_a)
result_oneRRMSE_a<-opt_general_nm_RRMSE_a(par_ini_a)

#vecteur de paramètres obtenus RMSE : les paramètres obtenus qui minimisent la fonction f 
parametersRMSEone_a<-as.numeric(result_oneRMSE_a[2])
print("Les valeurs de paramètres obtenues pour RMSE sont ")
print(parametersRMSEone_a)



#vecteur de paramètres obtenus RRMSE
parametersRRMSEone_a<-as.numeric(result_oneRRMSE_a[2])
print("Les valeurs de paramètres obtenues pour RRMSE sont ")
print(parametersRRMSEone_a)



