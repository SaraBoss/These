#code pour optimisation modèle prélèvement proportionnel à la hauteur : 





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

logistic_step<-function(H,r,Hmax){
  H_tplusun<-H*(r*(1-H/Hmax)+1)
  return(H_tplusun)
}

####################################################GENERAL LOGISTIC DISCRETE DELTA TIME#######################################################################
#on code la fonction de croissance logistique de l'herbe sous forme discrete (depend du pas précédent espacé de deltat) : approximation 
#input : (H : height (or something else) at time t), (r : growth rate), (Hmax : maximal height), (deltat : number of days between H and H_tplusun)
#output : H_tplusun : height (or something else) at time t+1

logistic_delta<-function(H,r,Hmax,deltat){
  H_tplusun<-H*(r*(1-H/Hmax)*deltat+1)
  return(H_tplusun)
}





#     B) CODAGE DES MODELES DE PRELEVEMENT


#################################################################FONCTION INTAKE#############################################################
###N.B :intake functions are always discrete time function

############                                            2) Intake proportional to height


hen_effet_propo<-function(H,p,c){
  consum=c*p*H
  return(consum)
}



simul_hen_effet_propo<-function(H0,r1,r2,r3,r4,Hmax1,Hmax2,Hmax3,Hmax4,t1,t2,t3,c1,c2,c3,c4,p_df,tmax){
  H=c(H0)
  I=c(0)
  p_dform<-p_df%>%
    mutate(datedays=(as.numeric(Date)-1626739200)/86400)
  
  for (t in 1:tmax){
    p=p_dform$Np[t]
    index=ifelse((t>0&t<=t1),1,ifelse((t>t1&t<=t2),2,ifelse((t>t2&t<=t3),3,ifelse((t>t3&t<=tmax),4,"ERROR"))))
    r=cbind(r1,r2,r3,r4)[index]
    Hmax=cbind(Hmax1,Hmax2,Hmax3,Hmax4)[index]
    c=cbind(c1,c2,c3,c4)[index]
    intake=hen_effet_propo(H[t],p,c)
    hplusun=logistic_step(H[t],r,Hmax)-hen_effet_propo(H[t],p,c)
    if (hplusun<=0|is.na(hplusun)|is.nan(hplusun)){
      hplusun=0
    }
    H=c(H,hplusun)
    I=c(I,intake)
  }
  return(data.frame(H,I))
}



############                                            2) Intake proportional to height


simul_hen_propo<-function(H0,r1,r2,r3,r4,Hmax1,Hmax2,Hmax3,Hmax4,t1,t2,t3,c,p_df,tmax){
  H=c(H0)
  p_dform<-p_df%>%
    mutate(datedays=(as.numeric(Date)-1626739200)/86400)
  
  for (t in 1:tmax){
    p=p_dform$Np[t]
    index=ifelse((t>0&t<=t1),1,ifelse((t>t1&t<=t2),2,ifelse((t>t2&t<=t3),3,ifelse((t>t3&t<=tmax),4,"ERROR"))))
    r=cbind(r1,r2,r3,r4)[index]
    Hmax=cbind(Hmax1,Hmax2,Hmax3,Hmax4)[index]
    hplusun=logistic_step(H[t],r,Hmax)-hen_effet_propo(H[t],p,c)
    if (hplusun<=0|is.na(hplusun)|is.nan(hplusun)){
      hplusun=0
    }
    H=c(H,hplusun)
  }
  return(H)
}
############                                            3) Intake proportional to height by parts

simul_hen_propo_parts<-function(H0,r1,r2,r3,r4,Hmax1,Hmax2,Hmax3,Hmax4,t1,t2,t3,c1,c2,c3,c4,p_df,tmax){
  H=c(H0)
  p_dform<-p_df%>%
    mutate(datedays=(as.numeric(Date)-1626739200)/86400)
  
  for (t in 1:tmax){
    p=p_dform$Np[t]
    index=ifelse((t>0&t<=t1),1,ifelse((t>t1&t<=t2),2,ifelse((t>t2&t<=t3),3,ifelse((t>t3&t<=tmax),4,"ERROR"))))
    r=cbind(r1,r2,r3,r4)[index]
    Hmax=cbind(Hmax1,Hmax2,Hmax3,Hmax4)[index]
    c=cbind(c1,c2,c3,c4)[index]
    hplusun=logistic_step(H[t],r,Hmax)-hen_effet_propo(H[t],p,c)
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
general_model_propo<-function(parameters){
  #parameters for the growth function
  h0=parameters[1]
  h0bis=parameters[2]
  r1=parameters[3]
  r2=parameters[4]
  r3=parameters[5]
  r4=parameters[6]
  Hmax1=parameters[7]
  Hmax2=parameters[8]
  Hmax3=parameters[9]
  Hmax4=parameters[10]
  t1=parameters[11]
  t2=parameters[12]
  t3=parameters[13]

  #parameters for the intake function
  c1=parameters[14]
  c2=parameters[15]
  c3=parameters[16]
  c4=parameters[17]

  #simulation first dataset : soleou temoin
  simul1=simul_hen_propo_parts(h0,r1,r2,r3,r4,Hmax1,Hmax2,Hmax3,Hmax4,t1,t2,t3,c1=0,c2=0,c3=0,c4=0,p_sp,tmax)
  #simulation second dataset : mistral temoin
  simul2=simul_hen_propo_parts(h0bis,r1,r2,r3,r4,Hmax1,Hmax2,Hmax3,Hmax4,t1,t2,t3,c1=0,c2=0,c3=0,c4=0,p_mp,tmax)
  #simulation third dataset : soleou poules
  simul3=simul_hen_propo_parts(h0,r1,r2,r3,r4,Hmax1,Hmax2,Hmax3,Hmax4,t1,t2,t3,c1,c2,c3,c4,p_sp,tmax)
  #simulation fourth dataset : mistral poules
  simul4=simul_hen_propo_parts(h0bis,r1,r2,r3,r4,Hmax1,Hmax2,Hmax3,Hmax4,t1,t2,t3,c1,c2,c3,c4,p_mp,tmax)

  #general simulation, for which we select days at which we have real data (timedays)
  simul_general<-c(simul1[timedays1+1],simul2[timedays2+1],simul3[timedays3+1],simul4[timedays4+1])

  #calculation of RMSE and RRMSE
  RMSE=sqrt(1/length(data)*sum((simul_general-data)^2))
  RRMSE=sqrt(1/length(data)*sum(((simul_general-data)/data)^2))

  return(c("RMSE"=RMSE,"RRMSE"=RRMSE))

}









  ############OPTIMISATION DE LA FONCTION AVEC ALGORITHME NELDER-MEAD EN PARTANT D'UN JEU DE DONNEES INITIALE ###########################
  
  #optimisation de la fonction avec Nelder-Mead : 
  
  
  #définition du vecteur initial de paramètres
  #h0,h0bis,r1,r2,r3,r4,Hmax1,Hmax2,Hmax3,Hmax4,t1,t2,t3,c1,c2,c3,c4
  #partir de plein de paramètres initiaux
  
  par_ini=c(14,14,0.03,0.04,0.03,0.03,25,5,80,20,73,182,280,0.001,0.001,0.001,0.001)
  
  #on borne l'exploration des paramètres par un vecteur minimal et un vecteur maximal
  #valeurs minimales de paramètres
  par_low=c(0,0,-1,-1,-1,-1,0,0,0,0,60,150,200,-0.5,-0.5,-0.5,-0.5)
  #valeurs maximales de paramètres
  par_up=c(30,30,1,1,1,1,100,100,100,100,120,250,340,0.1,0.1,0.1,0.1)
  
  
  
  #on optimise ensuite la fonction obtenue avec un algorithme de Nelder-Mead
  #fonction de calcul
  opt_general_nm_RMSE<-function(val_ini){
    resultat<-Nelder_Mead(fn=function(x){return(general_model_propo(x)[1])},par=val_ini,lower = par_low,upper=par_up)
    nm_result_RMSE<-as.vector(unlist(resultat))
    return(nm_result_RMSE)
  }
  
  #control=list(MinfMax=1,verbose=0,FtolAbs=1e-15,XtolAbs=1e-15,FtolRel=1e-25)
  
  #Calcul RRMSE
  opt_general_nm_RRMSE<-function(val_ini){
    resultat<-Nelder_Mead(fn=function(x){return(general_model_propo(x)[2])},par=val_ini,lower = par_low,upper=par_up,control=list(maxfun=1000000,MinfMax=0.00001,verbose=0,FtolAbs=1e-15,XtolAbs
                                                                                                                            =1e-15,FtolRel=1e-25))
    nm_result_RRMSE<-as.vector(unlist(resultat))
    return(nm_result_RRMSE)}
  
  
  
  #OPTIMISATION A PARTIR UN JEU DE VALEURS INITIALES
  #on fait tourner la fonction d'optimisation sur un jeu de paramètres initiaux
  
  result_oneRMSE<-opt_general_nm_RMSE(par_ini)
  result_oneRRMSE<-opt_general_nm_RRMSE(par_ini)
  
  #vecteur de paramètres obtenus RMSE : les paramètres obtenus qui minimisent la fonction f 
  parametersRMSE<-as.numeric(result_oneRMSE[2:(length(par_ini)+1)])
  print("Les valeurs de paramètres obtenues pour RMSE sont ")
  print(parametersRMSE)
  
  
  
  #vecteur de paramètres obtenus RRMSE
  parametersRRMSE<-as.numeric(result_oneRRMSE[2:(length(par_ini)+1)])
  print("Les valeurs de paramètres obtenues pour RRMSE sont ")
  print(parametersRRMSE)
  
  
  
  
  ############OPTIMISATION DE LA FONCTION AVEC ALGORITHME NELDER-MEAD EN PARTANT D'UNE GRILLE DE DONNEES INITIALES###########################
  

  #on préfère une grille d'exploration des paramètres qui soit aléatoire que déterministe
  #on tire 200 jeux de paramètres initiaux
  
  df_ini=as.data.frame(cbind("h0"=runif(200, min=0, max=30),"h0bis"=runif(200, min=0, max=30)))
  df_ini$r1=runif(200, min=0, max=1)            
  df_ini$r2=runif(200, min=0, max=1)  
  df_ini$r3=runif(200, min=0, max=1)
  df_ini$r4=runif(200, min=0, max=1) 
  df_ini$Hmax1=runif(200, min=0, max=100)
  df_ini$Hmax2=runif(200, min=0, max=100)
  df_ini$Hmax3=runif(200, min=0, max=100)
  df_ini$Hmax4=runif(200, min=0, max=100)
  df_ini$t1=runif(200, min=60, max=120)
  df_ini$t2=runif(200, min=150, max=250)
  df_ini$t3=runif(200, min=200, max=340)
  df_ini$c1=runif(200, min=-0.1, max=0.1)
  df_ini$c2=runif(200, min=-0.1, max=0.1)
  df_ini$c3=runif(200, min=-0.1, max=0.1)
  df_ini$c4=runif(200, min=-0.1, max=0.1)
  
  #simulation et obtention des résultats
  resultRMSE0<-apply(X = df_ini,MARGIN=1,FUN=unction(x){
    print("début") 
    return(opt_general_nm_RMSE(x))})
  
  
  resultRRMSE0<-apply(X = df_ini,MARGIN=1,FUN=function(x){
    print("début") 
    return(opt_general_nm_RRMSE(x))})
  
  
  #mise en forme du tableau de resultats
  
  resultRMSE<-data.frame(t(resultRMSE0))
  resultRRMSE<-data.frame(t(resultRRMSE0))
  
  #nom de colonnes simulation
  namesnmRMSE<-c("RMSE",names(df_ini),"convergence","NM.result","message","iprint","maxfun","FtolAbs","FtolRel","XtolRel","MinfMax","warnOnly","xst1","xst2","xst3","xst4","xst5","xst6","xst7","xst8","xst9","xst10","xst11","xst12","xst13","xst14","xst15","xst16","xst17","xt1","xt2","xt3","xt4","xt5","xt6","xt7","xt8","xt9","xt10","xt11","xt12","xt13","xt14","xt15","xt16","xt17","feval")
  namesnmRRMSE<-c("RRMSE",names(df_ini),"convergence","NM.result","message","iprint","maxfun","FtolAbs","FtolRel","XtolRel","MinfMax","warnOnly","xst1","xst2","xst3","xst4","xst5","xst6","xst7","xst8","xst9","xst10","xst11","xst12","xst13","xst14","xst15","xst16","xst17","xt1","xt2","xt3","xt4","xt5","xt6","xt7","xt8","xt9","xt10","xt11","xt12","xt13","xt14","xt15","xt16","xt17","feval")
  
  colnames(resultRMSE)<-namesnmRMSE
  colnames(resultRRMSE)<-namesnmRRMSE
  
  
  
  resultatRMSE<-resultRMSE%>%
    mutate_at(.vars=vars(-message,-warnOnly),.funs=as.numeric)
  
  
  resultatRRMSE<-resultRRMSE%>%
    mutate_at(.vars=vars(-message,-warnOnly),.funs=as.numeric)
  
  
  
  #RESULT OBTAINED  RMSE
  indiceminRMSE=which.min(resultRMSE$RMSE)
  
  parametersRMSE<-resultRMSE[indiceminRMSE,2:length(df_ini)]
  
  #RESULT OBTAINED  RRMSE
  indiceminRRMSE=which.min(resultRRMSE$RRMSE)
  
  parametersRRMSE<-resultRRMSE[indiceminRRMSE,2:length(df_ini)]
  
  
  #export of data
  
  
write.csv(resultatRMSE,"Simul/2022_09_19_sim_propo_partsRMSE.csv")
write.csv(resultatRRMSE,"Simul/2022_09_19_sim_propo_partsRRMSE.csv")
  
  