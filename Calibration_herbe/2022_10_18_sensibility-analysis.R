###Analyse de sensibilité#######



#analyse de sensibilité classique : on teste des changements de paramètres de -10% et +10 % et on regarde l'impact sur le RMSE

# fonction qui calcule les RMSE/RRMSE down (-10%)/up(+10%) suite aux changements de paramètres



sensibility_test<-function(name,parameters,p_variation){
  position_parameter<-case_when(
    name== "r1"~ 1,
    name== "r2"~ 2,
    name== "r3"~ 3,
    name== "r4"~ 4,
    name== "Hmax1"~ 5,
    name== "Hmax2"~ 6,
    name== "Hmax3"~ 7,
    name== "Hmax4"~ 8,
    name== "t1"~ 9,
    name== "t2"~ 10,
    name== "t3"~ 11,
    name== "a"~ 12,
    name== "b"~ 13,
    name== "hmin"~ 14
  )
  print(position_parameter)
  par=parameters[position_parameter]
  
  par_up=(1+(p_variation/100))*par
  par_down=(1-(p_variation/100))*par
  
  #calcul avec valeurs basses
  parameters[position_parameter]=par_up
  calcul_up=general_model_propo_lin(parameters)
  RMSE_up=calcul_up[[1]]
  RRMSE_up=calcul_up[[2]]
  #calcul avec valeurs hautes
  parameters[position_parameter]=par_down
  calcul_down=general_model_propo_lin(parameters)
  RMSE_down=calcul_down[[1]]
  RRMSE_down=calcul_down[[2]]
  result_values=data.frame("Parameter"=name,"RMSE_down"=RMSE_down,"RRMSE_down"=RRMSE_down,"RMSE_up"=RMSE_up,"RRMSE_up"=RRMSE_up)
  return(result_values)
}








