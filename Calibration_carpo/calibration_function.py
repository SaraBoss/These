# -*- coding: utf-8 -*-
"""
Created on Fri Jan 13 12:38:22 2023

@author: Sara
"""
import scipy as sci
from matplotlib import pyplot as plt
import numpy as np
import pandas as pd


#In this code, we try to calibrate the value of beta. For thus, we simulate populations dynamics. Then, we build a function that makes the link between populations and potential damage and we calibrate it on the result
##Files that contains defintion of simulation fonction
execfile('D:/home/Sara/Modelo/1-Ravageurs/1-Modelisation_carpocapse/4-Simulations/Calibration_effet_poules/simul_fct.py')


##Files that contains defintion of common parameters
execfile('D:/home/Sara/Modelo/1-Ravageurs/1-Modelisation_carpocapse/4-Simulations/Calibration_effet_poules/parameters_simulations.py')



#########################################################################################################################################################
################################################################SIMULATIONS 2021#########################################################################
#########################################################################################################################################################

#########################################################################################################################################################
################################################################INSIDE THE NET###########################################################################

##Files that contains parameters for the year 2021
execfile('D:/home/Sara/Modelo/1-Ravageurs/1-Modelisation_carpocapse/4-Simulations/Calibration_effet_poules/parameters_simulations_2021.py')

execfile('D:/home/Sara/Modelo/1-Ravageurs/1-Modelisation_carpocapse/4-Simulations/Calibration_effet_poules/fruit_parameters_2021.py')


################################################################SOLEOU###################################################################################
#Inoculum
S0_2021_st=1  #hypothesis
#Poultry
vector_poultry_2021_st=[0]*365

#treatments 
d_treat_2021_st=[]

result2021_st_inside=simul_fct(S0_2021_st,year_DD_2021_st,vector_poultry_2021_st,d_treat_2021_st,alpha,neta,time_harvest_st_2021,time_harvest_st_2020)


################################################################MISTRAL##################################################################################
S0_2021_mt=3    #hypothesis
#Poultry
vector_poultry_2021_mt=[0]*365
#treatments 
d_treat_2021_mt=[]

result2021_mt_inside=simul_fct(S0_2021_mt,year_DD_2021_mt,vector_poultry_2021_mt,d_treat_2021_mt,alpha,neta,time_harvest_mt_2021,time_harvest_mt_2020)


#########################################################################################################################################################
################################################################OUTSIDE THE NET##########################################################################
##Files that contains parameters for the year 2021
execfile('D:/home/Sara/Modelo/1-Ravageurs/1-Modelisation_carpocapse/4-Simulations/Calibration_effet_poules/parameters_simulations_2021_outsidenet.py')


################################################################SOLEOU###################################################################################
#Inoculum
S0_2021_st=1  #hypothesis
#Poultry
vector_poultry_2021_st=[0]*365
#treatments 
d_treat_2021_st=[]

result2021_st_outside=simul_fct(S0_2021_st,year_DD_2021_st,vector_poultry_2021_st,d_treat_2021_st,alpha,neta,time_harvest_st_2021,time_harvest_st_2020)


################################################################MISTRAL##################################################################################
S0_2021_mt=3    #hypothesis
#Poultry
vector_poultry_2021_mt=[0]*365
#treatments 
d_treat_2021_mt=[]

result2021_mt_outside=simul_fct(S0_2021_mt,year_DD_2021_mt,vector_poultry_2021_mt,d_treat_2021_mt,alpha,neta,time_harvest_mt_2021,time_harvest_mt_2020)




#########################################################################################################################################################
################################################################SIMULATIONS 2022#########################################################################
#########################################################################################################################################################
##Files that contains parameters for the year 2021
execfile('D:/home/Sara/Modelo/1-Ravageurs/1-Modelisation_carpocapse/4-Simulations/Calibration_effet_poules/parameters_simulations_2022.py')
execfile('D:/home/Sara/Modelo/1-Ravageurs/1-Modelisation_carpocapse/4-Simulations/Calibration_effet_poules/fruit_parameters_2022.py')

execfile('D:/home/Sara/Modelo/1-Ravageurs/1-Modelisation_carpocapse/4-Simulations/Calibration_effet_poules/fruit_parameters_2022.py')

################################################################SOLEOU###################################################################################
#Inoculum
S0_2022_st=1  #hypothesis
#Poultry
vector_poultry_2022_st=[0]*365
#treatments 
d_treat_2022_st=[193]

result2022_st_inside=simul_fct(S0_2022_st,year_DD_2022_st,vector_poultry_2022_st,d_treat_2022_st,alpha,neta,time_harvest_st_2022,time_harvest_st_2021)

################################################################MISTRAL###################################################################################
S0_2022_mt=3    #hypothesis
#Poultry
vector_poultry_2022_mt=[0]*365
#treatments 
d_treat_2022_mt=[]

result2022_mt_inside=simul_fct(S0_2022_mt,year_DD_2022_mt,vector_poultry_2022_mt,d_treat_2022_mt,alpha,neta,time_harvest_mt_2022,time_harvest_mt_2021)



#########################################################################################################################################################
################################################################OUTSIDE THE NET##########################################################################
##Files that contains parameters for the year 2022
execfile('D:/home/Sara/Modelo/1-Ravageurs/1-Modelisation_carpocapse/4-Simulations/Calibration_effet_poules/parameters_simulations_2022_outsidenet.py')


################################################################SOLEOU###################################################################################
#Inoculum
S0_2022_st=1  #hypothesis
#Poultry
vector_poultry_2022_st=[0]*365
#treatments 
d_treat_2022_st=[193]

result2022_st_outside=simul_fct(S0_2022_st,year_DD_2022_st,vector_poultry_2022_st,d_treat_2022_st,alpha,neta,time_harvest_st_2022,time_harvest_st_2021)


################################################################MISTRAL##################################################################################
S0_2022_mt=3    #hypothesis
#Poultry
vector_poultry_2022_mt=[0]*365
#treatments 
d_treat_2021_mt=[]

result2022_mt_outside=simul_fct(S0_2022_mt,year_DD_2022_st,vector_poultry_2022_mt,d_treat_2022_mt,alpha,neta,time_harvest_mt_2022,time_harvest_mt_2021)


################################################################RANCE###################################################################################
#Inoculum
S0_2022_rt=1  #hypothesis
#Poultry
vector_poultry_2022_rt=[0]*365
#treatments 
d_treat_2022_rt=[]

result2022_rt_inside=simul_fct(S0_2022_rt,year_DD_2022_rt,vector_poultry_2022_rt,d_treat_2022_rt,alpha,neta,time_harvest_rt_2022,time_harvest_rt_2021)

################################################################MISTRAL###################################################################################
S0_2022_dt=1   #hypothesis
#Poultry
vector_poultry_2022_dt=[0]*365
#treatments 
d_treat_2022_dt=[]

result2022_dt_inside=simul_fct(S0_2022_dt,year_DD_2022_dt,vector_poultry_2022_dt,d_treat_2022_dt,alpha,neta,time_harvest_dt_2022,time_harvest_rt_2021)




#########################################################################################################################################################
############################################################CALIBRATION DE BETA##########################################################################
#########################################################################################################################################################


execfile('D:/home/Sara/Modelo/1-Ravageurs/1-Modelisation_carpocapse/4-Simulations/Calibration_effet_poules/fruit_parameters_2021.py')

execfile('D:/home/Sara/Modelo/1-Ravageurs/1-Modelisation_carpocapse/4-Simulations/Calibration_effet_poules/fruit_parameters_2022.py')




    ##########CALIBRATION OF BETA##########

def RMSE(simul,data):
    RMSE_result=np.sqrt(np.mean((np.array(simul)-np.array(data))**2))
    return(RMSE_result)

def RRMSE(simul,data):
    RRMSE_result=np.sqrt(np.mean(((np.array(simul)-np.array(data))/np.array(data))**2))
    return(RRMSE_result)


##################################################FUNCTION TO MINIMIZE MICHAELIS MENTEN ###########################################################

###calibration uniquement sur la récolte et sur mt et st uniquement 



def beta_calibration(beta):
    ####CALCULATION OF DAMAGES FOR G1 AND HARVEST DEPENDING ON POPULATION SIMULATION AND BETA
    dam_2021_st_simul_in=damages_mic(result2021_st_inside.newLg_tot_cum,year_DD_2021_st,time_g1_st_2021,time_harvest_st_2021,beta)
    dam_2021_mt_simul_in=damages_mic(result2021_mt_inside.newLg_tot_cum,year_DD_2021_mt,time_g1_mt_2021,time_harvest_mt_2021,beta)
    dam_2021_st_simul_out=damages_mic(result2021_st_outside.newLg_tot_cum,year_DD_2021_st,time_g1_st_2021,time_harvest_st_2021,beta)
    dam_2021_mt_simul_out=damages_mic(result2021_mt_outside.newLg_tot_cum,year_DD_2021_mt,time_g1_mt_2021,time_harvest_mt_2021,beta)
    
    dam_2022_st_simul_in=damages_mic(result2022_st_inside.newLg_tot_cum,year_DD_2022_st,time_g1_st_2022,time_harvest_st_2022,beta)
    dam_2022_mt_simul_in=damages_mic(result2022_mt_inside.newLg_tot_cum,year_DD_2022_mt,time_g1_mt_2022,time_harvest_mt_2022,beta)
    dam_2022_st_simul_out=damages_mic(result2022_st_outside.newLg_tot_cum,year_DD_2022_st,time_g1_st_2022,time_harvest_st_2022,beta)
    dam_2022_mt_simul_out=damages_mic(result2022_mt_outside.newLg_tot_cum,year_DD_2022_mt,time_g1_mt_2022,time_harvest_mt_2022,beta)
    
    # dam_2022_rt_simul_in=damages_mic(result2022_rt_inside.newLg_tot_cum,year_DD_2022_rt,time_g1_rt_2022,time_harvest_rt_2022,beta)
    # dam_2022_dt_simul_in=damages_mic(result2022_dt_inside.newLg_tot_cum,year_DD_2022_dt,time_g1_dt_2022,time_harvest_dt_2022,beta)
    
    
    #Vector of simulation results
    
    # sim_damages=dam_2021_st_simul_in+dam_2021_mt_simul_in+dam_2021_st_simul_out+dam_2021_mt_simul_out+dam_2022_st_simul_in+dam_2022_mt_simul_in+dam_2022_st_simul_out+dam_2022_mt_simul_out+dam_2022_rt_simul_in+dam_2022_dt_simul_in
    # data_damages=[mean_damage_g1_st_2021,mean_damage_recolte_st_2021,mean_damage_g1_mt_2021,mean_damage_recolte_mt_2021,mean_damage_g1_st_out_2021,mean_damage_recolte_st_out_2021,mean_damage_g1_mt_out_2021,mean_damage_recolte_mt_out_2021,mean_damage_g1_st_2022,mean_damage_recolte_st_2022,mean_damage_g1_mt_2022,mean_damage_recolte_mt_2022,mean_damage_g1_st_out_2022,mean_damage_recolte_st_out_2022,mean_damage_g1_mt_out_2022,mean_damage_recolte_mt_out_2022,mean_damage_g1_rt_2022,mean_damage_recolte_rt_2022,mean_damage_g1_dt_2022,mean_damage_recolte_dt_2022]
   
    sim_damages=[dam_2021_st_simul_in[1],dam_2021_mt_simul_in[1],dam_2021_st_simul_out[1],dam_2021_mt_simul_out[1],dam_2022_st_simul_in[1],dam_2022_mt_simul_in[1],dam_2022_st_simul_out[1],dam_2022_mt_simul_out[1]]
    data_damages=[mean_damage_recolte_st_2021,mean_damage_recolte_mt_2021,mean_damage_recolte_st_out_2021,mean_damage_recolte_mt_out_2021,mean_damage_recolte_st_2022,mean_damage_recolte_mt_2022,mean_damage_recolte_st_out_2022,mean_damage_recolte_mt_out_2022]
    
    ####CALCULATION OF DISTANCE TO REAL DATA
    RMSE_result=RMSE(sim_damages,data_damages)
    RRMSE_result=RRMSE(sim_damages,data_damages)
    return(np.array([RMSE_result,RRMSE_result]))
    


##################################################EXPLORATION###########################################################
#find value of beta 

beta_grille_wide=np.arange(0,10000,100).tolist()

RMSE_RRMSE_result_wide=[beta_calibration(0)]

for j in range(1,len(beta_grille_wide)):
    b=beta_grille_wide[j]
    RMSE_RRMSE_result_wide=np.vstack([RMSE_RRMSE_result_wide,beta_calibration(b)])
    

df_result_beta_wide=pd.DataFrame(RMSE_RRMSE_result_wide)

index_min_RMSE_wide=np.where(df_result_beta_wide[0]==np.nanmin(df_result_beta_wide[0]))
index_min_RRMSE_wide=np.where(df_result_beta_wide[1]==np.nanmin(df_result_beta_wide[1]))

beta_min_RMSE_wide=beta_grille_wide[index_min_RMSE_wide[0][0]]
beta_min_RRMSE_wide=beta_grille_wide[index_min_RRMSE_wide[0][0]]




#find value of beta precise

beta_grille=np.arange(400,600,0.001).tolist()

RMSE_RRMSE_result=[beta_calibration(0)]

for i in range(1,len(beta_grille)):
    b=beta_grille[i]
    RMSE_RRMSE_result=np.vstack([RMSE_RRMSE_result,beta_calibration(b)])

df_result_beta=pd.DataFrame(RMSE_RRMSE_result[0]df_result_hens[:,1])

index_min_RMSE=np.where(df_result_beta[0]==np.nanmin(df_result_beta[0]))
index_min_RRMSE=np.where(df_result_beta[1]==np.nanmin(df_result_beta[1]))

beta_min_RMSE=beta_grille[index_min_RMSE[0][0]]
beta_min_RRMSE=beta_grille[index_min_RRMSE[0][0]]





#Sensitivity analysis for +10/-10% for beta 
beta=beta_min_RMSE

RMSE_up_beta=beta_calibration(beta*1.1)[0]
RMSE_down_beta=beta_calibration(beta*0.9)[0]
RMSE_fin=np.nanmin(df_result_beta[0])



#plt.scatter(np.arange(1,9),sim_damages)
#plt.scatter(np.arange(1,9),data_damages)



# def lam_B_calibration(lam,B):
#     ####CALCULATION OF DAMAGES FOR G1 AND HARVEST DEPENDING ON POPULATION SIMULATION AND BETA
#     dam_2021_st_simul_in=damages_sig(result2021_st_inside.newLg_tot_julian_sum,time_g1_st_2021,time_harvest_st_2021,lam,B)
#     dam_2021_mt_simul_in=damages_sig(result2021_mt_inside.newLg_tot_julian_sum,time_g1_mt_2021,time_harvest_mt_2021,lam,B)
#     dam_2021_st_simul_out=damages_sig(result2021_st_outside.newLg_tot_julian_sum,time_g1_st_2021,time_harvest_st_2021,lam,B)
#     dam_2021_mt_simul_out=damages_sig(result2021_mt_outside.newLg_tot_julian_sum,time_g1_mt_2021,time_harvest_mt_2021,lam,B)
    
#     dam_2022_st_simul_in=damages_sig(result2022_st_inside.newLg_tot_julian_sum,time_g1_st_2022,time_harvest_st_2022,lam,B)
#     dam_2022_mt_simul_in=damages_sig(result2022_mt_inside.newLg_tot_julian_sum,time_g1_mt_2022,time_harvest_mt_2022,lam,B)
#     dam_2022_st_simul_out=damages_sig(result2022_st_outside.newLg_tot_julian_sum,time_g1_st_2022,time_harvest_st_2022,lam,B)
#     dam_2022_mt_simul_out=damages_sig(result2022_mt_outside.newLg_tot_julian_sum,time_g1_mt_2022,time_harvest_mt_2022,lam,B)
    
#     dam_2022_rt_simul_in=damages_sig(result2022_rt_inside.newLg_tot_julian_sum,time_g1_rt_2022,time_harvest_rt_2022,lam,B)
#     dam_2022_dt_simul_in=damages_sig(result2022_dt_inside.newLg_tot_julian_sum,time_g1_dt_2022,time_harvest_dt_2022,lam,B)
    
    
#     #Vector of simulation results
    
#     sim_damages=dam_2021_st_simul_in+dam_2021_mt_simul_in+dam_2021_st_simul_out+dam_2021_mt_simul_out+dam_2022_st_simul_in+dam_2022_mt_simul_in+dam_2022_st_simul_out+dam_2022_mt_simul_out+dam_2022_rt_simul_in+dam_2022_dt_simul_in
#     data_damages=[mean_damage_g1_st_2021,mean_damage_recolte_st_2021,mean_damage_g1_mt_2021,mean_damage_recolte_mt_2021,mean_damage_g1_st_out_2021,mean_damage_recolte_st_out_2021,mean_damage_g1_mt_out_2021,mean_damage_recolte_mt_out_2021,mean_damage_g1_st_2022,mean_damage_recolte_st_2022,mean_damage_g1_mt_2022,mean_damage_recolte_mt_2022,mean_damage_g1_st_out_2022,mean_damage_recolte_st_out_2022,mean_damage_g1_mt_out_2022,mean_damage_recolte_mt_out_2022,mean_damage_g1_rt_2022,mean_damage_recolte_rt_2022,mean_damage_g1_dt_2022,mean_damage_recolte_dt_2022]
    
#     ####CALCULATION OF DISTANCE TO REAL DATA
    
#     ####CALCULATION OF DISTANCE TO REAL DATA
#     RMSE_result=RMSE(sim_damages,data_damages)
    
#     return(RMSE_result)
    

# ##################################################EXPLORATION###########################################################

# lam_grille=np.random.uniform(0, 2, 10000)
# B_grille=np.random.uniform(0, 1, 10000)

# RMSE_resultv2=[]
# for i in range(0,len(lam_grille)):
#     lam=lam_grille[i]
#     B=B_grille[i]
#     RMSE_resultv2.append(lam_B_calibration(lam,B))



# np.where(RMSE_resultv2==np.nanmin(RMSE_resultv2))
# RMSE_resultv2[3854]
# lam_grille[3854]
# B_grille[3854]

# plt.scatter(np.arange(1,21),sim_damages)
# plt.scatter(np.arange(1,21),data_damages)



#########################################################################################################################################################
############################################################CALIBRATION EFFECT OF POULTRY################################################################
#########################################################################################################################################################

####DATA################

##COMMENT FAIRE LE LIEN AVEC L'ANNEE D'AVANT ????
beta=beta_min_RMSE

carpo_diapause_2021_st=max(result2021_st_inside.Lg_diapausing)
carpo_diapause_2021_mt=max(result2021_mt_inside.Lg_diapausing)


def poultry_calibration(alpha,neta):
    
    #year1 : mp and sp
    
    pop_sp_year1=simul_fct(S0_2021_st,year_DD_2021_st,sp_hens_year1,d_treat_2021_st,alpha,neta,time_harvest_st_2021,time_harvest_st_2020)
    sim_sp_year1=damages_mic(pop_sp_year1.newLg_tot_cum,year_DD_2021_st,time_g1_st_2021,time_harvest_st_2021,beta)
    
    pop_mp_year1=simul_fct(S0_2021_mt,year_DD_2021_mt,mp_hens_year1,d_treat_2021_mt,alpha,neta,time_harvest_mt_2021,time_harvest_mt_2020)
    sim_mp_year1=damages_mic(pop_mp_year1.newLg_tot_cum,year_DD_2021_mt,time_g1_mt_2021,time_harvest_mt_2021,beta)
    
    #mortality of inoculum to correspond to calibration done for the mt and st
    S0_2022_sp=max(pop_sp_year1.Lg_diapausing)/carpo_diapause_2021_st
    S0_2022_mp=max(pop_mp_year1.Lg_diapausing)/carpo_diapause_2021_mt
     
    #quel inoculum pour l'année 2 ?? très gros inoculum si repart de celui ci !!
    #year2 : mp, sp, dp, rp
    sim_sp_year2=simul_fct_damages(S0_2022_sp,year_DD_2022_st,sp_hens_year2,d_treat_2022_st,alpha,neta,time_g1_st_2022,time_harvest_st_2022,time_harvest_st_2021,beta)
    sim_mp_year2=simul_fct_damages(S0_2022_mp,year_DD_2022_mt,mp_hens_year2,d_treat_2022_mt,alpha,neta,time_g1_mt_2022,time_harvest_mt_2022,time_harvest_mt_2021,beta)
    sim_dp_year2=simul_fct_damages(S0_2022_dt,year_DD_2022_dt,dp_hens_year2,d_treat_2022_dt,alpha,neta,time_g1_dt_2022,time_harvest_dt_2022,time_harvest_dt_2021,beta)
    sim_rp_year2=simul_fct_damages(S0_2022_rt,year_DD_2022_rt,rp_hens_year2,d_treat_2022_rt,alpha,neta,time_g1_rt_2022,time_harvest_rt_2022,time_harvest_rt_2021,beta)

    sim_damages_hens=sim_sp_year1+sim_mp_year1+sim_sp_year2+sim_mp_year2+sim_dp_year2+sim_rp_year2
    data_damages_hens=[mean_damage_g1_sp_2021,mean_damage_recolte_sp_2021,mean_damage_g1_mp_2021,mean_damage_recolte_mp_2021,mean_damage_g1_sp_2022,mean_damage_recolte_sp_2022,mean_damage_g1_mp_2022,mean_damage_recolte_mp_2022,mean_damage_g1_dp_2022,mean_damage_recolte_dp_2022,mean_damage_g1_rp_2022,mean_damage_recolte_rp_2022]
    
    RMSE_result=RMSE(np.array(sim_damages_hens)*100,np.array(data_damages_hens)*100)
    RRMSE_result=RRMSE(np.array(sim_damages_hens)*100,np.array(data_damages_hens)*100)
    return(np.array([RMSE_result,RRMSE_result]))

##################################################EXPLORATION###########################################################

alpha_grille=np.random.uniform(0.0000001, 0.05, 100)
neta_grille=np.random.uniform(0.0000001, 0.05, 100)

RMSE_RRMSE_hens=[]
for i in range(0,len(alpha_grille)):
    alpha=alpha_grille[i]
    neta=neta_grille[i]
    RMSE_RRMSE_hens.append(poultry_calibration(alpha,neta))

df_result_hens=pd.DataFrame(RMSE_RRMSE_hens)

index_min_RMSE_hens=np.where(df_result_hens[0]==np.nanmin(df_result_hens[0]))
index_min_RRMSE_hens=np.where(df_result_hens[1]==np.nanmin(df_result_hens[1]))

alpha_min_RMSE=alpha_grille[index_min_RMSE_hens[0][0]]
alpha_min_RRMSE=alpha_grille[index_min_RRMSE_hens[0][0]]

neta_min_RMSE=neta_grille[index_min_RMSE_hens[0][0]]
neta_min_RRMSE=neta_grille[index_min_RRMSE_hens[0][0]]


#plt.scatter(np.arange(1,13),sim_damages)
#plt.scatter(np.arange(1,13),data_damages)

#essai_inoculum_ini=[]

#for q in range(1,100):
 #   dam=simul_fct_damages(S0_2021_st/q,year_DD_2021_st,vector_poultry_2021_st,[],alpha,neta,time_g1_st_2021,time_harvest_st_2021,time_harvest_st_2020,1412)
  #  essai_inoculum_ini.append(dam)
  
 alpha=9.638e-03
 neta=2.720e-02

 RMSE_up_alpha=poultry_calibration(alpha*1.1,neta)[0]
 
 RMSE_down_alpha=poultry_calibration(alpha*0.9,neta)[0]
 
 
 RMSE_alpha=poultry_calibration(alpha,neta)[0]
 
 
 
 RMSE_up_neta=poultry_calibration(alpha,neta*1.1)[0]
 
 RMSE_down_neta=poultry_calibration(alpha,neta*0.9)[0]
 
 
 RMSE_alpha=poultry_calibration(alpha,neta)[0]
  