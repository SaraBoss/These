# -*- coding: utf-8 -*-
"""
Created on Mon Jan 16 17:39:28 2023

@author: Sara
"""

#define simulation function

from matplotlib import pyplot as plt
import numpy as np
import datetime
import pandas as pd

#Importation of the model used

execfile('D:/home/Sara/Modelo/0-Ateliers_codes/Newactual/Rshiny_codev2.py')

#function that takes in arguments the entry of the model and gives back a number of individuals

def simul_fct(S0ini,year_DD,vector_poultry,date_treatments_julian,alpha_hens_summer,neta_hens_winter,t_harvest,t_harvest_n_1):
    

    
    max=int(year_DD[len(year_DD)-1])
    date_treatments_DD_new=treatments_newLg(date_treatments_julian,year_DD,mort_CpV_max)
    date_treatments_DD_old=treatments_oldLg(date_treatments_julian,year_DD,mort_CpV_max)
    #vector of impact of poultry between the 21st march and the harvest : consumption depending on the charging (//treatment)
    impact_poultry_vector_summer_DD=i_hens_summer_DD(vector_poultry,year_DD,alpha,t_harvest_n_1)
    
    #impact of poultry between the harvest and the 21st march : decrease the mortality of codling moth
    m0=m0bis-i_hens_winter_DD(vector_poultry,date_end_impact_hens_winter,neta,t_harvest_n_1)
    if m0<0:
        m0=0
  
    
    
    result=Pop_cd(S0ini,m0)
    #on commence les simulations à t=2 ?
    
    for i in range(1,max):
        result.emerging_proba(i)
    
    for i in range(1,max):
        result.population_larvae_diapausis_ini(i,year_DD)
    
    
    for i in range(1,max):
        result.survival_proba(i)
        
    for i in range(1,max):
        result.population_Ag1(i)
    
    
    
    for i in range(1,max):
        result.oviposition_vector(i,net_g1,net_g2,net_recolte)
        
    for i in range(1,max):
        result.development_vector(i)
          
        
    for i in range(1,max):
        result.population_Eg1(i)    

    
    for i in range(1,max):
        result.population_Lg1(i,date_treatments_DD_new,date_treatments_DD_old,year_DD,impact_poultry_vector_summer_DD)
        
    for i in range(1,max):
        result.population_Ag2(i)
        
    for i in range(1,max):
        result.population_Eg2(i)
        
    for i in range(1,max):
        result.population_Lg2(i,date_treatments_DD_new,date_treatments_DD_old,year_DD,impact_poultry_vector_summer_DD)
        
    
    for i in range(1,max):
        result.population_Ag3(i)
    
    for i in range(1,max):
        result.population_Eg3(i)
        
    for i in range(1,max):
        result.population_Lg3(i,date_treatments_DD_new,date_treatments_DD_old,year_DD,impact_poultry_vector_summer_DD)

        
    result.population_Ag_julian(year_DD)
    result.new_Lg_tot(year_DD)
    
    
    return(result)



#function that takes in arguments the entry of the model and gives back tha damages

def simul_fct_damages(S0ini,year_DD,vector_poultry,date_treatments_julian,alpha_hens_summer,neta_hens_winter,t_g1,t_harvest,t_harvest_n_1,beta):
    total_result=simul_fct(S0ini,year_DD,vector_poultry,date_treatments_julian,alpha_hens_summer,neta_hens_winter,t_harvest,t_harvest_n_1)
    damages_tot=damages_mic(total_result.newLg_tot_cum,year_DD,t_g1,t_harvest,beta)
    return(damages_tot)

##########DAMAGE FUNCTIONS##########

#MICHAELIS AND MENTEN FUNCTION
def damages_mic(vector_newLg_cum,year_DD,time_g1,time_recolte,beta):
    index_timeg1=year_DD[time_g1-1]
    index_timerecolte=year_DD[time_recolte-1]
    
    damages_g1=vector_newLg_cum[index_timeg1]/(vector_newLg_cum[index_timeg1]+beta)
    damages_recolte=vector_newLg_cum[index_timerecolte]/(vector_newLg_cum[index_timerecolte]+beta)
    return([damages_g1,damages_recolte])
    
    
    
#SIGMOIDE FUNCTION
def damages_sig(vector_newLg, time_g1,time_recolte,lam,B):
    p_g1=vector_newLg[time_g1-1]
    p_recolte=vector_newLg[time_recolte-1]
    
    
    damages_g1=np.exp(lam*p_g1)/(1+B*np.exp(lam*p_g1))/1000
    damages_recolte=np.exp(lam*p_recolte)/(1+B*np.exp(lam*p_recolte))/1000
    return([damages_g1,damages_recolte])

    
    
    
    
    
    
    
    
    
    
    
    