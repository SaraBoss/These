# -*- coding: utf-8 -*-
"""
Created on Fri Jan 13 14:24:46 2023

PARAMETERS CONCERNING FRUITS SOLEOU AND MISTRAL YEAR 2022

@author: Sara
"""

import scipy as sci
from matplotlib import pyplot as plt
import numpy as np
import pandas as pd

##########################################MISTRAL TEMOIN############################

mean_damage_g1_mt_2022=0.120     
mean_damage_recolte_mt_2022=0.528

#The time (in julian days) at which the count of G has been done
time_g1_mt_2022=181    #30 juin 2022
time_harvest_mt_2022=250     #7 septembre 2022


year_DD_2022_mt=year_DD_2022[0:(time_harvest_mt_2022+2)]    

##########################################SOLEOU TEMOIN############################

mean_damage_g1_st_2022=0.000844
mean_damage_recolte_st_2022=0.0179

#The time at which the count of G has been done
time_g1_st_2022=181      #30 juin 2022
time_harvest_st_2022=300    #27 octobre 2021

year_DD_2022_st=year_DD_2022[0:(time_harvest_st_2022+2)]    

##########################################MISTRAL TEMOIN OUTSIDE THE NET############################

mean_damage_g1_mt_out_2022=0.697  
mean_damage_recolte_mt_out_2022=0.989

#timing : same as mt

    
##########################################SOLEOU TEMOIN OUTSIDE THE NET############################

mean_damage_g1_st_out_2022=0.0622     
mean_damage_recolte_st_out_2022=0.361

#timing : same as st




##########################################RANCE TEMOIN############################

mean_damage_g1_rt_2022= 0.005969301
mean_damage_recolte_rt_2022=0.101

#The time (in julian days) at which the count of G has been done
time_g1_rt_2022=179   #28 juin 2022
time_harvest_rt_2022=279   #6 octobre 2022
time_harvest_rt_2021=284   #11 octobre 2021


year_DD_2022_rt=year_DD_2022[0:(time_harvest_rt_2022+2)]    
    
##########################################DURANCE TEMOIN############################

mean_damage_g1_dt_2022=0.0369
mean_damage_recolte_dt_2022=0.07  #problem : lot of incertain fruits : damage between 1% and 14% : choose a mean at 7%

#The time at which the count of G has been done
time_g1_dt_2022=181      #30 juin 2022
time_harvest_dt_2022=293    #20 octobre 2022
time_harvest_dt_2021=300   #27 octobre 2021


year_DD_2022_dt=year_DD_2022[0:(time_harvest_dt_2022+2)]  


##########################################VECTOR OF HENS############################

sp_hens_year2=np.loadtxt('D:/home/Sara/Modelo/1-Ravageurs/1-Modelisation_carpocapse/3-Data/Hens/sp_hens_year2.txt', dtype=int)
mp_hens_year2=np.loadtxt('D:/home/Sara/Modelo/1-Ravageurs/1-Modelisation_carpocapse/3-Data/Hens/mp_hens_year2.txt', dtype=int)

dp_hens_year2=np.loadtxt('D:/home/Sara/Modelo/1-Ravageurs/1-Modelisation_carpocapse/3-Data/Hens/dp_hens_year2.txt', dtype=int)
rp_hens_year2=np.loadtxt('D:/home/Sara/Modelo/1-Ravageurs/1-Modelisation_carpocapse/3-Data/Hens/rp_hens_year2.txt', dtype=int)

###MISTRAL###
mean_damage_g1_mp_2022=0.0680
mean_damage_recolte_mp_2022=0.22

##SOLEOU###
mean_damage_g1_sp_2022=0.0022
mean_damage_recolte_sp_2022=0.0147

###DURANCE###
mean_damage_g1_dp_2022=0.114
mean_damage_recolte_dp_2022=0.191

###RANCE###

mean_damage_g1_rp_2022=0.00715
mean_damage_recolte_rp_2022=0.28

