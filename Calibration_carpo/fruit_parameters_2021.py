# -*- coding: utf-8 -*-
"""
Created on Fri Jan 13 14:24:46 2023

PARAMETERS CONCERNING FRUITS SOLEOU AND MISTRAL YEAR 2021

@author: Sara
"""

import scipy as sci
from matplotlib import pyplot as plt
import numpy as np
import pandas as pd

##########################################MISTRAL TEMOIN############################

mean_damage_g1_mt_2021=0.486
mean_damage_recolte_mt_2021=0.751

#The time (in julian days) at which the count of G has been done

time_g1_mt_2021=203    #22 juillet 2021
time_harvest_mt_2021=260     #17 septembre 2021
time_harvest_mt_2020=260     #hypothesis because not entry into fructification (doesn't matter for the result)



year_DD_2021_mt=year_DD_2021[0:(time_harvest_mt_2021+2)]
##########################################SOLEOU TEMOIN############################

mean_damage_g1_st_2021=0.301
mean_damage_recolte_st_2021=0.506

#The time at which the count of G has been done
time_g1_st_2021=202      #21 juillet 2021
time_harvest_st_2021=281    #8 octobre 2021
time_harvest_st_2020=281    #hypothesis because not entry into fructification (doesn't matter for the result)


year_DD_2021_st=year_DD_2021[0:(time_harvest_st_2021+2)]
##########################################MISTRAL TEMOIN OUTSIDE THE NET############################

mean_damage_g1_mt_out_2021=0.91
mean_damage_recolte_mt_out_2021=0.93

#harvest and g1 identical to mt

    
##########################################SOLEOU TEMOIN OUTSIDE THE NET############################

mean_damage_g1_st_out_2021=0.299
mean_damage_recolte_st_out_2021=0.657        

#harvest and g1 identical to st

##########################################VECTOR OF HENS############################

sp_hens_year1=np.loadtxt('D:/home/Sara/Modelo/1-Ravageurs/1-Modelisation_carpocapse/3-Data/Hens/sp_hens_year1.txt', dtype=int)
mp_hens_year1=np.loadtxt('D:/home/Sara/Modelo/1-Ravageurs/1-Modelisation_carpocapse/3-Data/Hens/mp_hens_year1.txt', dtype=int)

###MISTRAL###
mean_damage_g1_mp_2021=0.470
mean_damage_recolte_mp_2021=0.664

###SOLEOU###
mean_damage_g1_sp_2021=0.319
mean_damage_recolte_sp_2021=0.510


