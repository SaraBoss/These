# -*- coding: utf-8 -*-
"""
Created on Fri Jan 13 14:07:04 2023
PArameters that depend on the year
@author: Sara
"""


import scipy as sci
from matplotlib import pyplot as plt
import numpy as np
import pandas as pd


#########################################2022#########################################
##Import external data

##Degree-days data
filename = 'D:/home/Sara/Modelo/1-Ravageurs/1-Modelisation_carpocapse/3-Data/Degree_days/degree_days2022.txt'
year_DD_2022=np.loadtxt(filename, dtype=int)
n_week=int(np.floor(len(year_DD_2022)/7))

biofix=year_DD_2022[109]      #19 avril selon le BSV 2022 à Avignon

##Time of operations : g1 counts and harvest
t_g1_2022_=181  #30/06/2022 for mistral and soleou
t_recolte=250  #07/09/2022  for mistral

#Netting system 
net_g1=1
net_g2=1
net_recolte=1

supp_mortality=0
#S0=500

##Presence of poultry
poultry_vector=[0]*(len(year_DD_2022))
t_em=[0]*(year_DD_2022)
