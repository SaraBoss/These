
# -*- coding: utf-8 -*-
"""
Created on Fri Jan 13 14:56
Parameters that depend on the year 2022 for the outside of the net
@author: Sara
"""


import scipy as sci
from matplotlib import pyplot as plt
import numpy as np
import pandas as pd


#########################################2021 temoin not under net#########################################
##Import external data

##Degree-days data
filename = 'D:/home/Sara/Modelo/1-Ravageurs/1-Modelisation_carpocapse/3-Data/Degree_days/degree_days2022.txt'
year_DD=np.loadtxt(filename, dtype=int)
n_week=int(np.floor(len(year_DD)/7))

biofix=year_DD[109]      #19 avril selon le BSV 2022 à Avignon

##Time of operations : g1 counts and harvest
t_g1=181  #30/06/2022 for mistral and soleou
t_recolte=250  #07/09/2022  for mistral

#Netting system 
net_g1=0
net_g2=0
net_recolte=0
#This correspond to the fact that in 2021, some of the apple were removed from the plots between g1 and harvest : retrieve this number to the number of Lg1
supp_mortality=0  #to be estimated

##Presence of poultry
poultry_vector=[0]*(len(year_DD))
t_em=[0]*(year_DD)
