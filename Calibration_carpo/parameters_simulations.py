# -*- coding: utf-8 -*-
"""
Created on Fri Jan 13 13:56:15 2023
Common parameters for all simulations, not depending on the year concerned
@author: Sara
"""


import scipy as sci
from matplotlib import pyplot as plt
import numpy as np
import pandas as pd


##parameters that can change :
    
#vector of date of treatments in julian days
date_treatments_julian=[124,138,152,166,180,194,208]

#half life of treatment (in days) of the impact of the treatment : 2 and 3 days according to Arthurs 2004
half_life_CpV=3 
    
    
    
#Code#

##Import external data


###Related articles###
# Boivin 2025 : Modelling the interactions between phenology and insecticide resistance genes in the codling moth Cydia pomonella
# Graf 2018

###Parameters names####
# degree_days (called DD): number of Degree Days accumulated since january of year X
# sex : sex of the individu ("F" pour Female and "M" for Male)
# biofix (in DD) : the number of DD at which 50% of the trap are
# A_fem and A_mal : values for gompertz function for survival rate for female (fem) and male (mal)
# G_fem and G_mal : values for gompertz function for survival rate for female (fem) and male (mal)


####Initialisation of parameters####
# Separation between the name of the variables and the parameters needed for functions


sd_biofix_2005 = 118

A_fem = 0.0023  # Jones 2008
G_fem = 0.0444  # Jones 2008
A_mal = 0.0028  # Jones 2008
G_mal = 0.0486  # Jones 2008
SR = 1.21  # Sex ratio for female (female/total) at emergence (Graf 2018)


#Reproduction parameters
m_g1 = 55.26  # Boivin 2005 on SvSv population
c_g1 = 0.41  # Boivin 2005 on SvSv population
b_g1 = 65  # Boivin 2005 on SvSv population
m_g2_g3 = 56.78  # Boivin 2005 on SvSv population
b_g2_g3 = 45  # Boivin 2005 on SvSv population
c_g2_g3 = 0.15  # Boivin 2005 on SvSv population
# N_eggs = 267.67  # Value to be defined (see Boivin 2005 and Graf 2018)
#N_factor=40 #Blomefield 2012
N_factor_g1=4015.12 #Boivin 2005, corresponds to sum of eggs : 15*total number laid 267.67 
N_factor_g2_g3=971.57  #Boivin 2005, corresponds to sum of eggs : 15*total number laid 64 
fecondity=91.6  #Sauphanor 2012
fecondity_net=60  #Beneat 2022 : number of eggs laid by female under netting systems
#Developpmental parameters
alph_egg=0.01104 #Boivin 2005
kap_egg=26 #Boivin 2005
alph_preimag=0.0021 #Boivin 2005 larvae to adult
kap_preimag=40 #Boivin 2005A larvae to adult
alph_larvae=0.00332 #Boivin 2005 larvae to L5
kap_larvae=50 #Boivin 2005

#Diapausis parameters 
CD50=216.07 #Boivin 2005 
#CD50 corresponds to the Julian date of exposure of neonates to natural photoperiod for which diapause was induced in 50% of larvae
u=0.34 #Boivin 2005


#make the model quantitative : 
#S0 is the initial number of diapausis cydia pomonella : should correspond to the number of 5th instar larvae from previous year if we use m0bis
#S0=500
#m0 is the mortality of diapausing larvae (not dynamical)
m0nat=0.19   #Graf 2018 but controlled conditions
m0bis=0.80 #Wearing 1974 (field conditions)
#date at which consider that poultry don't have impact on the stock of diapausis larvae because start of emergence
date_end_impact_hens_winter=80       #hypothesis 21 march ?

#mortality of eggs g1 and g2-g3
#mort_egg_g1=0.082  #Blomefield 2012
mort_egg_g1=0.25    #Brown 1978, Ferro 1975, Westigard 1976
mort_egg_g2_g3=0.212     #Blomefield 2012

#mortality of larvae g1 and g2-g3
mort_larv_g1=0.333 #Blomefield 2012
mort_larv_g2_g3=0.325  #Blomefield 2012


#Parameters concerning damage estimation
# ck=0.0001248747   #extrapolated from Ricci 2014
# alpha_1=0.08
#beta=1.2


#Parameters concerning trapping efficiency
#beta=0.50

#mortality induced by treatments : à redéfinir
mort_CpV_max=0.80


#impact of poultry 

alpha=0.0001
neta=0.0001


