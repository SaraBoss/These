# -*- coding: utf-8 -*-
"""
Created on Wed Jan 18 15:39:52 2023

@author: Sara
"""
"""
""""""""""""""""""CODE USED IN RSHINY""""""""""""""""""
"""
###########LIBRARY USED############################################

import scipy as sci
from matplotlib import pyplot as plt
import numpy as np
import pandas as pd

###Parameters names####
# degree_days (called DD): number of Degree Days accumulated since january of year X
# sex : sex of the individu ("F" pour Female and "M" for Male)
# biofix (in DD) : the number of DD at which 50% of the trap are
# A_fem and A_mal : values for gompertz function for survival rate for female (fem) and male (mal)
# G_fem and G_mal : values for gompertz function for survival rate for female (fem) and male (mal)


####Initialisation of parameters####F
# Separation between the name of the variables and the parameters needed for functions

sd_biofix_2005 = 118.71

A_fem = 0.0023  # Jones 2008
G_fem = 0.0444  # Jones 2008
A_mal = 0.0028  # Jones 2008
G_mal = 0.0486  # Jones 2008
SR = 1.21  # Sex ratio for female (female/total) at emergence (Graf 2018)


#Reproduction parameters
m_g1 = 55.26  # Boivin 2005 on SvSv population
b_g1 = 65  # Boivin 2005 on SvSv population
c_g1 = 0.41  # Boivin 2005 on SvSv population
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


#Parameters concerning damage estimation : not used 
# ck=0.0001248747   #extrapolated from Ricci 2014
# alpha_1=0.08
#beta=1.2


#Parameters concerning trapping efficiency
#beta=0.50

#mortality induced by treatments : à redéfinir
mort_CpV_max=0.80
#half life of treatment (in days) of the impact of the treatment : 2 and 3 days according to Arthurs 2004
half_life_CpV=3 
    


#impact of poultry 

#alpha=0.0001
#neta=0.0001

#make the model quantitative : 
#S0 is the initial number of diapausis cydia pomonella : should correspond to the number of 5th instar larvae from previous year if we use m0bis
#S0=500




##############################################################################################################################################################################
######################################################################################################CODE####################################################################
##############################################################################################################################################################################
####Fonctions needed for the simulation####

"""Basic Maths function

"""

def factorial(n):
    num = 1
    while n >= 1:
        num = num * n
        n = n - 1
    return num

#function that allows to convert DD into a julian day 
#input : vector_DD : vector of DD per julian day, time_DD : a date in cumulated DD
#output : return the indice of the vector of DD, that hence corresponds to the julian date

def find_closest_JD(vector_DD,time_DD):
    result=1
    for i in range(0, (len(vector_DD)-1)):
        if (time_DD>vector_DD[i])&(time_DD<=vector_DD[i+1]):
            result = i+2
    return result


""" Function 
"""

def emergence_g1(t_DD, biofix, sd_biofix):
         p_emerg=(1/(sd_biofix*np.sqrt(2*np.pi)))*np.exp(-(t_DD-biofix-2*sd_biofix)**2/(2*sd_biofix**2))
         return p_emerg

        
"""Simulation code"""


class Pop_cd:
    def __init__(self,S0,m0):
        self.S0_diapausis=[S0]
        self.S0_diapausis_dead=[m0*self.S0_diapausis[0]]
        self.S0_diapausis_alive=[(1-m0)*self.S0_diapausis[0]]
        self.p_emerg_g1=[0]
        self.p_emerg_cumu_g1=[0]
        self.Ag1_fem = [0]
        self.Ag1_mal = [0]
        self.pyramid_age_Ag1_fem=[[0]]
        self.pyramid_age_Ag1_mal=[[0]]
        self.survival_Ag1_fem=[1-A_fem]
        self.survival_Ag1_mal=[1-A_mal]
        
        #fertility g1, g2 and g3
        self.fertility_g1=[0]
        self.fertility_g2=[0]
        self.fertility_g3=[0]
        
        
        self.Eg1 = [0]
        self.vec_dvt_Egg=[0]
        self.vec_dvt_Egg_cumu=[0]
        self.pyramid_age_Eg1=[0]

        
        self.pyramid_age_Lg1_nondia=[0]
        self.pyramid_age_Lg1_dia=[0]
        self.Lg1=[0]
        self.vec_dvt_Lg=[0]
        self.vec_dvt_Lg_cumu=[0]
        
   
        self.pyramid_age_Ag2_fem=[0]
        self.pyramid_age_Ag2_mal=[0]
        self.Ag2_fem=[0]
        self.Ag2_mal=[0]
        self.newAg2=[0]
        
        self.pyramid_age_Eg2=[0]
        self.Eg2=[0]
        
        self.pyramid_age_Lg2_dia=[0]
        self.pyramid_age_Lg2_nondia=[0]
        self.Lg2=[0]


        self.pyramid_age_Ag3_fem=[0]
        self.pyramid_age_Ag3_mal=[0]
        self.Ag3_fem=[0]
        self.Ag3_mal=[0]
    
        
        self.pyramid_age_Eg3=[0]
        self.Eg3=[0]
        
        
        self.pyramid_age_Lg3=[0]
        self.Lg3_nondiapausing=[0]
        self.Lg3_diapausing=[0]
        self.Lg3=[0]  
        self.pyramid_age_Lg3_dia=[0]
        self.pyramid_age_Lg3_nondia=[0]
        self.vec_dvt_L5=[0]
        self.vec_dvt_L5_cumu=[0]
        self.pyramid_age_L5=[0]
        self.Lg3_L5=[0]
        self.Lg3_L5_dia=[0]
        self.Lg3_L5_nondia=[0]
        
        self.newAg1=[0]
        self.Lg_diapausing=[0]
        self.newLg1=[0]
        self.newegg1=[0]
        self.newAg2_tot=[0]
        self.newclutchg2=[0]
        self.newLg2=[0]
        self.newAg3=[0]
        self.newEg3=[0]
        self.newLg3_nymp=[0]
        self.newLg3_diapausis=[0]
        self.newLg3_tot=[0]
        
        
        self.Ag1_mal_julian=[0]
        self.Ag2_mal_julian=[0]
        self.Ag3_mal_julian=[0]
        self.Ag_tot_mal_julian=[0]
        
        self.Ag_mal_week_trap=[0]
        self.damage_tot=[0]
        self.newLg1_julian=[0]
        self.newLg2_julian=[0]
        self.newLg3_julian=[0]
        self.newLg_tot=[0]
        self.newLg_tot_cum=[0]
        
        
        
#calculation of population distribution
#cumulated probability of emergence
            
        
    """function of general dynamics"""
    def cydia_dynamics_DD(self, t_DD):
        self.population_larvae_diapausis_ini(t_DD)
        self.population_Ag1(t_DD)
        self.population_Eg1(t_DD)
        self.larvae_g1_dynamics(t_DD)
        self.preim_g1_dynamics(t_DD)
        self.adult_g2_dynamics(t_DD)

    
    """function of dynamics for each time"""
   #emergence probability from diapausis cumulated and not cumulated 
    def emerging_proba(self,t_DD):
            self.p_emerg_g1.append(emergence_g1(t_DD, biofix, sd_biofix_2005))
            self.p_emerg_cumu_g1.append((np.cumsum(self.p_emerg_g1)[t_DD]))

    
    def population_larvae_diapausis_ini(self, t_DD,year_DD):
        t=find_closest_JD(year_DD, t_DD-1)
        self.S0_diapausis_dead.append(self.S0_diapausis_dead[t_DD-1])
        self.S0_diapausis_alive.append(self.S0_diapausis_alive[t_DD-1]-self.S0_diapausis_alive[0]*self.p_emerg_g1[t_DD-1])
        self.S0_diapausis.append(self.S0_diapausis_alive[t_DD]+self.S0_diapausis_dead[t_DD])
        
    #survival probability at each time for Ag1 female and male
    def survival_proba(self,t_DD):
        self.survival_Ag1_fem.append(1-mortality_density(t_DD, A_fem, G_fem))
        self.survival_Ag1_mal.append(1-mortality_density(t_DD, A_mal, G_mal))

        
    #Function that gives the Ag1 population aged from 0 to t_DD-1 DD at time t_DD
    #Depends on : t_DD, biofix, sd_biofix, SR, A, G, 
    #Returns: the pyramid of age for Ag1 female and male, and the total population for female and male for Ag1
    
    def population_Ag1(self,t_DD):
            #Ag1 population aged from 1 to t_DD-1 DD
            vec_old_fem=np.multiply(self.pyramid_age_Ag1_fem[t_DD-1],self.survival_Ag1_fem[0:t_DD-1])
            vec_old_mal=np.multiply(self.pyramid_age_Ag1_mal[t_DD-1],self.survival_Ag1_mal[0:t_DD-1])
            #Ag1 population aged 0 DD (just emerging)
            emerging_pop=(self.S0_diapausis_alive[0]*self.p_emerg_g1[t_DD-1])
            #Vector giving the number of Ag1 of age 0 to t_DD-1 at time t_DD
            vec_total_fem=np.append(SR/(1+SR)*emerging_pop,vec_old_fem)
            vec_total_mal=np.append(1/(1+SR)*emerging_pop,vec_old_mal)
      
            self.pyramid_age_Ag1_fem.append(vec_total_fem)
            self.pyramid_age_Ag1_mal.append(vec_total_mal)
            self.Ag1_fem.append(sum(self.pyramid_age_Ag1_fem[t_DD]))
            self.Ag1_mal.append((sum(self.pyramid_age_Ag1_mal[t_DD])))
            
            self.newAg1.append(emerging_pop)
            
  

    #function to calculate the oviposition vector: fertility is a vector of length t_DD+1 that gives the mean number of eggs laid for female at the age 0, 1  ... until t_DD
    def oviposition_vector(self,t_DD,net_g1,net_g2,net_recolte):
        self.fertility_g1.append(oviposition(t_DD, m_g1, b_g1, c_g1,N_factor_g1,net_g1))
        self.fertility_g2.append(oviposition(t_DD,m_g2_g3,b_g2_g3,c_g2_g3,N_factor_g2_g3,net_g2))
        self.fertility_g3.append(oviposition(t_DD,m_g2_g3,b_g2_g3,c_g2_g3,N_factor_g2_g3,net_recolte))
        
        
    #Function that calculates the developmental vectors for egg, for larvae and pre-imag
    def development_vector(self,t_DD):
        self.vec_dvt_Egg.append(development(t_DD,alph_egg,kap_egg))
        self.vec_dvt_Egg_cumu.append(self.vec_dvt_Egg_cumu[t_DD-1]+development(t_DD,alph_egg,kap_egg))
        
        self.vec_dvt_Lg.append(development(t_DD, alph_preimag, kap_preimag))
        self.vec_dvt_Lg_cumu.append(self.vec_dvt_Lg_cumu[t_DD-1]+development(t_DD,alph_preimag,kap_preimag))
        
        self.vec_dvt_L5.append(development(t_DD, alph_larvae, kap_larvae))
        self.vec_dvt_L5_cumu.append(self.vec_dvt_L5_cumu[t_DD-1]+development(t_DD,alph_larvae, kap_larvae))
        
        
        
        
    #Function that gives the number of new eggs laid at each time
    def population_Eg1(self,t_DD):
        #new clutch at time t_DD
        new_clutch=sum(np.multiply(self.pyramid_age_Ag1_fem[t_DD-1],self.fertility_g1[0:t_DD-1]))

        #Eg1 population aged from 1 to t_DD-1 DD
        vec_previous=np.multiply(self.pyramid_age_Eg1[t_DD-1],np.repeat(1.0,t_DD-1)-self.vec_dvt_Egg_cumu[0:t_DD-1])
        #Calculation of the pyramid of age    
        vec_total=np.append(new_clutch,vec_previous)             
        self.pyramid_age_Eg1.append(vec_total)
        #Calculation of the total of Egg
        self.Eg1.append(sum(self.pyramid_age_Eg1[t_DD]))
        self.newegg1.append(new_clutch)
    


    def population_Lg1(self,t_DD,d_treat_newLg,d_treat_oldLg,year_DD,intake_hens):
        
        #impacts of treatment and intake of larvae by hens : creates mortality
        treat_intake_old=(1-d_treat_oldLg[t_DD-1]-intake_hens[t_DD-1])
        treat_intake_new=(1-d_treat_newLg[t_DD-1]-intake_hens[t_DD-1])
        
        #to correct the intensity of intake depending on charging 
        if (treat_intake_old<=0):
          treat_intake_old=0
        if (treat_intake_new<=0):
          treat_intake_new=0
        
        #neonates for which we considere egg unviability, treatment and intake by hens
        new_larvae=sum(np.multiply(self.pyramid_age_Eg1[t_DD-1],self.vec_dvt_Egg_cumu[0:t_DD-1]))*(1-mort_egg_g1)*treat_intake_new
        
        if new_larvae<=0:
            new_larvae=0
            
        #diapause induction depends on date at which neonate appear 
        #find julian date corresponding to date in degree days
        t=find_closest_JD(year_DD,t_DD)
        #diapausing neonate
        new_larvae_diapausing=new_larvae*diapausis(t)
        #nymphosing neonate
        new_larvae_nondia=new_larvae*(1-diapausis(t))
        
        #older larvae for which we considere egg unviability, treatment and intake by hens
        vec_previous_larvae_dia=np.array(np.multiply(self.pyramid_age_Lg1_dia[t_DD-1],np.repeat(1.0,t_DD-1)))*treat_intake_old
        vec_previous_larvae_nondia=np.array(np.multiply(self.pyramid_age_Lg1_nondia[t_DD-1],np.repeat(1.0,t_DD-1)-self.vec_dvt_Lg_cumu[0:t_DD-1]))*treat_intake_old
        
        #pyramid of age for larvae diapausing
        self.pyramid_age_Lg1_dia.append(np.append(new_larvae_diapausing,vec_previous_larvae_dia)) 
        #pyramid of age for larvae non diapausing
        self.pyramid_age_Lg1_nondia.append(np.append(new_larvae_nondia,vec_previous_larvae_nondia))
      
        #total number of Lg1
        self.Lg1.append(sum(self.pyramid_age_Lg1_nondia[t_DD])+sum(self.pyramid_age_Lg1_dia[t_DD]))
        self.newLg1.append(new_larvae_diapausing+new_larvae_nondia)
       
        
    
    def population_Ag2(self,t_DD):
        
        survival=(1-mort_larv_g1-supp_mortality)
        if (survival<=0):
            survival=0
        #Ag2 population aged 0 DD (just emerging)
        new_Ag2=sum(np.multiply(self.pyramid_age_Lg1_nondia[t_DD-1],self.vec_dvt_Lg_cumu[0:t_DD-1]))*survival
        
        
        #Vector giving the number of Ag2 of age 0 to t_DD-1 at time t_DD
        vec_previous_Ag2_fem=np.multiply(self.pyramid_age_Ag2_fem[t_DD-1],self.survival_Ag1_fem[0:t_DD-1])
        vec_previous_Ag2_mal=np.multiply(self.pyramid_age_Ag2_mal[t_DD-1],self.survival_Ag1_mal[0:t_DD-1])
        
        #vector giving the pyramid of age for Ag2
        vec_total_fem=np.append(SR/(1+SR)*new_Ag2,vec_previous_Ag2_fem)
        vec_total_mal=np.append(1/(1+SR)*new_Ag2,vec_previous_Ag2_mal)
        

        #Pyramid of age for Ag2
        self.pyramid_age_Ag2_fem.append(vec_total_fem)
        self.pyramid_age_Ag2_mal.append(vec_total_mal)
        
        #Population of Ag2
        self.Ag2_fem.append(sum(self.pyramid_age_Ag2_fem[t_DD]))       
        self.Ag2_mal.append(sum(self.pyramid_age_Ag2_mal[t_DD]))
        self.newAg2_tot.append(new_Ag2)
        

    def population_Eg2(self,t_DD):
        #new clutch at time t_DD
        new_clutch=sum(np.multiply(self.pyramid_age_Ag2_fem[t_DD-1],self.fertility_g2[0:t_DD-1]))

        #Eg1 population aged from 1 to t_DD-1 DD
        vec_previous=np.multiply(self.pyramid_age_Eg2[t_DD-1],np.repeat(1.0,t_DD-1)-self.vec_dvt_Egg_cumu[0:t_DD-1])
        
        #Calculation of the pyramid of age    
        vec_total=np.append(new_clutch,vec_previous)             
        self.pyramid_age_Eg2.append(vec_total)
        #Calculation of the total of Egg
        self.Eg2.append(sum(self.pyramid_age_Eg2[t_DD]))
        self.newclutchg2.append(new_clutch)

    def population_Lg2(self,t_DD,d_treat_newLg,d_treat_oldLg,year_DD,intake_hens):
        #impacts of treatment and intake of larvae by hens : creates mortality
        treat_intake_old=(1-d_treat_oldLg[t_DD-1]-intake_hens[t_DD-1])
        treat_intake_new=(1-d_treat_newLg[t_DD-1]-intake_hens[t_DD-1])
        
        #to correct the intensity of intake depending on charging 
        if (treat_intake_old<=0):
          treat_intake_old=0
        if (treat_intake_new<=0):
          treat_intake_new=0
      
        new_larvae=sum(np.multiply(self.pyramid_age_Eg2[t_DD-1],self.vec_dvt_Egg_cumu[0:t_DD-1]))*(1-mort_egg_g2_g3)*treat_intake_new
        
        if new_larvae<=0:
            new_larvae=0
            
        #diapause induction depends on date at which neonate appear 
        #find julian date corresponding to date in degree days
        t=find_closest_JD(year_DD,t_DD)
        #diapausing neonate
        new_larvae_diapausing=new_larvae*diapausis(t)
        #nymphosing neonate
        new_larvae_nondia=new_larvae*(1-diapausis(t))
        
        #older larvae for which we considere egg unviability, treatment and intake by hens
        vec_previous_larvae_dia=np.array(np.multiply(self.pyramid_age_Lg2_dia[t_DD-1],np.repeat(1.0,t_DD-1)))*treat_intake_old
        vec_previous_larvae_nondia=np.array(np.multiply(self.pyramid_age_Lg2_nondia[t_DD-1],np.repeat(1.0,t_DD-1)-self.vec_dvt_Lg_cumu[0:t_DD-1]))*treat_intake_old
        
        #pyramid of age for larvae diapausing
        self.pyramid_age_Lg2_dia.append(np.append(new_larvae_diapausing,vec_previous_larvae_dia)) 
        #pyramid of age for larvae non diapausing
        self.pyramid_age_Lg2_nondia.append(np.append(new_larvae_nondia,vec_previous_larvae_nondia))
      
        #total number of Lg2
        self.Lg2.append(sum(self.pyramid_age_Lg2_nondia[t_DD])+sum(self.pyramid_age_Lg2_dia[t_DD]))
        self.newLg2.append(new_larvae_diapausing+new_larvae_nondia)
     
        
        
        
        
        
        
    
    def population_Ag3(self,t_DD):
        #Ag3 population aged 0 DD (just emerging)
        new_Ag3=sum(np.multiply(self.pyramid_age_Lg2_nondia[t_DD-1],self.vec_dvt_Lg_cumu[0:t_DD-1]))*(1-mort_larv_g2_g3)
        
        
        #Vector giving the number of Ag2 of age 0 to t_DD-1 at time t_DD
        vec_previous_Ag3_fem=np.multiply(self.pyramid_age_Ag3_fem[t_DD-1],self.survival_Ag1_fem[0:t_DD-1])
        vec_previous_Ag3_mal=np.multiply(self.pyramid_age_Ag3_mal[t_DD-1],self.survival_Ag1_mal[0:t_DD-1])
        
        #vector giving the pyramid of age for Ag3
        vec_total_fem=np.append(SR/(1+SR)*new_Ag3,vec_previous_Ag3_fem)
        vec_total_mal=np.append(1/(1+SR)*new_Ag3,vec_previous_Ag3_mal)
        

        #Pyramid of age for Ag2
        self.pyramid_age_Ag3_fem.append(vec_total_fem)
        self.pyramid_age_Ag3_mal.append(vec_total_mal)
        
        #Population of Ag2
        self.Ag3_fem.append(sum(self.pyramid_age_Ag3_fem[t_DD]))       
        self.Ag3_mal.append(sum(self.pyramid_age_Ag3_mal[t_DD]))
        self.newAg3.append(new_Ag3)
        

    def population_Eg3(self,t_DD):
        #new clutch at time t_DD
        new_clutch=sum(np.multiply(self.pyramid_age_Ag3_fem[t_DD-1],self.fertility_g3[0:t_DD-1]))

        #Eg1 population aged from 1 to t_DD-1 DD
        vec_previous=np.multiply(self.pyramid_age_Eg3[t_DD-1],np.repeat(1.0,t_DD-1)-self.vec_dvt_Egg_cumu[0:t_DD-1])
        
        #Calculation of the pyramid of age    
        vec_total=np.append(new_clutch,vec_previous)             
        self.pyramid_age_Eg3.append(vec_total)
        #Calculation of the total of Egg
        self.Eg3.append(sum(self.pyramid_age_Eg3[t_DD]))
        self.newEg3.append(new_clutch)
    

    def population_Lg3(self,t_DD,d_treat_newLg,d_treat_oldLg,year_DD,intake_hens):
        #impacts of treatment and intake of larvae by hens : creates mortality
        treat_intake_old=(1-d_treat_oldLg[t_DD-1]-intake_hens[t_DD-1])
        treat_intake_new=(1-d_treat_newLg[t_DD-1]-intake_hens[t_DD-1])
        
        #to correct the intensity of intake depending on charging 
        if (treat_intake_old<=0):
          treat_intake_old=0
        if (treat_intake_new<=0):
          treat_intake_new=0
        
        
        new_larvae=sum(np.multiply(self.pyramid_age_Eg3[t_DD-1],self.vec_dvt_Egg_cumu[0:t_DD-1]))*(1-mort_egg_g2_g3)*treat_intake_new
        
        if new_larvae<=0:
            new_larvae=0
        
        """Diapausing"""
        #larvae going to diapause
        t=find_closest_JD(year_DD,t_DD)
        #larvae going to diapausis
        new_larvae_L3_dia=new_larvae*diapausis(t)
        #larvae not going to diapausis
        new_larvae_L3_nondia=new_larvae*(1-diapausis(t))
        
        
        vec_previous_larvae_dia=np.multiply(self.pyramid_age_Lg3_dia[t_DD-1],np.repeat(1.0,t_DD-1)-self.vec_dvt_L5_cumu[0:t_DD-1])*treat_intake_old
        vec_previous_larvae_nondia=np.multiply(self.pyramid_age_Lg3_dia[t_DD-1],np.repeat(1.0,t_DD-1)-self.vec_dvt_L5_cumu[0:t_DD-1])*treat_intake_old
       
        self.pyramid_age_Lg3_dia.append(np.append(new_larvae_L3_dia,vec_previous_larvae_dia))
        self.pyramid_age_Lg3_nondia.append(np.append(new_larvae_L3_nondia,vec_previous_larvae_nondia))
        self.Lg3_diapausing.append(new_larvae_L3_dia+sum(vec_previous_larvae_dia))
        self.Lg3_nondiapausing.append(new_larvae_L3_nondia+sum(vec_previous_larvae_nondia))
            
            
        self.newLg3_nymp.append(new_larvae_L3_dia)
        self.newLg3_diapausis.append(new_larvae_L3_nondia)
        self.newLg3_tot.append(new_larvae_L3_nondia+new_larvae_L3_dia)     
        
        #not all of those larvae are going to survive : only those who reach the L5 stage will be able to enter diapausis
        
        """Larvae reaching the L5 stage"""
        new_larvae_L5_dia=sum(np.multiply(self.pyramid_age_Lg3_dia[t_DD-1],self.vec_dvt_L5_cumu[0:t_DD-1]))*treat_intake_old
        new_larvae_L5_nondia=sum(np.multiply(self.pyramid_age_Lg3_nondia[t_DD-1],self.vec_dvt_L5_cumu[0:t_DD-1]))*treat_intake_old
        
        
        if new_larvae_L5_dia<=0:
            new_larvae_L5_dia=0
        if new_larvae_L5_nondia<=0:
            new_larvae_L5_nondia=0
            
        #application of treatments to old larvae : just at time where the treatment is done
        old_larvae_L5_dia_treat=np.array(self.Lg3_L5_dia[t_DD-1])*treat_intake_old
        
        old_larvae_L5_nondia_treat=np.array(self.Lg3_L5_nondia[t_DD-1])*treat_intake_old
        
        self.Lg3_L5_dia.append(old_larvae_L5_dia_treat+new_larvae_L5_dia)
        self.Lg3_L5_nondia.append(old_larvae_L5_nondia_treat+new_larvae_L5_nondia)
        self.Lg_diapausing.append(sum(self.pyramid_age_Lg1_dia[t_DD])+sum(self.pyramid_age_Lg2_dia[t_DD])+self.Lg3_L5_dia[t_DD])
     
       
       
        
       
        
    def population_Ag_julian(self,year_DD):
         self.Ag1_mal_julian=DD_to_julian(self.Ag1_mal,year_DD)
         self.Ag2_mal_julian=DD_to_julian(self.Ag2_mal,year_DD)
         self.Ag3_mal_julian=DD_to_julian(self.Ag3_mal,year_DD)
         Ag1=np.array(self.Ag1_mal_julian)
         Ag2=np.array(self.Ag2_mal_julian)
         Ag3=np.array(self.Ag3_mal_julian)
         self.Ag_tot_mal_julian=Ag1+Ag2+Ag3
          
    def new_Lg_tot(self,year_DD):
        Lg1=np.array(self.newLg1)
        Lg2=np.array(self.newLg2)
        Lg3=np.array(self.newLg3_tot)
        self.newLg_tot=Lg1+Lg2+Lg3    
        self.newLg_tot_cum=np.cumsum(self.newLg_tot)
   
        
        
    def damage(self):
        self.damage_tot.append(damage_pop(self.newLg_tot_julian_sum,t_g1,beta))
        self.damage_tot.append(damage_pop(self.newLg_tot_julian_sum,t_recolte,beta))




"""
Emergence function [Boivin 2005]
Function that gives the probability of adult G1 emerging depending of 
the accumulated number of days (t_DD), the biofix and its standard deviation
Output : p_emergence : a probability of emergence
"""



#Function that generates the probability to emerge depending on time
 #generated before the script because takes to much time to be generated in the function
# def emergence_g1(t_DD, biofix, sd_biofix):
#     p_emergence=(1/(sd_biofix*np.sqrt(2*np.pi)))*np.exp(-(t_DD-biofix-2*sd_biofix)**2/(2*sd_biofix**2))
#     return p_emergence

def emergence_g1(t_DD, biofix, sd_biofix):
        p_emergence=(1/(sd_biofix*np.sqrt(2*np.pi)))*np.exp(-(t_DD-biofix-2*sd_biofix)**2/(2*sd_biofix**2))
        return p_emergence


def emerging_time(t_DD):
   return t_emerg[t_DD]
        


"""Mortality function : 
    Function that gives the mortality hazard at each age_DD. Correspond to a survival function following a Gompertz distribution (cf Survival Function Jones 2008)
    Input : A is the initial mortality rate, G is the rate of senescence
    Output : 
    N.B : I decided to constrain mortality_density function to maximum 1 
"""

def mortality_density(age_DD,A,G):
    p_density_mortality=A*np.exp(G*age_DD)
    if p_density_mortality<=1:
        p=p_density_mortality
    else :
        p=1
    return p



"""Product of survival probability
   Function that gives the product of survival probabilities 
   
   Input : A : initial mortality rate
           G : rate of senescence
   Output : product of survival probability for example :   prod(4)=Psurvival(1)*Psurvival(2)*Psurvival(3)*Psurvival(4)

"""

def prod_survi(j,A,G):
    prod=1
    for i in range(0,j+1):
       prod=prod*(1-mortality_density(i,A,G))
    return(prod)





"""
Oviposition function [Boivin 2005]
Function that gives the proportion of total eggs laid by females depenging on
the age in DD (age_DD), m, b and c, 3 parameters of the log-normal function 
and N the total number of eggs laid
m is the maximum fecondity achieved at age b, and c is a dispersion parameters 
of the curve, N the total number of eggs laid
Output : probability density function of fertility
"""

#for the moment, we don't use N in the function : the objective is to have the number of eggs laid
#in Script R of Boivin, 15 used to divide but not reason ?

def oviposition(age_DD, m, b, c,N_factor,net):
    if net==1:
        F=fecondity_net
    else:
        F=fecondity
        
    p_lay = (m*np.exp(-0.5*(np.log(age_DD/b)/c)**2)/N_factor)*F
    return(p_lay)




"""Developmental function : works for egg, for neonate etc
Function that gives the probability to emerge tau_DD degres days after the
entrance in the given life stage
alph and kap are parameters specific to the life stage (egg, larval, pre-imaginal)
Output : a probability to emerge from the specific stage at time t_DD
"""

def development(t_DD,alph,kap):
    p_transit=(((alph*kap)**kap)*t_DD**(kap-1)*np.exp(-kap*alph*t_DD))/factorial(kap-1)
    return(p_transit)





"""Facultative diapausis
Function that gives the proportion of larvae entering diapausis, depending on JULIAN DATE 
Output

"""
def diapausis(t):
    D=(np.exp(u*(t-CD50)))/(1+np.exp(u*(t-CD50)))
    return D



        

"""From DD to julian days : 
    Function that convert population dynamics (in DD) into population dynamics in julian date : faux completement car faut repartir des nouveaux individus !
    le faire en cumulatif : à reprendre 
"""

def DD_to_julian(vector_dynamics,DD_in_julian):
    vector_in_julian=[]
    for i in range(0,len(DD_in_julian)-2):
            vector_in_julian.append(vector_dynamics[int(DD_in_julian[i])])
    return vector_in_julian
            

"""From population to traped population
    Function that makes the link between real population and traped population
"""
def trap(vector_Ag_week,beta):
    vector_Ag_trap_week=[]
    for i in range(len(vector_Ag_week)):
        vector_Ag_trap_week.append(beta*vector_Ag_week[i])
    return vector_Ag_trap_week

"""From population to damage : function with saturation from michaelis and menten
    Function that makes the link between larvae population and the percent of damages
"""
def damage_pop(vector_new_larvae,n_jour,beta):
    damages=vector_new_larvae[n_jour-1]/(vector_new_larvae[n_jour-1]+beta)
    return damages



"""Treatments : convert a date (in julian days) of treatment to the date in DD 
    take the date of treatments in julian days and converts it into 
    
"""

def treatments_newLg(date_treatments,year_DD,mortality_treatment_max):
    treat_DD_newLg=(np.repeat(0.0,np.max(year_DD)))
    for i in range(0,len(date_treatments)):
        #date in DD corresponding to the beginning of the julian day
        d_DD_matin=year_DD[int(date_treatments[i]-2)]
        #date in DD corresponding to the end of the julian day 
        d_DD_soir=year_DD[int(date_treatments[i]-1)]
        #number of DD concerned by treatment
        range_DD_treat=int(d_DD_soir-d_DD_matin)
        
        ###persistence of treatment : impact future generations
        #date in DD corresponding to the end of the persistence period of treatment
        d_DD_persist=year_DD[int(date_treatments[i]+2)]
        #number of DD concerned by persistence of treatment
        range_DD_persist=int(d_DD_persist-d_DD_soir)
     
        if range_DD_treat>=1:
            for j in (range(1,range_DD_treat)):
                treat_DD_newLg[j+int(d_DD_matin)]=mortality_treatment_max
            if range_DD_persist>=1:
                for h in (range(0,range_DD_persist)):
                    treat_DD_newLg[h+int(d_DD_soir)]=mortality_treatment_max*np.exp(-h*np.log(2)/half_life_CpV)
    return(treat_DD_newLg)



def treatments_oldLg(date_treatments,year_DD,mortality_treatment_max):
    treat_DD_oldLg=(np.repeat(0.0,np.max(year_DD)))
    for i in range(0,len(date_treatments)):
        #date in DD corresponding to the beginning of the julian day
        d_DD_treatment=year_DD[int(date_treatments[i]-2)]+1
        treat_DD_oldLg[int(d_DD_treatment)]=mortality_treatment_max
       
    return(treat_DD_oldLg)

"""Effect of poultry : convert a vector of charging in hens into a vector of effects for the emerging period
    Entry : vector_charging_hens which is a vector charging from time of harvest to time of harvest
    give a vector of impact in DD
    
"""
##impact of poultry during the whole year on mortality of larvae Lg1, Lg2, Lg3 correspond only to an effect from january to time of harvest

def i_hens_summer_DD(vector_charging_hens,year_DD,alpha,t_harvest_n_1):
    #in winter, the same DD correspond to different julian days
    impact_DD_rep=(np.repeat(0.0,np.max(year_DD)))
    #vector of hens just from the 1st january to the date of harvest t_harvest in julian days
    vec_jan_harvest_hens=vector_charging_hens[int(365-t_harvest_n_1-1):len(vector_charging_hens)]
    for i in range(0,int(np.max(year_DD))):
        indice_DD_same=list(np.where(year_DD==i)[0])
        number_DD_same=len(indice_DD_same)
        if (number_DD_same>1):
            if (indice_DD_same[0]>1):
                charg_somme=0
                for j in range(int(indice_DD_same[0]-1),int(indice_DD_same[0]+number_DD_same-1)):
                    
                    charg_somme=vector_charging_hens[j]+charg_somme
                    impact_DD_rep[i]=charg_somme*alpha
            
    #in summer, different DD correspond to the same julian day  
    impact_DD_new=(np.repeat(0.0,np.max(year_DD)))
    for h in range(1,int(len(year_DD)-1)):
        diff_DD=year_DD[h]-year_DD[h-1]
        
        for g in range(int(year_DD[h-1]+1),int(year_DD[h]+1)):
           impact_DD_new[g]=vector_charging_hens[h]*alpha/diff_DD
  
    return(impact_DD_new+impact_DD_rep)
   
    
##impact of poultry during the winter on the mortality of diapausing larvae for the moment only between january and march (for the real model, to implement between)
#number of days with impact : days_hens_impact_winter"
#unitary impact of poultry : neta (proportionnal to hens/day)
def i_hens_winter_DD(vector_charging_hens,date_impact_julian,neta,t_harvest_n_1):
    days_hens_impact_winter=int(365-t_harvest_n_1-1+date_impact_julian)
    
    somme_charging=sum(vector_charging_hens[0:int(days_hens_impact_winter-1)])
    dec_morta=somme_charging*neta
    return(dec_morta)
        
"""From population to damage : 
   Function that makes the link between number of new larvae and the damages
"""

def damages_mic(vector_newLg_cum,year_DD,time_g1,time_recolte,beta):
    index_timeg1=int(year_DD[int(time_g1-1)])
    index_timerecolte=int(year_DD[int(time_recolte-1)])
    
    damages_g1=vector_newLg_cum[index_timeg1]/(vector_newLg_cum[index_timeg1]+beta)
    damages_recolte=vector_newLg_cum[index_timerecolte]/(vector_newLg_cum[index_timerecolte]+beta)
    return([damages_g1,damages_recolte])

##############################################################################################################################################################################
######################################################################################################SIMULATION##############################################################
##############################################################################################################################################################################

#parameters to be defined definitively S0ini, year_DD, t_harvest, t_harvest_n_1

filename = 'D:/home/Sara/modelo/1-Ravageurs/1-Modelisation_carpocapse/3-Data/Degree_days/degree_days2022.txt'
year_DD_2022=np.loadtxt(filename, dtype=int)
t_harvest=int(272)          #jimmy planche
t_harvest_n_1=int(270)    #jimmy planche
biofix=year_DD_2022[109]    #BSV
net_g1=1
net_g2=1
net_recolte=1
t_g1=170          #dates comptage g1 : 19 juin 2022
supp_mortality=0
date_impact_julian=80    
#beta=1963
beta=482

#alpha=0.013892892093211928
alpha=0.009638036292329016   

#neta=0.012120157874995461
neta=0.027195815640601122

#entry from users : vector_poultry and date_treatments_julian

def simul_fct(S0ini,year_DD,vector_poultry,date_treatments_julian,t_harvest,t_harvest_n_1):
    
    max=int(year_DD[len(year_DD)-1])
    date_treatments_DD_new=treatments_newLg(date_treatments_julian,year_DD,mort_CpV_max)
    date_treatments_DD_old=treatments_oldLg(date_treatments_julian,year_DD,mort_CpV_max)
    #vector of impact of poultry between the 21st march and the harvest : consumption depending on the charging (//treatment)
    impact_poultry_vector_summer_DD=i_hens_summer_DD(vector_poultry,year_DD,alpha,t_harvest_n_1)
    
    #impact of poultry between the harvest and the 21st march : decrease the mortality of codling moth
    m0=m0nat-i_hens_winter_DD(vector_poultry,date_end_impact_hens_winter,neta,t_harvest_n_1)
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
    damages_tot=damages_mic(result.newLg_tot_cum,year_DD,t_g1,t_harvest,t_harvest_n_1)
    
    return(damages_tot)


#essai2=simul_fct(3,year_DD_2022,vector_poultry_2021_st,d_treat_2021_st,t_harvest,t_harvest_n1)