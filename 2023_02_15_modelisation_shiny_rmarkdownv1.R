#code Rshiny

##################LIBRAIRIES UTILISEES##########

library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(shinythemes)
library(zoo)
# library(annmatrix)
library(readxl)
library(base)
library(ggplot2)
library(reshape2)
library(matrixStats)
library(dplyr)
library(tidyr)
library(ggpubr)
library(RColorBrewer)
library(wesanderson)
library(grid)
library(lubridate)
library(bbmle)
library(lme4)
library(deSolve)
library(matrixcalc)
library(reticulate)
library(shinydashboard)
library(ggpattern)
library(forcats)
library(magick)
library(DT)
library(data.table)
library(rmarkdown)
library(grid)
library(gridExtra)
library(cowplot)

library(reticulate)
use_condaenv("C:/Users/Sara/anaconda3/envs/Shiny/")
# py_install("scipy")
# py_install("matplotlib")
# py_install("pandas")
# py_install("numpy")

#on copie le report dans le dossier temporaire
file.copy(from = "D://home/Sara/Modelo/0-Ateliers_codes/Newactual/report.Rmd",to = paste0(tempdir(), "\\report.Rmd"))
file.copy(from = "D://home/Sara/Modelo/0-Ateliers_codes/Newactual/sol_nu.png",to = paste0(tempdir(), "\\sol_nu.png"))

##################CODE ET FONCTIONS UTILISEES DANS L'INTERFACE################
#pour les hauteurs d'herbe et le sol nu

############PARAMETRES UTILISES POUR LA SIMULATION ############

#Modèle enherbement
#parameters<-c("r1"=8.405530e-03,"r2"=1.343684e-03,"r3"=8.049574e-02,"r4"=1.124394e-02,"Hmax1"=3.502689e+01,"Hmax2"=2.821328,"Hmax3"=8.090064e+01,"Hmax4"=2.378807e+01,"t1"=8.195681e+01,"t2"=242.00,"t3"=3.206591e+02,"a"=1.473158e-04,"b"=-6.125339e-04,"hmin"=4.095693)
#inc_value=10   #incertitude du modèle : 10 % ?
#cutoff=5      #seuil ù on se rapproche surpâturage

#Modèle sol nu #valeurs de paramètres calibrés : 
q=1.683001e-05
K=1.002399
h=8.186525e-04

SNini=15
par_SN=c(q,K,h)

#Modèle carpocapse   : paramètres définis dans le modèle 
#début simulation
date_ini=as.Date("2021-11-01")
#début simulation pour les traitements
date_ini_sim_treat=as.Date("2022-01-01")
S0_ini=3

date_min=date_ini
date_max=as.Date("2022-11-01")

#infos sur les chargements des poules 
Surface=5000    #surface de la parcelle étudiée : 5000 m2
c_charg_nombre=round(10000/Surface)   #correspond au fait que le nombre de poules est donné pour 5000 m2 donc on convertit en poules/ha


#seuil de temps ###A MODIFIER 

seuil_t0=-50   #en dessous de -50 heures/an, considère qu'on économise du temps de travail : entre -50h et 0h
seuil_t1=104   #entre 0 et 2h de plus par semaine
seuil_t2=416     #entre 2h et 8h de plus par semaine
seuil_t3=832  #entre 8h et 16h de plus par semaine


#seuil économique
seuil_ecomin=-1000 #en dessous de -1000 euros/an, considère qu'on perd de l'argent
seuil_ecomax=1000  #au dessus de +1000 euros/an, considère qu'on perd de l'argent

t_ech1=7
t_ech2=14
t_ech3=21


###Tableau de chargement poules 
#1er tableau du 1er novembre 2021 au 1er mai 2022 inclus
tab_charging1 <- t(data.frame(Semaine = c("S44", "S45","S46","S47","S48","S49","S50","S51","S52","S1","S2","S3","S4","S5","S6","S7","S8","S9"),
                         Nombre = rep(0,18)))

colnames(tab_charging1)<-c("Nov"," "," "," "," ","Déc"," "," "," ","Jan"," "," "," "," ","Fév"," "," "," ")

tab_charging2<-t(data.frame(Semaine = c("S10","S11","S12","S13","S14","S15","S16","S17","S18","S19","S20","S21","S22","S23","S24","S25","S26"),
                            Nombre = rep(0,17)))
colnames(tab_charging2)<-c("Mar"," "," "," ","Avr"," "," "," ","Mai"," "," "," "," ","Juin"," "," "," ")

#2ème tableau du 2mai novembre au 31 octobre 2022 inclus                  
tab_charging3 <- t(data.frame(Semaine = c("S27","S28","S29","S30","S31","S32","S33","S34","S35","S36","S37","S38","S39","S40","S41","S42","S43"),
                              Nombre = rep(0,17)))
colnames(tab_charging3)<-c("Juil"," "," "," ","Aout"," "," "," "," ","Sep"," "," "," ","oct"," "," "," ")

#vecteur qui contient le premier jour de chaque semaine
first_days_week<-seq(as.Date("2021-11-01"),as.Date("2022-10-30"),by="weeks")

##########Fonction pour update un tableau 


modFunction <- function(input, output, session,data) {
  
  v <- reactiveValues(data = data)
  
  proxy = dataTableProxy("mod_table")
  l=ncol(data)
  observeEvent(input$mod_table_cell_edit, {
    info = input$mod_table_cell_edit
    str(info)
    i = info$row
    j = info$col
    k = info$value
    str(info)
    
    
    isolate({
    print(match(c("Semaine","Nombre"), names(v$data)))
        v$data[i, j:l] <<- DT::coerceValue(k, v$data[i, j])
       
    }
    )
    replaceData(proxy, v$data, resetPaging = FALSE)  # replaces data displayed by the updated table
  })

  output$mod_table <- DT::renderDataTable({
    DT::datatable(v$data, editable = TRUE,class = 'cell-border',options=list(searching=FALSE, paging=FALSE,ordering=FALSE,info=FALSE,select.items='cell'))
  
  })
  
return(v)
  
  }


modFunctionUI <- function(id) {
  ns <- NS(id)
  tab<-DT::dataTableOutput(ns("mod_table"))
  tab
}



##FONCTION QUI convertit semaine et chargement en tableau de chargement à l'année

semaine_charg<-function(df){
  df_tot=data.frame(rbind(df,first_days_week))
  

}



#####################DEFINITION DE L'INTERFACE UI ############
#Deux pages avec une page "Paramètres" pour entrer les données et une autre "Résultats" pour afficher les résultats

ui<-fluidPage(
  navbarPage("Pilotage d'un verger pâturé", theme = shinytheme("lumen"),
#Onglet avec les paramètres
  tabPanel("Paramètres", fluid = TRUE, icon = icon("keyboard"),
  titlePanel("Interface avec les modèles"),
  
  sidebarLayout(mainPanel(h2(textOutput("print"), style="color:red")),
    sidebarPanel(width=11,
                 
   
  #Demande quels résultats afficher : 
  fluidPage(fluidRow(column(3,prettyCheckbox(inputId ="Enh",label = "Enherbement",value = FALSE,shape = "curve")),
                     column(3,prettyCheckbox(inputId ="Rav",label = "Ravageurs",value = FALSE,shape = "curve")),
                     column(3,prettyCheckbox(inputId ="Eco",label = "Economique",value = FALSE,shape = "curve")),
                     column(3,prettyCheckbox(inputId ="Tps",label = "Temps de travail",value = FALSE,shape = "curve"))),
 
  #   
  #   checkboxGroupButtons(
  #   inputId = "what_to_plot",
  #   label = "Quelles données montrer ?",
  #   selected=NULL,
  #   choices=c("Enherbement","Ravageurs","Economique","Temps de travail"),
  #   
  # ),
  
  ###Entrées des données sur le nombre de poules
  downloadButton("rap","Télécharger"),
  #downloadLink("downloadPlot", "Download Plot"),
  fluidRow(column(3,numericInput("n_groupe","Numéro de groupe",NULL)),column(3,numericInput("n_strategie","Stratégie n°",NULL))),
  tags$hr(),
  titlePanel("Nombre de poules dans le verger : "),
  h2(" Novembre-Février"),
  modFunctionUI("editable1"),
  
  h2("Mars-Juin"),
  modFunctionUI("editable2"),
  
  h2("Juillet-Août"),
  modFunctionUI("editable3"),
  
  ###Entrées des données sur le nombre d'abattages
  numericInput("n_abattages","Nombre d'abattages ?",0),
  fluidRow(conditionalPanel("input.n_abattages>0",dateInput("dabattage1",NULL))),
  fluidRow(conditionalPanel("input.n_abattages>1",dateInput("dabattage2",NULL))), 
  fluidRow(conditionalPanel("input.n_abattages>2",dateInput("dabattage3",NULL))),

  
  ###FAUCHES
  titlePanel("Dates de fauche"),
 
  checkboxGroupButtons(
    inputId = "d_fauche",
    label = "Dates fauche", 
    choices = c("2022-10-01")
  ),
  
  checkboxInput("change_d_fauche",label = "Changement de dates de fauche ?",value=FALSE),
  conditionalPanel(condition="input.change_d_fauche==1",
                   column(3,dateInput(inputId="fauche1",label=NULL,value="2022-03-16",format="dd/mm/yyyy",language="fr")),
                   column(3,dateInput(inputId="fauche2",label=NULL,value="2022-04-03",format="dd/mm/yyyy",language="fr")),
                   column(3,dateInput(inputId="fauche3",label=NULL,value="2022-05-03",format="dd/mm/yyyy",language="fr")),
                   column(3,dateInput(inputId="fauche4",label=NULL,value="2022-06-10",format="dd/mm/yyyy",language="fr")),
                   column(3,dateInput(inputId="fauche5",label=NULL,value="2022-09-25",format="dd/mm/yyyy",language="fr"))),
  
  numericInput("plusfauches",label = "Fauches en plus ?",value=0),
  fluidRow(conditionalPanel(condition="input.plusfauches>1",column(12,dateInput(inputId="fauche6",label=NULL,value=NULL,format="dd/mm/yyyy",language="fr"))),
  conditionalPanel(condition="input.plusfauches>2",column(12,dateInput(inputId="fauche7",label=NULL,value=NULL,format="dd/mm/yyyy",language="fr"))),
  conditionalPanel(condition="input.plusfauches>3",column(12,dateInput(inputId="fauche8",label=NULL,value=NULL,format="dd/mm/yyyy",language="fr"))),
  conditionalPanel(condition="input.plusfauches>4",column(12,dateInput(inputId="fauche9",label=NULL,value=NULL,format="dd/mm/yyyy",language="fr")))),
  checkboxInput("change_h_fauche",label = "Changement de hauteur de fauche ?",value=FALSE), 
  conditionalPanel(condition="input.change_h_fauche==1",column(12,numericInput("hfauche","Hauteur de fauche (en cm) :",10))),
 
  ###FERTILISATION
  # titlePanel("Fertilisation"),
  # 
  # checkboxGroupButtons(
  #   inputId = "d_ferti",
  #   label = "Dates de fertilisation", 
  #   choices = c("2022-10-01")
  # ),
  # checkboxInput("change_d_ferti",label = "Changement de dates de fertilisation ?",value=FALSE),
  # conditionalPanel(condition="input.change_d_ferti==1",
  # column(6,dateInput(inputId="ferti1",label=NULL,value="2022-03-18",format="dd/mm/yyyy",language="fr")),
  # column(6,dateInput(inputId="ferti2",label=NULL,value="2022-04-18",format="dd/mm/yyyy",language="fr"))),
  
  ###TRAITEMENTS CARPOCAPSE
  titlePanel("Traitements carpocapse"),

  checkboxGroupButtons(inputId = "d_treat",label = "Dates traitement granulovirus",choices = c("2022-10-01")),
  
  checkboxInput("change_d_treat",label = "Changement de dates de traitement ?",value=FALSE),
  conditionalPanel(condition="input.change_d_treat==1",
  column(2,dateInput(inputId="treat1",label=NULL,value="2022-05-17",format="dd/mm/yyyy",language="fr")),
  column(2,dateInput(inputId="treat2",label=NULL,value="2022-05-27",format="dd/mm/yyyy",language="fr")),
  column(2,dateInput(inputId="treat3",label=NULL,value="2022-06-07",format="dd/mm/yyyy",language="fr")),
  column(2,dateInput(inputId="treat4",label=NULL,value="2022-06-17",format="dd/mm/yyyy",language="fr")),
  column(2,dateInput(inputId="treat5",label=NULL,value="2022-06-27",format="dd/mm/yyyy",language="fr")),
  column(2,dateInput(inputId="treat6",label=NULL,value="2022-07-07",format="dd/mm/yyyy",language="fr")),
  column(6,dateInput(inputId="treat7",label=NULL,value="2022-07-17",format="dd/mm/yyyy",language="fr")),
  column(6,dateInput(inputId="treat8",label=NULL,value="2022-07-27",format="dd/mm/yyyy",language="fr"))),
  br(),
  br(),
  ###TRAITEMENTS TAVELURE ET PUCERONS
  # titlePanel("Traitements tavelure et pucerons"),
  # fluidRow(checkboxInput("chgt_trait_pt",label = "Retrait de traitements pucerons et tavelure ?",value=FALSE)),
  # conditionalPanel(condition = "input.chgt_trait_pt==1",
  # checkboxGroupButtons(inputId = "d_treat_pt",label = "Dates traitement pucerons et/ou tavelures",choices = c("2022-10-01")),
  # 
  # checkboxInput("change_d_treat_pt",label = "Changements de date de traitement pucerons/tavelure ?",value=FALSE),
  # conditionalPanel(condition="input.change_d_treat_pt==1",
  #                  column(2,dateInput(inputId="treat_pt1",label=NULL,value="2022-03-12",format="dd/mm/yyyy",language="fr")),
  #                  column(2,dateInput(inputId="treat_pt2",label=NULL,value="2022-03-23",format="dd/mm/yyyy",language="fr")),
  #                  column(2,dateInput(inputId="treat_pt3",label=NULL,value="2022-04-03",format="dd/mm/yyyy",language="fr")),
  #                  column(2,dateInput(inputId="treat_pt4",label=NULL,value="2022-04-08",format="dd/mm/yyyy",language="fr")),
  #                  column(2,dateInput(inputId="treat_pt5",label=NULL,value="2022-04-17",format="dd/mm/yyyy",language="fr")),
  #                  column(2,dateInput(inputId="treat_pt6",label=NULL,value="2022-05-06",format="dd/mm/yyyy",language="fr")),
  #                  column(2,dateInput(inputId="treat_pt7",label=NULL,value="2022-05-17",format="dd/mm/yyyy",language="fr")),
  #                  column(2,dateInput(inputId="treat_pt8",label=NULL,value="2022-06-12",format="dd/mm/yyyy",language="fr")))),

  ###SYSTEME D'ELEVAGE
  fluidRow(titlePanel("Définition du système d'élevage")),
 
  column(6,pickerInput(inputId = "poulailler",label = "Type de poulailler ?",choices = c("Mobile bricolé", "Mobile acheté", "Fixe bricolé", "Fixe acheté"))),
  #Rotation poulailler
  conditionalPanel(condition="input.poulailler=='Mobile bricolé'||input.poulailler=='Mobile acheté'",
  column(6,numericInput("freq_deplact", "Fréquence de déplacement (en semaines)", 4, min = 0, max = NA, step = 1)),
  fluidRow(column(12,pickerInput(inputId="strat_loc",label="Stratégie de localisation du poulailler mobile",choices=c("Mixte","Verger","Hors_verger"))))),
  #Type de stratégie d'alimentation
  fluidRow(column(12,pickerInput(inputId = "strat_alim",label = "Stratégie d'alimentation?",choices = c("Moderee","Complet","Restreinte")))),
  #Lumière
  checkboxInput("lum",label = "Complémentation lumière ? ",value=FALSE),
  
  ###SYSTEME DE COMMERCIALISATION
  titlePanel("Définition du système de commercialisation"),
  #Comm des fruits
  fluidRow(column(6,pickerInput(inputId = "systeme_comm_fruits",label = "Système de commercialisation des fruits ?",choices = c("AMAP", "Marché"))), 
  
  #Comm des oeufs  
  column(6,conditionalPanel("input.systeme_comm_oeufs=='Intégré'",checkboxInput(inputId="comm_sup",label = "Système de commercialisation supplémentaire ?",value=FALSE)))),
  #nombre d'AMAP à fournir si choix du système d'AMAP
  fluidRow(conditionalPanel("input.systeme_comm_fruits=='AMAP'&input.systeme_comm_oeufs=='Intégré'",numericInput(inputId ="nAMAP",label="Nombre d'AMAP fournies ?",1, min = 0, max = 2, step = 1))),
  #Comm supp
  fluidRow(column(6,pickerInput(inputId = "systeme_comm_oeufs",label = "Système de commercialisation des oeufs ?",choices = c("Intégré", "Non intégré")))), 
  conditionalPanel("input.systeme_comm_fruits=='AMAP'&&input.systeme_comm_oeufs=='Intégré'"),      
  conditionalPanel("input.comm_sup||input.systeme_comm_oeufs=='Non intégré'",
  pickerInput(inputId = "systeme_comm_supp",label = "Système de commercialisation supplémentaire ?",choices = c("Marché","Par un intermédiaire","Bouche-à-oreille"),selected=NULL)),
  #Embauche de main d'oeuvre
  checkboxInput("embauche",label = "Embauche main d'oeuvre ?",value=FALSE),
  conditionalPanel("input.embauche",column(6,numericInput("ETP", "ETP", 0, min = 0, max = NA, step = 0.5)),column(6,dateRangeInput("dates_embauche","Périodes embauche")))
  
  )),
                  
         
    
))
#onglet de résultats
#  tabPanel("Résultats", fluid = TRUE, icon = icon("chart-mixed", lib = "glyphicon"),
#     titlePanel("Résultats"),
#     sidebarLayout(
#     sidebarPanel(width=0,
#                  h2("Pensez à imprimer les résultats en PDF en cliquant sur le bouton S en haut à droite de l'écran"),
#                          fluidPage()),
# ##Panel avec les résultats 
#   mainPanel(width=12,
#     conditionalPanel("input$Enh",h2("Nombre de poules dans la parcelle et fauches réalisées")),
#     plotOutput("infos_fauche",width="100%",height="200px"),
#     h2("Evolution de la hauteur d'herbe dans la parcelle"),
#     plotOutput("curve"),
#     dataTableOutput("economique"),
#     
#     #dataTableOutput("zones"),
# 
# 
#     h2("Etat de l'enherbement en hiver"),
#     column(3,imageOutput("gradient",height="2500px")),
#    
#     
#     fluidPage(column(9,
#     fluidRow(column(8,imageOutput("SN1")),column(1,h2(textOutput("percentSN1")))),
#     fluidRow(column(8,imageOutput("SN2")),column(1,h2(textOutput("percentSN2")))),
#     fluidRow(column(8,imageOutput("SN3")),column(1,h2(textOutput("percentSN3")))),
#     fluidRow(column(8,imageOutput("SN4")),column(1,h2(textOutput("percentSN4")))))),
#     
#     h2("Gestion du carpocapse : "),
#     plotOutput("damages_count_ini",width="100%",height="300px"), 
# 
#     plotOutput("damages_count",width="100%",height="300px"),
#     h2("Les barres représentent la récolte, et la partie grisée, la partie de la récolte qui correspond aux fruits piqués"),
# 
#     
#     h2("Bilan économique par rapport à la situation initiale sans poules : "),
#     plotOutput("eco_visuel",width="50%",height="100px"),
#     h2("Surcharge de travail par rapport à la situation initiale sans poules : "),
#     plotOutput("temps_visuel",width="50%",height="100px"),
#     h2("Nombre de poules dans l'exploitation"),
#     plotOutput("infos_interv",width="100%",height="75px"),
#     h2("Surcharge de travail hebdomadaire"),
#     plotOutput("temps_hebdo"),
#     plotOutput("Oeufs",height="500px"),
#     
#     textOutput("taille"),
#     plotOutput("plotessai")
#     #textOutput("essaitab")
#     
#     #imageOutput("damagesinit"),
#     #imageOutput("damages"),
#     
#     #conditionalPanel("input.Enh==1",
# ))))

))


#####################SERVEUR ET SORTIE ############


server<-function(input,output,session){
  
##############UPDATE DE L'INTERFACE UTILISATEUR  SELON LES ENTREES ##############
  
  observe({
    
    #####UPDATE FAUCHE ferti
    
    d_fauche1<-as.character(format(as.Date(input$fauche1),"%d/%m/%Y"))
    d_fauche2<-as.character(format(as.Date(input$fauche2),"%d/%m/%Y"))
    d_fauche3<-as.character(format(as.Date(input$fauche3),"%d/%m/%Y"))
    d_fauche4<-as.character(format(as.Date(input$fauche4),"%d/%m/%Y"))
    d_fauche5<-as.character(format(as.Date(input$fauche5),"%d/%m/%Y"))
    d_fauche_supp1<-as.character(format(as.Date(input$fauche6)[as.Date(input$fauche6)<as.Date("2023-01-01")],"%d/%m/%Y"))
    d_fauche_supp2<-as.character(format(as.Date(input$fauche7)[as.Date(input$fauche7)<as.Date("2023-01-01")],"%d/%m/%Y"))
    d_fauche_supp3<-as.character(format(as.Date(input$fauche8)[as.Date(input$fauche8)<as.Date("2023-01-01")],"%d/%m/%Y"))
    
    updateCheckboxGroupButtons(session,"d_fauche", "Dates de fauche", choices=c(d_fauche1,d_fauche2,d_fauche3,d_fauche4,d_fauche5,d_fauche_supp1,d_fauche_supp2,d_fauche_supp3),checkIcon = list(yes = icon("ok",lib = "glyphicon")),selected=c(d_fauche1,d_fauche2,d_fauche3,d_fauche4,d_fauche5))
    
    # #####UPDATE FAUCHE ferti
    # 
    # d_ferti1<-as.character(format(as.Date(input$ferti1),"%d/%m/%Y"))
    # d_ferti2<-as.character(format(as.Date(input$ferti2),"%d/%m/%Y"))
    # d_ferti3<-as.character(format(as.Date(input$ferti3),"%d/%m/%Y"))
    # updateCheckboxGroupButtons(session,"d_ferti", "Dates de fertilisation", choices=c(d_ferti1,d_ferti2,d_ferti3),checkIcon = list(yes = icon("ok",lib = "glyphicon")),selected=c(d_ferti1,d_ferti2,d_ferti3)
    # )
    # 
    #####UPDATE CARPOTREATMENTS DATE
  
    d_treat1<-as.character(format(as.Date(input$treat1),"%d/%m/%Y"))
    d_treat2<-as.character(format(as.Date(input$treat2),"%d/%m/%Y"))
    d_treat3<-as.character(format(as.Date(input$treat3),"%d/%m/%Y"))
    d_treat4<-as.character(format(as.Date(input$treat4),"%d/%m/%Y"))
    d_treat5<-as.character(format(as.Date(input$treat5),"%d/%m/%Y"))
    d_treat6<-as.character(format(as.Date(input$treat6),"%d/%m/%Y"))
    d_treat7<-as.character(format(as.Date(input$treat7),"%d/%m/%Y"))
    d_treat8<-as.character(format(as.Date(input$treat8),"%d/%m/%Y"))
   
    updateCheckboxGroupButtons(session,"d_treat", "Dates treatment", choices=c(d_treat1,d_treat2,d_treat3,d_treat4,d_treat5,d_treat6,d_treat7,d_treat8),
                               selected=c(d_treat1,d_treat2,d_treat3,d_treat4,d_treat5,d_treat6,d_treat7,d_treat8),checkIcon = list(yes = icon("ok",lib = "glyphicon")))
    
    
    #####UPDATE TREATMENTS PUCERONS ET TAVELURE DATE
    
    # d_treat_pt1<-as.character(format(as.Date(input$treat_pt1),"%d/%m/%Y"))
    # d_treat_pt2<-as.character(format(as.Date(input$treat_pt2),"%d/%m/%Y"))
    # d_treat_pt3<-as.character(format(as.Date(input$treat_pt3),"%d/%m/%Y"))
    # d_treat_pt4<-as.character(format(as.Date(input$treat_pt4),"%d/%m/%Y"))
    # d_treat_pt5<-as.character(format(as.Date(input$treat_pt5),"%d/%m/%Y"))
    # d_treat_pt6<-as.character(format(as.Date(input$treat_pt6),"%d/%m/%Y"))
    # d_treat_pt7<-as.character(format(as.Date(input$treat_pt7),"%d/%m/%Y"))
    # d_treat_pt8<-as.character(format(as.Date(input$treat_pt8),"%d/%m/%Y"))
    # d_treat_pt9<-as.character(format(as.Date(input$treat_pt9),"%d/%m/%Y"))
    # updateCheckboxGroupButtons(session,"d_treat_pt", "Dates traitements pucerons tavelures", choices=c(d_treat_pt1,d_treat_pt2,d_treat_pt3,d_treat_pt4,d_treat_pt5,d_treat_pt6,d_treat_pt7,d_treat_pt8,d_treat_pt9)
    # , selected=c(d_treat_pt1,d_treat_pt2,d_treat_pt3,d_treat_pt4,d_treat_pt5,d_treat_pt6,d_treat_pt7,d_treat_pt8,d_treat_pt9),checkIcon = list(yes = icon("ok",lib = "glyphicon")))
    # 
    # 
    })
  

  
   output$char<-renderDataTable({
     data.frame(modFunctionUI("editable1"))
   })
   
############################       FONCTION REACTIVES AUX ENTREES        ############################
   
##################Fonctions supports ##################

   
   #TABLEAU DE DATES DE CHARGEMENT, NOMBRE DE POULES ET DATES
#récupération des data.table créés
   data1<-tab_charging1
   tabledit1<-callModule(modFunction,"editable1", data1)
   
   
   data2<-tab_charging2
   tabledit2<-callModule(modFunction,"editable2",data2)
   
   data3<-tab_charging3
   tabledit3<-callModule(modFunction,"editable3", data3)

   


#################SORTIES PREVUES ##################################

  #Sortie pour faire des essais : fonction qui permet de voir les sorties en intermédiaire
  output$economique<-renderDataTable({data.frame(oeufs())
  })
    
    
    
  
  #outputOptions(output, 'economique', suspendWhenHidden=TRUE)
  
  #fonction qui transforme date de fauche character en vraies dates
  char_to_date<-reactive({
    d_fauch_char=as.Date(input$d_fauche,format="%d/%m/%Y")
    d_fauche_date=as.POSIXlt.character(c())
    for (i in 1:length(d_fauch_char)){d_fauche_date=c(d_fauche_date,as.POSIXlt.character(d_fauch_char[i]))}
    d_fauche_date
    })
  

  
  
  #####
  
  poules_exploit_semaine<-reactive({
    date_abattage<-c(input$dabattage1,input$dabattage2,input$dabattage3)
    #on remplit le tableau
    poules=nombre_poules_exploit(z(),date_abattage)[[1]]
    
    #on fait la moyenne du nombre de poules dans l'exploit à la semaine
    poules_semaine=mean_vector(poules[[2]],7)
    poules_semaine
    # df_infos_poules=data.frame(t(cbind(tabledit1$data,tabledit2$data,tabledit3$data)))
    # colnames(df_infos_poules)=c("Date","Chargement")
    # #on doit transformer à la semaine
    # df_infos_p=data.frame(df_infos_poules,"Firstday"=as.Date(first_days_week))
    # df_infos_p
  })
  
  
  
  #Graph qui donne les infos sur le nombre de poules, les interventions pour aider à la compréhension du temps de travail
  output$infos_interv<-renderPlot({
    dates_semaine=as.Date(first_days_week)
    poules_week=poules_exploit_semaine()[1:length(dates_semaine)]
    
    donnees_poules=data.frame("Date"=dates_semaine,"Chargement"=poules_week)
    max_y=max(donnees_poules$Chargement)
    plot1<-ggplot(donnees_poules)+geom_step(aes(Date,as.numeric(Chargement)))+ggtitle("Nombre de poules")+ylim(0,NA)+theme_minimal()+theme(axis.line.x = element_blank(),axis.ticks.x = element_blank(),axis.text.x = element_blank(),axis.title.x = element_blank())
    # d_fauches=input$d_fauche
    #vecteur des semaines ; donnees_poules$Firstday
    
    index_fauches_week=which(interventions_week(as.Date("2021-11-01"),as.Date("2022-11-01"),input$d_fauche)>0)
    fauches_week=dates_semaine[index_fauches_week]
    loc_y_symb=max(donnees_poules$Chargement)
    df_symbol=data.frame("Dfauches"=fauches_week,"Symbole"=rep(loc_y_symb,length(fauches_week)))

    # #with symbols corresponding to fauche
    plot2<-plot1+geom_point(data=df_symbol,aes(Dfauches,Symbole),shape=1,fill="red",size=5)+ylim(0,NA)
    if(input$Tps==TRUE){return(plot2)}
    })
  
  


  

  #Courbes de pontes et de vente d'oeufs
  output$Oeufs<-renderPlot({
    df_oeufs=oeufs()%>%
      mutate("Boitepond"=floor(Oeufs.pondus/6),"Boitevendues"=floor(Oeufs.vendus/6))
    
    df_oeufs$Date=first_days_week
    boite<-c(system.file("img", "eggs_boitemi.png", package="ggpattern"))
    #contour du ruban pour afficher les oeufs perdus
    rubbon_lost<-data.frame("Date"=first_days_week,"Down"=df_oeufs$Boitevendues,"Up"=df_oeufs$Boitepond)
    
    #contour du ruban pour afficher les oeufs perdus
    rubbon_sold<-data.frame("Date"=first_days_week,"Down"=0,"Up"=df_oeufs$Boitevendues)
    
    #coordonnees pour écrire dans la partie rouge :
    if (TRUE%in%(rubbon_lost$Up>rubbon_lost$Down)){Text="Boites non vendues"
    l=length(rubbon_lost$Up>rubbon_lost$Down)
    coord_y=(rubbon_lost$Up[l/2]-rubbon_lost$Down[l/2])/2+rubbon_lost$Down[l/2]
    coord_x=rubbon_lost$Date[l/2]
    }else{Text=c()
    coord_y=0
    coord_x=as.Date("2022-02-02")}
    
    plot_oeufs=ggplot(df_oeufs)+geom_area_pattern(aes(Date,Boitepond),fill='transparent',pattern='image',pattern_type='tile',pattern_filename=boite)+theme_bw()+geom_ribbon(data=rubbon_lost,aes(x=Date,ymin=Down,ymax=Up),fill='firebrick',alpha=0.5,color='firebrick')+annotate("label",x=coord_x,y=coord_y,label=Text,fill='firebrick',color="white",size=15,alpha=0.8)+ylab("Nombre de boites d'oeufs produites")+geom_ribbon(data=rubbon_sold,aes(x=Date,ymin=Down,ymax=Up),fill='palegreen1',alpha=0.3,color='palegreen1')
    if(input$Eco==TRUE){return(plot_oeufs)}
    
    
  })
  
  
  
  
  count=0
  pdf_print<-eventReactive((input$Enh||input$Rav||input$Eco),
      {count=1
      return(count)})


  output$print<- renderText({
    phrase=as.character("")
    if (input$Enh&pdf_print()==1){phrase=as.character("Avez-vous pensé à enregistrer en PDF ???")}
    if (input$Rav&pdf_print()==1){phrase=as.character("Et cette fois-ci, avez-vous vraiment pensé à enregistrer en PDF ???")}
    if (input$Eco&pdf_print()==1){phrase=as.character("N'oubliez toujours pas d'enregistrer en pdf ???")}
    return(phrase)
    })
    

  output$rap<- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      parametress <- list(n = input$dabattage1)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = parametress
      )
    }
  )
  output$plotessai<-renderPlot({
    tracer_hauteur()
  })
  
  # output$downloadPlot <- downloadHandler(
  #   filename = function() { "output.pdf" },
  #   content = function(file) {
  #     pdf(file, paper = "default")
  #     plot(tracer_hauteur())
  #     dev.off()
  #   }
  # )


  
}
  
  shinyApp(ui=ui,server=server)
