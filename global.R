library(shiny)
library(readxl)
library(DT)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(plotly)
library(readr)
library(shinyBS)
library(highcharter)
library(shinycssloaders)
library(leaflet)
library(UnalR)


options(dplyr.summarise.inform = FALSE,digits = 10)

# DATA -------------------------------------------------------------------
Graduados <- read_csv("Consolidados/Microdatos.csv")


# DATA FACTORS ------------------------------------------------------------

Graduados$TIPO_NIVEL <- factor(Graduados$TIPO_NIVEL, levels = c('Postgrado', 'Pregrado'))
Graduados$NIVEL <- factor(Graduados$NIVEL, levels = c('Doctorado', 'Especialidades médicas', 'Especialización', 'Maestría', 'Pregrado'))
#Graduados$NIVEL <- factor(Graduados$NIVEL, levels = c('Doctorado', 'Especialidades médicas', 'Especialización', 'Maestría', 'Pregrado', 'Tecnología'))
Graduados$SEDE_NOMBRE_ADM <- factor(Graduados$SEDE_NOMBRE_ADM, levels = c('Amazonía', 'Bogotá', 'Caribe', 'La Paz', 'Manizales', 'Medellín', 'Orinoquía', 'Palmira', 'Tumaco'))
Graduados$SEDE_NOMBRE_MAT <- factor(Graduados$SEDE_NOMBRE_MAT, levels = c('Amazonía', 'Bogotá', 'Caribe', 'La Paz', 'Manizales', 'Medellín', 'Orinoquía', 'Palmira', 'Tumaco'))
Graduados$NACIONALIDAD <- factor(Graduados$NACIONALIDAD, levels = c('Colombiana', 'Extranjero', 'Sin información'))
Graduados$SEXO <- factor(Graduados$SEXO, levels = c('Hombres', 'Mujeres'))
Graduados$ESTRATO <- factor(Graduados$ESTRATO, levels = c('Estrato 2 o menos', 'Estrato 3', 'Estrato 4 o más', 'ND/NE'))
Graduados$TIPO_COL <- factor(Graduados$TIPO_COL, levels = c('Oficial', 'Otros', 'Privado', 'Sin información'))
Graduados$PBM <- factor(Graduados$PBM, levels = c('11 o menos', '12 a 17', '18 a 50', '51 a 100', 'Sin información'))
Graduados$MAT_PVEZ <- factor(Graduados$MAT_PVEZ, levels = c('No', 'Sí'))
Graduados$MOD_ADM <- factor(Graduados$MOD_ADM, levels = c('Especial', 'Regular'))
Graduados$TIPO_ADM <- factor(Graduados$TIPO_ADM, levels = c('PAES', 'PEAA', 'PEAMA', 'Regular'))
Graduados$PAES <- factor(Graduados$PAES, levels = c('Comunidades indígenas', 'Mejores bachilleres', 'Mejores bachilleres de municipios pobres', 'Población afrocolombiana', 'Victimas del conflicto armado interno en Colombia'))
Graduados$PEAMA <- factor(Graduados$PEAMA, levels = c('PEAMA - Amazonía', 'PEAMA - Caribe', 'PEAMA - Orinoquía'))
#Graduados$PEAMA <- factor(Graduados$PEAMA, levels = c('PEAMA - Amazonía', 'PEAMA - Caribe', 'PEAMA - Medellín - Sinifaná', 'PEAMA - Orinoquía', 'PEAMA - Sede Bogotá - Sumapaz', 'PEAMA - Sede Manizales - Caldas', 'PEAMA - Tumaco'))
Graduados$MOV_PEAMA <- factor(Graduados$MOV_PEAMA, levels = c('Etapa de movilidad', 'Etapa Inicial'))
Graduados$CONVENIO <- factor(Graduados$CONVENIO, levels = c('No', 'Sí', 'Sin información'))
Graduados$TIP_CONVENIO <- factor(Graduados$TIP_CONVENIO, levels = c('Externo', 'Interno', 'Sin información'))
Graduados$AREAC_SNIES <- factor(Graduados$AREAC_SNIES, levels = c('Agronomía, veterinaria y afines', 'Bellas artes', 'Ciencias de la educación','Ciencias de la salud', 'Ciencias sociales y humanas', 'Economía, administración, contaduría y afines', 'Ingeniería, arquitectura, urbanismo y afines', 'Matemáticas y ciencias naturales'))


# All years and semesters that are in the data
historic <- Graduados %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = as.numeric(Id)) %>% select(Id) %>% unique() %>% pull()


# Functions ---------------------------------------------------------------


#This functions complete ceros for a category that lacks information for the last years
add_ceros <- function(dt,Var,years){
  
  cat <- unique(dt$Clase)
  df_ceros <- expand.grid(cat,years)
  
  
  dt <- dt %>% add_row(
    Variable = rep(Var,nrow(df_ceros)),
    YEAR = as.numeric(substr(df_ceros[,2],1,4)),
    SEMESTRE = as.numeric(substr(df_ceros[,2],5,5)),
    Clase = df_ceros[,1],
    Total = rep(0,nrow(df_ceros))
  )
  
  return(dt)
}

# AGGREGATION FUNCTION

Agregar <- function(poblacion, var){
  
poblacion %>% group_by(.dots = c("YEAR", "SEMESTRE", var), .drop = FALSE) %>% 
    summarise(Total = n()) %>% 
    rename("Clase"=var) %>% 
    mutate(Variable = var) %>%
    select(Variable, YEAR, SEMESTRE, Clase, Total) %>%
    ungroup()
}


# TOTAL FUNCTION

Totales <- function(poblacion){
  
  poblacion %>% group_by(YEAR, SEMESTRE, .drop = FALSE) %>%  summarise(Total = n()) %>% ungroup() %>%
    mutate(Variable="TOTAL", YEAR=YEAR, SEMESTRE=SEMESTRE, Clase = "Total", Total=Total) %>%
    select(Variable, YEAR, SEMESTRE, Clase, Total)
}

