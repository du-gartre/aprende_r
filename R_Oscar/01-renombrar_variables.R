#01 Renombrar variables

#'Este scrip se va a utilizar para renombrar las variables de la base de datos
#'

# ctrl+shift+r (crear secciones)


# Importar librerias ------------------------------------------------------

library(tidyverse)
#a
# alt + (-) (Crear flechita)
# Importar base de datos --------------------------------------------------

df_eval_admon <- read.csv(file = "data-raw/eval_enlaces_admon.csv",
                          fileEncoding = "UTF-8",header=TRUE)

#Header sirve para definir si existen títulos de columnas

#Dimensión data frame

dim(df_eval_admon)


# Renombrar columnas ------------------------------------------------------



colnames(df_eval_admon)

v_new_colnames <- c('Marca temporal','Nombre Evaluador', 'Oficina de representación', 
                    'Evaluado', 'Puesto', '3.4 Comunicación', '3.3 Eficacia',
                    '3.2 Proactividad', '3.1 Diligencia', '2.1 Conocimiento Técnico',
                    '2.2 Dominio Normativo', 'Gest. R.H_1', 'Gest. R.H_2', 'Gest. R.H_3', 
                    'Recursos Mat_1', 'Recursos Mat_2', 'Recursos Mat_3', 'Recursos Mat_4', 
                    'Rec. Fin_1', 'Rec. Fin_2', 'Rec. Fin_3', 'Rec. Fin_4', 'Rec. Fin_5', 
                    'Gestión y Archivo_1', 'Gestión y Archivo_2', 'Gestión y Archivo_3',
                    'Actitud', 'Conocimiento', 'Resultados')
length(v_new_colnames)

#Asigna el vector de nuevos nombres a las columnas del DF

colnames(df_eval_admon) <- v_new_colnames


# Codificar valores de columnas -------------------------------------------
table(df_eval_admon$`3.4 Comunicación`) #Tabular valores de la columna

df_codificado <- df_eval_admon %>%        #crtl+shift+m (pipe)
  mutate(`3.4 Comunicación`=case_when(`3.4 Comunicación`=="Oportuna"~ 1,
                                      `3.4 Comunicación`=="Buena"~ 0.75,
                                      `3.4 Comunicación`=="Regular"~ 0.5,
                                      `3.4 Comunicación`=="Deficiente"~ 0.25),
         
         `3.3 Eficacia` = case_when(`3.3 Eficacia` == "Frecuentemente" ~ 1,
                                    `3.3 Eficacia` == "Algunas veces" ~ 0.75,
                                    `3.3 Eficacia` == "Pocas veces" ~ 0.5,
                                    `3.3 Eficacia` == "Nunca" ~ 0.25,),
         
         `3.2 Proactividad`= case_when(`3.2 Proactividad` == "Proactivo" ~1,
                                       `3.2 Proactividad` == "Reactivo" ~ 0.75,
                                       `3.2 Proactividad` == "Pasivo" ~ 0.5,
                                       `3.2 Proactividad` == "No se involucra" ~ 0.25),
         
         `3.1 Diligencia` = case_when(`3.1 Diligencia` == "Proactivo" ~1,
                                      `3.1 Diligencia` == "Reactivo" ~ 0.75,
                                      `3.1 Diligencia` == "Pasivo" ~ 0.5,
                                      `3.1 Diligencia` == "No se involucra" ~ 0.25),
         
         `2.1 Conocimiento Técnico` = case_when(`2.1 Conocimiento Técnico` == "Amplio" ~ 1,
                                                `2.1 Conocimiento Técnico` == "Suficiente" ~ 0.75,
                                                `2.1 Conocimiento Técnico` == "Regular" ~ 0.5,
                                                `2.1 Conocimiento Técnico` == "Deficiente" ~ 0.25),
         
         `2.2 Dominio Normativo` = case_when(`2.2 Dominio Normativo` == "Sobresaliente" ~ 1,
                                             `2.2 Dominio Normativo` == "Suficiente" ~ 0.75,
                                             `2.2 Dominio Normativo` == "Regular" ~ 0.5,
                                             `2.2 Dominio Normativo` == "Deficiente" ~ 0.25,),
         
         `Gest. R.H_1` = case_when(`Gest. R.H_1` == "Sí, el personal asiste" ~ 1,
                                   `Gest. R.H_1` == "Sí, el personal asiste parcialmente" ~ 0.75,
                                   `Gest. R.H_1` == "No, el personal casi no asiste" ~ 0.5,
                                   `Gest. R.H_1` == "No, no se involucra" ~ 0.25),
         
         `Gest. R.H_2` =case_when(`Gest. R.H_2` == "Sí, hay control de asistencia" ~ 1,
                                  `Gest. R.H_2` == "Sí, pero el control no es aplicado" ~ 0.75,
                                  `Gest. R.H_2` == "No, pero sí hay asistencia del personal" ~ 0.50,
                                  `Gest. R.H_2` == "No, no se involucra" ~ 0.25),
         
        `Recursos Mat_1` = case_when(`Recursos Mat_1` == "Completo y actualizado" ~ 1,
                                     `Recursos Mat_1` == "Poco reciente, pero completo" ~ 0.75,
                                     `Recursos Mat_1` == "Levantado hace más de 9 meses" ~ 0.5,
                                     `Recursos Mat_1` == "No cuenta con inventario" ~ 0.25),
        
        `Recursos Mat_2` = case_when(`Recursos Mat_2` == "Completo y actualizado" ~ 1,
                                     `Recursos Mat_2` == "Poco reciente, pero completo" ~ 0.75,
                                     `Recursos Mat_2` == "Levantado hace más de 9 meses" ~ 0.5,
                                     `Recursos Mat_2` == "No cuenta con inventario" ~ 0.25),
        
        `Recursos Mat_3` = case_when(`Recursos Mat_3` == "Completo y actualizado" ~ 1,
                                     `Recursos Mat_3` == "Poco reciente, pero completo" ~ 0.75,
                                     `Recursos Mat_3` == "Levantado hace más de 9 meses" ~ 0.5,
                                     `Recursos Mat_3` == "No cuenta con inventario" ~ 0.25),
        
        `Recursos Mat_4` = case_when(`Recursos Mat_4` == "Completo y actualizado" ~ 1,
                                     `Recursos Mat_4` == "Poco reciente, pero completo" ~ 0.75,
                                     `Recursos Mat_4` == "Levantado hace más de 9 meses" ~ 0.5,
                                     `Recursos Mat_4` == "No cuenta con inventario" ~ 0.25),
        
        `Rec. Fin_1`= case_when(`Rec. Fin_1` == "Eficiente" ~ 1,
                                `Rec. Fin_1` == "Buena y ordenada" ~ 0.75,
                                `Rec. Fin_1` == "Sin planeación adecuada" ~ 0.5,
                                `Rec. Fin_1` == "Deficiente" ~ 0.25),
        
        `Rec. Fin_2`= case_when(`Rec. Fin_2` == "Eficiente" ~ 1,
                                `Rec. Fin_2` == "Buena y ordenada" ~ 0.75,
                                `Rec. Fin_2` == "Sin planeación adecuada" ~ 0.5,
                                `Rec. Fin_2` == "Deficiente" ~ 0.25),
        
        `Rec. Fin_3`= case_when(`Rec. Fin_3` == "Eficiente" ~ 1,
                                `Rec. Fin_3` == "Buena y ordenada" ~ 0.75,
                                `Rec. Fin_3` == "Sin planeación adecuada" ~ 0.5,
                                `Rec. Fin_3` == "Deficiente" ~ 0.25),
        
        `Rec. Fin_4`= case_when(`Rec. Fin_4` == "Eficiente" ~ 1,
                                `Rec. Fin_4` == "Buena y ordenada" ~ 0.75,
                                `Rec. Fin_4` == "Sin planeación adecuada" ~ 0.5,
                                `Rec. Fin_4` == "Deficiente" ~ 0.25),
        
        `Rec. Fin_5`= case_when(`Rec. Fin_5` == "Eficiente" ~ 1,
                                `Rec. Fin_5` == "Buena y ordenada" ~ 0.75,
                                `Rec. Fin_5` == "Sin planeación adecuada" ~ 0.5,
                                `Rec. Fin_5` == "Deficiente" ~ 0.25),
        
        `Gestión y Archivo_1` = case_when(`Gestión y Archivo_1` == "Eficiente" ~ 1,
                                          `Gestión y Archivo_1` == "Buena y ordenada" ~ 0.75,
                                          `Gestión y Archivo_1` == "Sin planeación adecuada" ~ 0.5,
                                          `Gestión y Archivo_1` == "Deficiente" ~ 0.25),
        
        `Gestión y Archivo_2` = case_when(`Gestión y Archivo_2` == "Eficiente" ~ 1,
                                          `Gestión y Archivo_2` == "Buena y ordenada" ~ 0.75,
                                          `Gestión y Archivo_2` == "Sin planeación adecuada" ~ 0.5,
                                          `Gestión y Archivo_2` == "Deficiente" ~ 0.25),
        
        `Gestión y Archivo_3` = case_when(`Gestión y Archivo_3` == "Sobresaliente" ~ 1,
                                          `Gestión y Archivo_3` == "Buena" ~ 0.75,
                                          `Gestión y Archivo_3` == "Regular" ~ 0.5,
                                          `Gestión y Archivo_3` == "Rezagada" ~ 0.25)
                                          
                                
                                              
         )
   
table(df_eval_admon$`Recursos Mat_1`)


# Crear comulumnas con promedios ------------------------------------------



df_codif_sum <- df_codificado %>% 
  mutate(promedio_actitud= rowMeans(select(.,`3.4 Comunicación`, `3.3 Eficacia`,
                                           `3.2 Proactividad`, `3.1 Diligencia`)),
         promedio_conocimiento = rowMeans(select(., `2.1 Conocimiento Técnico`,`2.2 Dominio Normativo` )),
         
         promedio_resultados = rowMeans(select(.,`Gest. R.H_1`,`Gest. R.H_2`,`Recursos Mat_1`,`Recursos Mat_3`,`Recursos Mat_4`,`Rec. Fin_1`,
                                               `Rec. Fin_2`, `Rec. Fin_3` , `Rec. Fin_4`, `Rec. Fin_5`,`Gestión y Archivo_1`,`Gestión y Archivo_2`,`Gestión y Archivo_3`)))



# Imprimir Data frama -----------------------------------------------------

write.csv(df_codif_sum,'eval_enlaces_admon')




