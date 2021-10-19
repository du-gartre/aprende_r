
# 01 Renombrar variables

#' Este scrip se va utilizar para renombrar las variables de la base de datos
#' evaluacion_enlaces_admon 

# Ctrl + shift + R

# Importar librerías ------------------------------------------------------
library(tidyverse)

# alt + -
# Importar base de datos --------------------------------------------------
df_eval_admon <- read.csv(file = "data-raw/eval_enlaces_admon.csv",  
                          fileEncoding = "UTF-8", header = TRUE)

# Dimensiones data frame
dim(df_eval_admon)


# Renombrar columnas ------------------------------------------------------
length(v_new_colnames)

v_new_colnames <- c('Marca temporal', 'Nombre Evaluador', 
                    'Oficina de representación', 'Evaluado','Puesto', 
                    '3.4 Comunicación', '3.3 Eficacia', '3.2 Proactividad', 
                    '3.1 Diligencia', '2.1 Conocimiento Técnico', '2.2 Dominio Normativo', 
                    'Gest. R.H_1', 'Gest. R.H_2', 'Gest. R.H_3', 'Recursos Mat_1', 
                    'Recursos Mat_2', 'Recursos Mat_3', 'Recursos Mat_4', 'Rec. Fin_1', 
                    'Rec. Fin_2', 'Rec. Fin_3', 'Rec. Fin_4', 'Rec. Fin_5', 
                    'Gestión y Archivo_1', 'Gestión y Archivo_2', 
                    'Gestión y Archivo_3', 'Actitud', 'Conocimiento', 'Resultados')

length(v_new_colnames)

# Asigna el vector de nuevos nombres a las columnas del df
colnames(df_eval_admon) <- v_new_colnames


# Codificar valores de columnas -------------------------------------------

# Columna 6 (3.4 Comunicación) tiene 4 tipos de respuestas
table(df_eval_admon$`3.4 Comunicación`)   # tabular valores de columna

df_codificado <- df_eval_admon %>% 
  mutate(`3.4 Comunicación` = case_when(`3.4 Comunicación` == "Oportuna" ~ 1,
                                        `3.4 Comunicación` == "Buena" ~ 0.75,
                                        `3.4 Comunicación` == "Regular" ~ 0.5,
                                        `3.4 Comunicación` == "Deficiente" ~ 0.25,
                                        ))

class(df_codificado$`3.4 Comunicación`)

df_eval_admon %>% # Pipe 
  mutate(`3.4 Comunicación` = case_when(`3.4 Comunicación` == "Oportuna" ~ 1,
                   `3.4 Comunicación` == "Buena" ~ 0.75,
                   `3.4 Comunicación` == "Regular" ~ 0.5,
                   `3.4 Comunicación` == "Deficiente" ~ 0.25,
                   )) %>% 
  View()


df_codif_sum <- df_codificado %>% # Pipe 
  mutate(promedio = rowMeans(select(., `3.4 Comunicación`)))

