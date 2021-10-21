#Limpiar la base de datos evaluación del sector


# Importar base de datos --------------------------------------------------

library(tidyverse)
#AAA

# Importar base de datos

df_eval_sector <- read.csv(file="data-raw/df_eval_sector.csv", fileEncoding = "UTF-8",header=TRUE)

# Dimensión Data Frama

dim(df_eval_sector)

#Renombrar columnas

colnames(df_eval_sector) #entrega los nombres de las columnas

v_new_colnames <-  c('marca_temporal', 'entidad', 'cargo', 'nombre', 'oficina', 'asuntos_ran', 'asuntos_pa', 
                     'asuntos_insus', 'asuntos_conavi', 'apertura_ran', 'apertura_pa', 'apertura_insus', 'apertura_conavi', 
                     'com_ran', 'com_pa', 'com_insus', 'com_conavi', 'atencion_ran', 'atencion_pa', 'atencion_insus',
                     'atencion_conavi', 'pub_ran', 'pub_pa', 'pub_insus', 'pub_conavi', 'desem_ran', 'desem_pa', 'desem_insus', 
                     'desemp_conavi', 'colab_ran', 'colab_pa', 'colab_insus', 'colab_conabi', 'vicios_ran', 'vicios_pa',
                     'vicios_insus', 'vicios_conavi', 'observaciones ')

length(v_new_colnames)

#Asigna el vector de nuevos nombres a las columnas del DF

colnames(df_eval_sector) <- v_new_colnames


# Codificar valores de columnas -------------------------------------------

table(df_eval_sector$apertura_ran)

df_codif_sector <- df_eval_sector %>% 
  mutate(apertura_ran = case_when(apertura_ran == "Están abiertas y en operación continua" ~ 1,
                                  apertura_ran == "Están parcialmente abiertas o en horario restringido" ~ 0.75,
                                  apertura_ran == "Cierran frecuentemente" ~ 0.5,
                                  apertura_ran == "Están cerradas" ~ 0.25),
         
         apertura_pa = case_when(apertura_pa == "Están abiertas y en operación continua" ~ 1,
                                  apertura_pa == "Están parcialmente abiertas o en horario restringido" ~ 0.75,
                                  apertura_pa == "Cierran frecuentemente" ~ 0.5,
                                  apertura_pa == "Están cerradas" ~ 0.25),
         
         apertura_insus = case_when(apertura_insus == "Están abiertas y en operación continua" ~ 1,
                                 apertura_insus == "Están parcialmente abiertas o en horario restringido" ~ 0.75,
                                 apertura_insus == "Cierran frecuentemente" ~ 0.5,
                                 apertura_insus == "Están cerradas" ~ 0.25),
         
         apertura_conavi = case_when(apertura_conavi == "Están abiertas y en operación continua" ~ 1,
                                    apertura_conavi == "Están parcialmente abiertas o en horario restringido" ~ 0.75,
                                    apertura_conavi == "Cierran frecuentemente" ~ 0.5,
                                    apertura_conavi == "Están cerradas" ~ 0.25),
         
         com_ran = case_when(com_ran == "Fácil, pronta y eficaz" ~ 1,
                             com_ran == "Aceptable" ~ 0.75,
                             com_ran == "Regular" ~ 0.5,
                             com_ran == "Difícil o deficiente" ~ 0.25),
         
         com_pa = case_when(com_pa == "Fácil, pronta y eficaz" ~ 1,
                             com_pa == "Aceptable" ~ 0.75,
                             com_pa == "Regular" ~ 0.5,
                             com_pa == "Difícil o deficiente" ~ 0.25),
         
         com_insus = case_when(com_insus == "Fácil, pronta y eficaz" ~ 1,
                            com_insus == "Aceptable" ~ 0.75,
                            com_insus == "Regular" ~ 0.5,
                            com_insus == "Difícil o deficiente" ~ 0.25),
         
         com_conavi = case_when(com_conavi == "Fácil, pronta y eficaz" ~ 1,
                               com_conavi == "Aceptable" ~ 0.75,
                               com_conavi == "Regular" ~ 0.5,
                               com_conavi == "Difícil o deficiente" ~ 0.25),
         
         atencion_ran = case_when(atencion_ran == "Atención permanente" ~ 1,
                                  atencion_ran == "Atención parcial" ~ 0.75,
                                  atencion_ran == "Atención solo a asuntos rezagados" ~ 0.5,
                                  atencion_ran == "No dan atención" ~ 0.25),
         
         atencion_pa = case_when(atencion_pa == "Atención permanente" ~ 1,
                                  atencion_pa == "Atención parcial" ~ 0.75,
                                  atencion_pa == "Atención solo a asuntos rezagados" ~ 0.5,
                                  atencion_pa == "No dan atención" ~ 0.25),
         
         atencion_insus = case_when(atencion_insus == "Atención permanente" ~ 1,
                                 atencion_insus == "Atención parcial" ~ 0.75,
                                 atencion_insus == "Atención solo a asuntos rezagados" ~ 0.5,
                                 atencion_insus == "No dan atención" ~ 0.25),
         
         atencion_conavi = case_when(atencion_conavi == "Atención permanente" ~ 1,
                                    atencion_conavi == "Atención parcial" ~ 0.75,
                                    atencion_conavi == "Atención solo a asuntos rezagados" ~ 0.5,
                                    atencion_conavi == "No dan atención" ~ 0.25),
         
         pub_ran = case_when(pub_ran == "Atención permanente en horario de oficina" ~ 1,
                             pub_ran == "Atención en días y horarios limitados" ~ 0.75,
                             pub_ran == "Atención solo con cita" ~ 0.5,
                             pub_ran == "No dan atención" ~ 0.25),
         
         pub_pa = case_when(pub_pa == "Atención permanente en horario de oficina" ~ 1,
                             pub_pa == "Atención en días y horarios limitados" ~ 0.75,
                             pub_pa == "Atención solo con cita" ~ 0.5,
                             pub_pa == "No dan atención" ~ 0.25),
         
         pub_insus = case_when(pub_insus == "Atención permanente en horario de oficina" ~ 1,
                            pub_insus == "Atención en días y horarios limitados" ~ 0.75,
                            pub_insus == "Atención solo con cita" ~ 0.5,
                            pub_insus == "No dan atención" ~ 0.25),
         
         pub_conavi = case_when(pub_conavi == "Atención permanente en horario de oficina" ~ 1,
                               pub_conavi == "Atención en días y horarios limitados" ~ 0.75,
                               pub_conavi == "Atención solo con cita" ~ 0.5,
                               pub_conavi == "No dan atención" ~ 0.25),
         
         desem_ran = case_when(desem_ran == "Eficiente" ~ 1,
                               desem_ran == "Bueno" ~ 0.75,
                               desem_ran == "Regular" ~ 0.5,
                               desem_ran == "Deficientes" ~ 0.25),
         
         desem_pa = case_when(desem_pa == "Eficiente" ~ 1,
                               desem_pa == "Bueno" ~ 0.75,
                               desem_pa == "Regular" ~ 0.5,
                               desem_pa == "Deficientes" ~ 0.25),
         
         desem_insus = case_when(desem_insus == "Eficiente" ~ 1,
                              desem_insus == "Bueno" ~ 0.75,
                              desem_insus == "Regular" ~ 0.5,
                              desem_insus == "Deficientes" ~ 0.25),
         
         desemp_conavi = case_when(desemp_conavi == "Eficiente" ~ 1,
                                 desemp_conavi == "Bueno" ~ 0.75,
                                 desemp_conavi == "Regular" ~ 0.5,
                                 desemp_conavi == "Deficientes" ~ 0.25),
         
         colab_ran = case_when(colab_ran == "Voluntad de colaboración y coordinación" ~ 1,
                               colab_ran == "Responsiva tras la solicitud" ~ 0.75,
                               colab_ran == "Reactiva tras varias solicitudes" ~ 0.5,
                               colab_ran == "Deficiente e informal" ~ 0.25),
         
         colab_pa = case_when(colab_pa == "Voluntad de colaboración y coordinación" ~ 1,
                               colab_pa == "Responsiva tras la solicitud" ~ 0.75,
                               colab_pa == "Reactiva tras varias solicitudes" ~ 0.5,
                               colab_pa == "Deficiente e informal" ~ 0.25),
         
         colab_insus = case_when(colab_insus == "Voluntad de colaboración y coordinación" ~ 1,
                              colab_insus == "Responsiva tras la solicitud" ~ 0.75,
                              colab_insus == "Reactiva tras varias solicitudes" ~ 0.5,
                              colab_insus == "Deficiente e informal" ~ 0.25),
         
         colab_conabi = case_when(colab_conabi == "Voluntad de colaboración y coordinación" ~ 1,
                                 colab_conabi == "Responsiva tras la solicitud" ~ 0.75,
                                 colab_conabi == "Reactiva tras varias solicitudes" ~ 0.5,
                                 colab_conabi == "Deficiente e informal" ~ 0.25),
         
         )


# Crear base de datos con promedios ---------------------------------------


df_codif_sum_sector <- df_codif_sector %>% 
  mutate(prom_ran = rowMeans(select(., apertura_ran,  com_ran , atencion_ran , pub_ran , desem_ran , colab_ran , ) , na.rm = TRUE ),
         prom_pa = rowMeans(select(. , apertura_pa,  com_pa , atencion_pa , pub_pa , desem_pa , colab_pa ), na.rm = TRUE),
         prom_insus = rowMeans(select(. , apertura_insus,  com_insus , atencion_insus , pub_insus , desem_insus , colab_insus ), na.rm = TRUE),
         prom_conavi = rowMeans(select(. , apertura_conavi,  com_conavi , atencion_conavi , pub_conavi , desemp_conavi , colab_conabi), na.rm = TRUE),
         
         )

prom_ran <- mean(df_codif_sum_sector$prom_ran, na.rm = TRUE)
prom_pa <- mean(df_codif_sum_sector$prom_pa, na.rm = TRUE)
prom_insus <- mean(df_codif_sum_sector$prom_insus, na.rm = TRUE)
mean_conavi <- mean(df_codif_sum_sector$prom_conavi, na.rm = TRUE)

write.csv(df_codif_sector,"resultados_codif_sector.csv")
