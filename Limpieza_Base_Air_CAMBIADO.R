#configuraciones iniciales

rm(list = ls())
gc()
options(scipen = 999)

packages <- c("tidyverse","stringi","readxl","stringr", "openxlsx") 
# librerias instaladas o no, en caso de que no esten instaladas, las instala.
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])}
invisible(lapply(packages, library, character.only = TRUE))# Carga todas las librerias
#####LLAMAR LIBRERIAS########################################################################

library(readxl)
library(tidyverse)
library(stringi)
library(stringr)
library(openxlsx)
library(dplyr)

#############################################################################################
#PUNTO 1
#############################################################################################
unicef_ini <- read.csv("C:/Users/JMINCHALO/Desktop/UNICEF CURSO_AVAN/Bases en bruto/UNICEF-29_12v1.csv", 
                       header = TRUE,
                       sep = ",")


#####DEPURACIC??N Y ESTANDARIZACIC??N############################################################
#######COPIAR ELEMENTOS DE CANTON_ECU Y PEGARLOS EN CANTON Y ELIMINAR CANTON_ECU###############

####QUITAR ACENTO EN NOMBRE DE VARIABLES####

colnames(unicef_ini) <- iconv(colnames(unicef_ini), to = "ASCII//TRANSLIT")
colnames(unicef_ini) <- tolower(colnames(unicef_ini))

#####Se cambia guiones puntos, guiones y caracteres especiales###########
names(unicef_ini) <- gsub("#","",names(unicef_ini))
names(unicef_ini) <- gsub(",","",names(unicef_ini))
names(unicef_ini) <- gsub("[\\ \\-\\...\\.\\..]+", "_", names(unicef_ini))
names(unicef_ini) <- gsub("\\.", "_", names(unicef_ini))
names(unicef_ini) <- gsub("__|___", "_", names(unicef_ini))
names(unicef_ini) <- gsub(pattern = "\\?",replacement = "",names(unicef_ini))

######ESTANDARIZAR COLUMNAS################################################

total_lin <- nrow(unicef_ini)

# val_cant <- unicef_ini$canton_ecu [10300:total_lin] 

unicef_ini$canton[2092:total_lin] <- unicef_ini$canton_ecu [2092:total_lin]

unicef_ini <- subset(unicef_ini, select = c(-canton_ecu))

######ESTANDARIZAR VALORES DEL CANTON PARA CRUCE DE BASES#################
unicef_ini$canton <- iconv(unicef_ini$canton, to = "ASCII//TRANSLIT")
unicef_ini$cargo <- iconv(unicef_ini$cargo, to = "ASCII//TRANSLIT")
unicef_ini$compania_o_institucion_de_trabajo <- 
  iconv(unicef_ini$compania_o_institucion_de_trabajo, to = "ASCII//TRANSLIT")

unicef_ini$canton <- tolower(unicef_ini$canton)
unicef_ini$canton <- trimws(unicef_ini$canton)
unicef_ini$cargo <- tolower(unicef_ini$cargo)
unicef_ini$compania_o_institucion_de_trabajo <- 
  tolower(unicef_ini$compania_o_institucion_de_trabajo)
unicef_ini$correo_electronico_institucional <- tolower(unicef_ini$correo_electronico_institucional)

#########CARGAR BASE ZONAS###################
zonas <- read_excel("C:/Users/JMINCHALO/Desktop/UNICEF CURSO_AVAN/GENERAL CONJUNTA.xlsx", 
                    sheet = "ZONAS")

zonas$canton <- iconv(zonas$canton, to = "ASCII//TRANSLIT")
zonas$canton <- tolower(zonas$canton)


#####CRUZAR BASES###########

unicef2 <- left_join(unicef_ini, zonas, by="canton")
unicef2 <- unicef2 %>% select(-zona.x, -distrito.x)
# unicef2 <- unicef2 %>% select(form_lastname, form_name, documento_de_identidad, 
# edad, genero, pais_de_residencia, zona.y, distrito.y,
# canton, correo_electronico_personal,
# correo_electronico_institucional, area_donde_trabaja_si_aplica,
# establecimiento_donde_trabaja_si_aplica,
# sector_de_la_empresa_o_institucion_de_trabajo, cargo, 
# describa_brevemente)

############QUITAR VALORES DUPLICADOS##############

unicef2 <- unicef2[!duplicated(unicef2$numero_de_identidad),]

###########seleccionar solo las variables necesearias y en orden de cada base####

unicef2 <- unicef2 %>% select("nombres",	"apellido",	"edad",
                              "genero",	"x_usted_cuenta_con_algun_tipo_de_discapacidad_",
                              "x_usted_se_reconoce_como_",
                              "pais_de_nacimiento",
                              "pais_de_residencia",
                              "provincia_ecu",
                              "correo_electronico_personal",
                              "correo_electronico_personal_copy",
                              "numero_de_celular",
                              "url_de_su_perfil_de_linkedin",
                              "x_se_encuentra_estudiando_",
                              "x_se_encuentra_estudiando_si",
                              "nivel_educativo_finalizado_u_obtenido_",
                              "nombre_del_ultimo_titulo_recibido_por_ejemplo_licenciatura_en_especializacion_en_",
                              "anos_de_experiencia",
                              "x_actualmente_trabaja_",
                              "correo_electronico_institucional",
                              "compania_o_institucion_de_trabajo",
                              "sector_de_la_empresa_o_institucion_de_trabajo",
                              "x_cual_es_su_perfil_de_desempeno_publico",
                              "x_cual_es_su_nivel_ocupacional_publico",
                              "x_cual_es_su_perfil_de_desempeno_privado",
                              "x_cual_es_su_perfil_de_desempeno_tercer_sector",
                              "x_cual_es_su_perfil_de_desempeno_academia",
                              "x_cual_es_su_perfil_de_desempeno_colegio",
                              "x_cual_es_su_perfil_de_desempeno_universidad",
                              "x_cual_es_su_perfil_de_desempeno_colegio_y_universidad",
                              "cargo",
                              "acepto_recibir_informacion_de_caf_en_mi_correo_electronico",
                              "x_esta_de_acuerdo_con_el_tratamiento_de_sus_datos_personales_",
                              "x_ha_leido_y_aceptado_la_politica_de_privacidad_de_caf_",
                              "numero_de_identidad",
                              "canton",
                              "area_donde_trabaja",
                              "x_acepta_compartir_sus_datos_con_terceros_",
                              "tratamiento_de_datos_personales",
                              "distrito.y", "zona.y", "provincia")


############ECONTRAR CARACTERES DENTRO DE LAS COLUMNAS
# 
# unicef3$insti_1 <- unicef3 %>% mutate(unicef3, insti_1 = 
#                                 (str_extract(string = 
#                                                unicef3$correo_electronico_institucional,
#                                              pattern = c("infancia","msp"))))
# 
# 
# unicef3$insti_1 <- str_extract(string = unicef3$correo_electronico_institucional,
#                                pattern = c("infancia","msp"))


#####DISCRIMINACIC??N POR VARIABLE ESTABLECIMIENTO################################
################################################################################

# Definir patrones para buscar "msp" y "mag"
patrones_insti1 <- c("^mies", "mag", "^cdi", "c.d.i", "cb", "cbi", 
                     "centro de desarrollo", "centro infantil",
                     "inclusion", "centros de desarrollo", "centros infantiles",
                     "cibv", "cnh", "convenio", "desarrollo infantil",
                     "educadora", "mintel","digercic", "registrocivil", "gad",
                     "msp", "salud", "hospital",
                     "telecomunicaciones", "municipio", "stecsdi",
                     "setecsdi", "agricultura","iess", "^ist", "instituto superio",
                     "ecuador crece","educacion")

# FunciC3n para extraer los caracteres de un texto
extraer_insti1 <- function(texto) {
  resultado <- str_extract_all(texto, paste(patrones_insti1, collapse = "|"))
  return(resultado)
}

# Agregar una nueva columna con insti_1 al dataframe unicef3
unicef2 <- unicef2 %>%
  mutate(insti1 = sapply(compania_o_institucion_de_trabajo, extraer_insti1))

unicef2$insti1 <-  as.character(unicef2$insti1)

#nrow(unicef2)


unicef3 <- mutate(unicef2, insti_1 =
                    case_when(unicef2$insti1 == "msp" ~ "MSP",
                              unicef2$insti1 == "salud" ~ "MSP",
                              unicef2$insti1 == "hospital" ~ "MSP",
                              unicef2$insti1 == "mag" ~ "MAG",
                              unicef2$insti1 == "agricultura" ~ "MAG",
                              unicef2$insti1 == "inclusion" ~ "MIES",
                              unicef2$insti1 == "mies" ~ "MIES",
                              unicef2$insti1 == "cdi" ~ "MIES",
                              unicef2$insti1 == "c.d.i" ~ "MIES",
                              unicef2$insti1 == "educadora" ~ "MIES",
                              unicef2$insti1 == "centro de desarrollo" ~ "MIES",
                              unicef2$insti1 == "centro infantil" ~ "MIES",
                              unicef2$insti1 == "centros de desarrollo" ~ "MIES",
                              unicef2$insti1 == "centros infantiles" ~ "MIES",
                              unicef2$insti1 == "cibv" ~ "MIES",
                              unicef2$insti1 == "cnh" ~ "MIES",
                              unicef2$insti1 == "convenio" ~ "MIES",
                              unicef2$insti1 == "desarrollo infantil" ~ "MIES",
                              unicef2$insti1 == "mintel" ~ "MINTEL",
                              unicef2$insti1 == "registrocivil" ~ "REGISTRO CIVIL",
                              unicef2$insti1 == "digercic" ~ "REGISTRO CIVIL",
                              unicef2$insti1 == "stecsdi" ~ "STECSDI",
                              unicef2$insti1 == "setecsdi" ~ "STECSDI",
                              unicef2$insti1 == "ecuador crece" ~ "STECSDI",
                              unicef2$insti1 == "iess" ~ "IESS",
                              unicef2$insti1 == "mineduc" ~ "MINEDUC"
                              # unicef2$insti1 == "educacion" ~ "MINEDUC"
                              # unicef2$insti1 == "gad" ~ "GAD"
                    ))


#DISCRIMINACIC??N POR VARIABLE CORREO##############################################
#################################################################################

# Definir patrones para buscar "msp" y "mag"
patrones_insti2 <- c("@msp", "@mag", "inclusion", "infancia", "mintel", "registrocivil",
                     "@uce","@espe","iess", "educacion")

# FunciC3n para extraer "msp" y "mag" de un texto
extraer_insti2 <- function(texto) {
  resultado <- str_extract_all(texto, paste(patrones_insti2, collapse = "|"))
  return(resultado)
}

# Agregar una nueva columna con "msp" y "mag" al dataframe unicef3
unicef3 <- unicef3 %>%
  mutate(insti2 = sapply(correo_electronico_institucional, extraer_insti2))

unicef3$insti2 <-  as.character(unicef3$insti2)

#nrow(unicef2)


unicef4 <- mutate(unicef3, insti_2 =
                    case_when(unicef3$insti2 == "@msp" ~ "MSP",
                              unicef3$insti2 == "@mag" ~ "MAG",
                              unicef3$insti2 == "inclusion" ~ "MIES",
                              unicef3$insti2 == "mintel" ~ "MINTEL",
                              unicef3$insti2 == "registrocivil" ~ "REGISTRO CIVIL",
                              unicef3$insti2 == "@uce" ~ "UCE",
                              unicef3$insti2 == "@espe" ~ "ESPE",
                              unicef3$insti2 == "iess" ~ "IESS",
                              unicef3$insti2 == "infancia" ~ "STECSDI",
                              unicef3$insti2 == "educacion" ~ "MINEDUC"
                    ))

unicef4 <- unicef4 %>%
  mutate(insti_1 = ifelse(is.na(insti_1), insti_2, insti_1))


# frecuencias <- unicef4 %>%
#   group_by(insti_1) %>%
#   summarise(Conteo = n())


# write.xlsx(unicef4, "C:/Users/JMINCHALO/Desktop/UNICEF CURSO_AVAN/Bases depuradas/UNICEF-21-8v1.xlsx")

# summary_df <- unicef_conca %>%
#   group_by(insti_1) %>%
#   summarize(count = n())

nrow(unicef4)
#####unir a base CONJUNTA GENERAL y crear una nueva tabla con los valores que tiene

########INCLUIR UNA VARIABLE DE AGRUPACIÓN PARA LAS INSTITUCIONES##############

# Definir patrones para buscar "msp" y "mag"

# Definir patrones para buscar "msp" y "mag"
patrones_insti3 <- c("ESPE", "ESPOL", "IST", "ist", "GAD", "IESS", "MAG", "MIES",
                     "MINEDUC", "MINTEL", "MSP", "PUCE", "REGISTRO CIVIL","SENESCYT",
                     "STECSDI", "UCE", "ULEAM", "UNACH", "UNEMI", "UNIVERSIDAD",
                     "UNL", "UPS", "UTB","N/A")

# FunciC3n para extraer "msp" y "mag" de un texto
extraer_insti3 <- function(texto) {
  resultado <- str_extract_all(texto, paste(patrones_insti3, collapse = "|"))
  return(resultado)
}

# Agregar una nueva columna con "msp" y "mag" al dataframe unicef3
unicef_final <- unicef4 %>%
  mutate(insti3 = sapply(insti_1, extraer_insti3))

unicef_final$insti3 <-  as.character(unicef_final$insti3)

#nrow(unicef2)


unicef_final <- mutate(unicef_final, insti_3 =
                         case_when(unicef_final$insti3 == "ESPE" ~ "ESCUELA POLITECNICA",
                                   unicef_final$insti3 == "ESPOL" ~ "ESCUELA POLITECNICA",
                                   unicef_final$insti3 == "IST" ~ "INST. SUPERIOR TECONOLÓGICO",
                                   unicef_final$insti3 == "GAD" ~ "GADs",
                                   unicef_final$insti3 == "IESS" ~ "IESS",
                                   unicef_final$insti3 == "MAG" ~ "MAG",
                                   unicef_final$insti3 == "MIES" ~ "MIES",
                                   unicef_final$insti3 == "MINEDUC" ~ "MINEDUC",
                                   unicef_final$insti3 == "MINTEL" ~ "MINTEL",
                                   unicef_final$insti3 == "MSP" ~ "MSP",
                                   unicef_final$insti3 == "PUCE" ~ "UNIVERSIDADES",
                                   unicef_final$insti3 == "REGISTRO CIVIL" ~ "REGISTRO CIVIL",
                                   unicef_final$insti3 == "SENESCYT" ~ "SENESCYT",
                                   unicef_final$insti3 == "STECSDI" ~ "STECSDI",
                                   unicef_final$insti3 == "UCE" ~ "UNIVERSIDADES",
                                   unicef_final$insti3 == "ULEAM" ~ "UNIVERSIDADES",
                                   unicef_final$insti3 == "UNIVERSIDAD" ~ "UNIVERSIDADES",
                                   unicef_final$insti3 == "UNL" ~ "UNIVERSIDADES",
                                   unicef_final$insti3 == "UNL" ~ "UNIVERSIDADES",
                                   unicef_final$insti3 == "UPS" ~ "UNIVERSIDADES",
                                   unicef_final$insti3 == "UTB" ~ "UNIVERSIDADES",
                                   unicef_final$insti3 == "N/A" ~ "OTROS"
                         ))





unicef_bc <- read.xlsx("C:/Users/JMINCHALO/Desktop/UNICEF CURSO_AVAN/Bases depuradas/base_final_3.xlsx")

# unicef_fal <- read.xlsx("C:/Users/JMINCHALO/Desktop/UNICEF CURSO_AVAN/Bases depuradas/faltantes.xlsx")

names(unicef_final) <- gsub("\\.y","",names(unicef_final))
names(unicef_bc) <- gsub("\\.y","",names(unicef_bc))

unicef_bc$numero_de_identidad <- as.double(unicef_bc$numero_de_identidad)



########HACER UN MATCH ENTRE LAS 2 TABLAS Y AUMENTAR POR CEDULA CUALES NO ESTAN

unicef_uni <- full_join(unicef_bc, unicef_final, by = "numero_de_identidad")

unicef_nuev <- unicef_uni %>% filter(is.na(nombres.x)) %>% 
  select(50:83, numero_de_identidad, 84:96)

#####HOMOGENEIZAR LOS NOMBRE DE LAS BASES ######################################

names(unicef_nuev) <- gsub("\\.y","",names(unicef_nuev))

unicef_nuev <- unicef_nuev %>% 
  mutate(canton_mod = (str_to_title(canton)))

unicef_nuev <- unicef_nuev %>% select("nombres",	"apellido",	"edad",
                              "genero",	"x_usted_cuenta_con_algun_tipo_de_discapacidad_",
                              "x_usted_se_reconoce_como_",
                              "pais_de_nacimiento",
                              "pais_de_residencia",
                              "provincia_ecu",
                              "correo_electronico_personal",
                              "correo_electronico_personal_copy",
                              "numero_de_celular",
                              "url_de_su_perfil_de_linkedin",
                              "x_se_encuentra_estudiando_",
                              "x_se_encuentra_estudiando_si",
                              "nivel_educativo_finalizado_u_obtenido_",
                              "nombre_del_ultimo_titulo_recibido_por_ejemplo_licenciatura_en_especializacion_en_",
                              "anos_de_experiencia",
                              "x_actualmente_trabaja_",
                              "correo_electronico_institucional",
                              "compania_o_institucion_de_trabajo",
                              "sector_de_la_empresa_o_institucion_de_trabajo",
                              "x_cual_es_su_perfil_de_desempeno_publico",
                              "x_cual_es_su_nivel_ocupacional_publico",
                              "x_cual_es_su_perfil_de_desempeno_privado",
                              "x_cual_es_su_perfil_de_desempeno_tercer_sector",
                              "x_cual_es_su_perfil_de_desempeno_academia",
                              "x_cual_es_su_perfil_de_desempeno_colegio",
                              "x_cual_es_su_perfil_de_desempeno_universidad",
                              "x_cual_es_su_perfil_de_desempeno_colegio_y_universidad",
                              "cargo",
                              "acepto_recibir_informacion_de_caf_en_mi_correo_electronico",
                              "x_esta_de_acuerdo_con_el_tratamiento_de_sus_datos_personales_",
                              "x_ha_leido_y_aceptado_la_politica_de_privacidad_de_caf_",
                              "numero_de_identidad",
                              "canton",
                              "canton_mod",
                              "area_donde_trabaja",
                              "x_acepta_compartir_sus_datos_con_terceros_",
                              "tratamiento_de_datos_personales",
                              "distrito", "zona", "provincia","insti1", "insti_1",
                              "insti2", "insti_2", "insti3", "insti_3")



unicef_conca <- rbind(unicef_bc, unicef_nuev)




##########ELIMINAR DUPLICADOS###################################################

unicef_conca <- unicef_conca[!duplicated(unicef_conca$numero_de_identidad),]


#############################EXPORTAR ####################################



####################EXPORTAR Y CREAR LA NUEVA BASE##############################

write.xlsx(unicef_conca, "C:/Users/JMINCHALO/Desktop/UNICEF CURSO_AVAN/Bases depuradas/UNICEF-29_12v1.xlsx")

