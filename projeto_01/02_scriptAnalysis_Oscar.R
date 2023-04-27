#-----------------------------------------------------------------------------#
# Names: Oscar Jose Ospino Ayala
# Proceso Seletivo Data Mining
#-----------------------------------------------------------------------------#

## clear data
rm(list = ls())

## localization
wd <- "" 
setwd(wd)

## packages
require(tidyverse)
require(RSQLite)
require(magrittr)
require(plotly)
require(gridExtra)

## data
filename <- file.path(wd, "02_Prueba_DataMining.xlsx")
file.exists(filename)

bbdd <- readxl::read_excel(filename, sheet = "BBDD") 

#------------------- 1. preprocessing character fields ------------------------#

# statistics
summary(bbdd)
summary(bbdd[, c(5, 15, 17)]) %>% 
  knitr::kable(caption = "Tabla 1. Resumen estadístico relevante")

# transformation
bbdd <- bbdd %>%
  dplyr::mutate(fecha = lubridate::dmy(fecha), 
                .before = "intervalo") %>%
  tidyr::separate(col = Id_Cola, into = c("Id", "Id_Cola"), sep = "_") %>%
  dplyr::mutate(Id_Cola = as.integer(Id_Cola)) %>%
  dplyr::select(-Id)

 # strings fields
bbdd$fecha[is.na(bbdd$fecha)]
summary(bbdd$fecha)

which(is.na(bbdd$intervalo)) # 26304 27406 NA
which(is.na(bbdd$campana))   # 26304 27406 NA
which(is.na(bbdd$Id_Cola))   # 26304 27406 NA

 # int field
bbdd %>% 
  dplyr::select(canal:tiempo_conversacion) %>%
  summary()

aus1 <- bbdd %>%
  dplyr::select(entrante:abandonada_en_20) %>%
  purrr::map(.f = function(x){which(is.na(x))})


sum(which(is.na(bbdd$tiempo_respuesta)) %in% c(26303, 27405))
sum(which(is.na(bbdd$tiempo_abandono)) %in% c(26303, 27405))
sum(which(is.na(bbdd$tiempo_conversacion)) %in% c(26303, 27405))

which(is.na(bbdd$tiempo_respuesta)) %in% which(is.na(bbdd$tiempo_conversacion)) %>%
  sum()
sum(which(is.na(bbdd$tiempo_respuesta)) %in% c(26303, 27405))
sum(which(is.na(bbdd$tiempo_abandono)) %in% c(26303, 27405))

  # continuation
bbdd[c(26303, 27405), ]

nrow(bbdd) - 25120 

  # remove

bbdd <- bbdd[-c(26303, 27405), ] %>%
  dplyr::select(-canal)

## Format

bbdd %>% 
  dplyr::pull(intervalo) %>% 
  unique()


bbdd %>% 
  dplyr::pull(campana) %>% 
  unique()

bbdd %>% 
  dplyr::pull(Id_Cola) %>% 
  unique()


bbdd %<>%
  dplyr::select(entrante:tiempo_conversacion) %>% 
  purrr::map_df(.f = as.integer) %>% 
  dplyr::mutate(dplyr::select(bbdd, fecha:Id_Cola), .before = entrante)%>% 
  dplyr::select(-campana) 

## Outiler

bbdd %>%
  dplyr::select(entrante:contestada_en_10) %>%
  tidyr::pivot_longer(cols = everything(),
                      names_to = "Caterogia", 
                      values_to = "Valores") %>%
  ggplot2::ggplot(aes(Caterogia, Valores)) + 
  ggplot2::geom_boxplot(aes(colour = Caterogia), 
                        outlier.colour = "blue", 
                        na.rm = TRUE)  +
  ggplot2::xlab("")  +
  ggplot2::scale_colour_discrete("Campos") -> b1

bbdd %>%
  dplyr::select(contestada_en_15:abandonada_en_20) %>%
  tidyr::pivot_longer(cols = everything(),
                      names_to = "Caterogia", 
                      values_to = "Valores") %>%
  ggplot2::ggplot(aes(Caterogia, Valores)) + 
  ggplot2::geom_boxplot(aes(colour = Caterogia), 
                        outlier.colour = "blue", 
                        na.rm = TRUE) +
  ggplot2::xlab("") +
  ggplot2::scale_colour_discrete("Campos") -> b2

bbdd %>%
  dplyr::select(tiempo_respuesta:tiempo_conversacion) %>%
  tidyr::pivot_longer(cols = everything(),
                      names_to = "Caterogia", 
                      values_to = "Valores") %>%
  ggplot2::ggplot(aes(Caterogia, Valores)) + 
  ggplot2::geom_boxplot(aes(colour = Caterogia), 
                        outlier.colour = "blue", 
                        na.rm = TRUE) +
  ggplot2::xlab("") +
  ggplot2::scale_colour_discrete("Campos") -> b3


X11()
grid.arrange(b1, b3,
             ncol = 2, nrow = 1)

X11()
b2

#
bbdd %>%
  ggplot2::ggplot(aes(entrante)) +
  ggplot2::geom_histogram(color = 1, binwidth = 1) + 
  ggplot2::xlab("Entrante") + ggplot2::ylab("Cuenta") +
  ggplot2::theme_light() -> f1


bbdd %>%
  ggplot2::ggplot(aes(contestada)) +
  ggplot2::geom_histogram(color = 1, binwidth = 1) + 
  ggplot2::xlab("Contestadas") + ggplot2::ylab("Cuenta") +
  ggplot2::theme_light()  -> f2

bbdd %>%
  ggplot2::ggplot(aes(contestada_en_10)) +
  ggplot2::geom_histogram(color = 1, binwidth = 1) + 
  ggplot2::xlab("Contestadas en 10") + ggplot2::ylab("Cuenta") +
  ggplot2::theme_light() -> f3

bbdd %>%
  ggplot2::ggplot(aes(contestada_en_15)) +
  ggplot2::geom_histogram(color = 1, binwidth = 1) + 
  ggplot2::xlab("Contestadas en 15") + ggplot2::ylab("Cuenta") +
  ggplot2::theme_light() -> f4

bbdd %>%
  ggplot2::ggplot(aes(contestada_en_20)) +
  ggplot2::geom_histogram(color = 1, binwidth = 1) + 
  ggplot2::xlab("Contestadas en 20") + ggplot2::ylab("Cuenta") +
  ggplot2::theme_light() -> f5

bbdd %>%
  ggplot2::ggplot(aes(abandonada_en_5)) +
  ggplot2::geom_histogram(color = 1, binwidth = 1) + 
  ggplot2::xlab("Abandonada en 5") + ggplot2::ylab("Cuenta") +
  ggplot2::theme_light() -> f6

bbdd %>%
  ggplot2::ggplot(aes(abandonada_en_10)) +
  ggplot2::geom_histogram(color = 1, binwidth = 1) + 
  ggplot2::xlab("Abandonada en 10") + ggplot2::ylab("Cuenta") +
  ggplot2::theme_light() -> f7

bbdd %>%
  ggplot2::ggplot(aes(abandonada_en_15)) +
  ggplot2::geom_histogram(color = 1, binwidth = 1) + 
  ggplot2::xlab("Abandonada en 15") + ggplot2::ylab("Cuenta") +
  ggplot2::theme_light() -> f8

bbdd %>%
  ggplot2::ggplot(aes(abandonada_en_20)) +
  ggplot2::geom_histogram(color = 1, binwidth = 1) + 
  ggplot2::xlab("Abandonada en 20") + ggplot2::ylab("Cuenta") +
  ggplot2::theme_light() -> f9

bbdd %>%
  ggplot2::ggplot(aes(tiempo_respuesta)) +
  ggplot2::geom_histogram(color = 1, binwidth = 1) + 
  ggplot2::xlab("Tiempo de respuesta") + ggplot2::ylab("Cuenta") +
  ggplot2::theme_light() -> f10

bbdd %>%
  ggplot2::ggplot(aes(tiempo_abandono)) +
  ggplot2::geom_histogram(color = 1, binwidth = 1) + 
  ggplot2::xlab("Tiempo de abandono") + ggplot2::ylab("Cuenta") +
  ggplot2::theme_light() -> f11

bbdd %>%
  ggplot2::ggplot(aes(tiempo_conversacion)) +
  ggplot2::geom_histogram(color = 1, binwidth = 1) + 
  ggplot2::xlab("Tiempo de conversacion") + ggplot2::ylab("Cuenta") +
  ggplot2::theme_light() -> f12

X11()
grid.arrange(f1, f2, f3,
             f4, f5, f6, 
             ncol = 3, nrow = 2)

X11()
grid.arrange(f7, f8, f9, 
             f10, f11, f12, 
             ncol = 3, nrow = 2)


#------------------- 2. Table Import------------------#

# tabela horas
horas <- readxl::read_excel(path = "02_Prueba_DataMining.xlsx",
                            sheet = "Param_Horas") %>%
  dplyr::mutate(Hora_Num = as.integer(Hora_Num), .before = "Hora_24")

input1 <- horas %>%
  dplyr::select(Hora_24:Intervalo_60) %>%
  purrr::map_df(.f = base::format, "%H:%M:%S") 

horas %<>% mutate(input1, .after = Hora_24)


# tabela datas considerando ano 2022
fechas <- readxl::read_excel(path = "02_Prueba_DataMining.xlsx",
                             sheet = "Param_Fechas") %>% 
  dplyr::rename(Ano = Anio) %>% 
  dplyr::rename(Semana_Ano = Semana_Anio) %>%
  dplyr::mutate(Fecha = lubridate::date(Fecha)) %>%
  dplyr::filter(Ano == 2022)

input2 <- fechas %>%
  dplyr::select(c(Ano, Num_Mes, Num_Semestre, Mes_Semestre, 
                  Num_Trimestre, Mes_Trimestre, Num_Bimestre,
                  Mes_Bimestre, Num_Dia, Num_Habil)) %>%
  purrr::map_df(.f = as.integer)


fechas %<>% mutate(input2) 


# tabela caudas
Colas <- readxl::read_excel(path = "02_Prueba_DataMining.xlsx",
                             sheet = "Param_Colas") %>%
  tidyr::separate(Id_Cola, c("Id", "Id_Cola"), sep = "_") %>%
  tidyr::separate(Id_Canal, c("cn", "Id_Canal"), sep = "_") %>%
  dplyr::select(-c(Id, cn)) %>%
  dplyr::mutate(Id_Cola = as.integer(Id_Cola), 
                Id_Canal = as.integer(Id_Canal))

# tabela canales
canales <- readxl::read_excel(path = "02_Prueba_DataMining.xlsx",
                            sheet = "Param_Canales") %>%
  tidyr::separate(Id_Canal, c("cn", "Id_Canal"), sep = "_") %>%
  dplyr::select(-c(cn)) %>%
  dplyr::mutate(Id_Canal = as.integer(Id_Canal))


#--------------- 3. Option .csv ---------------#

readr::write_csv(bbdd,    "bbddNew.csv")
readr::write_csv(fechas,  "fechasNew.csv")
readr::write_csv(Colas,   "colasNew.csv")
readr::write_csv(canales, "canalesNew.csv")
readr::write_csv(horas,   "horasNew.csv")

#------------------- 4. SQL Table Normalization  ------------------#

# Connect
con <- RSQLite::dbConnect(RSQLite::SQLite(), "dbseletivo.sqlite3")

# Create table fechas

str(fechas)
send1 <- "CREATE TABLE Fechas(
          Ano           INTEGER,
          Mes           VARCHAR(50),
          Num_Mes       INTEGER,
          Fecha         TEXT PRIMARY KEY NOT NULL,
          Num_Semestre  INTEGER,
          Mes_Semestre  INTEGER,
          Num_Trimestre INTEGER,
          Mes_Trimestre INTEGER,
          Num_Bimestre  INTEGER,
          Mes_Bimestre  INTEGER,
          Semana_Ano    VARCHAR(50),
          Semana_Mes    VARCHAR(50),
          Dia_Semana    VARCHAR(50),
          Dia_Festivos  VARCHAR(50),
          Tipo_Dia      VARCHAR(50),
          Num_Dia       INTEGER,
          Num_Dia_Mes   VARCHAR(50),
          Num_Dia_Habil VARCHAR(50),
          Num_Habil     INTEGER,
          Tipo_Recargo_Dia VARCHAR(50)
          );"
query1 <- "SELECT f.* FROM Fechas AS f;"
RSQLite::dbExecute(con, send1)
RSQLite::dbWriteTable(con, "Fechas", fechas, append = TRUE)
RSQLite::dbGetQuery(con, query1) %>% dplyr::as_tibble()

# Create table horas
str(horas)
send2 <- "CREATE TABLE Horas(
         Hora_Num           INTEGER PRIMARY KEY NOT NULL,        
         Hora_24            VARCHAR(50),
         Hora_12            VARCHAR(50),
         Intervalo_15       VARCHAR(50),
         Intervalo_30       VARCHAR(50),
         Intervalo_60       VARCHAR(50),
         Franja_AMPM        VARCHAR(50),
         Franja_Turno       VARCHAR(50),
         Tipo_Recargo_Hora  VARCHAR(50)
        );"
query2 <- "SELECT h.* FROM Horas AS h;"
RSQLite::dbExecute(con, send2)
RSQLite::dbWriteTable(con, "Horas", horas, append = TRUE)
RSQLite::dbGetQuery(con, query2) %>% dplyr::as_tibble()

# Create table Canales
str(canales)
send3 <- "CREATE TABLE Canales(
         Id_Canal  INTEGER PRIMARY KEY NOT NULL,        
         Canal     VARCHAR(50)
        );"
query3 <- "SELECT C.* FROM Canales AS C;"
RSQLite::dbExecute(con, send3)
RSQLite::dbWriteTable(con, "Canales", canales, append = TRUE)
RSQLite::dbGetQuery(con, query3) %>% dplyr::as_tibble()


# Create table Colas
str(Colas)
send4 <- "CREATE TABLE Colas(
         Id_Cola  INTEGER PRIMARY KEY NOT NULL,        
         Id_Canal INTEGER,
         nombre_cola VARCHAR(50),
         FOREIGN KEY (Id_Canal) REFERENCES Canales (Id_Canal)
        );"
query4 <- "SELECT C.* FROM Colas AS C;"
RSQLite::dbExecute(con, send4)
RSQLite::dbWriteTable(con, "Colas", Colas, append = TRUE)
RSQLite::dbGetQuery(con, query4) %>% dplyr::as_tibble()

# Create table bbdd 
str(bbdd)
bbdd1 <- bbdd %>% dplyr::mutate(fecha = as.character(fecha), .before = "intervalo")
RSQLite::dbRemoveTable(con, 'bbdd')

send5 <- "CREATE TABLE bbdd (
          fecha TEXT,
          intervalo VARCHAR(50),
          Id_Cola INTEGER,
          entrante INTEGER,
          contestada INTEGER,
          contestada_en_10 INTEGER,
          contestada_en_15 INTEGER,
          contestada_en_20 INTEGER,
          abandonada_en_5 INTEGER,
          abandonada_en_10 INTEGER,
          abandonada_en_15 INTEGER,
          abandonada_en_20 INTEGER, 
          tiempo_respuesta INTEGER,
          tiempo_abandono INTEGER,
          tiempo_conversacion INTEGER,
          CONSTRAINT fk_horasbbdd FOREIGN KEY (tiempo_conversacion) REFERENCES Horas (Hora_Num),
          CONSTRAINT fk_fecbbdd FOREIGN KEY (fecha) REFERENCES Fechas (fecha),
          CONSTRAINT fk_colabbdd FOREIGN KEY (Id_Cola) REFERENCES Colas (Id_Cola)
          )"

query5 <- "SELECT b.* FROM bbdd AS b;"

RSQLite::dbExecute(con, send5)
RSQLite::dbWriteTable(con, "bbdd", bbdd1, append = TRUE)
RSQLite::dbGetQuery(con, query5) %>% dplyr::as_tibble()

#----------------------------- 5. Analysis ---------------------------#

RSQLite::dbListTables(con)
RSQLite::dbListFields(con, "bbdd")

# porcentaje de llamadas entrantes es contestado en 15 segundos

query6 <- "SELECT SUM(b.contestada_en_10 +
                      b.contestada_en_15 + 
                      b.contestada_en_20) AS subtotal_constada,
                  SUM(b.contestada_en_10) AS contestada_10,
                  SUM(b.contestada_en_15) AS contestada_15 ,
                  SUM(contestada_en_20) AS contestada_20  
                  FROM bbdd AS b;"

query7 <- "SELECT b.contestada_en_15,
                  b.contestada_en_10,
                  b.contestada_en_20  
                  FROM bbdd AS b"

(RSQLite::dbGetQuery(con, query6) / RSQLite::dbGetQuery(con, query6)$subtotal_constada) %>% 
  dplyr::select(-subtotal_constada) %>%
  tidyr::pivot_longer(cols = contestada_10:contestada_20,
                      names_to = "Tipos",
                      values_to = "Participación") %>%
  ggplot2::ggplot(aes(x = 1, y = Participación, fill = Tipos)) +
  ggplot2::geom_bar(width = 1, stat = "identity") +
  ggplot2::coord_polar("y") +
  ggplot2::geom_text(aes(label = paste(round(Participación, 3), "%")),
                         position = position_stack(vjust = 0.7)) + 
  ggplot2::theme_void() + 
  ggplot2::scale_fill_brewer(palette = "RdGy")

# tiempo promedio de abandono de los clientes que se comunican

RSQLite::dbListTables(con)
RSQLite::dbListFields(con, "bbdd")

query8 <- "SELECT AVG(b.tiempo_abandono) AS media_abandono FROM bbdd AS b"
RSQLite::dbGetQuery(con, query8) # 440.8313 


# estacionalidad en el comportamiento de las interacciones entrantes

query9 <- "SELECT b.* FROM bbdd AS b"
query9 <- "SELECT b.fecha, COUNT(b.entrante) AS n_entrante
           FROM bbdd AS b
           GROUP BY b.fecha
           ORDER BY b.fecha;"
X11()
RSQLite::dbGetQuery(con, query9)  %>% 
  dplyr::mutate(fecha = lubridate::ymd(fecha)) %>% 
  dplyr::as_tibble() %>% 
  ggplot2::ggplot(aes(fecha, n_entrante)) +
  ggplot2::geom_line(col= "blue") +
  ggplot2::scale_x_date(date_breaks = "3 week",
                        date_labels = "%d/%m") +
  ggplot2::xlab("Cada 3 semanas") +
  ggplot2::scale_y_continuous(breaks = seq(0, 270, 50)) +
  ggplot2::ylab("Cuenta") + 
  ggplot2::ggtitle("Interacciones entrantes 2022") +
  ggplot2::geom_rect(xmin = as.Date("2022-03-27"),
                     xmax = as.Date("2022-03-31"),
                     ymin = -Inf,
                     ymax = Inf,
                     alpha = 0.01,
                     fill = "gray") + 
  ggplot2::theme_light()


# Disconnect SQLite
RSQLite::dbDisconnect(con)
