# limpar varavies
rm(list = ls())

# pacotes
library(tidyverse)
library(RSQLite2)

# tabelas

# 2.1. sim
tb_1 <- tidyr::table1

# 2.2. nao
tb_2 <- tidyr::table2 %>% 
        tidyr::pivot_wider(names_from = "type",
                           values_from = "count")

# 2.3. nao
tb_3 <- tidyr::table3 %>%
        tidyr::separate(col = "rate",
                        into = c("cases", "population"),
                        sep = "/",
                        convert = TRUE)

# 2.4. nao
tb_4 <- tidyr::table4a %>% 
        tidyr::pivot_longer(cols = c(`1999`, `2000`),
                            names_to = "year",
                            values_to = "cases",
                            names_transform = list(year = as.integer))

# 2.5. nao
tb_5 <- tidyr::table4b %>%
        tidyr::pivot_longer(cols = c(`1999`, `2000`),
                            names_to = "year",
                            values_to = "population",
                            names_transform = list(year = as.integer))

# 3. 
rate <- tb_1 %>% 
        dplyr::mutate(rate = cases / population * 100000)

# 4. 
tb_1 %>%
  dplyr::group_by(year) %>%
  summarise(total = sum(cases))

# 5. 
tb_1 %>%
  dplyr::group_by(country) %>%
  dplyr::summarise(total = sum(cases))

# 6. 
tb_1 %>%
  dplyr::select(-population)

# 7. 
rate %>%
  ggplot(aes(x = year, y = rate, colour = country)) + geom_line()

tb_1 %>%
  ggplot(aes(x = year, y = cases / population * 100000, colour = country)) +
  geom_line()

# 8. 
tb_2 %>% 
  dplyr::mutate(rate = cases / population * 100000)

# (a funcao object.size permite saber o tamanho de um objeto na memoria
# um objeto inteiro consume menos memoria do que um dbl)

# 9 (caso da ajuda do R, relig_income)

# forma 1:
tidyr::relig_income %>% 
  tidyr::pivot_longer(cols = `<$10k`:`Don't know/refused`,
               names_to = "ordinal",
               values_to = "frequency",
               values_transform  = list(frequency = as.integer)) %>%
 dplyr:: mutate(ordinal = stringr::str_replace_all(ordinal, "[<$k]", "")) %>%
  dplyr::mutate(ordinal = stringr::str_replace(ordinal, "Don't now/refused", "dn"))

# forma 2:

tidyr::relig_income %>% 
  tidyr::pivot_longer(cols = !religion,
                      names_to = "ordinal",
                      values_to = "frequency",
                      values_transform  = list(frequency = as.integer)) %>%
  dplyr:: mutate(ordinal = stringr::str_replace_all(ordinal, "[<$k]", "")) %>%
  dplyr::mutate(ordinal = stringr::str_replace(ordinal, "Don't now/refused", "dn"))


# 10 (caso da ajuda do R, billboard)

tidyr::billboard %>%
  tidyr::pivot_longer(cols = starts_with("wk"),
                      names_to = "week",
                      values_to = "frequency",
                      values_transform = list("frequency" = as.integer),
                      values_drop_na = TRUE) %>%
  dplyr::mutate(week = as.integer(stringr::str_replace(week, "wk", "")))


# 11 (caso da ajuda do R, anscombe)
anscombe %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = c(".value", "set"),
    names_pattern = "(.)(.)")

# 12 importacao de dados volumosos

# forma 01
bd_1 <- readr::read_csv(
  file      = "C:/Users/oscar/Desktop/manipulacao_banco_dados/02_aquivos_dados/flights.csv.zip",
  col_names = T,
  col_types = cols(YEAR  = col_integer(),
                   MONTH = col_integer(),
                   DAY   = col_integer(),
                   DAY_OF_WEEK = col_integer(),
                   AIRLINE = col_character(),
                   FLIGHT_NUMBER = col_integer(),
                   TAIL_NUMBER = col_character(),
                   ORIGIN_AIRPORT = col_character(),
                   DESTINATION_AIRPORT = col_character(),
                   SCHEDULED_DEPARTURE = col_character(),
                   DEPARTURE_TIME = col_character(),
                   DEPARTURE_DELAY = col_integer(),
                   TAXI_OUT = col_integer(),
                   WHEELS_OFF = col_character(),
                   SCHEDULED_TIME = col_integer(),
                   ELAPSED_TIME = col_integer(),
                   AIR_TIME = col_integer(),
                   DISTANCE = col_integer(),
                   WHEELS_ON = col_character(),
                   TAXI_IN = col_integer(),
                   SCHEDULED_ARRIVAL = col_character(),
                   ARRIVAL_TIME = col_character(),
                   ARRIVAL_DELAY = col_integer(),
                   DIVERTED = col_integer(),
                   CANCELLED = col_integer(),
                   CANCELLATION_REASON = col_character(),
                   AIR_SYSTEM_DELAY = col_integer(),
                   SECURITY_DELAY = col_integer(),
                   AIRLINE_DELAY = col_integer(),
                   LATE_AIRCRAFT_DELAY = col_integer(),
                   WEATHER_DELAY = col_integer())
  )


# forma 02 

bd_1 <- readr::read_csv(
  file      = "C:/Users/oscar/Desktop/manipulacao_banco_dados/02_aquivos_dados/flights.csv.zip",
  col_names = T,
  col_types = cols(.default = col_integer(),
                   AIRLINE = col_character(),
                   TAIL_NUMBER = col_character(),
                   ORIGIN_AIRPORT = col_character(),
                   DESTINATION_AIRPORT = col_character(),
                   SCHEDULED_DEPARTURE = col_character(),
                   DEPARTURE_TIME = col_character(),
                   WHEELS_OFF = col_character(),
                   WHEELS_ON = col_character(),
                   SCHEDULED_ARRIVAL = col_character(),
                   ARRIVAL_TIME = col_character(),
                   CANCELLATION_REASON = col_character()
                   )
)



