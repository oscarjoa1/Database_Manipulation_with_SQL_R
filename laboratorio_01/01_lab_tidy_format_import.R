# limpar varavies
rm(list = ls())

# pacotes
library(tidyverse)

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


