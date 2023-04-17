#------------------------------------------------------#
# Autor: Oscar J. O. Ayala
#------------------------------------------------------#

# limpar varavies
rm(list = ls())

#1. pacote
library(tidyverse)
library(magrittr)

#2. tabelas
tidyr::table1 %>%
  knitr::kable(caption = "Table 1. Tidy format")

tidyr::table2 %>%
  knitr::kable(caption = "Table 2. Not tidy format")

tidyr::table3 %>%
  knitr::kable(caption = "Table 3. Not tidy format") 

tidyr::table4a %>%
  knitr::kable(caption = "Table 4a. Not tidy format")

tidyr::table4b %>%
  knitr::kable(caption = "Table 4b. Not tidy format") 

# 3. 
taxas <- with(tidyr::table1, cases / population * 10000)

# 4.
tidyr::table1 %>% dplyr::group_by(year) %>% 
  dplyr::summarise(total_cases = sum(cases))

# 5.
tidyr::table1 %>% 
  dplyr::group_by(country) %>% 
  dplyr::summarise(total_cases = sum(cases))

# 6.
tidyr::table1 %>% dplyr::select(-population)

# 7.
tidyr::table1 %>% 
  dplyr::mutate(taxas = cases / population * 10000)  %>% 
  ggplot2::ggplot(ggplot2::aes(year, taxas, group = country)) +
  ggplot2::geom_line(ggplot2::aes(color = country)) + 
  ggplot2::geom_point(ggplot2::aes(color = country), shape = 15) +
  ggplot2::scale_x_continuous(breaks = c(1999, 2000)) +
  ggplot2::labs(title = "Mudança casos de Tuberculoses", x = "ano",
                y = "Taxa por 10.000 pessoas") +
  ggplot2::scale_color_discrete("País") +
  ggplot2::theme_light()

# 8. 
table2 %>% tidyr::pivot_wider(id_cols = c("country", "year"), 
                              names_from = type, values_from = count) %>% 
  dplyr::mutate(taxas = cases / population * 10000) %>% 
  knitr::kable(caption = "Formato tidy")

# 9. 
table4a %<>% tidyr::pivot_longer(cols = c("1999", "2000"), names_to = "year", 
                                values_to = "cases", values_transform = as.integer)

table4b %<>% tidyr::pivot_longer(cols = c("1999", "2000"), names_to = "year", 
                                values_to = "population", values_transform = as.integer)

table_ab <- dplyr::inner_join(table4a, table4b, by = c("country", "year"))
table_ab %>% knitr::kable(caption = "Inner join")

# 10. 
table3 %>% 
  tidyr::separate(col = rate, into = c("cases", "population"), 
                  sep = "/", convert = TRUE) %>% 
  knitr::kable(caption = "Formato tidy")

#-----------------------------------FIM--------------------------------#

