# Importaçõa e manipulação de dados volumosos

- Os dados em estudo estão disponíveis em https://me315-unicamp.github.io/material/ e ferem-se a informações sobre voos nos Estados Unidos no ano de 2015, possui 5.714.008 observações ocupando 1GB de RAM com 6 tipos de variáveis: i ) Dia , mês, ano, dia da semana; ii) Companhia aérea, número do voo, matrícula da aeronave; iii) Aeroportos de partida e chegada; iv) Horas de partida e chegada (reais e previstas); v) Tempo de voo e distância percorrida; e vi) Chegada tardia.

- Lendo `100.000` observações por vez, determine o percentual de vôos por Cia. Aérea que apresentou atraso na chegada (`ARRIVAL_DELAY`) superior a `10` minutos. As companhias a serem utilizadas são: `AA`, `DL`, `UA` e `US`. A estatística de interesse deve ser calculada para cada um dos dias de `2015`. 

- Os arquivos *laboratorio_02.Rmd* contém a solução em linguagem *R* e *laboratorio_02.html* a saída em html com uma bonita apresentação.
