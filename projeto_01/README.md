# Introdução

O conjunto de dados reais em questão foi disponibilizado pela empresa *Millenium s.a*, que
refere-se a *5* (cinco) planilhas Excel de dados na sua forma bruta, rotuladas como *BBDD*, *Param_Horas*,
*Param_Datas*, *Param_Queues* e *Param_Channels*. As tabelas não possuem chaves identificadas, portanto foi necessário identificar as chaves primária e secundária e fazer a normalização e relacionamentos.
O objetivo é descobrir conhecimentos dos dados que permitem obter indicadores sobre a situação de uma determinada organização. 
É usada o Metodologia de *Mineração de Dados* para realizar validações de qualidade e análises estatísticas relevantes, juntamente com a abordagem de banco de dados relacional e ETL (Extração, Transformação e Carga). O suportes informáticos utilizados para a correta análise, criação de bases de dados e apresentação de
os resultados foram *R*, *SQLite* (por ser open source), *Excel*, *RMarkdown* e *Power BI*. Os arquivos se descrevem assim:

- O arquivo *02_scriptAnalysis_Oscar.R* contém a solução em linguagem *R* e *SQL*.
- O arquivo *02_DataMining.xlsx* contém os dados brutos.
- Os quivos em formato *.csv* contém as tabelas preparadas e normalizadas para a análises estatística. 
- O arquivo *03_dbseletivo.sqlite3* contém o banco de dados em *SQL*
- O arquivo *04_mapa_relaciones.pbix* contém o modelo interativo em *Power BI*
