library(knitr)
library(dplyr)
library(tidyverse)
library(magrittr)
library(ggplot2)
theme_set(theme_classic())

# Questão 9 ----------------------------------------------------------

# Visualizando distribuições a partir de gráfico de barra para
# variáveis categóricas

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))


# Mostrando os valores de cada barra

diamonds %>% 
  count(cut)

# Visualizando distribuições a partir de histogramas para
# variáveis contínuas

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

# Mostrando os valores de cada "barra" em blocos de 0,5

diamonds %>% 
  count(cut_width(carat, 0.5))

# É importante olhar a distribuição em diferentes "bandwidths"
# variável de interesse. Vamos observar como a distribuição 
# muda ao olharmos apenas os diamantes com carat < 3

smaller <- diamonds %>% 
  filter(carat < 3)

ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.1)

# Para comparar múltiplos histogramas, utilizar geom_freqpoly.

ggplot(data = smaller, mapping = aes(x = carat, colour = cut)) +
  geom_freqpoly(binwidth = 0.1)
