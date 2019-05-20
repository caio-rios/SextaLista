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

# Plotando um histograma de carat para observar fatores
# interessantes dos dados. Como a quantidade de diamantes com 
# carat inteiro.

ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

# Plotando outro histograma para observar como os dados se 
# comportam. Neste caso, observa-se doi clusters

ggplot(data = faithful, mapping = aes(x = eruptions)) + 
  geom_histogram(binwidth = 0.25)

# Quando há valores extremos ou outliers, o histograma inteiro
# fica concentrado em umas parte do eixo x como mostrado no
# exemplo abaixo.

ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5)

# Não conseguimos ver os valores para além de y=10 pois existem
# muitas observações no início da variável. Se limitarmos o
# eixo y para plotar apenas os valores menores do que 60,
# poderemos ver esses valores raros.

ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

# Observa-se que existem três valores raros próximo a 0, 30 e 60´.
# Para observá-los utilizamos o seguinte código.

unusual <- diamonds %>% 
  filter(y < 3 | y > 20) %>% 
  select(price, x, y, z) %>%
  arrange(y)
unusual

# Para melhor observar a distribuição, podemos retirar da base
# os valores raros.

diamonds2 <- diamonds %>% 
  filter(between(y, 3, 20))

# É mais recomendado substituir os valores raros por NA.

diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y))

# Ao plotar essa nova base, o ggplot2 remove os NAs mas, pelo 
# menos avisa quantos dados foram removidos.

ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point()

# Nem sempre NA significa missing values. No exemplo abaixo,
# a base registra os voos feitos e NA significa voos cancelados.
# Para observá-los podemos utilizar a função is.na.

nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(sched_dep_time)) + 
  geom_freqpoly(mapping = aes(colour = cancelled), binwidth = 1/4)

# Analisando covariação entre variáveis categóricas e contínuas.
# Vamos observar a covariação do preço do diamante e a qualidade. 

ggplot(data = diamonds, mapping = aes(x = price)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

# Difícil observar pois a quantidade varia muito.

ggplot(diamonds) + 
  geom_bar(mapping = aes(x = cut))

# Vamos trocar o y para densidade ao em vez de contagem.

ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

# Outra maneira de observar essa relação é por boxplot.

ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot()

# Vamos observar agora uma variável categórica que não é ordinal.

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()

# Para melhorar a visualização podemos reordenar o eixo x.

ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, 
                                         FUN = median), 
                             y = hwy))

# Caso o nome das variáveis sejam longas, podemos inverter os 
# eixos.

ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, 
                                         FUN = median), 
                             y = hwy)) +
  coord_flip()

# Para observar duas variáveis categóricas precisamos contar
# o número de observações para cada categoria.

ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color))

# Cada círculo mostra quantas ocorrências combinadas houveram.


# Podemos, também, calcular cada ocorrência combinada com dplyr.

diamonds %>% 
  count(color, cut)

# Outra maneira seria com uma especie de mapa de calor. 
# O geom_tile.

diamonds %>% 
  count(color, cut) %>%  
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n))


# Para duas variáveis contínuas podemos observar um gráfico de
# dispersão de pontos.

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price))

# Esse gráficos se tornam menos úteis quanto maior for a 
# quantidade de dados. Podemos adicionar transparência aos pontos
# para o gráfico ficar mais interessante.

ggplot(data = diamonds) + 
  geom_point(mapping = aes(x = carat, y = price), 
             alpha = 1 / 100)

# Uma maneira mais interessante ainda de observar grandes bases
# de dados é utilizar o geom_bin2d e o geom_hex. Com ele, os 
# pontos que se sobrepões recebem cores diferentes.

ggplot(data = smaller) +
  geom_bin2d(mapping = aes(x = carat, y = price))

library("hexbin")

ggplot(data = smaller) +
  geom_hex(mapping = aes(x = carat, y = price))

# Podemos, ainda, tratar uma variável contínua como uma 
# categórica e plotar boxplots. Com o cut_width podemos agrupar
# os dados.

ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))

# Podemos também plotar em cada boxplot o mesmo número de pontos.

ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_number(carat, 20)))

# Padrões podem ser visualizados ao plotar os nossos dados.
# Neste exemplo, claramente observa-se dois clusters.

ggplot(data = faithful) + 
  geom_point(mapping = aes(x = eruptions, y = waiting))

# As vezes é difícil entender a relação entre duas variáveis,
# pois outra variável pode está correlacionada com ambas,
# variáveis de interesse. Um exemplo disso é a relação entre 
# a qualidade do diamente e o preço pois a variável peso 
# está relacionada tanto com o preço quanto com a qualidade do
# diamante. Podemos retirar a relação entre peso e preço do 
# diamante ao substituímos essa relação pelo seu resíduo.
# Com os resíduos podemos observar o preço do diamante sem o 
# efeito do peso dele.

library(modelr)

mod <- lm(log(price) ~ log(carat), data = diamonds)

diamonds2 <- diamonds %>% 
  add_residuals(mod) %>% 
  mutate(resid = exp(resid))

ggplot(data = diamonds2) + 
  geom_point(mapping = aes(x = carat, y = resid))

# Uma vez retirada essa relação do peso com o preço, podemos
# de fato observar a relação entre qualidade e preço dos 
# diamentes.

ggplot(data = diamonds2) + 
  geom_boxplot(mapping = aes(x = cut, y = resid))


# Questão 10 -------------------------------------------------

load("vote_growth_usa.RData")
reg <- lm(Vote ~ Growth, data = bd) 
summary(reg)


# Questão 11--------------------------------------------------

# Selecionando os dados de 1876 a 1932

bd_select <- bd[1:15, ]

# Análise de Regressão

reg2 <- lm(Vote ~ Growth, data = bd_select) 
summary(reg2)
