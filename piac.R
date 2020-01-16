# PIAC
getwd()
setwd('C:/Users/jadso.FRANCISCOJADSON/Documents/OMT/Construcao_Civil')

library(readxl)
library(magrittr)
library(dplyr)
library(ggcorrplot)
library(stats)

dados <- read_excel('piac.xlsx', sheet = 1)


#Fazendo subset

# dados brasil

dados$uf <- as.factor(dados$uf)
niveis <- levels(dados$uf)

pessoal_ocupado_b <- dados %>%
  filter(uf == niveis[1]) %>%
  select(pessoal_ocupado)
         
empresas_ativas_b <- dados %>%
  filter(uf == niveis[1]) %>%
  select(empresas_ativas)

r_liquida_b <- dados %>%
  filter(uf == niveis[1]) %>%
  select(receita_liquida)

br <- pessoal_ocupado_b
br[2] <- empresas_ativas_b
br[3] <- r_liquida_b


corr <- round(cor(br), 1)
p.mat <- cor_pmat(br)


ggcorrplot::ggcorrplot(corr, method = 'square', 
                       colors = c('red', 'white', 'green'),
                       type = 'upper',
                       #ggtheme = ggthemes::theme_clean(),
                       lab = T,
                       hc.order = T,
                       outline.color = 'grey',
                       legend.title = 'Correlação')

#dados no
pessoal_ocupado_n <- dados %>%
  filter(uf == niveis[2]) %>%
  select(pessoal_ocupado)

empresas_ativas_n <- dados %>%
  filter(uf == niveis[2]) %>%
  select(empresas_ativas)

r_liquida_n <- dados %>%
  filter(uf == niveis[2]) %>%
  select(receita_liquida)

no <- pessoal_ocupado
no[2] <- empresas_ativas
no [3] <- r_liquida


corr <- round(cor(no), 1)

ggcorrplot::ggcorrplot(corr, method = 'square', 
                       colors = c('red', 'white', 'green'),
                       type = 'upper',
                       #ggtheme = ggthemes::theme_clean(),
                       lab = T,
                       hc.order = T,
                       outline.color = 'grey',
                       legend.title = 'Correlação')
# dados ma

pessoal_ocupado_m <- dados %>%
  filter(uf == niveis[2]) %>%
  select(pessoal_ocupado)

empresas_ativas_m <- dados %>%
  filter(uf == niveis[2]) %>%
  select(empresas_ativas)

r_liquida_m <- dados %>%
  filter(uf == niveis[2]) %>%
  select(receita_liquida)

ma <- pessoal_ocupado
ma[2] <- empresas_ativas
ma [3] <- r_liquida


corr <- round(cor(br), 1)

ggcorrplot::ggcorrplot(corr, method = 'square', 
                       colors = c('red', 'white', 'green'),
                       type = 'upper',
                       #ggtheme = ggthemes::theme_clean(),
                       lab = T,
                       hc.order = T,
                       outline.color = 'grey',
                       legend.title = 'Correlação')


