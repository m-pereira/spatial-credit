## carregar os pacotes
library(tidyverse) # ferramentas para análise de dados
library(janitor) # ferramentas para limpeza de dados
library(skimr) # análise de qualidade de dados 
library(dataxray) # exploração de dados automática
library(explore) # exploração de dados automática
library(DataExplorer) # exploração de dados automática
library(correlationfunnel) # análise de correlação
library(ggcorrplot) # análise de correlação

### carregar os dados
dados  <-  readRDS(here::here("data","final.RDS"))
## primeiro resumo dos dados com glimpse 
dados %>% glimpse()
dados %>% head()
## avaliar qualidade dos dados com skimr
skim(dados)

## EDA automática com DataExplorer
set.seed(50)
dados %>% 
  create_report(., y = "credito")

## EDA automática com xray
dados %>% 
  make_xray() %>% 
  view_xray()
## EDA automática com 
dados %>% explore() 

## análise de correlação
dados %>% 
  select(-1) %>% 
  cor() %>% 
  ggcorrplot(
    type = "lower",
    insig = "blank",
    #   lab = TRUE,
    digits = 3
  )

## correlation funnel
dados %>% 
  select_if(is.numeric) %>%
  select(-outros,-trator_rodas) %>% 
  binarize(n_bins = 2) %>% 
  glimpse()


dados %>% 
  select_if(is.numeric) %>%
  select(-outros,-trator_rodas) %>% 
  binarize(n_bins = 2) %>% 
  correlate(target = credito__7244782_Inf) %>% 
  plot_correlation_funnel(interactive = TRUE)
