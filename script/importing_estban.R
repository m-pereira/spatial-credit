


## buscando as funçõees usadas
source(here::here("script","functions.R"))
# importando os dados
importing_estban(1) 

## lendo no R o arquivore gerado
my_tbl <- 
  readRDS(here::here("data","raw","estban.RDS"))  %>% 
  # selecionando colunas de interesse
  formating() %>% 
  # limpando a coluna data
  clean_date(.,x_data_base) %>%
  # agregando por UF
  aggregate_mun(.,codmun_ibge,data_ref) 

my_tbl %>% glimpse()
# salvando o arquivo
my_tbl %>% 
  filter(codmun_ibge !=0) %>% 
  saveRDS(file = here::here("data","cleaned.RDS"))  
