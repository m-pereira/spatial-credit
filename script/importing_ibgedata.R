# 
library(tidyverse)
pib <- readxl::read_excel(here::here("data","pib.xls")) %>% 
  janitor::clean_names()
pib %>% names()
pib <- 
pib %>% 
  filter(ano == 2020) %>% 
  transmute(
    cod_ibge = codigo_do_municipio,
    uf = tolower(sigla_da_unidade_da_federacao),
    municipio = abjutils::rm_accent(tolower(nome_do_municipio)),
    microrregiao = nome_da_microrregiao,
    mesorregiao = nome_da_mesorregiao,
    nome_da_regiao_rural,
    pib_prc_corr = produto_interno_bruto_a_precos_correntes_r_1_000,
    vab_agro = valor_adicionado_bruto_da_agropecuaria_a_precos_correntes_r_1_000,
    vab_ind = valor_adicionado_bruto_da_industria_a_precos_correntes_r_1_000,
    vab_serv = valor_adicionado_bruto_dos_servicos_a_precos_correntes_exceto_administracao_defesa_educacao_e_saude_publicas_e_seguridade_social_r_1_000,
    vab_total = valor_adicionado_bruto_total_a_precos_correntes_r_1_000,
    impostos = impostos_liquidos_de_subsidios_sobre_produtos_a_precos_correntes_r_1_000
  )
pib

frota <- readxl::read_excel(here::here("data","frota.xls"),skip = 2) %>% 
  janitor::clean_names()
frota <- 
frota %>% 
  mutate(
    uf = tolower(uf),
    municipio = abjutils::rm_accent(tolower(municipio))
    
  )

# area plantada lavoura temporaria
area_plantada_temp <- 
  readxl::read_excel(here::here("data","tabela1612.xlsx"),skip = 4) %>% 
  janitor::clean_names() %>% 
  transmute(cod_ibge = x1,
         area_plantada_temp = total)

# valor  lavoura temporaria
valor_area_temp <- 
  readxl::read_excel(here::here("data","tabela1612.xlsx"),skip = 4,sheet =2) %>% 
  janitor::clean_names()%>% 
  transmute(cod_ibge = x1,
         valor_area_temp = total)

# lavoura permanente
area_plantada_perm <- 
  readxl::read_excel(here::here("data","tabela1613.xlsx"),skip = 4) %>% 
  janitor::clean_names()%>% 
  transmute(cod_ibge = x1,
         area_plantada_perm = total)

# valor lavoura permanente
valor_area_perm <- 
  readxl::read_excel(here::here("data","tabela1613.xlsx"),skip = 4,sheet =2) %>% 
  janitor::clean_names()%>% 
  transmute(cod_ibge = x1,
         valor_area_perm = total)


# cabeça de gado
cabeca <- 
  readxl::read_excel(here::here("data","tabela3939.xlsx"),skip = 4) %>% 
  janitor::clean_names() %>% 
  transmute(
    cod_ibge = x1)


# população estimada
#pop21 <- sidra::get_sidra(6579, period = "2021", geo = "City")
pop_est <- 
  readxl::read_excel(here::here("data","tabela6579.xlsx"),skip = 3) %>% 
  janitor::clean_names() %>% 
  transmute(cod_ibge = x1,
         pop_est = x2021)

# estban 
estban <- readRDS(here::here("data","cleaned.RDS")) %>% 
  ungroup() %>% 
  transmute(
    cod_ibge=as.character(codmun_ibge),
    credito)


## joins 
dados_ibge <- 
  pib %>% 
  mutate(cod_ibge = as.character(cod_ibge)) %>% 
  left_join(valor_area_temp) %>% 
  left_join(area_plantada_temp) %>% 
  left_join(area_plantada_perm) %>% 
  left_join(valor_area_perm) %>% 
  left_join(cabeca) %>% 
  left_join(pop_est)


my_df <- 
  left_join(dados_ibge,frota) %>% 
  left_join(estban)
my_df <- my_df %>% 
  mutate_if(is.numeric,tidyr::replace_na,replace = 0)
my_df %>% glimpse()

my_df %>% saveRDS(here::here("data","final.RDS"))

