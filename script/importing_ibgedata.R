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
    regiao_metropolitana,
    pib_prc_corr = produto_interno_bruto_a_precos_correntes_r_1_000,
    pib_pc = produto_interno_bruto_per_capita_a_precos_correntes_r_1_00,
    vab_agro = valor_adicionado_bruto_da_agropecuaria_a_precos_correntes_r_1_000,
    vab_ind = valor_adicionado_bruto_da_industria_a_precos_correntes_r_1_000,
    vab_serv = valor_adicionado_bruto_dos_servicos_a_precos_correntes_exceto_administracao_defesa_educacao_e_saude_publicas_e_seguridade_social_r_1_000,
    vab_total = valor_adicionado_bruto_total_a_precos_correntes_r_1_000,
    impostos = impostos_liquidos_de_subsidios_sobre_produtos_a_precos_correntes_r_1_000,
    prop_vab_ag = vab_agro/vab_total,
    prop_vab_ind = vab_ind/vab_total,
    prop_vab_serv = vab_serv/vab_total,
    
  )
pib

frota <- readxl::read_excel(here::here("data","frota.xls"),skip = 2) %>% 
  janitor::clean_names()
frota <- 
frota %>% 
  transmute(
    uf = tolower(uf),
    municipio = abjutils::rm_accent(tolower(municipio)),
    total_frota = total,
    automovel,
    moto = motocicleta+ motoneta,
    trator = trator_estei+trator_rodas,
    onibus = onibus + micro_onibus
    
  )

# area plantada lavoura temporaria
area_plantada_temp <- 
  readxl::read_excel(here::here("data","tabela1612.xlsx"),skip = 4) %>% 
  janitor::clean_names() %>% 
  transmute(cod_ibge = x1,
         area_plantada_temp = as.numeric(total))

# valor  lavoura temporaria
valor_area_temp <- 
  readxl::read_excel(here::here("data","tabela1612.xlsx"),skip = 4,sheet =2) %>% 
  janitor::clean_names()%>% 
  transmute(cod_ibge = x1,
         valor_area_temp = as.numeric(total))

# lavoura permanente
area_plantada_perm <- 
  readxl::read_excel(here::here("data","tabela1613.xlsx"),skip = 4) %>% 
  janitor::clean_names()%>% 
  transmute(cod_ibge = x1,
         area_plantada_perm = as.numeric(total))

# valor lavoura permanente
valor_area_perm <- 
  readxl::read_excel(here::here("data","tabela1613.xlsx"),skip = 4,sheet =2) %>% 
  janitor::clean_names()%>% 
  transmute(cod_ibge = x1,
         valor_area_perm = as.numeric(total))


# cabeça de gado
cabeca <- 
  readxl::read_excel(here::here("data","tabela3939.xlsx"),skip = 4,sheet = 1) %>% 
  janitor::clean_names() %>% 
  transmute(
    cod_ibge = as.character(x1),
    bovino,suino_total,galinaceos_total)
# porcos


# população estimada
#pop21 <- sidra::get_sidra(6579, period = "2021", geo = "City")
pop_est <- 
  readxl::read_excel(here::here("data","tabela6579.xlsx"),skip = 3) %>% 
  janitor::clean_names() %>% 
  transmute(cod_ibge = as.character(x1),
         pop_est = as.numeric(x2021))

# area urbanizada
area_urb <- 
  readxl::read_excel(here::here("data","tabela8418.xlsx"),skip = 3,sheet =1) %>% 
  janitor::clean_names() %>% 
  transmute(cod_ibge = as.character(x1),
            area_urbanizada = as.numeric(x2019))

area_mapeada <- 
  readxl::read_excel(here::here("data","tabela8418.xlsx"),skip = 3,sheet =2) %>% 
  janitor::clean_names() %>% 
  transmute(cod_ibge = as.character(x1),
            area_mapeada = as.numeric(x2019))


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
  left_join(pop_est) %>% 
  left_join(area_urb) %>% 
  left_join(area_mapeada)


my_df <- 
  left_join(dados_ibge,frota) %>% 
  left_join(estban)
my_df <- my_df %>% 
  mutate_if(is.numeric,tidyr::replace_na,replace = 0) %>% 
  mutate(
    automovel_pc = automovel / pop_est,
    total_auto_pc = total_frota / pop_est) %>% 
  mutate_if(is.character,tidyr::replace_na,replace = "NA")

my_df %>% glimpse()

my_df %>% 
  # remover SP e RJ reduziu muito outliers e o peso da cauda.
  filter(!cod_ibge %in% c(3550308,3304557)) %>% 
  saveRDS(here::here("data","final.RDS"))

