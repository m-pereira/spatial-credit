# metadados
fonte <- 
  tribble(
    ~dados , ~fonte, ~link,
    "pib e vab" , "ibge" , "https://www.ibge.gov.br/estatisticas/economicas/contas-nacionais/9088-produto-interno-bruto-dos-municipios.html?=&t=resultados",
    "crédito", "estban", "https://www4.bcb.gov.br/fis/cosif/estban.asp?frame=1",
    "produção/valor - temporárias", "sidra", "https://sidra.ibge.gov.br/tabela/1612",
    "produção/valor - permanente", "sidra", "https://sidra.ibge.gov.br/tabela/1613",
    "efetivo - gado", "sidra", "https://sidra.ibge.gov.br/tabela/3939",
    "alternativa - pib", "ibge", "https://sidra.ibge.gov.br/tabela/5938",
    "frota veículos" , "gov", "https://www.gov.br/infraestrutura/pt-br/assuntos/transito/conteudo-Senatran/frota-de-veiculos-2022",
    "população estimada" , "sidra", "https://sidra.ibge.gov.br/tabela/5938",
    "saneamento", "sidra", "https://sidra.ibge.gov.br/tabela/1364",
    "estrutura municipal","sidra", "https://sidra.ibge.gov.br/tabela/6328",
    "descupados/sem trabalho", "sidra", "https://sidra.ibge.gov.br/tabela/4093"
  )

fonte
# https://www.modelodomundo.com/post/2023-01-20-importando-dados-do-sidra/
