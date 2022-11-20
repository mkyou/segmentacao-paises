library(rvest)
library(dplyr)
library(readr)

#populações------------------------------------------------------------
url_pop = "https://pt.wikipedia.org/wiki/Lista_de_países_por_população"

tab_pop = url_pop |>  
  read_html() |>
  html_node('table.wikitable') |> html_table()

tab_pop |> head()

tab_pop = tab_pop |> rename(pais = `País (ou território dependente)`,
                  populacao = `Estimativa da ONU`)
tab_pop = tab_pop |> select(pais, populacao)

tab_pop = tab_pop |> mutate(populacao = gsub(" ", "", populacao)) |>
  mutate(populacao = as.numeric(populacao))

#crescimento_pop---------------------------------------------------------
url_cresc_pop = c("https://pt.wikipedia.org/wiki/", 
            "Lista_de_países_por_crescimento_populacional")
url_cresc_pop = paste(url_cresc_pop[1], url_cresc_pop[2], sep = "")

tab_cresc_pop = url_cresc_pop |>  
  read_html() |>
  html_node('table.wikitable') |> html_table()

tab_cresc_pop |> head()

tab_cresc_pop = tab_cresc_pop |> rename(pais = `País`,
                            crescimento = 
                              `Índice de crescimento populacional anual(%)`)

tab_cresc_pop = tab_cresc_pop |> select(pais, crescimento)

tab_cresc_pop = tab_cresc_pop |> mutate(pais = gsub("\\[\\d\\]", "", pais)) |>
  mutate(crescimento = as.numeric(crescimento))

tab_cresc_pop = tab_cresc_pop |> mutate(pais = gsub("\\(\\.+\\)", "", pais))

tab_cresc_pop = tab_cresc_pop |> 
  mutate(pais = ifelse(pais == "França (França Metropolitana apenas)",
                                "França", pais))

#taxa_urbanizacao---------------------------------------------------------
url_tx_urb = c("https://pt.wikipedia.org/wiki/",
               "Lista_de_países_e_territórios_por_índice_de_urbanização")

url_tx_urb = paste(url_tx_urb[1], url_tx_urb[2], sep = "")

tab_tx_urb = url_tx_urb |>  
  read_html() |>
  html_node('table.wikitable') |> html_table()

tab_tx_urb |> head()

tab_tx_urb = tab_tx_urb |> rename(pais = `Território`,
                                  perc_urb = `População urbana (%)`,
                                  tx_urb = `Taxa de urbanização (%)`)

tab_tx_urb = tab_tx_urb |> select(pais, perc_urb, tx_urb)

tab_tx_urb = tab_tx_urb |> 
  mutate(perc_urb = as.numeric(gsub("\\[\\d\\]", "", perc_urb))) |>
  mutate(tx_urb = as.numeric(tx_urb))



#pip_per_capta--------------------------------------------------------
url_pib = c("https://pt.wikipedia.org/wiki/Lista_de_países_por_PIB_",
"(Paridade_do_Poder_de_Compra)_per_capita")
url_pib = paste(url_pib[1], url_pib[2], sep = "")

tab_pib = url_pib |>  
  read_html() |>
  html_node('table.wikitable') |> html_table()

tab_pib = tab_pib |> rename(pib_per_capta = `Int$`,
                  pais = `País`) |> select(-`Posição`) |>
  mutate(pib_per_capta = gsub(",", "", pib_per_capta)) |>
  mutate(pib_per_capta = as.numeric(pib_per_capta))

#expectativa_de_vida--------------------------------------------------
url_exp_vida = c("https://pt.wikipedia.org/wiki/",
"Lista_de_países_por_esperança_média_de_vida_à_nascença")

url_exp_vida = paste(url_exp_vida[1], url_exp_vida[2], sep = "")

tab_exp_vida = url_exp_vida |>
  read_html() |> 
  html_node('table.wikitable') |> html_table()

tab_exp_vida = tab_exp_vida |> select(`País`, `Expectativa de vida geral`) |>
  rename(exp_vida = `Expectativa de vida geral`, pais = `País`) |>
  mutate(exp_vida = gsub("\\[\\d\\]", "", exp_vida)) |>
  mutate(exp_vida = as.numeric(exp_vida))

#taxa_natalidade-----------------------------------------------------
url_tx_natalidade = c("https://pt.wikipedia.org/wiki/",
"Lista_de_países_por_taxa_de_natalidade")

url_tx_natalidade = paste(url_tx_natalidade[1], url_tx_natalidade[2], 
                          sep = "")

tab_tx_natalidade = url_tx_natalidade |>
  read_html() |>
  html_node('table.wikitable') |> html_table()

names(tab_tx_natalidade) = c('id1', 'id2', 'pais', 'tx_natalidade', 'ano')
tab_tx_natalidade = tab_tx_natalidade[-seq(1,2),] |> 
  select(pais, tx_natalidade) |>
  mutate(tx_natalidade = as.numeric(tx_natalidade))

tab_tx_natalidade = tab_tx_natalidade |> 
  mutate(pais = ifelse(pais == "República Popular da China", "China", pais))

#taxa_mortalidade---------------------------------------------------
url_tx_mortalidade = c("https://pt.wikipedia.org/wiki/",
"Lista_de_países_por_índice_de_mortalidade")

url_tx_mortalidade = paste(url_tx_mortalidade[1], url_tx_mortalidade[2], 
                           sep = "")

tab_tx_mortalidade = url_tx_mortalidade |>
  read_html() |>
  html_node('table.wikitable') |> html_table()

tab_tx_mortalidade = tab_tx_mortalidade |> 
  select(`País`, `Índice de mortalidade ‰`) |>
  rename(pais = `País`, tx_mortalidade_per_mil = `Índice de mortalidade ‰`)

#taxa_mortalidade_infantil-----------------------------------------
url_tx_mort_inf = c("https://pt.wikipedia.org/wiki/",
"Lista_de_países_por_índice_de_mortalidade_infantil")

url_tx_mort_inf = paste(url_tx_mort_inf[1], url_tx_mort_inf[2],
                        sep = "")

tab_tx_mort_inf = url_tx_mort_inf |>
  read_html() |>
  html_node('table.wikitable') |> html_table()

tab_tx_mort_inf = tab_tx_mort_inf |> 
  select(`País ou território`, `2015 /2020`)

tab_tx_mort_inf = tab_tx_mort_inf[-1,] |>
  rename(pais = `País ou território`,
         tx_mortalidade_inft = `2015 /2020`)

#indice_educacao--------------------------------------------------
url_educ = "https://pt.wikipedia.org/wiki/índice_de_educação"

tab_educ = url_educ |>
  read_html() |>
  html_node('table.wikitable') |> html_table()

tab_educ = tab_educ |> select(c(1, 15))
tab_educ = tab_educ[-1,] |>
  rename(pais = `País`,
         ind_educ = `Índice de Educação`)


#df---------------------------------------------------------------

df = tab_educ |> inner_join(tab_exp_vida) |> inner_join(tab_pib) |>
  inner_join(tab_pop) |> inner_join(tab_cresc_pop) |>
  inner_join(tab_tx_urb) |>
  inner_join(tab_tx_mort_inf) |> 
  inner_join(tab_tx_mortalidade) |>
  inner_join(tab_tx_natalidade)
  

write_csv(df, "data/dados_paises.csv")
