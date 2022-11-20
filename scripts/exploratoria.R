library(dplyr)
library(readr)
library(ggplot2)
library(tibble)
library(tidyr)

df = read_csv("data/dados_paises.csv")

df = df |> filter(pais != "Camarões" | tx_mortalidade_inft != 61.18)

#exploratoria-------------------------------------------------------
theme_set(theme_minimal())
df |> select(-pais) |> GGally::ggpairs()

df_aux = c(
  'Mínimo',
  'Mediana',
  'Média',
  'Máximo',
  'Desvio Padrão',
  'Coeficiente de Variação'
) |>
  bind_cols(
    df |> select(-pais) |> summarise_all(min, na.rm = T) |> 
      bind_rows(df |> select(-pais) |>
                  summarise_all(median, na.rm = T)) |> 
      bind_rows(df |> select(-pais) |> 
                  summarise_all(mean, na.rm = T)) |>
      bind_rows(df |> select(-pais) |>
                  summarise_all(max, na.rm = T)) |>
      bind_rows(df |> select(-pais) |>
                  summarise_all(sd, na.rm = T)) |>
      bind_rows(df |> select(-pais) |>
                  summarise_all(cmstatr::cv, na.rm = T))
  ) |>
  rename('Função' = '...1') 

df_aux = df_aux |> pivot_longer(col = -1) |> 
  pivot_wider(names_from = 'Função') |>
  rename(variavel = name)

df_aux = df_aux |> mutate(across(2:7, round, 3))
 
df = na.omit(df)

df |> select(-pais) |> lattice::parallelplot()

#boxplots----------------------------------------------------------
box1 = df |> ggplot(aes(y = ind_educ)) +
  geom_boxplot() + 
  labs(subtitle = "Boxplot do índice de educação",
       y = 'Índice de educação')

box2 = df |> ggplot(aes(y = exp_vida)) +
  geom_boxplot() + 
  labs(subtitle = "Boxplot da expectativa de vida",
       y = 'Expectativa de vida')

box3 = df |> ggplot(aes(y = pib_per_capta)) +
  geom_boxplot() + 
  labs(subtitle = "Boxplot do Pib per Capta",
       y = 'Pib per Capta')

box4 = df |> ggplot(aes(y = populacao)) +
  geom_boxplot() + 
  labs(subtitle = "Boxplot da população",
       y = 'População')

box5 = df |> ggplot(aes(y = tx_mortalidade_inft)) +
  geom_boxplot() + 
  labs(subtitle = "Boxplot da taxa de mortalidade infantil",
       y = 'Taxa de mortalidade infantil')

box6 = df |> ggplot(aes(y = tx_mortalidade_per_mil)) +
  geom_boxplot() + 
  labs(subtitle = "Boxplot da taxa de mortalidade per mil",
       y = 'Taxa de mortalidade per mil')

box7 = df |> ggplot(aes(y = tx_natalidade)) +
  geom_boxplot() + 
  labs(subtitle = "Boxplot da taxa de natalidade",
       y = 'Taxa de natalidade')

box8 = df |> ggplot(aes(y = tx_natalidade)) +
  geom_boxplot() + 
  labs(subtitle = "Boxplot do crescimento populacional",
       y = 'Crescimento populacional')

box9 = df |> ggplot(aes(y = perc_urb)) +
  geom_boxplot() + 
  labs(subtitle = "Boxplot do percentual urbano",
       y = 'Percentual urbano')

box10 = df |> ggplot(aes(y = tx_urb)) +
  geom_boxplot() + 
  labs(subtitle = "Boxplot da taxa de urbanização",
       y = 'Taxa de urbanização')



ggpubr::ggarrange(box1, box2, box3, box4, ncol = 2, nrow = 2)
ggpubr::ggarrange(box5, box6, box7, box8, ncol = 2, nrow = 2)
ggpubr::ggarrange(box9, box10, ncol = 2, nrow = 1)

par(mfrow = c(2,2))

hist(df$ind_educ, main = 'Indice de educação', col = 'chocolate')
hist(df$exp_vida, main = 'Expectativa de vida', col = 'chocolate')
hist(df$pib_per_capta, main = 'Pib per capta', col = 'chocolate')
hist(df$populacao, main = 'População', col = 'chocolate')

hist(df$tx_mortalidade_inft, main = 'Taxa de mortalidade infantil', 
     col = 'chocolate')
hist(df$tx_mortalidade_per_mil, main = 'Taxa de mortalidade per mil', 
     col = 'chocolate')
hist(df$tx_natalidade, main = 'Taxa de natalidade', 
     col = 'chocolate')
hist(df$crescimento, main = 'Crescimento populacional', 
     col = 'chocolate')

par(mfrow = c(1,2))
hist(df$perc_urb, main = 'Percentual urbano', 
     col = 'chocolate')
hist(df$tx_urb, main = 'Taxa de urbanização', 
     col = 'chocolate')

#verificando se pca é adequado
psych::KMO(df |> select(-pais)) #adequado
psych::cortest.bartlett(df |> select(-pais))

#Teste de Henze-Zirklers normalidade multivariada
#componentes principais não normais
mvnTest::HZ.test(df |> select(PC1, PC2, PC3))

#variáveis também não são normais multivariadas
mvnTest::HZ.test(df |> select(-PC1, -PC2, -PC3, -pais))

