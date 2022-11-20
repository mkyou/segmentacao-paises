library(highcharter)
nome_paises = read_csv("data/nome_paises.csv")

new_df = df |> inner_join(nome_paises)

new_df = new_df |> mutate(cluster = factor(cluster))
new_df = new_df |> select(cluster, name)

new_df = new_df [order(new_df$cluster, decreasing = F),]

new_df = new_df |> 
  mutate(value = cumsum(!duplicated(cluster)))

new_df

dta_clss = new_df |> 
  mutate(value = cumsum(!duplicated(cluster))) |> 
  group_by(cluster) |> 
  summarise(value = unique(value)) |> 
  arrange(value) |> 
  rename(name = cluster, from = value) |> 
  mutate(to = from + 1) |> 
  list_parse()

hcmap(joinBy = c("name"), data = new_df, value = "PC1") |>
  hc_colorAxis(
  dataClassColor = "category",
  dataClasses = dta_clss
) |> hc_title(text = "Mapa do mundo, agrupado pelos clusters")

#ranking de países pela PC1
select(df, pais, cluster)[order(df$PC1, decreasing = T),] |> 
  write_csv("data/ranking_paises_pca1.csv")
