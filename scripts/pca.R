par(mfrow = c(1,1))

df = read_csv("data/dados_paises.csv")
df = df |> filter(pais != "Camarões" | tx_mortalidade_inft != 61.18)
df = na.omit(df)

#pca com variáveis padronizadas
pca = prcomp(df |> select(-pais), scale. = T)
#componentes
summary(pca)

#vamos ficar com 3
pca$x[, 1:2]
plot(pca, type = "l")

par(mfrow = c(2,2))
pca3d::pca2d(pca, components = 1:2,
             title = "Biplot das componentes principais")
pca3d::pca2d(pca, components = c(1, 3))
pca3d::pca2d(pca, components = 2:3)

df = df |> bind_cols(pca$x[, 1:3])

cor(df |> select(-pais))[seq(1, 10), -seq(1,10)]

#componente 1 -> indices sociais como educacional, expectativa de vida
#pib, baixo crescimento populacional, baixa tx de urbanização (já estão
#urbanizados) baixíssima natalidade e mortalidade infantil

#componente 2 -> população

#componente 3 mais difícil de interpretar


df_sort_pc1 = df [order(df$PC1, decreasing = T),]
view(df_sort_pc1)  
