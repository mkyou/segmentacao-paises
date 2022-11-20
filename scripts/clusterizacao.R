library(cluster)
library(purrr)
par(mfrow = c(1,1))

wss = function(k){
  kmeans(df |> select(PC1, PC2, PC3), k, nstart = 25)$tot.withinss
}

avg_sil = function(k){
  km_res = kmeans(df |> select(PC1, PC2, PC3), k, nstart = 25)
  ss = silhouette(km_res$cluster, dist(df |> select(PC1, PC2, PC3)))
  mean(ss[, 3])
}

k_values = 2:15

wss_values = map_dbl(k_values, wss)

plot(k_values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Número de clusters",
     ylab="Soma de quadrados totais intra-clusters",
     main = "Método de Elbow")

avg_sil_values = map_dbl(k_values, avg_sil)

plot(k_values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Número de clusters K",
     ylab = "Silhouette média",
     main = "Silhouettes")

#melhor valor é o que maximiza
gap_stat = clusGap(df |> select(PC1, PC2, PC3), 
                   FUN = kmeans, nstart = 25,
                   K.max = 10, B = 50)

gap_stat

#os métodos indicaram um número de clusters diferentes
#mas em todos eles 4 era um dos melhores números.

final = kmeans(df |> select(PC1, PC2, PC3), 4, nstart = 25)
df = df |> bind_cols(final$cluster) |> rename(cluster = ...15)
df |> group_by(cluster) |> select(-PC1, -PC2, -PC3, -pais) |>
  summarise_all(mean) |> view()


