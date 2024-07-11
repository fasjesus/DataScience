# Carregar bibliotecas necessárias
install.packages("igraph")
install.packages("ggplot2")
install.packages("corrplot")
library(corrplot)
library(igraph)
library(ggplot2)

# Definir o caminho para o arquivo CSV
file_path <- "dataset.csv"

# Ler os dados a partir do arquivo CSV, usando a primeira coluna como nomes das linhas
dados <- read.csv(file_path, row.names = 1)

# Verificar se o dataframe tem pelo menos duas colunas
if (ncol(dados) < 2) {
  stop("O dataframe deve ter pelo menos duas colunas para calcular correlacoes.")
}

# Calcular a matriz de correlacao
cor_matrix <- cor(dados)

# Funcao para teste de correlacao
cor_test <- function(x, y) {
  cor.test(x, y)$p.value
}

# Obter nomes das colunas
column_names <- names(dados)

# Inicializar uma lista para armazenar correlacoes significativas
significant_correlations <- list()

# Loop atraves de todas as combinacoes de pares de variaveis
for (i in 1:(length(column_names) - 1)) {
  for (j in (i + 1):length(column_names)) {
    p_value <- cor_test(dados[[i]], dados[[j]])
    if (p_value < 0.05) {
      significant_correlations <- append(significant_correlations, list(c(column_names[i], column_names[j])))
    }
  }
}

# Visualizacao das correlacoes significativas
cat("\n\n") 
print("Variaveis com correlacao significativa (p-value < 0.05):")
cat("\n") 
print(significant_correlations)

# Visualizar a matriz de correlacao usando corrplot
corrplot(cor_matrix, method = "circle", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black")

# Determinar o numero ideal de clusters usando o metodo do cotovelo
wss <- (nrow(dados)-1)*sum(apply(dados,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(dados, centers=i)$withinss)

# Plotar o metodo do cotovelo
plot(1:15, wss, type="b", pch = 19, frame = FALSE, 
     xlab="Numero de clusters K",
     ylab="Soma dos Quadrados Intra-Clusters")

# Escolher o numero de clusters com base no grafico (substituir 4 pelo numero escolhido)
num_clusters <- 4  # Mudar conforme a analise do grafico

# Clusterizacao (k-means)
set.seed(123)  # Para reprodutibilidade
kmeans_result <- kmeans(dados, centers = num_clusters)

# Adicionar o resultado da clusterizacao ao dataframe original
dados$cluster <- as.factor(kmeans_result$cluster)

# Visualizar os resultados da clusterizacao
cat("\n\n") 
print("Resultado da clusterizacao: ")
cat("\n") 
print(dados)

# Visualizacao dos Resultados da Clusterizacao
# Converter o dataframe em um data.frame para ggplot2
dados_plot <- as.data.frame(dados)

# Selecionar as duas primeiras colunas para o grafico de dispersao
colnames(dados_plot) <- make.names(colnames(dados_plot)) # Ajustar nomes das colunas
first_two_columns <- names(dados_plot)[1:2]

# Criar um grafico de dispersao dos resultados da clusterizacao
ggplot(dados_plot, aes_string(x = first_two_columns[1], y = first_two_columns[2], color = "cluster")) +
  geom_point(size = 3) +
  labs(title = "Resultados da Clusterizacao",
       x = first_two_columns[1], y = first_two_columns[2]) +
  theme_minimal()