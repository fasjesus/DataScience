# Instalar e carregar pacotes necessários
install.packages("caret")
install.packages("rpart.plot")
install.packages("e1071")
install.packages("ggplot2")

library(caret)
library(rpart)
library(rpart.plot)
library(e1071)
library(ggplot2)

# Definir o diretório de trabalho
setwd("C://Users/fdpc0/OneDrive/Área de Trabalho/uesclem/2024/DataScience/Laranjas")

# Importando o dataset
laranjas <- read.csv("laranja.csv", row.names = 1)

# Transformando dados de produção em (baixa, média, alta)
high_limit <- quantile(laranjas$Produção, 0.75)
low_limit <- quantile(laranjas$Produção, 0.25)

# Nova coluna no dataset com as categorias
laranjas$cat_prod <- cut(laranjas$Produção,
                         breaks = c(-Inf, low_limit, high_limit, Inf), 
                         labels = c("Baixa", "Média", "Alta"))

# Definir uma semente para reprodutibilidade
set.seed(42)

# Dividir os dados
train_index <- createDataPartition(laranjas$cat_prod, p = 0.8, list = FALSE)
train_data <- laranjas[train_index, ]
test_data <- laranjas[-train_index, ]

# Treinar o modelo de árvore de decisão
tree_model <- rpart(cat_prod ~ Cidade + Ano + Área_Plantada + Clima + Fertilizantes + Preço,
                    data = train_data, method = "class")

rpart.plot(tree_model)

# Fazer previsões sobre os dados de teste
test_predictions <- predict(tree_model, test_data, type = "class")

# Comparar previsões com os valores reais
confusion_matrix <- confusionMatrix(test_predictions, test_data$cat_prod)

# Exibir a matriz de confusão
print(confusion_matrix)

# Função para plotar a matriz de confusão
plot_confusion_matrix <- function(conf_matrix) {
  # Transformar a matriz de confusão em data frame
  conf_matrix_df <- as.data.frame(conf_matrix$table)
  
  # Criar o plot usando ggplot2
  ggplot(conf_matrix_df, aes(x = Prediction, y = Reference, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%d", Freq)), vjust = 1) +
    scale_fill_gradient(low = "white", high = "steelblue") +
    labs(x = "Predicted", y = "Actual", fill = "Frequency") +
    theme_minimal()
}

# Plotar a matriz de confusão
plot_confusion_matrix(confusion_matrix)
