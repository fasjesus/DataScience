# Carregar bibliotecas necessarias
install.packages("corrplot")
install.packages("corrgram")
install.packages("ggplot2")
install.packages("dplyr")

library(corrplot)
library(corrgram)
library(readr)   # Para leitura de dados
library(dplyr)   # Para manipulação de dados
library(ggplot2) # Para plotagem de gráficos

# Dados fornecidos
dados <- "
,Matéria_1,Matéria_2,Matéria_3,Matéria_4,Matéria_5,Matéria_6,Matéria_7,Matéria_8,Matéria_9,Matéria_10,Matéria_11,Matéria_12,Matéria_13,Matéria_14,Matéria_15
Aluno_1,95,89,8,8,90,10,0,100,91,33,54,45,45,94,61
Aluno_2,98,88,4,0,96,93,0,100,51,74,93,57,57,0,55
Aluno_3,91,91,1,2,97,2,100,100,10,60,59,55,52,44,88
Aluno_4,100,100,4,1,93,3,100,0,33,45,24,47,43,55,27
Aluno_5,98,90,8,5,100,3,0,0,88,58,7,59,42,19,94
Aluno_6,97,85,5,3,97,7,100,50,75,98,79,53,37,59,21
Aluno_7,93,96,6,13,2,5,100,0,83,90,14,48,59,48,42
Aluno_8,93,87,3,1,2,96,0,0,37,69,49,46,66,5,43
Aluno_9,95,89,3,6,6,10,0,0,93,56,78,53,62,18,94
Aluno_10,98,95,9,13,4,4,0,100,51,52,66,41,38,36,99
Aluno_11,97,85,6,4,10,1,0,0,48,82,19,54,66,61,100
Aluno_12,98,88,4,10,91,9,0,0,36,46,33,49,43,43,93
Aluno_13,100,99,0,0,97,8,0,100,29,28,57,41,46,81,48
Aluno_14,94,92,3,5,1,9,0,100,92,98,24,51,41,49,64
Aluno_15,99,96,0,2,91,1,0,100,91,85,25,41,64,47,9
Aluno_16,98,100,10,15,3,10,0,100,23,18,91,46,59,66,20
Aluno_17,98,91,6,10,98,92,0,0,3,16,98,40,53,76,77
Aluno_18,97,88,6,10,90,0,0,50,52,80,21,56,60,55,82
Aluno_19,99,98,2,2,100,0,0,100,16,2,90,54,50,75,36
Aluno_20,92,94,9,6,94,3,0,50,76,31,66,47,69,59,97
Aluno_21,90,85,1,3,91,97,0,50,56,54,49,40,67,39,60
Aluno_22,100,86,7,12,2,3,100,100,65,18,13,44,38,62,77
Aluno_23,98,100,8,7,0,1,0,50,19,18,22,44,52,64,8
Aluno_24,92,90,9,13,100,9,100,50,40,7,45,40,60,10,72
Aluno_25,90,98,0,0,8,93,100,0,23,41,89,49,66,4,14
Aluno_26,98,88,7,12,97,7,0,100,87,52,55,53,49,13,31
Aluno_27,90,99,8,1,4,100,0,0,54,76,13,57,66,89,49
Aluno_28,96,98,0,13,96,99,100,0,68,15,8,49,47,14,96
Aluno_29,90,99,8,8,97,5,100,100,91,62,83,48,56,91,96
Aluno_30,96,89,8,7,5,6,0,50,18,46,32,48,63,95,64
Aluno_31,91,90,6,0,95,6,100,0,73,34,84,54,50,24,76
Aluno_32,99,92,10,4,95,99,0,0,27,36,69,52,38,40,87
Aluno_33,98,100,8,11,9,8,100,0,57,29,58,48,67,34,17
Aluno_34,95,87,3,13,5,92,100,100,56,40,85,58,47,78,71
Aluno_35,95,95,3,13,8,100,0,100,17,44,46,55,68,63,14
Aluno_36,93,86,3,5,92,2,0,100,82,69,51,46,54,46,16
Aluno_37,95,96,7,7,93,4,100,0,85,92,62,45,41,12,41
Aluno_38,97,97,3,2,96,3,0,100,62,19,35,55,42,54,84
Aluno_39,93,94,7,10,0,5,0,0,29,4,6,58,59,13,53
Aluno_40,94,96,7,9,94,2,100,0,25,45,2,55,42,12,10
Aluno_41,92,90,10,14,94,2,100,100,41,92,65,54,50,24,54
Aluno_42,96,85,2,13,3,9,100,0,89,70,52,55,57,2,71
Aluno_43,91,97,3,14,8,2,100,0,82,68,80,51,46,87,51
Aluno_44,93,91,6,13,97,9,0,0,64,28,71,44,41,69,52
Aluno_45,98,100,2,14,3,8,0,100,84,49,10,55,40,68,17
Aluno_46,98,91,10,7,90,2,100,50,77,9,55,55,69,99,61
Aluno_47,92,97,6,11,8,9,0,100,24,49,84,41,56,15,50
Aluno_48,95,88,8,0,98,9,100,100,25,75,61,50,53,91,80
Aluno_49,96,91,4,14,98,92,100,50,70,97,81,60,40,81,70
Aluno_50,95,95,4,1,4,7,100,100,68,82,42,54,54,98,92
Aluno_51,100,96,6,1,99,0,0,100,53,56,95,43,49,33,97
Aluno_52,98,97,0,6,10,100,100,100,78,20,56,42,47,91,39
Aluno_53,96,96,1,2,91,0,0,0,35,97,60,44,51,25,1
Aluno_54,97,94,9,9,10,2,0,50,19,55,24,45,61,78,35
Aluno_55,90,99,3,7,98,0,100,50,51,59,37,54,45,6,23
Aluno_56,96,86,10,3,93,1,100,0,7,76,6,57,63,17,77
Aluno_57,100,99,6,5,98,6,100,50,17,35,49,55,37,30,57
Aluno_58,95,85,3,15,95,8,0,0,43,81,95,43,53,40,64
Aluno_59,98,99,9,14,97,9,0,0,43,49,43,59,49,31,31
Aluno_60,91,99,7,4,3,90,0,0,13,27,8,52,64,4,58
Aluno_61,94,98,4,6,1,10,100,50,43,68,52,45,56,0,19
Aluno_62,92,100,5,7,6,3,0,50,74,76,90,58,54,22,70
Aluno_63,98,97,9,9,100,3,100,0,33,26,94,46,38,4,52
Aluno_64,96,88,2,12,98,99,0,100,14,40,87,43,55,30,48
Aluno_65,93,94,10,8,96,7,0,100,50,13,15,47,64,27,48
Aluno_66,94,98,7,2,92,6,100,100,64,44,40,49,41,6,65
Aluno_67,97,97,1,3,0,1,100,0,64,20,78,46,52,55,9
Aluno_68,92,88,8,3,99,10,0,0,100,2,14,60,53,71,51
Aluno_69,97,86,9,2,94,93,100,100,32,87,12,55,44,68,8
Aluno_70,93,98,6,4,1,5,100,100,61,62,81,50,53,78,2
Aluno_71,98,99,2,2,96,7,100,50,76,60,55,59,59,37,81
Aluno_72,92,90,2,15,100,8,0,100,53,45,28,54,45,45,57
Aluno_73,92,100,0,13,97,7,0,50,47,92,26,53,57,26,54
Aluno_74,93,98,10,1,10,91,0,0,57,11,44,48,58,24,17
Aluno_75,96,96,8,11,6,2,0,50,95,80,9,41,59,7,15
Aluno_76,91,98,1,10,6,9,0,0,49,57,96,53,40,29,5
Aluno_77,91,90,6,13,7,1,100,0,90,32,1,45,43,22,72
Aluno_78,94,93,10,13,91,7,100,50,32,84,21,45,50,22,6
Aluno_79,96,88,9,1,2,96,100,50,100,45,33,57,63,20,46
Aluno_80,97,91,0,3,99,8,100,0,26,24,97,50,61,88,17
Aluno_81,91,85,9,4,90,96,100,100,52,51,14,40,50,43,23
Aluno_82,97,98,0,15,100,4,0,50,93,26,42,46,69,28,5
Aluno_83,91,100,6,8,100,1,0,100,14,41,83,47,44,99,47
Aluno_84,98,92,9,11,97,90,100,0,40,15,66,54,45,7,40
Aluno_85,96,91,3,14,7,10,0,100,7,92,9,50,38,68,66
Aluno_86,93,86,1,14,6,5,100,100,80,4,80,57,59,47,32
Aluno_87,95,98,4,13,0,6,100,50,87,31,20,56,48,55,87
Aluno_88,99,91,8,13,95,7,0,0,99,16,44,42,68,51,13
Aluno_89,94,97,4,7,7,3,0,100,17,5,25,59,56,66,1
Aluno_90,96,94,5,9,95,9,100,50,17,17,24,57,62,60,82
Aluno_91,98,89,4,4,96,94,100,50,99,41,96,42,68,98,36
Aluno_92,98,100,5,11,2,3,0,0,77,55,17,52,62,85,72
Aluno_93,99,90,7,13,6,7,0,50,77,57,52,58,44,92,13
Aluno_94,100,95,4,5,0,92,100,50,72,32,90,49,54,43,0
Aluno_95,99,86,3,4,94,97,0,0,95,88,46,48,63,80,68
Aluno_96,98,91,10,5,92,3,0,0,92,100,83,56,45,91,47
Aluno_97,95,93,6,2,6,7,100,100,97,43,11,46,69,39,68
Aluno_98,94,98,7,4,100,6,0,100,28,30,52,42,61,67,24
Aluno_99,99,88,8,15,2,6,100,0,33,12,61,47,50,78,94
Aluno_100,92,97,5,10,8,99,0,0,91,53,82,56,59,12,20
Aluno_101,91,87,2,12,5,0,100,100,75,9,69,45,44,43,42
Aluno_102,94,96,0,2,92,0,0,50,47,39,41,49,59,40,73
Aluno_103,94,94,6,12,95,96,0,50,20,92,0,58,52,58,68
Aluno_104,91,100,7,3,94,2,100,0,96,95,97,41,69,11,54
Aluno_105,94,100,8,2,95,0,0,50,41,44,49,45,38,70,16
Aluno_106,95,89,4,2,9,7,100,50,52,39,96,48,52,1,26
Aluno_107,93,89,9,11,91,94,100,100,40,7,40,52,55,24,8
Aluno_108,99,89,5,11,1,5,100,50,50,37,15,50,37,82,34
Aluno_109,96,86,6,9,3,97,0,100,28,45,70,47,48,34,87
Aluno_110,95,95,1,10,4,2,100,0,52,10,88,54,62,20,61
Aluno_111,97,87,0,7,7,8,0,50,14,100,6,48,66,67,32
Aluno_112,91,88,10,14,92,100,0,0,12,69,29,44,59,77,87
Aluno_113,94,92,2,7,90,0,0,100,0,31,35,58,64,11,37
Aluno_114,96,87,6,15,99,99,100,100,13,85,48,43,69,11,35
Aluno_115,98,94,10,11,0,3,100,50,78,66,41,41,64,67,7
Aluno_116,100,95,9,12,92,3,0,100,33,77,49,59,54,74,24
Aluno_117,95,86,1,1,8,94,0,50,72,42,25,53,46,53,3
Aluno_118,94,85,4,11,6,4,100,100,24,59,77,46,38,99,51
Aluno_119,93,94,6,10,91,1,100,100,56,77,85,60,46,16,41
Aluno_120,93,100,4,14,93,9,100,50,35,56,57,43,66,17,57
Aluno_121,94,90,10,4,94,4,100,100,19,94,25,46,69,2,84
Aluno_122,99,89,6,5,93,95,100,0,97,89,61,59,48,96,76
Aluno_123,93,99,3,13,91,7,0,100,7,16,55,52,66,19,44
Aluno_124,91,92,5,11,9,94,100,0,28,51,51,42,49,77,17
Aluno_125,90,99,5,7,0,94,0,50,94,1,26,45,66,49,42
Aluno_126,94,96,8,10,5,93,0,0,58,39,70,52,52,0,80
Aluno_127,94,94,7,12,9,96,0,100,78,32,39,48,43,25,64
Aluno_128,96,92,9,0,5,1,0,50,92,57,55,56,66,20,82
Aluno_129,91,92,5,12,10,99,0,50,71,12,67,59,46,73,79
Aluno_130,92,90,6,10,9,4,100,50,42,77,27,44,56,46,96
Aluno_131,93,95,4,9,9,1,0,50,43,17,32,57,59,89,47
Aluno_132,95,98,0,14,7,2,0,0,50,99,59,42,58,63,60
Aluno_133,96,98,8,0,8,5,100,100,73,21,8,44,47,10,30
Aluno_134,100,90,3,11,96,3,100,0,46,45,47,40,61,14,58
Aluno_135,95,93,5,13,92,7,0,100,90,65,60,44,46,37,95
Aluno_136,97,94,7,14,4,0,100,0,50,22,42,60,61,60,67
Aluno_137,90,95,4,11,90,92,100,100,69,83,1,57,58,1,32
Aluno_138,96,91,7,6,95,3,100,100,89,92,40,46,48,63,20
Aluno_139,99,97,0,10,9,5,100,100,52,56,76,41,63,90,73
Aluno_140,98,97,4,14,100,1,100,50,63,100,76,41,52,35,15
Aluno_141,90,94,1,12,2,5,0,0,7,83,84,41,48,60,92
Aluno_142,95,96,9,8,3,5,0,0,52,31,94,52,69,20,36
Aluno_143,92,86,7,14,1,7,100,50,74,55,14,56,63,47,77
Aluno_144,90,98,6,10,5,2,0,50,91,93,56,41,67,22,44
Aluno_145,97,94,10,2,8,10,100,0,60,2,66,41,61,74,42
Aluno_146,96,96,10,11,99,5,100,0,48,76,23,58,50,95,38
Aluno_147,98,91,6,14,3,5,0,50,95,25,83,46,52,73,64
Aluno_148,99,87,6,5,92,98,0,50,17,81,63,52,45,59,41
Aluno_149,90,87,8,1,99,4,0,100,78,64,99,60,54,42,52
Aluno_150,96,95,0,2,94,3,100,100,63,48,89,42,40,32,53
Aluno_151,98,98,3,12,100,7,100,0,50,23,25,45,40,83,86
Aluno_152,100,95,5,10,90,2,0,50,57,8,23,43,53,67,49
Aluno_153,90,92,2,6,10,95,100,0,27,49,0,60,40,52,67
Aluno_154,96,96,9,0,2,7,0,100,95,48,83,42,46,33,46
Aluno_155,97,93,0,13,94,9,0,100,90,67,24,40,67,41,97
Aluno_156,90,96,9,7,90,4,100,100,51,87,67,43,59,19,37
Aluno_157,94,95,5,14,97,1,100,100,76,96,42,40,59,26,78
Aluno_158,94,95,7,6,5,96,0,50,91,87,56,56,38,52,88
Aluno_159,93,97,10,8,8,4,0,0,72,67,6,40,59,90,96
Aluno_160,97,87,3,9,90,4,100,100,71,54,100,53,61,40,31
Aluno_161,98,100,4,9,4,97,0,0,18,41,42,49,62,5,0
Aluno_162,94,96,3,3,8,90,100,100,42,94,22,48,67,96,80
Aluno_163,95,92,5,4,0,0,100,50,88,58,2,48,56,69,94
Aluno_164,99,92,0,0,92,9,0,50,13,37,80,43,49,1,59
Aluno_165,98,97,7,15,97,0,100,50,45,64,39,57,63,41,77
Aluno_166,100,91,6,15,7,91,0,0,59,70,45,47,37,25,4
Aluno_167,95,89,8,0,2,5,0,0,72,32,95,50,44,47,76
Aluno_168,98,96,10,14,8,1,0,50,34,34,96,46,46,48,11
Aluno_169,98,87,10,10,1,3,100,50,88,2,24,54,60,4,75
Aluno_170,97,90,6,10,94,95,0,50,23,58,66,57,53,16,79
Aluno_171,99,91,4,2,96,4,100,100,74,13,81,42,46,61,27
Aluno_172,94,96,1,5,9,6,100,50,86,63,38,60,61,44,62
Aluno_173,90,95,8,0,100,4,100,50,22,80,46,60,44,76,2
Aluno_174,100,85,2,15,5,2,0,0,94,30,29,59,44,12,91
Aluno_175,91,90,1,12,91,96,100,100,14,45,48,51,66,79,33
Aluno_176,97,88,4,5,6,0,100,0,39,13,93,45,37,72,28
Aluno_177,99,94,3,2,93,0,100,50,10,10,61,56,59,84,24
Aluno_178,96,90,10,12,7,94,100,100,27,45,43,51,39,92,47
Aluno_179,95,91,1,3,96,2,0,100,84,90,50,45,49,25,100
Aluno_180,93,86,5,14,8,0,100,100,67,54,77,43,48,98,74
Aluno_181,99,88,10,14,94,90,0,50,42,27,67,41,58,29,52
Aluno_182,95,88,6,12,5,91,0,50,47,63,47,58,64,36,96
Aluno_183,99,96,3,1,9,5,100,50,26,91,71,47,42,70,82
Aluno_184,95,98,9,4,2,1,0,0,89,89,97,48,52,21,27
Aluno_185,92,89,2,2,6,97,0,0,89,15,87,58,50,77,30
Aluno_186,100,95,9,3,95,7,0,0,59,21,22,40,41,91,49
Aluno_187,93,88,2,7,0,99,0,100,36,45,82,58,43,50,73
Aluno_188,98,97,4,0,2,7,0,100,36,74,39,43,65,34,35
Aluno_189,97,95,9,11,5,8,0,0,61,73,44,45,54,7,95
Aluno_190,100,96,6,15,4,2,100,0,72,56,37,55,65,85,46
Aluno_191,90,88,7,3,100,2,100,0,24,66,43,52,57,61,72
Aluno_192,90,88,9,13,0,100,0,0,53,27,20,60,47,20,44
Aluno_193,97,88,9,2,10,2,0,50,8,0,74,55,45,84,75
Aluno_194,96,86,7,10,4,98,100,100,25,83,21,47,45,6,8
Aluno_195,99,97,9,7,100,96,100,100,56,48,73,44,57,7,39
Aluno_196,96,93,5,11,92,100,0,50,79,16,26,55,57,61,1
Aluno_197,95,92,4,5,90,95,0,0,36,32,57,47,50,32,58
Aluno_198,96,97,9,3,1,91,0,100,99,65,13,47,56,14,37
Aluno_199,91,88,3,14,98,3,100,0,10,10,62,45,53,87,86
Aluno_200,92,85,3,10,95,97,0,50,16,27,31,58,40,39,82
"


# Leitura dos dados
dados <- read.csv(text = dados, row.names = 1)

dados <- dados[-1, -1]

# Cálculo das correlações
cor_matrix <- cor(dados)

# Encontrar correlações significativas
significant_correlations <- which(abs(cor_matrix) > 0.4 & cor_matrix != 1, arr.ind = TRUE)
significant_correlations <- significant_correlations[significant_correlations[,1] < significant_correlations[,2], ]

# Cálculo das correlações
cor_matrix <- cor(dados)

# Ajustar o tamanho da fonte e do gráfico
par(cex = 0.6) # Define o tamanho da fonte
corrplot(cor_matrix, method = "square", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black", mar = c(0,0,2,0)) # Ajusta as margens


# Visualizar a matriz de correlacao usando corrgram
corrgram(dados, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Correlograma das Variaveis")

# Funcao para teste de correlacao
cor_test <- function(x, y) {
  cor.test(x, y)$p.value
}

# Obter nomes das colunas
column_names <- names(dados)


# Calcular as correlações e os p-values
correlation_results <- data.frame(Var1 = character(),
                                  Var2 = character(),
                                  Correlation = numeric(),
                                  PValue = numeric(),
                                  stringsAsFactors = FALSE)

for (i in 1:(ncol(dados) - 1)) {
  for (j in (i + 1):ncol(dados)) {
    cor_val <- cor.test(dados[, i], dados[, j])$estimate
    p_val <- cor.test(dados[, i], dados[, j])$p.value
    
    if (abs(cor_val) > 0.4 && p_val < 0.05) {
      correlation_results <- rbind(correlation_results, 
                                   data.frame(Var1 = colnames(dados)[i],
                                              Var2 = colnames(dados)[j],
                                              Correlation = cor_val,
                                              PValue = p_val))
    }
  }
}


# Imprimir as correlações significativas
cat("Variáveis com correlação significativa (|r| > 0.4 e p-value < 0.05):\n\n")

if (nrow(correlation_results) > 0) {
  print(correlation_results)
} else {
  cat("Nenhuma correlação significativa encontrada.\n")
}

##############################################################################

#PERSON

# Calcule as correlações de Pearson e p-values
correlacoes <- cor(dados)
p_values <- cor.test(dados)$p.value

# Filtre as correlações significativas
corr_significativas <- which(abs(correlacoes) > 0.4 & p_values < 0.05, arr.ind = TRUE)

# Crie um data frame com as correlações significativas
dados_corr_significativas <- dados[, corr_significativas]
nomes_variaveis <- names(dados)[corr_significativas]

# Crie o gráfico de dispersão
ggplot(dados_corr_significativas, aes(x = !!nomes_variaveis[1], y = !!nomes_variaveis[2])) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Gráfico de Correlação de Pearson", x = nomes_variaveis[1], y = nomes_variaveis[2])

# Personalize o gráfico (opcional)
# Adicione cores aos pontos de acordo com o valor da correlação
# Adicione legendas com o valor da correlação e p-value

# Exiba o gráfico
ggsave("grafico_correlacao_significativas.png")

