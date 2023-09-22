#######################
## TRABAJO PRÁCTICO 2 #
##    LUCÍA OTERO     #
#######################


# Construir un script de R que :
#   
# El archivo winequality-red.csv es un archivo que tiene mediciones de un conjunto de variables 
# de diversos vinos y una clasificación de calidad (campo quality) la cual buscaremos predecir.
# 
# install.packages(reshape2)
#install.packages("GGally")
library(reshape2)
library(ggplot2)
library(plotly)
library(dplyr)
library(caret)
library(GGally)

library(AppliedPredictiveModeling)
library(corrplot)
library(datasets)
library(PerformanceAnalytics)
library(randomForest)

wine_data <- read.csv("winequality-red.csv")
View(wine_data)

# Antes de comenzar a trabajar con los datos, se debe realizar un análisis exploratorio de los mismos, 
# graficando cada variable con box-plots, y analizando la distribución de las calidades con un histograma. 
str(wine_data)
head(wine_data)
tail(wine_data)
min(wine_data$quality)
max(wine_data$quality)

# Reviso si hay NA
if (any(is.na(wine_data))) {
  print("There are NA values in the dataframe")
} else {
  print("There are no NA values in the dataframe")
}

# Convierto quality a factor 
wine_data$quality = as.factor(wine_data$quality)
str(wine_data)

boxplot(wine_data, main="Boxplots of Wine Attributes")


# Boxplots para las distintas variables
par(mfrow=c(2, 3))
for (col in names(wine_data)) {
  boxplot(wine_data[[col]], main=paste("Boxplot of", col))
}

for (col in names(wine_data)) {
  if (col != "quality") {
    plot(wine_data$quality, wine_data[[col]], main=paste(col, "vs quality"),
         xlab="quality", ylab=col)
    #abline(lm(wine_data$quality ~ wine_data[[col]]), col = "red")
  }
}

quality_count <- table(wine_data$quality)
print(quality_count)

ggplot(wine_data, aes(x = quality)) +
  geom_bar(fill = "#800020", color = "black") +
  labs(title = "Frequency of Quality Categories", x = "Quality", y = "Frequency")

color_palette <- colorRampPalette(c("red", "blue"))(6)
ggplot(data = as.data.frame(quality_counts), aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = color_palette, color = "black") +
  labs(title = "Frequency of Quality Categories", x = "Quality", y = "Frequency")

pairs(wine_data[,1:11], lower.panel=NULL, col = color_palette[wine_data$quality])

# Scatter plot matrix
# scatter_matrix <- ggpairs(wine_data, columns = 1:11,
#                           aes(colour = as.factor(quality)),
#                           lower = list(continuous = wrap("smooth", alpha = 0.3)),
#                           title = "Scatter Plot Matrix")
# scatter_matrix <- scatter_matrix + scale_colour_manual(values = color_palette)
# print(scatter_matrix)


# Convertir 'quality' a numérico
wine_data$quality <- as.numeric(as.character(wine_data$quality))

# Crear un nuevo factor clasificando los niveles de calidad
wine_data$quality_group <- cut(wine_data$quality, 
                               breaks = c(2, 4, 6, 8), 
                               labels = c("low", "medium", "high"), 
                               include.lowest = TRUE)

# Convertir la variable en factor
wine_data$quality_group <- factor(wine_data$quality_group, levels = c("low", "medium", "high"))

# Crear un nuevo dataframe wine_data_grouped
wine_data_grouped <- wine_data

# Verificar la creación del nuevo dataframe
head(wine_data_grouped)

View(wine_data_grouped)
str(wine_data)
wine_data$quality = as.factor(wine_data$quality)
View(wine_data)

colores = c ("red", "white","blue")
pairs(wine_data[,1:11], lower.panel=NULL, col = colores[wine_data$quality_group])

wine_data_extremes <- wine_data_grouped[wine_data_grouped$quality_group != "medium", ]
pairs(wine_data_extremes[,1:11], lower.panel=NULL, col = colores[wine_data_extremes$quality_group])

color_palette_3c <- colorRampPalette(c("red", "blue"))(3)
pairs(wine_data[,1:11], lower.panel=NULL, col = color_palette_3c[wine_data$quality_group])


############################### Prueba ###############################
# Create an empty list to store the plots
plot_list <- list()

# Iterate through each column (excluding 'quality')
for (col in names(wine_data_grouped)[!names(wine_data_grouped) %in% c("quality", "quality_group")]) {
  # Create a scatter plot for each column against 'quality'
  p <- ggplot(wine_data_grouped, aes(x = quality, y = .data[[col]], color = quality_group)) +
    geom_point() +
    labs(title = paste("Scatter Plot of", col, "vs Quality"), x = "Quality", y = col)
  
  # Add the plot to the list
  plot_list[[col]] <- p
}

# Arrange the plots in a grid layout
library(gridExtra)
grid.arrange(grobs = plot_list, ncol = 2)
########################################################################


# Dados estos datos es muy difícil que el algoritmo aprenda a clasificar bien al menos un par 
# de las calidades de vinos, discutir cual es dicha dificultad y que estrategia se le ocurriría para solucionarlo 
# (solo discutirlo, no hace falta implementarlo)
# 
# Agrupar calidades
# Se ve que no hay relación directa entre ninguna variable y calidad
# PCA?
# Relacionar variables
# relacion pH-acidez
# acidos volatiles bajos se relaciona con buena calidad, tiene sentido proque. nadie quiere un vino avinagrado
# mas alcohol y más sulfatos, mejor vino
# desbalance de la cantidad de buenos y malos




#quality es variable categórica, no podemos decir que el número 6 es el doble que el 3
# hacer matriz de confusion 
# fijarse cómo confunde los datos
# accuracy alto
# scaterplot performance analytics
# correlation plot
# hacer los boxplot segun cada calidad de vino
# hay desbalance de clases
# si svm no importa centrado de variables, pero para knn hay que escalar, usar también RF y CART
# hacer tabla y ver resultado de la predicción
# relacionar variables entre sí, coloreando según calidad, así entiendo cómo se relacionan (pH vs ashes) las variables
# correlation plot: 
colores = c ("red", "orange", "pink", "purple", "yellow","blue")
pairs(wine_data[,1:11], lower.panel=NULL, col = colores[wine_data$quality])
# 

#create data partition para asegurarse distribución balanceada de datos

sample = createDataPartition(wine_data$quality, p = 0.8, list=FALSE)
train1 = wine_data[sample, ]
test = wine_data[-sample, ]
#fit = knn3(wine_data[,1:11], wine_data[,12], k=5)
#fit = knn3(train[,1:11], train[,12], k=5)
fit = train(train1[,1:11], train1[,12], method = "knn", preProcess=c ("scale"))
fit
#ver dónde va la parte de preprocess
# bowplot compara los métodos


fit2 = train(train1[,1:11], train1[,12], method = "rf")
fit2

library(support)
fit3 = train(train1[,1:11], train1[,12], method = "svm")
fit3

prediction = predict(fit2, test[,1:11])
table(prediction, test$quality)









# Usando un 20% de los datos para validación, y el resto para entrenamiento y testeo, 
# se deben implementar al menos 4 algoritmos de clasificación, con al menos un pre-procesamiento 
# para escalar los datos de forma de mejorar las posibilidades de éxito del algoritmo.
# 
# Evaluar los modelos entrenados, usando los datos de validación, graficando al menos los resultados 
# de accuracy y kappa.
# 
# Graficar un compatativo de los resultados de los modelos, seleccionar uno de ellos para su uso 
# y explicar el criterio de selección del algoritmo.
# 
# Seleccionar un algoritmo de los entrenados y realizar al menos una optimización por hiperparametrización