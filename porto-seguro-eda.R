# usando pacotes úteis
library(tidyverse)
library(corrplot)
library(ggcorrplot)
library(ggfortify)
library(ggplot2)

# mudando a pasta
setwd("~/porto-seguro-data-challenge")

# lendo os metadados com tipos das variáveis
metadata <- read_csv(file='metadata.csv')
view(metadata)


#-------------------------------------------------------------------------------
# agora importaremos train,csv

# vendo o dataframe, percebe-se que na é representado como -999
# lendo train.csv com "-999" sendo trocado por na
train <- read.csv('train.csv', na.strings="-999")

view(train)

# vamos explorar um pouco melhor os dados de train
head(train)
tail(train)

# e agora ver a quantidade de valores únicos de cada coluna
sapply(train, function(x) length(unique(x)))


#-------------------------------------------------------------------------------
# vamos trocar as variáveis para os tipos apropriados

# começamos vendo quais são os tipos possíveis pelos metadados
unique(metadata$`Variavel tipo`)

# então criamos um vetor com os tipos na ordem correspondente às variáveis
rtypes = character(0)
for (i in 1:length(metadata$`Variavel tipo`)) rtypes[i] <- metadata$`Variavel tipo`[i]


# e trocamos as variáveis no vetor pelo atalho com c sendo char, n sendo numeric 
counter <- 1
for (var in rtypes) {
  rtypes[counter] <- switch(var, "Qualitativo nominal" = "n", 
                            "Qualitativo ordinal" = "o", 
                            "Quantitativo discreto" = "d", 
                            "Quantitativo continua" = "c")
  
  counter <- counter + 1  
}

print(rtypes)

# e convertemos cada coluna para seu tipo adequado
names <- colnames(train)

for (i in 1:length(rtypes)) {
  type <- rtypes[i]
  name <- names[i]
  
  if (type == "n") {
    train[[name]] <- as.factor(train[[name]])
    
  } else if (type == "o") {
    train[[name]] <- factor(train[[name]], order=TRUE)
    
  } #else if (type == "d") {
    #train[[name]] <- as.integer(train[[name]])} 
    else {
    train[[name]] <- as.numeric(train[[name]])
    
  }
}


#-------------------------------------------------------------------------------
# Agora podemos fazer uma análise exploratória mais profunda

head(train)
tail(train)
str(train)

# Vejamos a estatistica descritiva, colocando em um dataframe para facilitar
# Aqui dividimos qualitativos de quantitativos, veremos sumario dos dois
qualitativeSummary <- data.frame(summary(train %>% select_if(is.factor)))
quantitativeSummary <- data.frame(summary(train %>% select_if(is.numeric)))


# de fato analisando
summary(train %>% select_if(is.factor))
summary(train %>% select_if(is.numeric))

# agora uma análise gráfica dos dados numéricos
boxplot(train %>% select_if(is.numeric))

# façamos um separado para var52, outro para as vars 40, 45, 46, 48 e um 
# terceiro para todo o resto
grupo1 <- subset(train, select=c(var40, var45, var46, var48)) %>% select_if(is.numeric)
grupo2 <- subset(train, select=c(var52)) %>% select_if(is.numeric)
grupo3 <- subset(train, select=-c(var40, var45, var46, var48, var52)) %>% select_if(is.numeric)

boxplot(grupo1, xlab='Característica', ylab='Valor', las=2,
        main='Grupo 1 Variáveis numéricas', col=rgb(214, 162, 232, maxColorValue = 255))

boxplot(grupo2, xlab='Característica', ylab='Valor', las=2,
        main='va52', col=rgb(254, 164, 127, maxColorValue = 255))

boxplot(grupo3, xlab='Característica', ylab='Valor', las=2,
        main='Grupo 3 Variáveis numéricas', col=rgb(154, 236, 219, maxColorValue = 255))


#-------------------------------------------------------------------------------
# para ver a correlação, precisamos excluir os valores faltantes
# checando a correlação entre variáveis numéricas
numcorr <- cor(train %>% select_if(is.numeric), use="complete.obs")

# agora plotando
corrplot(numcorr, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# vamos calcular o PCA para essas variáveis, para isso precisamos ignorar os NA
pca <- prcomp(na.omit(train %>% select_if(is.numeric)), center = TRUE,scale. = TRUE)
autoplot(pca, loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

# vamos procurar correlação entre as variáveis categóricas, considerando y como categórica
train2 <- train
train2$y <- as.factor(train2$y)


for (name in colnames(train2)) {
  print(chisq.test(table(train2[c(name, "y")])))
}
