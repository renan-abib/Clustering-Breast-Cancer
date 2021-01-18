#Importando datasets e bibliotecas
wdbc <-read.csv("C:\\Users\\pasto\\Documents\\Engenharia de Software\\Graduação PUCC\\6o sem\\IA e Sistemas Inteligentes\\Projetos\\Projeto1_IA\\wdbc.data", sep = ",", header = FALSE, na.strings = '?')
library(dplyr)
library(ggplot2)
library(class)
library(factoextra)
library("viridis")
library(tibble)

# Remoção de possíveis elementos ou linhas duplicadas 
wdbc <- unique(wdbc)

# Removendo instâncias (linhas) com valores iguais a "?"
wdbc_novo <- wdbc[complete.cases(wdbc),] 

#Ajustando os nomes de colunas dos datasets

# dataset wdbc
nomes_colunas_dataset2 <- c("ID number", "Diagnosis", "Mean Radius", "Mean Texture", "Mean Perimeter", "Mean Area", "Mean Smoothness", "Mean Compactness", "Mean Concavity", "Mean Concave Points", "Mean Symmetry", "Mean Fractal Dimension", 
                            "Radius SE", "Texture SE", "Perimeter SE", "Area SE", "Smoothness SE", "Compactness SE", "Concavity SE", "Concave Points SE", "Symmetry SE", "Fractal Dimension SE", 
                            "Worst Radius", "Worst Texture", "Worst Perimeter", "Worst Area", "Worst Smoothness", "Worst Compactness", "Worst Concavity", "Worst Concave Points", "Worst Symmetry", "Worst Fractal Dimension")
colnames(wdbc_novo)<- nomes_colunas_dataset2

# ordenando o dataset
wdbc_order <- wdbc_novo[order(wdbc_novo$Diagnosis),]

# Separando a coluna da Diagnosis (se é benigno ou maligno) para a criação da matriz de confusão
classesWDBC <- wdbc_order[,2]

# Quantidade real de B
qtd_benigno <- sum(wdbc_order$Diagnosis == "B")
print(qtd_benigno)

# Quantidade real de M
qtd_maligno <- sum(wdbc_order$Diagnosis == "M")
print(qtd_maligno)

# Removendo colunas de classe e identificação
wdbc_novo <- wdbc_order[,-2]
wdbc_novo <- wdbc_novo[,-1]


# ---------------- Aplicando o k-means ----------------- #

set.seed(123)

# k = 2

# printando o vetor de clusterização para k = 2
kmeans(wdbc_novo, 2, nstart = 50, iter.max = 15)

wdbc.km2 <- kmeans(wdbc_novo, 2, nstart = 50, iter.max = 15)


fviz_cluster(wdbc.km2, data = wdbc_novo,
             palette = c("#836FFF", "#FF0000"), 
             geom = c("point", "text"),
             shape = NULL,
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

# Verificando taxa de acerto para k=2 em relação a coluna Diagnosis

table(wdbc.km2$cluster)
table(classesWDBC)
table(classesWDBC, wdbc.km2$cluster)


# k = 3

# printando o vetor de clusterização para k = 3
kmeans(wdbc_novo, 3, nstart = 50, iter.max = 15)

wdbc.km3 <- kmeans(wdbc_novo, 3, nstart = 50, iter.max = 15)

fviz_cluster(wdbc.km3, data = wdbc_novo,
             palette = c("#836FFF", "#FF0000", "#2E8B57"), 
             geom = c("point", "text"),
             shape = NULL,
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

# -------- Elbow Method para achar o número ótimo de clusters ------ #
set.seed(123)

fviz_nbclust(wdbc_novo, kmeans, method = "wss", k.max = 10) 


# -------------- Silhueta ------------ #

require(cluster)
X <- wdbc_novo

# silhueta com 2 clusters
D <- daisy(X)
plot(silhouette(wdbc.km2$cluster, D), col=c("#9933ff", "#cc0000"), border=NA, main="Silhouette plot de WDBC com 2 clusters")

# silhueta com 3 clusters
wdbc.km3 <- kmeans(wdbc_novo, 3, nstart = 50, iter.max = 15)
D <- daisy(X)
plot(silhouette(wdbc.km3$cluster, D), col=c("#00e600", "#9933ff", "#cc0000"), border=NA, main="Silhouette plot de WDBC com 3 clusters")


# Largura da Silhueta média para o k-means
fviz_nbclust(wdbc_novo, kmeans, method = "silhouette", k.max = 10)

