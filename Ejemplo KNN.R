df <- data(iris) # Cargamos los datos
head(iris) # Vemos los primeros 6 renglones
str(iris)
##Generamos un muestra del 90% de todos los datos
ran <- sample(1:nrow(iris), 0.9 * nrow(iris)) 
ran
##Creamos una funcion de normalizacion
nor <-function(x) { (x -min(x))/(max(x)-min(x))   }

##Aplicamos la normalizacion a las primeras 4 columnas (predictoras)
iris_norm <- as.data.frame(lapply(iris[,c(1,2,3,4)], nor))
iris_norm

summary(iris_norm)
##   Sepal.Length     Sepal.Width      Petal.Length     Petal.Width     
##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min. :0.00
##  1st Qu.:0.2222   1st Qu.:0.3333   1st Qu.:0.1017   1st Qu.:0.08  
##  Median :0.4167   Median :0.4167   Median :0.5678   Median :0.50
##  Mean   :0.4287   Mean   :0.4406   Mean   :0.4675   Mean   :0.45
##  3rd Qu.:0.5833   3rd Qu.:0.5417   3rd Qu.:0.6949   3rd Qu.:0.70
##  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.00
##Creamos un objeto que contiene al conjunto de entrenamiento
iris_train <- iris_norm[ran,] 
##Creamos un objeto que contiene al conjunto de prueba
iris_test <- iris_norm[-ran,] 
##Extraemos la columna de respuesta (clasificacion)
iris_target_category <- iris[ran,5]
##Extraemos la columna de respuesta del conjunto de prueba
iris_test_category <- iris[-ran,5]
##Cargamos el pkt class que contiene la funcion knn
library(class)
##Usamos la funcion knn
pr <- knn(iris_train,iris_train,cl=iris_target_category,k=10)
pr
iris_test_category
##Creamos una matrix para los resultados
#tab <- table(pr,iris_target_category)
tab <- table(pr,iris_test_category)
tab
##this function divides the correct predictions by total number of predictions that tell us how accurate teh model is.

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)

