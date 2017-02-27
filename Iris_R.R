iris <- read.csv("iris.csv")
View(iris)
summary(iris)
iris.clean <- iris
iris.clean$Species <- NULL
View(iris.clean)
iris.clean$X <- NULL
View(iris.clean)
summary(iris.clean)
install.packages("ggvis")
library(ggvis)
iris %>% ggvis(~Sepal.Length, ~Sepal.Width, fill = ~Species) %>% layer_points()
iris %>% ggvis(~Petal.Length, ~Petal.Width, fill = ~Species) %>% layer_points()
str(iris)
table(iris$Species)
summary(iris.clean)
results <- kmeans(iris.clean,3)
View(results)
results
table(iris$Species, results$cluster)
plot(iris[c("Petal.Length", "Petal.Width")], col = results$cluster)
plot(iris[c("Petal.Length", "Petal.Width")], col = iris$Species)
plot(iris[c("Sepal.Length", "Sepal.Width")], col = iris$Species)
plot(iris[c("Sepal.Length", "Sepal.Width")], col = results$cluster)
install.packages("cluster")
library(cluster)
clusplot(iris.clean, results$cluster, main='2D representation of the Cluster solution', color=TRUE, shade=TRUE,labels=2, lines=0)
x <- rbind(iris.clean$Sepal.Length, iris.clean$Sepal.Width, iris.clean$Petal.Length)
x <- t(x)
result_fuzzy <- cmeans(x, 3)
result_fuzzy <- cmeans(x, 3)
install.packages("e1071")
library(e1071)
result_fuzzy <- cmeans(x, 3)
result_fuzzy
table(iris$Species, result_fuzzy$cluster)
plot(iris[c("Petal.Length", "Petal.Width")], col=result_fuzzy$cluster)
plot(iris[c("Sepal.Length", "Sepal.Width")], col=result_fuzzy$cluster)
clusplot(iris.clean, result_fuzzy$cluster, main='2D representation of the Cluster solution', color=TRUE, shade=TRUE, labels=2, lines=0)
iris_train <- iris[1:100,1:4 ]
iris_test <- iris[101:150,1:4]
iris_train_target <- iris[1:100, 5]
iris_test_target <- iris[101:150, 5]
require(class)
m1 <- knn(train=iris_train, test=iris_test, cl=iris_train_target, k=11)
m1
table(iris_test_target, m1)
install.packages("ggplot2")
library(ggplot2)
ggplot(iris_test, aes(Petal.Length, Petal.Width, color = m1_plot)) + geom_point()
ggplot(iris, aes(Petal.Length, Petal.Width, color = m1_plot)) + geom_point()
ggplot(iris, aes(Petal.Length, Petal.Width)) + geom_point()
