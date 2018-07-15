iris
mtcars
install.packages("ggplot2")

library(ggplot2)
ggplot(iris , aes(x= Sepal.Length))+
  geom_bar(fill = "red" , colour= "black")

ggplot(mtcars , aes(x= gear))+
  geom_bar(fill = "green" , colour= "red")

ggplot(iris, aes(x = Sepal.Length , fill =  Species))+
  geom_histogram(binwidth = 1)

ggplot(iris , aes(x = Sepal.Length , y =   Petal.Length, colour = Species))+
  geom_point()

ggplot(iris , aes(x= Species , y = Sepal.Width))+
  geom_boxplot()

library(GGally)
library(scales)
ggpairs(mtcars,lower= list(continuous = wrap("points", shape = I('.'))),
        upper = list(combo = wrap("box", outlier.shape = I('.'))))