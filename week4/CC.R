library(ggplot2)
library(dplyr)

TB$default = factor(TB$default)%>%
  as.numeric()




ggplot(TB , aes( x= education ))+
  geom_bar(fill = "lightblue")
ggplot(TB , aes( x= sex ))+
  geom_bar(fill = "lightblue")
ggplot(TB , aes( x= marriage ))+
  geom_bar(fill = "lightblue")
ggplot(TB , aes( x= age ))+
  geom_bar(fill = "lightblue")


ggplot(TB, aes(x = age, fill = default)) +
  geom_bar() +
  labs(x = 'Age') 
ggplot(TB, aes(x = marriage, fill = default)) +
  geom_bar() +
  labs(x = 'Marriage') 
ggplot(TB, aes(x = sex, fill = default)) +
  geom_bar() +
  labs(x = 'Gender') 
ggplot(TB, aes(x = age, fill = default)) +
  geom_bar() +
  labs(x = 'Age') 
ggplot(TB, aes(x = education, fill = default)) +
  geom_bar() +
  labs(x = 'Education') 

ggplot(TB, aes(x = Sep, y = default, fill = education))+
  geom_col()+
  facet_wrap(~education)

ggplot(TB, aes(x = Aug, y = default))+
  geom_col()
ggplot(TB, aes(x = Jul, y = default))+
  geom_col()
ggplot(TB, aes(x = Jun, y = default))+
  geom_col()
ggplot(TB, aes(x = May, y = default))+
  geom_col()
ggplot(TB, aes(x = Apr, y = default))+
  geom_col()
ggplot(dl, aes(x = Sep, y = default))+
  geom_col()

