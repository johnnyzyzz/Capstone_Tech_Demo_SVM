#--------------------------------------------------------------------------------------------------------------------------------------------

# Iris data

library(readr)
iris <- read_csv("C:/Users/jburr/Documents/SVM/Iris.csv")
attach(iris)

# plotting useful predictors
library(ggplot2)
ggplot(data=iris, aes(x=PetalLengthCm, y=PetalWidthCm, color=Species, shape=Species)) + geom_point(size=5) +theme(axis.text=element_text(size=23),axis.title=element_text(size=25,face="bold"),legend.text=element_text(size=15))

# create svm model
library(e1071)
mod = svm(as.factor(Species)~PetalLengthCm+PetalWidthCm,data=iris,kernal='linear')
summary(mod)

# check predictions
table(predict(mod),Species)
1 - mean(predict(mod) != iris$Species)

# plot SVM hyperplanes
plot(mod,iris,PetalWidthCm~PetalLengthCm,slice=list(PetalWidthCm=1,PetalLengthCm=2))

#-------------------------------------------------------------------------------------------------------------------------------------------

# create random data in a non linear fashion

d1 = rnorm(500,mean=3,sd=0.1)
d2 = rnorm(500,mean=5,sd=0.2)

df = data.frame(d1,d2)
df$class_actual = rep(0,nrow(df))
df <- within(df, class_actual[d1 >= 2.93 & d1 <= 3.03 & d2 >= 4.8 & d2 <= 5.2] <- 1)
df$class_actual = as.factor(df$class_actual)
attach(df)

# plot the random data
ggplot(data=df, aes(x=d1, y=d2, color=class_actual, shape=class_actual)) + geom_point(size=5) +theme(axis.text=element_text(size=23),axis.title=element_text(size=25,face="bold"),legend.text=element_text(size=15))

# classify using logistic regression
modglm = glm(class_actual ~.,family=binomial(link='logit'),data=df)

# diagnostics and accuracy
summary(modglm)
fitted.results = predict(modglm,newdata=df,type='response')
fitted.results = ifelse(fitted.results > 0.5,1,0)
1 - mean(fitted.results != df$class_actual)

# classify using SVM
modsvm = svm(class_actual~d1+d2,data = df)

# diagnostics and accuracy
summary(modsvm)

# C parameter ajusts margin of hyperplane (larger C = smaller margin)
# radial kernel indicates type of hyperplane
# cost refers to the size of the margin in c classification
# gamma is the parameter of the non linear kernel 

table(predict(modsvm),class_actual)
1 - mean(predict(modsvm) != df$class_actual)

# plot svm
plot(modsvm,df,d2~d1,slice=list(d2=1,d1=2))


#------------------------------------------------------------------------------------------------------------------------------

s1 = sample(1:5000,3000,replace=TRUE)
s2 = sample(1:5000,3000,replace=TRUE)
sf = data.frame(s1,s2)



