library("ggplot2")
mydata <- as.data.frame(HairEyeColor)
str(mydata)
mydata2 <- subset(mydata, Sex == "Female")
obj <- ggplot(data = mydata2, aes(x = Hair, y = Freq, fill = Eye)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))
obj


mydata  <- npk
str(mydata)
fit <- aov(yield ~ N * P, data=mydata)
summary(fit)
fit2 <- aov(yield ~ N * P * K, data=mydata)
summary(fit2)


mydatairis  <- iris
str(mydatairis)
fit3 <- aov(Sepal.Width ~ Species, data=mydatairis)
summary(fit3)


TukeyHSD(fit3)


mydata2 <- read.csv('Pillulkin.csv')
str(mydata2)
mydata2$patient <- as.factor(mydata2$patient)
str(mydata2)
fit1b <- aov(temperature ~ pill + Error(patient/pill), data = mydata2)
summary(fit1b)
fit2b <- aov(temperature ~ pill*doctor + Error(patient/(pill*doctor)), data = mydata2)
summary(fit2b)

my_fn_tst <- function(x){
  s <- x+100
  return(s)
}


my_vector <- c(14, NA, 7, NA, 11, 9, 12, 11, 17, 10, 11, 7, 6)
my_vector
t <- is.na(my_vector)
t

my_fn <- function(x){
  ret = c()
  num = 1
  for (i in x) {
    if (is.na(i)==T){
      ret = c(ret, num)
    }
    num <- num + 1
  }
  return(ret)
}
my_fn2 <- function(x){
  num = 0
  for (i in x) {
    if (is.na(i)==T){
      num <- num + 1
    }
    
  }
  return(num)
}
my_fn2(my_vector)

NA.counter <- function(x){    
  return(sum(is.na(x)))}


NA.position <- function(x){    
  which(is.na(x))}

filtered.sum(c(1, -2, 3, NA, NA))

filtered.sum <- function(x){
  num = 0
  for (i in x) {
    if (is.na(i)==F & i>0){
      num <- num + i
    }
    
  }
  return(num)
} 

filtered.sum <- function(x){    
  return(sum(x[x > 0], na.rm = T))}

del_v <- function(x){
  t <- quantile(x, probs = c(0.25, 0.75))
  q <- IQR(x)
  ret = c()
  for (i in x) {
    if (is.na(i)==F){
      if (i > t[2] + 1.5*q || i < t[1] - 1.5*q){
      }
      else{ret = c(ret, i)}
    }
  }
  return(ret)
}


outliers.rm <- function(x){    
  q <- quantile(x, 0.25) + quantile(x, 0.75)    
  return(x[abs(x - q/2) <= 2*IQR(x)])}



#IQR(x) - рассчитывает межквартильный размах вектора x

x <- rnorm(100)
x
del_v(x)
t <- quantile(x, probs = c(0.25, 0.75))
i <- IQR(x)  
i
str(t)

corr.calc <- function(x){
  library(psych)
  fit  <- corr.test(x) 
  res = c(fit$r[1,2])
  res = c(res, fit$p[1,2])
  return(res)
  
}

corr.calc <- function(test_data){    
  fit  <- cor.test(test_data[[1]], test_data[[2]])    
  r <- fit$estimate    
  p <- fit$p.value    
  return(c(r, p))}


corr.calc( mtcars[, c(1,5)] )



df  <- mtcars
df_numeric  <- df[, c(1,5)]

pairs(df_numeric)

cor(df_numeric)
library(psych)
fit  <- corr.test(df_numeric)
fit$r[1,2]
fit$p[1,2]
fit$adjust
fit
fit$p.value

filtered.cor <- function(x){
  library(psych)
  tp <- sapply(x, class)
  lst <- which(tp =="numeric")
  df_numeric  <- x[, lst]
  fit  <- corr.test(df_numeric)
  mat <- fit$r
  ls=c(0)
  for (i in 1:nrow(mat)){
    for(j in 1:ncol(mat)){
      if(i!=j){
        ls <- c(ls, mat[i,j])
      }
    }
  }
  res1 <- max(ls)
  res2 <- min(ls)
  if (abs(res1)>=abs(res2)){
    res = res1}
  else {res = res2}
  return(res)
}

filtered.cor <- function(x){    
  num_var <- sapply(x, function(x) is.numeric(x))    
  cor_mat <- cor(x[, num_var])    
  diag(cor_mat) <- 0    
  return(cor_mat[which.max(abs(cor_mat))])}


filtered.cor(iris)
iris$Petal.Length <- -iris$Petal.Length # сделаем отрицательной максимальную по модулю коррел€цию
filtered.cor(iris)

step6 <-  read.table("step6.csv",  header=TRUE, sep=',' )
filtered.cor(step6)




x <- mtcars
str(x)
tp <- sapply(x, class)
lst <- which(tp =="numeric")
lst
df_numeric  <- x[, lst]
fit  <- corr.test(df_numeric)
mat <- fit$r
mat
ls=c(0)
for (i in 1:nrow(mat)){
  for(j in 1:ncol(mat)){
    if(i!=j){
      ls <- c(ls, mat[i,j])
    }
  }
}
max(ls)
z <- fit$r!=1
z <- fit$r[fit$r!=1]
max(abs(z))
M <- apply(fit$r, 1, max)
M

smart_cor <- function(x){
  sh1 <- shapiro.test(x[[1]])
  sh2 <- shapiro.test(x[[2]])
  if (sh1$p.value<0.05 || sh2$p.value<0.05){
    qq2 <- cor.test(x[[1]], x[[2]], method = "spearman")
  }
  else{
    qq2 <- cor.test(x[[1]], x[[2]], method = "pearson")
  }
  tst <-c(qq2$p.value) 
  return(tst)
}


smart_cor <- function(x){    
  if (shapiro.test(x[[1]])$p < 0.05 | shapiro.test(x[[2]])$p < 0.05) {    
    return(cor.test(x[[1]], x[[2]], method = 'spearman')$estimate)    
  } else {    
    return(cor.test(x[[1]], x[[2]], method = 'pearson')$estimate)}}

x <- as.data.frame(list(col1 = c(-1.93, 1.67, -0.4, -1.15, 1.08, 0.28, -0.39, -1.51, -1.54, -0.1, 1.76, -1.78, 1.25, -1.31, -1.39, 0.34, -0.56, -1.61, 0.53, -0.05, -0.33, -1.77, 1.81, -0.8, 0.94, -0.05, -1.36, 0.78, 1.35, 0.54), col2 = c(0.68, -0.3, -1.3, 0.26, -0.95, -0.17, -0.77, 0.05, 0.49, -1.38, -1.02, 0, 0.76, -0.27, -1.34, 0.59, -1.44, -0.22, -0.14, -0.92, -1.25, 1.08, 0.09, -1.22, 0.68, 1.38, -0.36, 1.04, -1.29, -0.47)))
smart_cor(test_data)

sh1 <- shapiro.test(x[[1]])
sh2 <- shapiro.test(x[[2]])
if (sh1$p.value<0.05 || sh2$p.value<0.05){
  qq2 <- cor.test(x[[1]], x[[2]], method = "spearman")
} else {
  qq2 <- cor.test(x[[1]], x[[2]], method = "pearson")
  }
qq2$estimate
str(qq2)
tst <-c(qq2$p.value) 
tst
return(tst)


library(psych)
?corr.test
x <- mtcars
sh1 <- shapiro.test(x[[1]])
sh1
sh1$p.value

sh2
sh2$p.value


sh2 <- shapiro.test(x[[2]])
qq2 <- corr.test(x, method = "spearman")
qq2$r
qq2
qq2 <- corr.test(x, method = "spearman")
cor99 <- cor.test(x[[1]], x[[2]], method = "spearman")
str(cor99)
install.packages("nortest")



df  <- read.table("dataset_11508_12.txt")
View(df)


fit  <- lm(V1 ~ V2, df)
summary(fit)

dm <- diamonds
View(dm)
dm1 <- subset(dm,  carat == 0.46 & cut == 'Ideal')
str(dm1)
View(dm1)
fit1  <- lm(price ~ depth, dm1)
summary(fit1)
fit1$
fit_coef <- fit1$coefficients
fit_coef

cor_tst <- cor.test(x[[1]], x[[2]], method = "pearson")
cor_tst$p.value

regr.calc <- function(x){
  cor_tst <- cor.test(x[[1]], x[[2]], method = "pearson")
  if (cor_tst$p.value < 0.05){
    fit1  <- lm(x[[1]] ~ x[[2]], x)
    res <- summary(fit1)
    x  <- data.frame(x, fit = fit1$fitted.values )
    return(x)
  } else{
    print("There is no sense in prediction")
  }
}


regr.calc <- function(sample_data){    
  cor_result = cor.test(~sample_data[[1]] + sample_data[[2]])    
  if (cor_result$p.value < 0.05){    
    fit_model  <- lm(sample_data[[1]] ~ sample_data[[2]])    
    sample_data$fit  <- fit_model$fitted.values    
    return(sample_data)    
  } else {    
    return('There is no sense in prediction')}}


my_df = iris[,1:2] # на вход подаем данные iris только с переменными Sepal.Length и Sepal.Width
regr.calc(iris[,1:2]) # переменные значимо не коррелируют 
cor_tst <- cor.test(x[[1]], x[[2]], method = "pearson")
cor_tst$p.value

my_df = iris[,c(1,4)] # на вход подаем данные iris только с переменными Sepal.Length и Petal.Width
regr.calc(my_df) # переменные значимо коррелируют


library(ggplot2)
my_plot <- ggplot()
df <- iris
str(df)
my_plot <- ggplot(df, aes(df$Sepal.Width, df$Petal.Width, col = factor(df$Species)))+
  geom_point(size = 5)+
  geom_smooth(method = "lm") 
my_plot

str(ToothGrowth)
View(ToothGrowth)
library(ggplot2)
library(Hmisc)
obj <- ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, col = supp))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2))
  theme_bw()
obj

ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, col = supp, group = supp))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, lwd = 0.8, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', size = 3, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', position = position_dodge(0.2), pch=15)
  

pd = position_dodge(0.1)
ggplot(mydata, aes(x = store, y = price, color = origin, group = origin)) + 
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.2, lwd = 0.8, position = pd)+  
  stat_summary(fun.data = mean_cl_boot, geom = 'line', size = 1.5, position = pd) +
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 5, position = pd, pch=15) +
  theme_bw()

install.packages("Hmisc")

str(attitude)
model_full <- lm(rating ~ ., data = attitude) 
model_full
model_null <- lm(rating ~ 1, data = attitude)
model_null
scope = list(lower = model_null, upper = model_full)
ideal_model <- step(model_full, scope, direction = 'backward')
?step
ideal_model
anova(model_full, ideal_model)

str(LifeCycleSavings)
View(LifeCycleSavings)
?LifeCycleSavings
model <- lm(sr ~(pop15+pop75+dpi+ddpi)^2, LifeCycleSavings)
model        

my_df <- mtcars
fit  <- glm(am ~ disp + vs + mpg, my_df, family = "binomial")
summary(fit)

log_coef <- fit$coefficients
log_coef


ggplot(ToothGrowth, aes(supp, len))+
  geom_boxplot(aes(fill = as.factor(dose))) 

my_df <- read.csv("data.csv")
head(my_df)
str(my_df)
my_df1 <- subset(my_df, !is.na(my_df$admit))
head(my_df1)
fit  <- glm(admit ~ rank * gpa, my_df1, family = "binomial")
summary(fit)
exp(fit$coefficients)
head(predict(object = fit))
head(predict(object = fit, type = "response"))
my_df$prob  <- predict(object = fit, newdata = my_df, type = "response")

length(which(my_df$prob>=0.4 & is.na(my_df$admit)))
