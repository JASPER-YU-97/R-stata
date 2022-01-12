# regression fit model

library(aomisc)
library(drc)
library(broom)
library(tidyverse)
# Y~X:因变量~自变量,响应变量~解释变量

# Linear & Polynomials(线性,多项式)
# y = a*x + b 狭义的线性Linear
# y = a*x^2 + bx + c 广义的线性(包括指数)Polynomials
# 多项式模型过于复杂是不便于预测的,
# 所以一般是(1~3)阶多项式(a*x^3)
# 对广义的线性,回归分析也可能能够得出P值和R2(多数是没有这个数值的)
# 但是这数值解释能力是不可靠的,也称为伪的P或R2(seudo)
# 
X <- seq(5, 50, 5)
Y <- c(12.6, 74.1, 157.6, 255.6, 303.4, 462.8, 669.9, 805, 964.2, 1169)
plot(X, Y)
 
fit <- lm(Y ~ X)
lm(Y ~ poly(X, 1, raw = T))
# raw = T用原本的多项式,不然会按正交的方式来拟合模型。
# 1,就是degree = 1,一阶多项式
# 结果一致
summary(fit)
abline(fit)
# 看图可知,拟合线基本上是拟合的,但点还是表现出一定的弧度。
fit1 <- lm(Y ~ X + I(X^2))
fit2 <- lm(Y ~ poly(X, 2, raw = T))
# 如果不加raw = T会报错
# In abline(fit2):只用两个3回归系数中的第一个
summary(fit1)
summary(fit2)
# Y ~ poly(X, 2, raw = T)|Estimate  |Y ~ X + I(X^2)
# Intercept)              -24.97500   (Intercept)
# poly(X, 2, raw = T)1    6.10520       X 
# poly(X, 2, raw = T)2    0.35792      I(X^2) 
# 两种写法的实际各项参数是一致的,只不过项名称不一样。
#             Estimate |Std. Error|t value|Pr(>|t|)    
# (Intercept) -24.97500   31.74953  -0.787 0.457300    
# X             6.10520    2.65199   2.302 0.054820 .  
# I(X^2)        0.35792    0.04699   7.617 0.000125 ***
anova(fit, fit1)
# Pr二阶段I(X^2):0.000125 ***<0.05,差异非常显著,
# 在二阶处,模型出现的巨大变化,
# 但是没有办法判定是像好还是向坏变化。
# fit1/2是对多项式拟合中引入二阶自变量的两种不同写法。
fit3 <- lm(Y ~ X + I(X^2)+ I(X^3))
summary(fit3)
#               Estimate Std. Error t value Pr(>|t|)
# (Intercept) -3.180000  55.471631  -0.057    0.956
# X            2.238998   8.315724   0.269    0.797
# I(X^2)       0.525578   0.343050   1.532    0.176
# I(X^3)      -0.002032   0.004114  -0.494    0.639
# 同一组数据间比较模型店拟合度,
# 可以比较(RSE)Residual standard error:
# 一阶:76.96,二阶:26.99,三阶:28.58
# 相比一阶,二阶的拟合度有了巨大的提高,
# 而二阶和三阶段情况基本上一致,
# 加上三阶p值显示无明显差距。
# Adjusted R-squared:0.962(一阶),0.9954(二阶),0.9948(三阶)
# R2二三阶也基本一致, 可预测二三阶图像基本一致。
abline(fit2)
# abline不能用来画曲线, 画出来的是忽略二阶系数的一条直线。
plot(Y ~ X)
lines(X, predict(fit), col = 'green')
lines(X, predict(fit1), col = 'blue')
lines(X, predict(fit2), col = 'grey')
# fit1和fit2完全重合
lines(X, predict(fit3), col = 'red')
# fit3和fit1和预测一致,基本上一样
# 在拟合模型中一般遵循奥卡姆剃刀理论
# 如非必要,越简单越好,所以选择二阶拟合。
# 基于二阶模型,进行进一步精简化，
# 可以看到(Intercept)的P值0.4573>0.05是不显著的
fit4 <- lm(Y ~ X +I(X^2) - 1)
# - 1意味着将截距Intercept删掉,因为对模型没有显著改进。
lines(X, predict(fit4), col = 'black')
summary(fit4)
# Residual standard error: 26.34>26.99(未删减的二阶)
# R2：0.9982>0.9954(未删减的二阶),精简后更加拟合了

# 线性拟合(y = a*x + b)
# 非线性拟合(多项式,指数,渐进,幂函数,对数,
# 逻辑斯蒂,米氏方程,生长曲线,韦布尔模型)

model <- nls(Y ~ NLS.poly2(X, a, b, c))
model1 <- drm(Y ~ X, fct = DRC.poly2())
summary(model)
summary(model1)
# model函数和lm函数得出的结果一致,
# 但是自由度比较差,不能做删除截距的操作
#              Estimate   |Std. Error |t-value| p-value
# a:(Intercept) -24.975000  31.749531 -0.7866 0.4573005
# b:(Intercept)   6.105197   2.651989  2.3021 0.0548195
# c:(Intercept)   0.357924   0.046991  7.6168 0.0001245
# a,b,c分别代表截距,一阶系数,二阶系数
lines(X, predict(model), col = 'red')
# model和lm拟合出来的是一致的,曲线重叠
# predict(model) = predict(fit2)模型预测值
# 
plot(model1, log = "", col = 'purple')
# log = ""在横坐标没有取log的时候要给log赋空值。
# 没有设置就默认对x轴数值取log,
# model(nls)不行,model1(drm)可以

ggplot(df, aes(X, Y))+
  geom_point()+
  geom_smooth(method = 'lm', formula = 'y ~ x',
              se = F, color = 'green')+
  geom_smooth(method = 'lm', formula = 'y ~  poly(x, 2)',
              se = F, color = 'red')+
  geom_smooth(method = 'loess', 
              formula = y ~ splines::bs(x, 3),
              se = F, color = 'blue')+
  geom_smooth(method = 'lm', 
              formula = y ~ splines::bs(x, 3),
              se = F, color = 'black')
# ggplot(df, aes(X, Y))定数据和X~Y轴
# formula = 'y ~ x'一阶线性格式,'y ~  poly(x, 2)'二阶
# splines::样条


# ggplot优点:可以不断加图层来增加拟合的线,
# 可设定的地方比较多,method,formula,se,na.rm
# orientation ,show.legend,inherit.aes 
# 可以允许多种拟合方法拟合出的线在同一副图里面。
# 可选拟合方法:"lm", "glm", "gam", "loess"
# 缺点语句太长,一点语句出错可能导致整个长函数不能运行。

ggplot(mpg, aes(displ, hwy))+
  geom_point()+
  geom_smooth(method = 'lm', formula = 'y ~ x',
              se = F, color = 'green')+
  geom_smooth(method = 'lm', formula = 'y ~  poly(x, 2)',
              se = F, color = 'red')+
  geom_smooth(method = 'lm', 
              formula = y ~ splines::bs(x, 3),
              se = F, color = 'blue')+
  geom_smooth(se = F, color = 'yellow')
# 数据更加多和分散时,
# 可以更加明显的体现出更优的拟合模型的改善。
# geom_smooth(se = F)直接画出拟合最好的那条曲线。



