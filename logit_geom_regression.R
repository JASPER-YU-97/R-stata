# 线性拟合(y = a*x + b)
# 非线性拟合(多项式,指数,渐进,幂函数,对数,
# 逻辑斯蒂,米氏方程,生长曲线,韦布尔模型)
library(aomisc)
library(drc)
library(broom)
library(tidyverse)


# Logistic function
# sigmoidal curves:S形曲线
# Logistic函数或Logistic曲线是一种常见的S形函数。
# 起初阶段大致是指数增长,然后随着开始变得饱和，
# 增加变慢；最后，达到成熟时增加停止。
# Logistic回归更加适用于分类,不连续的var的情况
# S（x）=m*Logis（B,x+t）/（n+Logis（B,x+t））
# 两参数:Logis（B,x）/（n+Logis（B,x））
# 三参数:Logis（B,x）/（n+Logis（B,x））
# 
data("beetGrowth")
attach(beetGrowth)
model12 <- nls(weightInf/max(weightInf) ~ NLS.L2(DAE, b, e))
model13 <- nls(weightInf ~ NLS.L3(DAE, b, d, e))
model14 <- nls(weightInf ~ NLS.L4(DAE, b, c, d, e))

model15 <- drm(weightInf/max(weightInf) ~ DAE, fct = L.2())
model16 <- drm(weightInf ~ DAE, fct = L.3())
model17 <- drm(weightInf ~ DAE, fct = L.4())
# drm和nls两种方法, 结果一致。


summary(model12)
summary(model17)
# A：x趋近于无穷大或无穷小时，y的最大值；d
# D：x趋近于无穷大或无穷小时，y的最小值；c
# C：曲线拐点；e
# B：与拐点处曲线斜率相关 b
# y = (A - D) / [1 + (x/C)^B] + D
# Y=Bottom + (Top-Bottom)/(1+10^((LogEC50-X)*HillSlope))
# Y=Bottom + (X^Hillslope)*(Top-Bottom)/(X^HillSlope + EC50^HillSlope)
# y = ((25.149004+0.113031)/(1 + 50^-0.424805/57.638648^-0.424805))-0.113031

plot(model15, log = "", main = "Logistic function")
plot(model16, log = "", main = "Logistic function")
plot(model17, log = "", main = "Logistic function")
# 参数增加,更加拟合了。
# 正常画图plot时遇到一个X对多个Y的时候是取X对应的平均值Y
# ggplot的geom_point是画出全部点。
df7 <- tibble(V1 = seq(21, 186, 1),
              V2 = predict(model17, 
                           newdata = data.frame(seq(21, 186, 1))))
head(df7)
ggplot(beetGrowth, aes(DAE, weightInf))+
  geom_point()+
  geom_line(data = df7, aes(V1, V2), color = 'red')

# Gompertz function
# Gompertz曲线初期增长缓慢,以后逐渐加快,
# 当达到一定程度后,增长率又逐渐下降,最后接近一条水平线。
# 该曲线的两端都有渐近线,其上渐近线为Y=K,下渐近线为Y=0。
# 通常用于描述事物的发展由萌芽、成长到饱和的周期过程。
# 如工业生产的增长、产品的寿命周期、一定时期内的人口增长等
# 这些趋势都可以用Gompertz曲线来研究
# Y = k*(a^(b^x)

model18 <- nls(weightFree/max(weightFree) ~ NLS.G2(DAE, b, e))
summary(model18)
model19 <- nls(weightFree ~ NLS.G3(DAE, b, d, e))
summary(model19)
model20 <- nls(weightFree ~ NLS.G4(DAE, b, c, d, e))

model21 <- drm(weightFree/max(weightFree) ~ DAE, fct = G.2())
model22 <- drm(weightFree ~ DAE, fct = G.3())
summary(model22)
plot(model21, log = "", main = "Gompertz function")
# weightFree/max(weightFree)相对对数值标准化了(0~1)
df10 <- tibble(V1 = seq(21, 186, 1), 
               V2 = predict(model19, newdata = data.frame(DAE = seq(21, 186, 1))))
ggplot(beetGrowth, aes(DAE, weightFree))+
  geom_point()+
  geom_line(data = df10, aes(V1, V2), color = "red")

  