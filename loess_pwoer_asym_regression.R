# 线性拟合(y = a*x + b)
# 非线性拟合(多项式,指数,渐进,幂函数,对数,
# 逻辑斯蒂,米氏方程,生长曲线,韦布尔模型)
library(aomisc)
library(drc)
library(broom)
library(tidyverse)

# Local polynomial regression fitting
# Locally estimated scatterplot smoothing(loess)
# LOESS（局部加权回归）
# LOESS是一种用于局部回归分析的非参数方法，
# 由于Nadaraya-Watson估计方法的加权是基于整个样本点，
# 而且往往在边界上的估计效果并不理想。(傻瓜式方法)
# 把样本划分成多个小区间,对区间中的样本进行多项式拟合，
# 不断重复这个过程得到在不同区间的加权回归曲线，
# 最后再把这些回归曲线的中心连起来合成完整的回归曲线。

data.table(cars)
attach(cars)
cars.lo <- loess(dist ~ speed)
plot(cars)
lines(speed, predict(cars.lo))
# 曲线弯弯曲曲,不推荐使用,是因为很难获得方程表达式。
summary(cars.lo)
# Number of Observations: 50 
# Equivalent Number of Parameters: 4.78 
# Residual Standard Error: 15.29 
# Trace of smoother matrix: 5.24  (exact)
# Control settings:
# span     :  0.75 
# degree   :  2 
# family   :  gaussian
# surface  :  interpolate	  cell = 0.2
# normalize:  TRUE
# parametric:  FALSE
# drop.square:  FALSE
# 没有办法获得一个确定的方程表达式

# expontial function指数函数
data(degradation)
attach(degradation)
model2 <- nls(Conc ~ NLS.expoDecay(Time, a , k))
model3 <- drm(Conc ~ Time, fct = DRC.expoDecay())
# nls和drm两种不同方法结果一致
# nls需要带参数,更加麻烦一点,相比drm.
# expoDecay就是拟合exp指数函数
summary(model2)
summary(model3)
#                   Estimate    p-value 
# init:(Intercept) 99.6349312  2.2e-16 ***
# k:(Intercept)     0.0670391  2.2e-16 ***
# 函数模型:99.635*(e^-0.067)^x, 两个参数都显著需要
# 
plot(Conc ~ Time)
lines(Time, predict(model2))
# 看图可知拟合线并不是很平滑,因为点的数量少
lines(seq(0, 70, 0.1),
      predict(model2, newdata = data.frame(
        Time = seq(0, 70, 0.1))),
      col = 'red')
# newdata给拟合模型重新匹配数据,
# 要用'Time='因为模型里面的自变量名称就是Time
# seq(0, 70, 0.1):数据是从0到70，0.1是按0.1来生成数组。
plot(model3, log = "", main = "exponential decay")
# 相比nls模型,drm模型拟合出来的模型不受数据个数的影响
# 可以直接画出平滑的曲线
# 但是plot不能和plot产生叠加,
# 所以没办法在上面显示完整的点分布情况
# 上面的函数画出的是平滑的曲线加离曲线最近的7个点。

# 根据画图可以得出式子确实是:99.635*(e^-0.067)^x
# 底数大于1时是向上的曲线,小于1时是向下的曲线。
# 乘数99.635(e^0 = 1)从图中看到曲线的起点靠近100。
# 相当于在99.635的基础上不断乘以0.067,Y值会不断缩小。
# (2.718281828^0.067)^x = (0.9351952^30)*99.635
# exp(-0.067*10)*99.635

ggplot(degradation, aes(Time, Conc))+
  geom_point()+
  geom_smooth(method = 'nls', 
              formula = 'y ~ a*exp(-k*x)',
              method.args = list(start = list(a = 99, k = 0.06)),
              se = F)
# exp(2) = e^2, exp(k*x) = (e^k)^x
expfct <- function(x, a, k){
  a*exp(-k*x)}
# 定义一个函数,后面可以直接调用
ggplot(degradation, aes(Time, Conc))+
  geom_point()+
  geom_smooth(method = 'nls', 
              formula = 'y ~ expfct(x, a, k)',
              method.args = list(start = list(a = 99, k = 0.1)),
              se = F)
# start = list(a = 99, k = 0.06),要有两个参数的大致值。
# 所以先要用nls或者drm拟合出a和k的大致(初始)数值。
# start初始要找和结果距离适当的数，
# 如果相差太多就会匹配不出来报错:无法匹配
# 如果不存在aomsic包提供的自动拟合,
# 就需要自己一个个尝试,去逼近最后的结果。
selfStart(expfct, Conc ~ Time)

# Asymptotic function渐近线
# y = a - (a - b)*exp(-c*x) 
# a ~ plateau, b ~ init(initial), c ~ m
X1 <- c(1, 3, 5, 7, 9, 11, 13, 20)
Y1 <- c(8.22, 14.0, 17.2, 16.9, 19.2, 19.6,19.4, 19.6)
df <- tibble(X1, Y1)
# 形成有两列不同title一一对应的数组,可以直接用于拟合。
model4 <- nls(Y1 ~ NLS.asymReg(X1, init, m, plateau))
model5 <- drm(Y1 ~ X1, fct = DRC.asymReg())
summary(model4)
summary(model5)
# y = plateau - (plateau - init)*exp(-m*x)
# y = 19.62982 - (19.62982 - 3.75614)*(e^-0.33708)^x
# y = 19.62982 - 15.87368*(e^-0.33708)^x
# (2.718281828^-0.33708)^x = 0.7138517^x(e约等于2.718281828)
# 当x = 0是是19.62982-15.87368*1 = 3.75614
# 随着x增加,15.87368的乘数减少,无限趋近为0
# y值不断增加趋近于19.62982。
# 还是指数函数的变形。

plot(model5, log = "", col = 'red', 
     main = 'Asymptotic regression')
lines(seq(1, 20, 0.1), 
      predict(model4, newdata = data.frame(X1 = seq(1, 20, 0.1)),
              col = 'blue'))
df1 <- tibble(V1 = seq(1, 20, 0.1),
              V2 = predict(model4, 
                           newdata = data.frame(X1 = seq(1, 20, 0.1))))
ggplot(df, aes(X1, Y1))+
  geom_point()+
  geom_line(data = df1, aes(V1, V2), color = 'red')
# plot/lines/ggplot三种画法,同样的图像

# power funcion(幂函数)
# Y = a*X^b

data("speciesArea")
attach(speciesArea)
model6 <- nls(numSpecies ~ NLS.powerCurve(Area, a, b))
model7 <- drm(numSpecies ~ Area, fct = DRC.powerCurve())
summary(model7)
# Y = 4.348404* X ^ 0.329770

plot(speciesArea)
lines(seq(1, 256, 1), 
      predict(model6, 
              newdata = data.frame(Area = seq(1, 256, 1))),
      col = 'red')
plot(model7, log = "")
df3 <- tibble(V1 = seq(1, 256, 1),
              V2 = predict(model6, 
                           newdata = data.frame(Area = seq(1, 256, 1))))
ggplot(speciesArea, aes(Area, numSpecies))+
  geom_point()+
  geom_line(data = df3, aes(V1, V2), color = 'red')

# Logarithmic function对数函数
# Y = a + b*log(X)
X4 <- c(1 ,2, 4, 5, 7, 12)
Y4 <- c(1.97, 2.32, 2.67, 2.71, 2.86, 3.09)
model8 <- lm(Y4 ~ log(X4))
model9 <- nls(Y4 ~ NLS.logCurve(X4, a, b))
model10 <- drm(Y4 ~ X4, fct = DRC.logCurve())

summary(model8)
summary(model9)
summary(model10)
# 一致,都是Y = 1.99711 + 0.44797log(X)
plot(X4, Y4)
lines(seq(1, 12, 0.1), 
      predict(model8,
              newdata = data.frame(X4 = seq(1, 12, 0.1))))
plot(model10, log = "", main = "Logarithmic function")
df4 <- tibble(V1 = seq(1, 12, 0.5),
              V2 = predict(model9, newdata = data.frame(X4 = seq(1, 12, 0.5))))
df0 <- tibble(X4, Y4)
ggplot(df0, aes(X4, Y4))+
  geom_point()+
  geom_line(data = df4, aes(V1, V2), color = 'red')
# 要画点还是需要tibble(X4, Y4),原数据的集合

# Michaelis_Menten function/Retangular hyperbola
# 米氏方程/酶促方程
# Retangular hyperbola等轴双曲线; 直角双曲线 
# Y =  aX/(b+X)
# d = a, e = b 
X5 <- c(0, 5, 7, 22, 28, 39, 46, 20)
Y5 <- c(12.74, 13.66, 14.11, 14.43, 14.78, 14.86, 14.78, 14.91)

df5 <- tibble(X5, Y5)
# SSmicmen(input, Vm, K)
model11 <- drm(Y5 ~ X5, fct = MM.2())
summary(model11)
# Y = 15.00081X/(0.47095 + X)
df6 <- tibble(V1 = seq(0, 200, 2),
              V2 = predict(model11, 
                      newdata = data.frame(X5 = seq(0, 200, 2))))
plot(model11, log = "", main = 'Michaelis_Menten function')
# 从图上可知,X是存在一个陡然增加,然后趋于平缓(增加减速)。
ggplot(df5, aes(X5, Y5))+
  geom_point()+
  geom_line(data = df6, aes(V1, V2), color = 'red')+
  ylim(12, 15)

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

df7 <- tibble(V1 = seq(21, 186, 1),
              V2 = predict(model17, 
                           newdata = data.frame(seq(21, 186, 1))))
head(df7)
ggplot(beetGrowth, aes(DAE, weightInf))+
  geom_point()+
  geom_line(data = df7, aes(V1, V2), color = 'red')

# 