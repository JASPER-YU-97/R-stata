# fitting a linear model
# y = b+a*x

X <- seq(5, 50, 5)
Y <- c(12.6, 74.1, 157.6, 255.6, 303.4, 462.8, 669.9, 805, 964.2, 1169)
plot(X, Y)
# 点图
fit <- lm(Y ~ X)
# lm拟合一个线性回归模型
plot(X, Y)
abline(fit)
# 拟合后的直线
summary(fit)
# Coefficients:
#         Estimate Std.|Error   |  t value  |Pr(>|t|)  
#          系数估计值 |系数标准误差 |t检验值|对应t值概率的2倍
# (Intercept) -221.833     52.571   -4.22  0.00292 ** 
#   X             25.791      1.695   15.22 3.44e-07 ***
# y = -221.833 + 25.791*x(直线函数)

# 拟合优度判定系数          |修正的拟合优度判定系数
# Multiple R-squared: 0.9666,Adjusted R-squared:0.9624 
# R-squared(值范围0-1),描述输入变量对输出变量的解释程度。
# 在单变量线性回归中R-squared 越大,
# 说明拟合程度越好,模型对数据的预测越准确。

# Adjusted R-square自由度调整 r 平方,
# 接近1的值表示更好的匹配,
# 当您向模型中添加附加系数时,它通常是适合质量的最佳指示器。

# variance方差:一组数据的变异程度
# variance = 1/(n-1)*∑(xi-x)^2

total <- 0
for (i in 1:10){
  total <- total +1/9*(i-5.5)^2
}
total
var(1:10)
sd = total^0.5
sd
sd(1:10)
# covariance = 1/(n-1)*∑(xi -x)*(yi-y)协方差

data.table (cars)
attach(cars)    
total1 <- 0
for(i in 1:50){
  total1 <- total1+
    1/49*(speed[i]- mean(speed))*(dist[i]- mean(dist))
}
total1
cov(speed, dist)
plot(cars)
# 协方差用于衡量两个变量的总体误差。
# 如果两个变量的变化趋势一致,
# 其中一个大于自身期望值时另外一个也大于自身的期望值,
# 那么两个变量之间的协方差就是正值.
# 如果两个变量的变化趋势相反,
# 即其中一个变量大于自身期望值时另外一个却小于自身期望值，
# 那么两个变量之间的协方差就是负值。
# 直观看就是在XY图里面拟合出来的直线
# COV为正时,斜率>0,直线向上。COV为负时,斜率<0,直线向下。
# 但由于cov受数据本身大小(120和1000)的影响。
#      speed dist
#       24  120
# 假设：speed1 dist1
#       100   1000
# 在对比speed和dist与speed1和dist1的COV的时候
# 就会因为大小不同而丧失对比的意义。
# 因为cov不是一个比值,而是一个差相乘的绝对值。
# 在计算时要对数据进行标准化(无量纲化)correlation

# 相关性系数correlation
# correlation = cov(x, y)/sd(x)*sd(y))
cov(speed, dist)/(sd(speed)*sd(dist))
cor(speed, dist)
# 相关性系数在-1~1之间,正数代表正相关,负数代表负相关。
# COV的升级版。


# ordinary least squares(OLS, least square regression)
# 最小二乘法
# 方法就是先假设在平面上随意花一条直线
# 从各个点向直线上做垂线,得出距离
# 对距离平方,方便计算,同时将距离取出绝对值
# 最后累加,找出累加后最小的那一条,拟合度最高。
# E = ∑(y - yi)^2 = ∑(a*x + b -yi)^2(因为y = a*x + b)
# y - yi表示最理想的拟合状态,预测的所有y都和实际yi重合。
# err = yi - y = yi - (a*x + b) = ri = residual(残差)
# 残差是拟合线和点之间的距离,残差越小拟合程度越好。
# 由于大部分分析模型都存在拟合度问题,
# 所以在像ANOVA,T检验里面也存在残值的概念
fitcar <- lm(dist ~ speed)
plot(cars)
abline(fitcar)

# residual standard error残差的标准差(RSE)
summary(fitcar)
# Residual standard error: 15.38 on 48 degrees of freedom
residual <- dist - predict(fitcar)
sqrt(sum(residual^2)/(50-2))
# 15.37959
# sum(residual^2) = E = ∑(y - yi)^2
# 50-2 = n-p-1：自由度

# 自由度是指取值不受限制的变量个数,
# 当有约束条件时,自由度减少。
# 一个样本数为n的样本中,当已知均值(yi)，
# 同时如果知道其中n-1个数的值,第n个数的值也就确定了。
# 方差的无偏估计除的是（n-1)，因为方差表示离散程度，
# 在表示离散程度的时候用到了均值，既然用到了均值，
# 就受均值的限制，自由度就少了1。
# 只有n-1个数可以任意取值,估计样本方差的自由度为n-1。

# 另外一个角度:参数就是约束条件的一种
# 一元回归分析采用的是两点确定一条直线
# 也可以转化为k(斜率)和b(截距)确定一条直线。
# 有两个参数限制就是导致自由度为n-2。
# 同样,三点确定一个二元一次回归模型
# a*x+b*y+c=z, 需要a,b,c三个参数来确定。
# 有三个参数限制就是导致自由度为n-3。
# 推导可得自由度df = n-p-1, p为变量个数。

# 在比较两个样本均值时,已知和,已知两个均值,
# 因此总体自由度为（n1-1）+（n2-1）=n1+n2-2。

# residual sum of squares(RSS)
# = ∑(y - yi)^2 = ∑(a*x + b -yi)^2
# = (Residual standard error^2)*(n-p-1)
RSS <- sum((dist - predict(fitcar))^2)
sum(residual^2)
# RSS就是没有开方和除以自由度的RSE(残值标准差)

# Explained Sum of Squares(ESS)
# 回归平方和/解释平方和:
# 反映自变量与因变量之间的相关程度的偏差平方和。
ESS <- sum((predict(fitcar) - mean(dist))^2)

# total sum of squares(TSS)
TSS <- sum((dist - mean(dist))^2)
var(dist)*49
# TSS和RSS完全不同,TSS不涉及任何与拟合和残值相关的。
# TSS就是数据本身的var*自由度,
# 反映全部数据误差大小的平方和。

RSS+ESS
# TSS=RSS+ESS。

plot(cars, ylim = c(-50, 150), col = 'blue')
abline(fitcar, col = 'red')
abline(h = mean(dist), col = 'green')
# h = mean(dist)画一条y值等于YY的平行于x轴的横线。
abline(v = 15, col = 'green')
# v = 15画一条x值等于XX的平行于y轴的竖线。
# 红线fitcar拟合的结果看RSS
# 绿线mean拟合的结果看TSS,点到平均值这条线的距离。

# (R2/r2):coefficient of determination决定系数
# 可以决定拟合度到底好不好,
# 并且可以跨数据比较的,去除数据大小影响的系数。
# 要去除数据大小的影响,就不能用绝对数,要用比值。
# R2/r2 = 1- RSS/TSS = ESS/TSS
# 就是看拟合出函数的偏差平方和占总偏差平方和的比重。
# 自变量与因变量间相关程度的偏差平方和/
# 除以:全部数据误差大小的平方和
# 所以R2/r2是越大越好,越接近1越好
1- RSS/TSS
summary(fitcar)
# Multiple R-squared:  0.6511

# Adjusted R-squared:
# 在一元回归拟合中作用不明显,但是在多元回归拟合中
# 可能出现R2增加但是实际上模型拟合度没有增加的情况。
# 调整R2可以解决这个问题,更加真实反应拟合的情况。
# adj R2 = 1 - (1 - R2)*(n - 1)/(n - p - 1)
# /(n - p - 1)考虑到自由度对变量数量增加的影响
1 - (1 - ESS/TSS)*(50 - 1)/(50 - 1 - 1)

# null model是指零模型(又叫空模型、截距模型)。
# 即不含任何自变量的模型
# 在多水平模型中，零模型是模型分析的前提，
# 用于判断是否有必要考虑数据的多水平结构，
# 因为它能提供对组内相关系数的估计，
# 从而判断多水平模型的构建是否有其必要性。
# 通过零模型判断数据存在显著的相关性,多水平结构不能忽略，
# 才有必要继续多水平分析,否则用常规的多因素分析方法即可。
# 例:abline(h = mean(dist),col = 'green')就是一条零模型。

# p value
fitnull <- lm(rep(mean(dist), 50) ~ speed)
fitnull <- lm(dist ~ 1)
# 给y赋值,mean或者直接1, 两个都是一个意思
fitcar
plot(cars)
abline(fitnull)
abline(fitcar)
anova(fitcar, fitnull)
# p value就是对两个模型或者多个模型做anova,来看差异性。
# 对零模型和拟合模型做方差分析
# 理论上期望,拟合的最优模型和零模型存在巨大差异。
# Pr(>F)1.49e-12 ***差异非常显著
summary(lm(dist ~speed))
# anova(fitcar, fitnull)中Pr的值和
# summary(lm(dist ~speed))里面Pr的值是一致的,
# 所以这个参数是用拟合参数和零模型做anova分析得到p值。

# 总结：R^2可以量化模型响应变量与因变量间的关系强弱
# p-value检验可以决定拟合方程的可靠程度。