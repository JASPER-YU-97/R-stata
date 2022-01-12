set.seed(250)
# set.seed只能用一次，只对于后面的一个random起效果。
# 用过seed以后得到的runif就是 一个固定值，不会因为运行次数变化而变化。
a -> runif(3, min = 0, max = 100)
floor(a)
# 向下取整，取掉所有小数部分。
ceiling(a)
# ceiling是向上取整，去掉小数部分，并且加1。
round(a, 4)
# 四舍五入保留几位小数
?round
# 出help
??bin
# 出相关的内容help
b = rnorm(100, mean = 10, sd = 2)
# n不可少，不然会报错：缺少参数"n",也没有缺省值
print(b)
data1 = read.csv(file = 'C:/sers/PC/data_formal/data_test_1.csv')
print(data1)
# read.csv(file =)可以读出csv的内容和python一样。
# 但是不能用r'C:\Users\PC\'的形式。
# 会报错：malformed raw string literal at line 1
# 要用'C:/sers/PC/'的形式。
data2 = read.csv('C:/Users/PC/plot/vs.txt')
data3 <- read.table(file = "C:/Users/PC/plot/vs.txt", 
                    header = TRUE, strip.white = TRUE, 
                    fileEncoding = 'utf-8', skipNul=TRUE)
data4 <- read.table(file = "C:/sers/PC/data_formal/data_test_1.csv")
attach(data1)
# 进入对一个表格的操作。
View(data1)
# 打开一个表格视图
print(No4)
print(No5)
# 打印列信息。不需要加“”
# set.seed(250)
# x <- runif(100, min = 0, max = 100)
# 100个，最小是0，最大是100的随机数。
# set.seed(123)
# y <- runif(100, min = 0, max = 100)
# runif是生成随机数，而rnorm是生成符合正态分布的随机数。
hist(x, breaks = 20, xlab = 'namber', ylab = 'freq',
     main = 'x', col = 'red')
# main代表python里面的title。
?hist
# hist(x, breaks = "Sturges",
# freq = NULL, probability = !freq,
# include.lowest = TRUE, right = TRUE,
# density = NULL, angle = 45, col = "lightgray", border = NULL,
# main = paste("Histogram of" , xname),
# xlim = range(breaks), ylim = NULL,
# xlab = xname, ylab,
# axes = TRUE, plot = TRUE, labels = FALSE,
# nclass = NULL, warn.unused = TRUE, ...)
?plot
plot(density(x),main = 'the density of x')
plot(x, type = 'o')
# y轴是x的值，x轴是x数组的下标index, 索引。
# 相当于电脑给给每个x赋了一个值，从1到len(x):x的个数。
# type: 'l'线，’o‘拐点用圆圈标注。
boxplot(x, y, col = 'red')
# boxplot(time~sex)
# 将time的数据做箱图，但是按找sex性别分开，形成两个箱图。
?qqnorm
# qq就是从mean的两边取数，如果数据符合正态分布：
# 特点应该是取值主要集中在mean的左右。
# 同时基本上qqline基本上是一条从左到右斜率为正的直线(line)。
# sd = 0时，点的中心在0值，点在mean的左右是对称分布。
# sd有数值时，可以看出中心向右边移动，qqline的起点左移动。
# qq图里面是按x轴从负数到整数的发布， 同时x的值也是从小到大分布。
# mean的改变不会影响qq图，和它正态分布的性质。
set.seed(100)
g = rnorm(100, mean = 0, sd = 10, skew)
# 
set.seed(125)
x = rnorm(100, mean = 0, sd = 1)
set.seed(250)
y = rnorm(100, mean = 0, sd = 1)
e = rsnorm(100, mean = 0, sd = 1, xi = 1.5)
# 需要引入fGarch。
# rsnorm里面xi代表偏度。正数代表左偏。
w = rsnorm(100, mean = 0, sd = 1, xi = -1.5)
# 默认中立位置是0,为负数时代表右偏。
# 绝对值越小，产生的偏斜越小。
Kurtosis = mean(((e-mean(e))/sd(e))^4)-3
Kurtosis# 峰度（kurtosis），
# 表征概率密度分布曲线在平均值处峰值高低的特征数。
Skewness = mean(((e-mean(e))/sd(e))^3)
Skewness# 偏度（skewness），是统计数据分布偏斜方向和程度的度量，
# 是统计数据分布非对称程度的数字特征。
hist(e)
plot(density(e))
hist(e)
plot(density(g))
# 期望值μ=0，即曲线图象对称轴为Y轴.
# 标准差(sd)σ=1条件下的正态分布，记为N(0，1)。
par(pty = 's')
# pty。一个字符型参数，表示当前绘图区域的形状.
# "s"表示生成一个正方形区域，而"m"表示生成最大的绘图区域。
# mfcol，mfrow。用于设定图像设备的布局
# （简单的说就是将当前的绘图设备分隔成了nr*nc个子设备）.
# 参数形式为c(nr, nc)。
# 子图的绘图顺序是按列还是按行就分别根据是参数指定的是mfcol还是mfrow。
# 想要实现相同的功能还可以利用函数layout或者split.screen。
qqnorm(x)
qqline(x)
qqnorm(e)
qqline(e)
# e是左偏态分布，所以qq图里面的点集中在左下。
# qqline的斜率也更大，与y轴的交点更靠近原点（0，0）。
qqnorm(w)
qqline(w)
# w是右偏态分布，所以qq图里面的点集中在右上。
# qqline的斜率也更小，与y轴的交点更靠近y轴。
hist(g, freq=FALSE, border = 'white')
# freq=FALSE意思是改freq的个数表达为比例表达。
# border = 'white'边框颜色。

# rnorm, dnorm, pnorm, qnorm
# r = random = 随机， d= density = 密度，
# p= probability = 概率 ， q =quantile = 分位

qqplot(x, y)
# 普通 QQ 图用于评估两个数据集的分布的相似程度。
# 如果两个数据集具有相同的分布，QQ 图中的点将落在 45 度直线上。
