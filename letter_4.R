counts <- table(Arthritis$Improved)
# vcd包里面自带的关于关节炎的数据Arthritis$Improved。
counts
par(mfrow=c(2,2), las = 0.5)
barplot(counts, main="Simple Bar Plot",
        xlab="Improvement", ylab="Frequency")
barplot(counts, main="Horizontal Bar Plot",
        xlab = "Frequency", ylab = "Improvement",
        horiz=TRUE)
# horiz=TRUE代表是横向的barplot.
counts_1 <- table(Arthritis$Improved, Arthritis$Treatment)
counts_1
#     Placebo Treated
# None        29      13
# Some         7       7
# Marked       7      21
barplot(counts_1, main="Stacked Bar Plot", 
        xlab="Treatment", ylab="Frequency",
        col=c("red", "yellow","green"),
        legend=rownames(counts))
# Placebo和Treated作为x轴的两个bar的名称。
# 当中的some, none和marked都累加在一个bar上。
# 用不同的颜色标识，legend添加颜色的图例。
barplot(counts_1, main="Grouped Bar Plot",
          xlab="Treatment", ylab="Frequency",
          col=c("red", "yellow", "green"),
          legend=rownames(counts), beside=TRUE)
# beside=TRUE将累加起来的bar平摊开:一个名称对应3条bar.


# Pie Chart
library(plotrix)
par(mfrow=c(2,2), mar = c(4, 4, 1, 1))
slices <- c(10, 12, 4, 16, 8)
lbls <- c("US", "UK", "Australia",
          "Germany", "France")
pie(slices, labels = lbls,main="Simple Pie Chart",
    edges=300,radius=10)
# edges是画布方框的大小, radius是饼的半径大小。
# slices就是数据，画图是按数值占sum的比例画的饼图。
# label就是饼的每一块名称， 顺序按照slices的数据一一对应。

pct <- round(slices/sum(slices)*100)
# 比例
lbls2 <- paste(lbls, " ", pct, "%", sep="")
# paste可以将字母连接成一体，’,‘相当于是pytnon里面的+。
# , " ", pct, "%", sep=""代表名称和pct之间应该有空格，
# 但是不能太大，sep=""就是短空格的意思。
pie(slices, labels=lbls2, col=rainbow(15),
    main="Pie Chart with Percentages",edges=300,radius=1)
# rainbow后面要加(),不然报错：非矢量不能被复制
# rainbow()里面要填数值，整数，小数会自动省略小数部分。
# col 可以 = rainbow(), heat.colors(),
# terrain.colors(),topo.colors(),cm.color().
pie3D(slices, labels=lbls,explode=0.5,
      main="3D Pie Chart ",edges=300,radius=1)
# 3D饼状图
# explode是几个部分的分类程度。
mytable <- table(state.region)
# 美国几个州的数据。
lbls3 <- paste(names(mytable), "\n", mytable, sep="")
# "\n"是new line另起一行的意思。
pie(mytable,labels=lbls3,
    main="Pie Chart from a Table\n(with sample size)",
    edges=300,radius=1)
# Fan Plot扇形图
fan.plot(slices, labels = lbls, main="Fan Plot",
         edges = 400, radius = 1)

# Dot Plot点图
dotchart(mtcars$mpg,
         labels=row.names(mtcars),cex=0.7,
         main="Gas Mileage for Car Models",
         xlab="Miles Per Gallon")
# 每个title后面的数值（只有一个）用点标出。

# Descriptive statistics
head(mtcars)
# 前6行，看出所有的列的名称和大致数量/值。
summary(mtcars)
# 分析了所有列/variables的Min,1st qu,median, mean,
# 3rd qu, max-->基本的统计信息。和python一致。
# 加载一些包可以扩展summary的指标数量。
# 用function可以完成summary的工作。

# Frequency and contingency tables
attach(mtcars)
table(cyl)
table(mpg)
summary(mpg)
table(cut(mpg,seq(10,34,by=2)))
# 列出频率统计表，cut将连续/散点变量切割成分类变量。
# 10,34代表从10开始统计，一直到34，
# 不包括10，但包括34本身。左开右闭。
# by相当于seq或者step就是间隔，统计的区间是每2一隔断。
# (10,12] (12,14] (14,16] (16,18] (18,20] (20,22] 
#    2       1       7       3       5       5

# Correlations相关
states = state.x77[,1:6]
# state.x77:R语言自带美国50州1977年的
# 人口、收入、文盲率、预期寿命、谋杀率和高中毕业率数据。
states
cov(states)
# 两两对照的协方差，协方差矩阵
var(states)
# 方差variance，方差矩阵
cor = cor(states, method = "kendall")
# 相关系数矩阵, 对角线为1，自己对自己是1（100%相关）。
# 可以采用三种方法：Pearson相关系数(积差相关系数)
# Spearman等级相关系数，Kendall's Tau相关系数
# P：两个正态分布的连续变量，S：两个有序分类变量。
# K：适用于不满足Pearson相关系数正态分布要求的连续变量。
# 也可以用于有序分类变量的之间的相关性测量。

# 检验预期寿命与谋杀率相关性。
cor.test(states[, 3], states[, 5], method = "kendall")
# 检测出两个值之间的相关系数
# 更细节的数据包括p-value之内相关的系数。
# 同时也可以添加alternative:"two.sided", "less", "greater".
# 结果显示，P大于0.05。即两者之间相关性微乎其微。

# T-test
x = rnorm(100, mean = 10, sd = 1)
y = rnorm(100, mean = 30, sd = 10)
t.test(x, y, alt = "two.sided",paired=TRUE)
# R当中的paired是是否一一配对，默认false。
# 可参照第3条用途，paired是进行配对样本均值检验。
# conf.level是置信区间，默认0.95.
# var.equals是方差相等，默认false。

# sig如果大于0.05表示方差齐性，后面一行，如果小于表示不齐性。
# 看第二行。后面的sig.(双侧）就是P。
# P大于0.05不显著，小于0.05有显著性，小于0.01极显著性。

# t检验用途：
# 1,单样本均值检验（One-sample t-test）
# 用于检验 总体方差未知、正态数据或近似正态的
# 单样本的均值是否与已知的总体均值相等
# 2,两独立样本均值检验（Independent two-sample t-test）
# 用于检验 两对独立的正态数据或近似正态的样本的均值是否相等，
# 这里可根据总体方差是否相等分类讨论.
# 3,配对样本均值检验（Dependent t-test for paired samples）
# 用于检验, 一对配对样本的均值的差是否等于某一个值.
# 4,回归系数的显著性检验（t-test for regression coefficient significance）
# 用于检验回归模型的解释变量对被解释变量是否有显著影响.

