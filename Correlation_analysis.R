# 相关性分析(Correlation analysis)
# 接上节独立性检验(independence testing)
# 独立性检验是相关性分析的前提
# 两者只有存在非独立性(相关)，才能分析是正负什么形式的相关。

# Correlations相关
states = state.x77[,1:6]
# state.x77:R语言自带美国50州1977年的
# 人口、收入、文盲率、预期寿命、谋杀率和高中毕业率数据。
states
cov(states)
# 两两对照的协方差，协方差矩阵
var(states)
# 方差variance，方差矩阵
cor(states, method = "pearson")
# 相关系数矩阵, 对角线为1，自己对自己是1（100%相关）。
# 可以采用三种方法：Pearson相关系数(积差相关系数)
# Spearman等级相关系数，Kendall's Tau相关系数
# P：两个正态分布的连续变量，S：两个有序分类变量。
# K：适用于不满足Pearson相关系数正态分布要求的连续变量。
# 也可以用于有序分类变量的之间的相关性测量。

# Pearson积差相关系数衡量两个定量变量之间的线性相关
# Spearman等级相关系数衡量分级定序变量之间的相关关系
# Kendall相关系数是一种非参数的等级相关。
# Pearson的条件，不满足就要找Spearman和Kendall：
# 1样本数据必须满足正态分布。
# 2样本数据是连续的且数据之间的差异不能太大
# （不能包含离群点或异常值）。
# 3每组样本之间相互独立。
# 4两组数据（两个对象）之间呈线性关系。
# Pearson适用于连续变量，定量变量(比如年龄大小)
# 不是就考虑Spearman和Kendall。定序变量(A,B,C等级)

# 检验预期寿命与谋杀率相关性。
cor.test(states[, 3], states[, 5], method = "pearson")
# 检测出两个值之间的相关系数
# 更细节的数据包括p-value之内相关的系数。
# 同时也可以添加alternative:"two.sided", "less", "greater".
# 结果显示，P大于0.05。即两者之间相关性微乎其微。
# 结果：
# data:  states[, 3] and states[, 5]
# t = 6.8479, df = 48, p-value = 1.258e-08
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.5279280 0.8207295
# sample estimates:  cor 0.7029752 
# 主要看p-value:1.258e-08<0.05,所以显著相关。
# 第二看cor 0.7029752，正相关。

# 批量算cor:corr,需要library(psych)
corr.test(states)
# 出两个矩阵，一个是cor，有正有负，对角为1
# 另外一个是p值，越小越好，取整后会出现很多0
# p值矩阵对角为0.

head(states)
# (Population,Income,Illiteracy,Life Exp,Murder,HS Grad)
# 看头可以看出总共是6个var.

# 偏相关分析(partial correlation analysis)
# 剔除部分同组与研究相关性的两个var之外的可能会影响结果的var.
# 需要提前library(ggm)
pcor(c(1, 5, 2, 4, 6), cov(states))
# c(1, 5, 2, 4, 6):前两个是要做相关性分析的var，
# 后面的是要剔除影响的var的列号。
# ->第1列Population和第5列Murder之间的相关性分析。
# 0.4541633

pcor.test(pcor(c(1, 5, 2, 4, 6), cov(states))
          , 3, n = nrow(states))
# pcor.test对偏相关的检测, 出3个值：pvalue,tval,df.
# 主要看pvalue:0.001342918<0.05, 显著相关。

# pairs panel:将产生 X 的列之间两两相对的
# 成对散点图阵列（pairwise scatterplot matrix）
pairs.panels(states, stars = T)
# 能够一目了然数据的情况，不同var的发布，两两间的相关性。




