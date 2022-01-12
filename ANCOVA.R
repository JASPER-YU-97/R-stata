# 单/多因素协方差分析
# standard one/two-way ananlysis of variance(ANOVA)
# ne/two-way ananlysis of corvariance(ANCOVA)

# ANCOVA要求数据服从正态分布，以及各组方差相等，
# 同时还假定回归斜率相同。

# 类比偏相关分析(partial correlation analysis)
# 剔除部分同组与研究的两个var之外的可能会影响结果的var.
# ANCOVA就是剔除研究对象之外的影响。

library('multcomp')
data.table("litter")
head(litter)
# dose(剂量)weight(体重)gesttime(怀孕时间)number(几个崽)
# 研究某种药物剂量对老鼠怀孕的影响


# one-way ANCOVA 单因素协方差分析
# 与单因素ANOVA基本一致,只是比它多了一些协变量的存在。
# 第一,数据结构调整,观测数,
# 协变量观测值均由其它标识列唯一确定，为一组数据。
# 第二,使用aggregate函数求均值,但均值受协变量影响，
# 此处均值的意义不大，此处可省略。
# 第三,使用aov(formula,data)进行协方差分析,
# 把所有需要的协变量放到实际水平（或叫分组）变量前面,
# 如aov(y~coval1+coval2+…+covaln+val,data)。

# dose---->weight
# 去除-gesttime, weight ~ dose,目的:研究weight和dose关系
# 写法：weight ~ gesttime + dose加号连接。
# weight(y)~gesttime(要排除的因子,多少个都行)+dose(x)

# 做分析之前要对数据进行检验：正态性,方差齐性
# 独特的是回归斜率相同属性
summary(aov(weight ~ gesttime*dose, data = litter))
# gesttime:dose:P=0.17889>0.05说明交互作用不显著
# 回归后直线平行,延长后不相交。

summary(aov(weight ~ gesttime + dose, data = litter))
# P=0.00597(gesttime)**<0.05,说明怀孕时间对体重影响明显。
# P=0.04988 (dose)* <0.05,
# 说明排除怀孕时间的干扰,剂量本身对体重也有比较明显的影响.
summary(aov(weight ~ gesttime + dose, data = litter))
# P=0.00597(gesttime)**>0.05,
# 说明排除剂量的干扰,怀孕时间本身对体重有明显影响.
# P=0.04988(dose)*说明剂量对体重影响也有比较明显的影响.
# 说明不能乱换位子得出数据的意义已经不同了。

library(dplyr)
library(effects)

litter$dose <- as.factor(litter$dose)
# 因子化dose，因为dose是数字形式。
fit <- aov(weight ~ gesttime + dose, data = litter)
with(litter, effect('dose', fit))
# 0           5       50      500 
# 32.35367 28.87672 30.56614 29.33460 
# 怀孕时间的差异需要归一化,对每一个重量进行处理,
# 使怀孕时间统一，这样dose和重量之间关系更加明确。
# 例:两个工种工资的mean，数据有工种,工作时间,薪水
# 要看出工种间的差异就必须统一每个人的工作时间,
# 然后分工种计算mean,不能因几个人工作时间长而得出结论
# 虽是排除工作时间的影响，但实际上是对薪水的数据进行处理
# 最简单的归一化就是相除,但是得到的就是单位薪水,
# 而不是平均薪资。所以要确定标准，然后标准化。
# effect就是标准化

library(magrittr)
litter %>%
  group_by(dose) %>%
  summarise(mean = mean(weight))
# magrittr提供管道的方式用一条函数实现多条函数的事情。
#  dose   mean
# 1 0      32.3
# 2 5      29.3
# 3 50     29.9
# 4 500    29.6
# 这个是没有归一化之后的结果
litter$rate <- litter$weight/litter$gesttime
head(litter)
# litter$rate可以直接新命名一列数在data.frame格式里面。
# 根据litter$rate调整weight来消除gesttime的影响。
summary(aov(rate ~ dose, data = litter))
# P=0.0796,0.1<.<0.05比较显著(对比rate和协方差的标准相同)
# rate相当于手动标准化,但和协方差分析的结果还是有差距的。

library(HH)
# HH包ancova()函数,绘制因变量、协变量和因子之间的关系图查看。
ancova(weight ~ gesttime + dose, data = litter)
# 在不同的剂量下,重量均随着怀孕时间的增加而增加,
# 且各剂量的线相互平行
# 注:各直线平行是由于公式的假设而非结果。
# 4条回归线平行,代表不同的dose,只是截距项不同；
# 回归线拟合效果并不理想,只有少部分点发布在线的两边。


# two-way ANCOVA 多因素协方差分析
litter$type <- rep(c('a', 'b'), each =5, length = 74)
# length = 74总共74个数,5a+5b顺序循环排列
fit1 <- aov(weight ~ gesttime + dose*type, data = litter)
summary(fit1)
# 总结:在aov里面,*代表着增加一起受分析的因素,
# 变成多元素XX分析,包括两个因素本身和交互(A:B)
# 两个因素本身:A/B
# A+B+C是增加控制的因素,只有最后一个C是受分析的因素
# 前面不管增加几个都是受控制的因素
# A+B不等于B+A,因为前者是排除A的影响看B对结果的影响
# 后者是排除B的影响看A对结果的影响


# 重新测量方差分析
# ANOVA的基础是样本独立性,重复测量就是非独立的一种
# 之前的非独立是同一批人的发展阶段不同导致的非独立
# 14-25岁和35-39岁的身高之间的就非独立
# 50个男女生测体重，每个人都测了一次
# 就是标准的非重复抽样的统计。
# 如果是监督这50人体重变化，每周测一次，就是重复测量。

data(CO2)
CO2
# 草本植物对二氧化碳的响应情况
# Plant(6组植物)Type(地区)Treatment(寒冷的,不寒冷的)
# conc(CO2的含量)uptake(吸收率)

# 这组数据涉及到两次重复测量：一次是palnt，一次是conc
# plant是对一组7个植物重复编号为6组不同序号,产生随机6组数.
# 例:7个人在不同状态下对不同药物的反应,
# 人没有改变,但做每组实验的时候都重新将顺序打乱,再编号。
# 出现了完全的重复,表面上组间编号是不同的,
# 但是实际上实验对象是相同的,
# 表面上看是不重复,但是结合实验目的和背景就可以得出结论。

# Mc1,Mississippi,chilled就是编号Mc1这一组都是在
# Mississippi,chilled控制条件下的,
# 给不同的conc,看不同的uptake吸收率结果。

# conc:95,175,250,350,500,675,1000:7个数,对应一组7个植物。
# 产生了组间的重复
# Type不涉及组内重复,因为Quebec横跨了多个实验小组,
# 实验小组的定义是联系实验目的的,
# 实验目的就是看7个植物的不同uptake。

# 例:两个人在同一年中各月的身高变化,
# 这里就包含了一个因变量（身高）,
# 一个组间因子（人，有两个，所以是两个水平）,
# 一个组内因子（时间，12个月，就是12个水平）,
# 此时,对这两个人来说（测试对象）,他们每个人要测试12次，
# 即组内因子的水平数，所以叫重复测量方差分析。

chill <- subset(CO2, Treatment == 'chilled')
chill
# 可以看出chilled里面包括两个地区：
# Quebec魁北克和Mississippi密西西比,所以构成嵌套结构。
summary(aov(uptake ~ Type + Error(Plant/conc),chill))
# Error用来填重复的随机因子,用/隔开
# Error: Plant/Plant:conc/Within
# 可以看排除不同重复因子之后的P值和相关数据。
summary(aov(uptake ~ conc*Type + Error(Plant/conc), 
            chill))



# 非参数方差分析(non-parametric analysis)
# 重抽样方差分析
# 样本少的情况下很难让数据呈现正态分布
# 但不符合正态,无法进行常规的方差分析
# 通过重抽样来增加样本的个数,使其趋向于正态分布。
# 缺点:由于重抽样是随机的过程,所以会导致每次都结果不同。
library(lmPerm)
data.table(litter)
attach(litter)
summary(aovp(weight ~ dose*type))
summary(aov(weight ~ dose*type))
# P值两次略有不同,但是都能得出同一个结论:不显著
# 同时运行两次aovp得到的是两个相近而不同的P值。