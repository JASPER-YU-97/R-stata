# 差异性分析(diferences/variance ananlysis)
# 方差分析 (ANOVA),T检验（T-test）,卡方分析 (Chi-Square Analysis)
# 与3种相关性分析方法不同
# ANOVA和T-test适用于分类数据和定量数据之间的关系
# 如研究不同性别样本(分类）的满意度(打分,定量)差异情况。
# 卡方分析适用于分类数据和分类数据之间的关系
# 如性别(男女)和是否戴隐形眼镜之间(是否)的关系；
# 性别(男女)和是否购买理财产品(是否)的关系

# 与相关性分析方法不同，三种方法假设相同：
# 正态性假定
# 方差齐性假定：
# 方差齐性检验检测不同组别样本自身内部的态度波动情况。
# 例：研究三个年龄组对于电影的偏好，
# 如果三个组别样本内部态度波动基本一致,则说明方差齐.
# 内部波动一致就是不存在一个年龄组有多个偏好。

# 简单说方差齐性就是排除了组内波动对组间差异比较的影响。
# 如果方差一样，也就意味着值的波动程度是一样的，
# 在相同波动程度下，直接去比较均值，
# 如果均值之间存在显著差异,
# 就可以认为是不同组间处理带来的差异。

# 方差齐性检验是对两组样本的方差是否相同进行检验。
# 检验思想与均值之间差异性检验是一样的。 
# 方差比、Hartley检验、Levene检验、BF法、Bartlett检验。
# 方差齐也就排除了干扰性差异。

# 如果方差分析显示有差异时,
# 则研究人员需要具体对比各个组别的差异情况,
# 即对比各个组别的平均得分进一步分析说明,又称事后分析。

# T-test VS ANOVA---两组,水平 vs 大于两组,水平
# male/female -> T-test
# junior/senior/university -> ANOVA

data(UScrime)
us <- data.table(UScrime)
head(us)
# 不能用table获取，会报错企图建立一个元素多于2^31的表。
# 需要先library(data.table)
#  M So  Ed Po1 Po2  LF  M.F Pop  NW  U1 U2 GDP Ineq
#  Prob    Time    y
# so(south)是否位于南方
# U1(14~24年龄段城市男性失业率)4
# U2(35~39年龄段城市男性失业率)
# Prob(监禁的概率)

# independent T(独立性T检验)
# 南北监禁的差异情况
t.test(Prob~So, data = us)
# y~x是否在南边对监禁概率
# mean in group 0 mean in group 1 
#         0.03851265      0.06371269 
# prob的平均值可以看出0(北方)低于1(南方)。
# p-value = 0.0006506<0.05，极其显著的
# ->南方监禁率比北方高,而且差异是极其显著的(普遍现象)。

# dependent T(非独立性T检验)
# 两个年龄组之间城市男性失业率的差异性比较。
# 由于年龄组之间可能存在关系,因为二者都是城市男性失业率。
# 这两个因素是有相关性的,所以在做差异性分析之前：
# 先做独立性检验，再做相关性分析。
# 例:一个小区里面的孩子在小学时候的身高和大学时候的身高。
# 两者间是有持续发展(时间相关的变化)的关系的。

# 非独立T具体到应用就是配对T(paired T)
# 两个样本的样本量要相同；样本先后的顺序是一一对应的。
# 与独立样本t检验相比，配对样本T检验要求样本是配对的。

t.test(us$U1, us$U2, paired = T)
sapply(UScrime[c('U1', 'U2')], 
       function(x)c(mean = mean(x), sd = sd(x)))
#        U1       U2
# mean 95.46809 33.97872
# sd   18.02878  8.44545
# 14~24岁失业率要远远>35~39岁的人群
# p-value < 2.2e-16及其小的数远远小于0.05
# ->14~24岁和35~39岁间差异极其明显
# 但是sd(标准差)18.02878  8.44545差距也比较明显。
# 说明存在较大干扰性差异，需要做事后检验。

# sapply相当于是一个计算矩阵
# function(x)c(mean = mean(x))可以自定义对x里面数的计算。
# 第一位就是x，被计算数；[c('U1', 'U2')]形成矩阵
# data.table解析出来的数据sapply会报错
# 需要用data直接解析出数据。

# sd(标准差)差距明显可以用Welch T检验（Welch’s t test）。
# 若var.equal=T，则使用汇总的方差估计。
# 默认情况下，如果var.equal为FALSE，则为两组分别估计方差，
# 并使用对自由度的Welch修改。
# welch针对两种情况:样本方差不齐，样本量不一致。
# pair的情况下不存在样本量不一致的情况
# 样本方差不齐时会默认用welch,由于是pair,
# 所以welch不会显示，显示的是Paired t-test。
t.test(us$U1, us$U2, paired = T, var.equal = T)

# 样本不均等的差异性分析。
male <- c(55, 56, 57, 58, 56, 55, 60, 59)
female <- c(59, 56, 57, 54, 52, 55, 57, 58, 52, 57)
# male和female的样本个数不等，实际情况下很多：
# 例:一个班里面要研究男女生身高,但普遍男女生人数不等。
# 差异不大可以直接用独立T
var(male)
var(female)
t.test(male, female)
# 样本量不一致情况下会
# 默认var.equal为FALSE就是welch检测的情况
# 如果修改就是var.equal为T，使用汇总的方差估计。
# p-value = 0.2136
t.test(male, female, var.equal = T)
# p-value = 0.2269
# p值会不同。

data('iris')
iris


# 非参数检验(Nonparametric testing)
# 如果数据无法满足t检验或ANOVA的参数假设：
# 正态性和方差齐性，可以转而使用非参数方法
# 若两组数据独立(independent testing)
# 可以使用Wilcoxon秩和检验（Mann–Whitney U检验）
# 评估观测是否是从相同的概率分布中抽得的
# 在1个总体中获得更高得分的概率是否比另1个总体要大
# 和相关性分析中的Kendall一致都是非参数分析。

with(UScrime,by(Prob,So,median))
# with函数将数据归类计算，prob再so(0,1)的中位数。
# mean 0:0.03851265<1:0.06371269
# medium 0:0.038201<1: 0.055552
wilcox.test(Prob~So,data = UScrime)
# p-value = 8.488e-05<0.05,差异极其明显。
# wilcox的结果和独立t的检验一致
with(UScrime,wilcox.test(U1,U2,paired = TRUE))
# =wilcox.test(UScrime$U1,UScrime$U2, paired = TRUE)
# 针对非独立，可以添加参数paired=TRUE

#总结：
# T检验分为3种：独立T，配对T，welchT
# 前提有2个：正态性，方差齐性

# 如果方差不齐
# 方法1:wlech检验,方法2:variance汇总平均
# 样本sizes不一致：
# 如果var(male) = var(female)
# 直接用独立T，不行就回到方差不齐的问题2种解决办法。

# 如果不是正态分布
# 意味着3种检测都不行了，考虑非参数：wilcox.test

# paired T(配对T)要求样本一一对应。
# 两个var相互相关。



