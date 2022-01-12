# 独立性检验(independence testing)
# 独立：两变量不相关
# 方法：卡方，Fisher，Conchran-Mantel-Haenszel检验。

# 1，假设(hypothesis):原假设：没发生，备择假设：发生了。
# 同样也可以看成H0设定为否定，H1设定为肯定。
# 对所研究的总体做的假设H0与H1相反，反推出H1的可能性。
# 例：想要知道能够以多大的把握认为H1:“吸烟与患肺癌有关”，
# 为此先假设，H0:吸烟与患肺癌没关系
# 用A表示不吸烟，B表示不患肺癌，
# 则“吸烟与患肺癌没有关系”等价于“吸烟与患肺癌独立”
# 即假设H0等价于 P(AB)=P(A)P(B)。
# 原假设和备择假设只有一个能够发生，相当于梦想是否实现的问题。
# p-value来确定这两个假设那个更加可能发生。
# 因为两个因子相关是小概率的情况，而独立是大概率的情况。
# p值小，说明小概率事件发生了，也就是梦想实现了
# p值大，说明小概率事件没发生，梦想没有实现
# 一般p-value定为0.05，当p>0.05时不拒绝原假设
# 当p<0.05时拒绝原假设, p值越大，没有发生的原假设越靠谱。
# 同样p值越小说明，发生事情的备择假设越可能实现。
# p值的设置越小, 越精确。：0.01
# 如果数据的争议较大，p值可以设置大一些：0.1

# 卡方检验
library(vcd)
counts <- table(Arthritis$Improved, Arthritis$Treatment)
chisq.test(counts)
counts
#        Placebo    Treated
# None        29      13
# Some         7       7
# Marked       7      21
# 确定Improved和Treatment两个数值是不是有相关性/是否独立。 
# Placebo(药物实验无效对照剂)和Treated是Treatment的参数
# None,Some和Marked是Improved的参数，交叉形成矩阵。
# 结果：
# Chi-squared test for given probabilities
# data:  counts
# X-squared = 13.055, df = 2, p-value = 0.001463
# p-value = 0.0014<0.05,拒绝H0,H1发生的可能性高
# ->相互不独立，Improved和Treatment之间相关。
counts_2 <- table(Arthritis$Sex, Arthritis$Treatment)
chisq.test(counts_2)
# Arthritis$Sex, Arthritis$Treatment的顺序不影响结果。
counts_2
#         Placebo Treated
# Female      32      27
# Male        11      14
# data:  counts_2
# X-squared = 0.38378, df = 1, p-value = 0.5356
# p-value = 0.5356>0.05, 不拒绝H0,H1发生的可能性低。
# ->相互独立，Sex和Treatment之间不相关。

# fisher检验
# fisher检验适合样本小的检验，精确度低于卡方检验。
# 原理：边界固定的列联表中行和列是互相独立的。
fisher.test(counts_2)
# p-value = 0.4763
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval:
#   0.5320442 4.3286798
# sample estimates:
#   odds ration 1.500984 
# 同样得到结论：相互独立，Sex和Treatment之间不相关。

# Conchran-Mantel-Haenszel检验
# 原理:两个名义变量在第三个变量每一层中都是条件独立的。
# 按照原理可知需要三个var
counts_3 = xtabs(~Treatment+Sex+Improved, 
                 data = Arthritis)
counts_4 = xtabs(~Treatment+Improved+Sex, 
                 data = Arthritis)
# xtabs可以结合多个变量
mantelhaen.test(counts_3)
# data:  counts_3
# Mantel-Haenszel X-squared = 2.0863, df = 1,
# p-value = 0.1486
# alternative hypothesis: true common odds ratio is not equal to 1
# 95 percent confidence interval:0.8566711 8.0070521
# sample estimates:
#   common odds ratio 2.619048 
mantelhaen.test(counts_4)
# 三个var的顺序会影响最后的结果、
# counts_3的检验是
# Treatment和Sex在Improved的每一水平下是否独立。
# 而counts_4的检验是
# Treatment和Improved在Sex的每一水平下是否独立。
# 简单说还是前两个var的独立性，只不过增加了条件。
