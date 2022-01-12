# https://cloud.r-project.org -> package -> CRAN Task Views 包分类
# install.packages('quantmod')安装语句。
# 不要在rstudio里面用安装语句，在R自带的对话框里面输入。
# 第一次输入会让你选择镜像站点，选china就行了。
# 会出框显示下载和安装进度。最后显示：
# 下载的二进制程序包在
# C:\Users\PC\AppData\Local\Temp\RtmpqOEo0i\downloaded_packages里
# .libPaths()显示包安装的目录："F:/R-4.0.5/library"，
# library()显示安装过的包，会以单独的显视窗的形式。
# R基础包会在启动时加载进来
# 安装出了问题：Permission denied，删除包文件夹，然后重装。
# 包的载入有限制，需要把相关的包都安装完以后才能够顺利导入。
# 不然就会报错：载入需要的程辑包：xts，载入需要的程辑包：zoo
# 载入语句：library(quantmod)

length(1:60*2-1)
# 1:60是形成1到60的整数列共60个整数
# *2-1是指所有数列里面的数乘以2，然后再-1.
# length看数组里面的数字个数。
mode(1:60)
# "numeric"数字
a = c(1, 2, 3, 5, 9)
mode(a)
# 显示模型也是数字"numeric"
# c()函数: 将括号中的元素连接起来.
# paste()函数: 连接括号中的元素
y = c( 1, 2, 7, 12, 100)
x = c(1, 2)
z = c('z', 1, 2, 5, 10)
mode(y)
mode(z)
# 只要函数里面存在str字符串，输出的结果就是字符串。"character"
a[0: 5]
a[-3]
a[-(1:4)]
# 和python的切片不一样，R的切片是从1开始，所以[1: 5]和[0: 5]是一样的结果。
# a[5]是9，在python里面是a[4]是9.
# a[-3]是指除了第3个之外的所有数
# a[-(1:4)]是指除了第一个到第4个之外的所有数，注意，一定要打括号，不然会报错：
# Error in a[-1:4] : 只有负下标里才能有零
a[1, 2, 3]
# 会报错：量度数目不对
a[c(1, 2, 6)]
# 这个是对的，因为用c函数将数字向量化了。
# 但是会出现NA，因为a里面只有5个数，去读第6个数就只能够读出NA（空值）。
b = c(1:60)
b[b<20]
b[20<b & b<40]
b[a[5]]


m = rbind(a, y)
n = rbind(a, z)
q= rbind(a, x)
m
n
# rbind函数是将括号里面的数组组成数列，数字和字符串都可以成为构成部分。
# （row bind)所以组成的数组是按横行的方式组合，cbind（crow bind)按竖列的方式组合。
# 因为z的形式是字符串，所以组成的n的形式也是字符串。
# 里面每个数字都从数字形式变成了带引号的字符串的形式。
# 如果组成数组的a, x的length不一致，就会报错：
# number of columns of result is not a multiple of vector length (arg 2)
sum(m)
sum(n)
prod(m)
# prod是连乘，就是把m数组里面的所有数相乘得最后的乘积。
# sum可以针对单个的c函数，也可以针对组成的数组。
# n没有办法做任何统计学里面的计算，因为n是字符串的形式，如果使用会报错：
# Error in sum(n) : 'type'(character)参数不对
sum(X)
# 会报错，因为R里面大小写是区分开的。
var(m)
sd(m)
# 方差，标准差，看离散程度，python里面的标准差是std.

# 三合一：矩阵，向量，数列。
v = seq(5, 121, by = 1.5, length = 10)
v
# 取等差数列，从5开始，一直到最靠近121的一个间隔小于1.5的数列。（所以121可以不在数列里面。）
# by默认为1。length设定长度，从5到121取10个数，反向求出by，从而确定等差数列的内容。
# 如果同时存在by和length就会报错：太多参数。
letters[22:29]
LETTERS[23:26]
# 分别是26个小写字母和大写字母，超过就为NA空值。
which.max(v)
v[which.max(v)]
which(v < 10)
which(v == 6.5)
# 返回下标数, 就是第几个数是最大的，which后面可以接max和min,以及（v>/v<).
# which.max只会返回第一个最大值的位置信息。
# 所以这方法只适用于有单个最大值或者数组内元素不重复的。
v_1 = rev(v)
v_1
# 将数列的排列方式反过来，从大到小排。

mx = matrix(v[1:70], nrow = 10, ncol = 7, byrow = TRUE)
mx
# 报错：数据长度[78]不是矩阵行数[10]的整倍,裁剪一下数据就可以了。
# nrow(number of row), ncol(number of columns), byrow就是竖排。
t = t(mx)
t
# t就是转秩，行变列，列变行。
2*mx
mx+mx
mx-mx
# 矩阵可以相加\减,也可以乘常数。
mx%*%mx
# 矩阵的对应项相乘，不是矩阵的乘法，矩阵乘法应该是行乘列相加得所在位置的数，然后组成矩阵。
diag = diag(mx)
diag
# 返回矩阵对角线的数值。
mode(diag)#数值"numeric"
dd = diag(diag(mx))
dd
# 对单列矩阵可以重新构成一个除对角线有值，其他都是0的对角线数列。
diag(3)
# [,1] [,2] [,3]
# [1,]    1    0    0
# [2,]    0    1    0
# [3,]    0    0    1
# 形成3阶单位矩阵，对角线上都是1。

r = matrix(rnorm(16), 4, 4)
r
# 随机正态分布的数rnorm(random normaliza)
solve(r)
# 求逆矩阵就是r-1
b = c(1:4)
solve(b, r)
# 可以用方程组来理解，就相当于解一个4元1次方程组。
# r矩阵的每一个行相当于是一条4元1次方程组，行里面每个数相当于x,y,z,l四个变量的参数。
# 正好是四个式子解4元1次方程组，b一定要是