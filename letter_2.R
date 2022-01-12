x = matrix(1:20, nrow = 5, ncol = 4, byrow = FALSE)
z = matrix(1:20, nrow = 5, ncol = 4, byrow = TRUE)
# byrow是是否按一行一行的形式填入数据
print(z)
# 1    2    3    4
# 5    6    7    8
# 9   10   11   12
# 13   14   15   16
# 17   18   19   20
z[2,]
z[, 2]
# 第二行[2,]， 第二列[, 2]， [行， 列]
z[2, c(1,4)]
z[3:5, 2]
rnames = c('apple', 'orgen', 'balana', 'corn', 'melon')
cnames = c('cat', 'dogs', 'fish', 'birds')
rownames(x) = rnames
colnames(x) = cnames
print(x)
# 给每行每列赋上名字。

# array

dim1 = c('A1', 'A2')
dim2 = c('B1', 'B2', 'B3')
dim3 = c('C1', 'C2', 'C3', 'C4')
dim4 = c('D1', 'D2', 'D3')
Z = array(1:72, c(2, 3, 4, 3),
          dimnames = list(dim1, dim2, dim3,dim4))
Z
# 名字要与个数对齐，个数相乘要等于n总数。
# 名字一定要是list, 要用list()来转译。
Z[1, 2, 3, ]
# 返回的是一维第一行，二维第二行，
# 三维第三行的所有四维数据。
# 可以验证，在总表里面找所有对应的A1, B2 ,C3交集处的值。

patientID = c(1, 2, 3, 4)
age = c(25, 34, 28, 52)
disbetes = c('Type1', 'Type2', 'Type1', 'Type1')
status = c('Poor', 'Improved', 'Excellent', 'Poor')
patientdata = data.frame(patientID, age, disbetes, 
                         status)
patientdata
swim = read.csv("http://www.macalester.edu/~kaplan/ISM/datasets/swim100m.csv")
# csv读出来就是arrey的格式。
patientdata[1:2]
# 前两列
patientdata[1, 1:2]
# 第一行前两列。
# 也可以patientdata[c(1, 3), 1:2]

attach(mtcars)
# mtcars是R内置的一个数据。
par(cex.main = 5, cex.sub = 5)
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
hist(wt)
hist(mpg)
hist(disp)
detach()
# layout(matrix())：
# 双1代表第一副图占据2个位置，之后是第二、三副图。
# (c(1,1,2,0,3,4), 3, 2,byrow = TRUE)：
# 首先看c()后面的，3， 2代表有三行两列。
# byrow代表是按从左到右，然后从上到下的顺序。
# c()里面的0，代表位置所在的那一格为空。
# 非0数字代表绘制图形的顺序，相同数字代表占位符。
# ”0”代表空缺，不绘制图形。

dat = data.frame(swim, x)
# 用data.frame要求行数相等，不然会报错：
#  参数值意味着不同的行数: 62, 5
mylist = list(patientdata, swim, x)
mylist
# list可以无条件的合并所有数组。
# 但是会以一个大list{}的形式拼起来，
# list里面的数组之间没有合并。
mylist[1]
# 只出现第一部分。编号按list()里面的顺序。
mylist[[1]][1:2]
mylist[[1]][c(1, 2),]
mylist[[3]][c(1, 2), 3]
# 如果要对第一部分再取值（读出第一部分中的前两列）：
# [[1]][1:2]，第一个要双括号，不然会取NULL.

# Graphical parameters(par)
par(mfrow=c(2,2), adj = 0.8, mar = c(4, 4, 1, 1))
# 设定网格，这样一次可以画4个图（2行*2列）
# 这样可以同时有多个图在一个界面上.
# agj决定标题、x/ylab等字符文本的位置，默认0.5，居中。
# 数值向量c(bottom, left, top, right)
# 控制每一幅图四周的距离，只可用于par( )，以行数为单位.
plot(rnorm(50),pch=17)
title(main = 'normalization', cex.main = 5, font.main = 5)

plot(rnorm(20),type="o", lty = 6)
title(main = 'normalization', cex.main = 5, font.main = 5)
title(xlab = 'x', ylab = 'y', sub = 'new', cex.sub = 2)
axis(1, 0:20, 0:20, line = 2, col.ticks = 2, 
     cex.axis = 3, col = 2)
axis(2, -2:2, -2:2, tcl = -3, las=2.8)
# 一整套包括两个title:设置sub,main,x/ylab
# 两个axis:设置x轴和y轴刻度。注意：
# (side, at, value):(位置，从多少到多少，从多少到多少)

plot(rnorm(100),cex = 5)
title(xlab = 'x', ylab = 'y', font.lab = 4, cex.lab = 2)
plot(rnorm(200),lwd=50, type = 'l')
title(xlab = 'x', ylab = 'y', col.lab = 2, 
      font.lab = 4, cex.lab = 2)
# 关于绘图参数：
# pch是点的形状, type代表线的形状，('l', 'o') 
# cex代表点的大小（对线没用），默认为1，0.5就是一半大。
# 先有type, lty才有效果。
# lty代表具体线的形式，lty默认为1， 有1-6种形式。
# lwd是线的粗细(对点没有用)。

# 关于title:
# title(main =)加总标题, cex.main, font.main, col.main.
# cex是字号大小，font是字体，col是颜色。后面都跟数字。
# 书写方式是先写col再写main/sub/label
# sub是副标题，在图下面，main在图上面。
# main和sub一样是一张图下面配一句来添加的。
# y/xlab设定坐标轴标签，cex/font/col.设定大小、字体、颜色。

# axis:
# side表示坐标轴位置，取值1、2、3、4分别代表下、左、上、右
# at表示刻度线及刻度值所在位置
# labels表示刻度值
# las表示坐标刻度值文字方向，
# las=0表示文字方向与坐标轴平行，1表示始终为水平方向，
# 2表示与坐标轴垂直，3表示终为垂直方向。
# col是标尺的颜色，col.axis是数值的颜色。
# col.ticks表示坐标轴刻度线的颜色
# axis也是紧跟前一个plot。
# tcl默认值为-0.5，数值表示刻度线长度
# 负值表示刻度线朝外，正值朝里
# cex.axis表示坐标轴刻度值的字号大小，
# font.axis表示坐标轴刻度值的字体，
# font=1表示正体，2表示黑体，3表示斜体，4表示黑斜体。
# legend是图例