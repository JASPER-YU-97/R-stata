# R语言使用ggplot2作图，所支持的地图数据对象主要包括两类
# sp： SpatialPolygonDataFrame
# sf:     Simple feature list column
# 这两株数据对象都可以通过读取shp数据获得
# 画map第一步:找到需要的地图底图的shp版本。

# SP空间数据对象是dataframe(数据描述层)和polygons(几何映射层)两个对象的组合
# SP将地理数据分割为两大块:描述层和映射层,
# 可以使用rgdal包的readOGR（)函数读取数据。
# 描述层:记录各地理区域的名称、ID、编号、简写、iOS编码等信息，
# 可以通过data@data来获取描述曾数据框。
# 映射层:是每一个行政区域的多边形边界点，
# 按照order排序,按照group分组,
# 多边形分界点信息是一个多层嵌套的list结构，
# 但我们可以通过fortity函数将其装换位数据框。

# SP数据再用ggplot2绘制时，
# 要分离描述层和几何映射层数据,并为两者指定连接的id,
# 如果要绑定自己的分析数据,就共需要合并两次数据。
# sf:将每一个行政区划所对应的几何边界点封装成一个list对象，
# 这条记录就像其他普通文本记录,被排列在对应行政区划描述单元中
# 用sf包st_read()函数导入的空间数据对象是一个整齐的数据结构，
# 这些行列中包括了描述层和几何多边形的边界点信息。
# SF对象我们只需要指定一次合并即可,将描述层和你的分析数据合并，
# sf::st_read()函数读取数据可得到SF数据对象,为data.frame对象类型。



library(rgdal)
setwd('F:/map_1/bou2_4p')
# 设定运行文件夹
x <- readOGR('bou2_4p.shp',stringsAsFactors=FALSE) 
plot(x)
y <- read.table('data.txt',fileEncoding = 'UTF-8',
                header = T)
x@data <- cbind(x@data, y)

# 读取shp底图
getColor = function(x, provname, provcol){
  f = function(x, y) ifelse(x %in% y, which(y == x), 0);
  colIndex = sapply(x@data$NAME, f, provname);
  col = c(provcol)[colIndex + 1];
  return(col);
}
provname =c ("北京市", "天津市", "河北省", "山西省", "内蒙古自治区","辽宁省", "吉林省", "黑龙江省", "上海市", "江苏省","浙江省", "安徽省", "福建省", "江西省", "山东省","河南省", "湖北省", "湖南省", "广东省","广西壮族自治区", "海南省", "重庆市", "四川省", "贵州省","云南省", "西藏自治区", "陕西省", "甘肃省", "青海省","宁夏回族自治区", "新疆维吾尔自治区", "台湾省","香港特别行政区");
pop=c(111,28,65,35,18,39,14,43,101,129,428,200,101,162,145,278,4586,277,311,78,46,165,142,12,70,1,63,26,6,12,14,8,10);
provcol = rgb(blue = 1 - pop/max(pop)/2, green = 1-pop/max(pop)/2, red = 0.5);
plot(x, col= getColor(x, provname, provcol), xlab = "", ylab = "");

