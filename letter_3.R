#For-Loop
for( i in c(2, 9, 100, 500, 4))
{print(i)}
# for(初始化语句; 判断条件语句; 控制条件语句)
# {循环体语句;}
#While-Loop
i = 1
while(i <= 5){print(i)
  i=i+1}
# 再赋值语句不能和输出/执行语句，在一行，
# 之间不需要用‘,’隔开。
# while每次loop之后需要重新赋值，不然会直接出最后一个的值。
# 初始化语句；
# while(判断条件语句){循环体语句；
#   控制条件语句；}

#If statement
i = 1
if(i == 1)
{print("Hello World")}
# if语句后面不能加‘：’
# 同时if语句不能和后面的执行语句同一行。

# If-else statement
i = 2
if(i == 1)
  {print("Hello World!")}
if else(i == 2)
{print("Goodbye World!")}
else
{print("Goodbye")}
# 上面的会报错， 然后输出"Goodbye"
# 因为有了if以后，计算机会自然搜索else
# else if语句在R里面不存在。python里面有。

ifelse(i==2, "Hello World!", "Goodbye World!")
# 适合简单的ifelse语句，前面的是yes, 后面的是no.

i = 2
if(i == 1)
{print("Hello World!")}
else 
  {if (i == 2)
{print("Goodbye World!")}
else
{print("Goodbye")}}
# 下面的不会报错，因为else和if分开了，
# 构成了一个else{if else}的嵌套语法。

#switch
#switch(expression, cnoditions)
feelings = c("sad", "afraid")
for (i in feelings)
  {print(switch(i, happy = "I am glad you are happy",
                afraid = "There is nothing to fear",
                sad = "Cheer up",
                angry = "Calm down now"))}
# 相当于一一配对
i = 2
{print(switch(i, 1 = "Hello World!", 
              2 = "Goodbye World!",  3 = "Goodbye"))}
# switch相当于是字典查找。
# 所以里面存的key必须是名称\字母的形式，比方说'1'
# 不然会报错：意外的'=', i可以不是’‘的形式
# 输入后会自动转化为''的形式。

myfunction = function(x,a,b,c)
{return(a*sin(x)^2 - b*x + c)}
# 可以自己定义函数相当于python里面的def f(a, b):
# function()后面不需要：，只需要重启一行。
# return不能缺少()。return对于function()不是必须.
curve(myfunction(x,20,3,4),xlim=c(1,20))
# curve的第一个参数必须是函数, (带变量(x,y..)的式子)
# 可以提前用function(x)定义一个，也可以现写。
# xlim=c(1,20)x轴范围。
curve(sin(x)**2,xlim=c(1,20))
curve(exp(x))
# exp(x)就是指数函数。

myfeeling = function(x)
  {for (i in x)
  {print(switch(i,happy = "I am glad you are happy",
   afraid = "There is nothing to fear",
   sad = "Cheer up",angry = "Calm down now"))}}
# for (i in x)定义一个list作为可输入变量x。
feelings = c("sad", "afraid")
myfeeling(feelings)
