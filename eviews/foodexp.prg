'=========================================================================================================
'说明：以下为EViews编程文件foodexp.prg的代码
'将展示第三章中“英国家庭食物支出案例”主要分析步骤的“批量式命令驱动”实现方法(:：
'其中，符号'起始的行，为注释行，其他为EViews命令行。
'=========================================================================================================

'创建工作文件（工作文件名=food，子页命名=expenditure），无结构无日期，样本数为1519
wfcreate(wf=food,page=expenditure)  u 1519

'导入外部数据，路径为d:\github\books\data\Lab3-family-spends.xlsx
import d:\github\books\data\Lab3-family-spends.xlsx

'绘制散点图
graph scatter_classic.scat x y
graph scatter_loglog.scat log(x) log(y)
graph scatter_reclog.scat 1/x log(y)


' 生成7类线性回归模型的方程对象
equation m1.ls y c x              '经典模型m1
equation m2.ls y x                '过原点模型m2
equation m3.ls log(y) c log(x)    '双对数模型m3
equation m4.ls log(y) c x         '对数线性模型m4
equation m5.ls y c log(x)         '线性对数模型m5
equation m6.ls y c 1/x            '倒数模型m6
equation m7.ls log(y) c 1/x       '对数倒数模型m7


'得到7个线性回归模型的回归系数(向量)
vector(2) coef1=m1.@coefs
vector(1) coef2=m2.@coefs
vector(2) coef3=m3.@coefs
vector(2) coef4=m4.@coefs
vector(2) coef5=m5.@coefs
vector(2) coef6=m6.@coefs
vector(2) coef7=m7.@coefs

' 给定样本数据点(X0, Y0)
scalar x0=100
scalar Y0=30

'得到7个线性回归模型中y对x在(X0,Y0)处点斜率
scalar slp1= coef1(2)
scalar slp2= coef2(1)
scalar slp3= coef3(2)*x0/y0
scalar slp4= coef4(2)*y0
scalar slp5= coef5(2)/x0
scalar slp6= coef6(2)/x0^2
scalar slp7= coef7(2)*y0/x0^2

'得到7个线性回归模型中y对x在(X0,Y0)处的点弹性
scalar els_point1= coef1(2)*x0/y0
scalar els_point2= coef2(1)*x0/y0
scalar els_point3= coef3(2)
scalar els_point4= coef4(2)*x0
scalar els_point5= coef5(2)/y0
scalar els_point6= -coef6(2)/(x0*y0)
scalar els_point7= coef7(2)/x0

'得到7个线性回归模型中y对x的平均弹性
scalar els_avr1= coef1(2)*@mean(x)/@mean(y)
scalar els_avr2= coef2(1)*@mean(x)/@mean(y)
scalar els_avr3= coef3(2)
scalar els_avr4= coef4(2)*@mean(x)
scalar els_avr5= coef5(2)/@mean(y)
scalar els_avr6= -coef6(2)/(@mean(x)*@mean(y))
scalar els_avr7= coef7(2)/@mean(x)

' 把所有结果制作成表格
table(8,4) tab_show      '构造8行4列的表格

tab_show(1,1)="model"    '第1列存放模型代号
tab_show(2,1)="m1"
tab_show(3,1)="m2"
tab_show(4,1)="m3"
tab_show(5,1)="m4"
tab_show(6,1)="m5"
tab_show(7,1)="m6"
tab_show(8,1)="m7"

tab_show(1,2)="slop"     '第2列存放Y对X的点斜率
tab_show(2,2)=slp1
tab_show(3,2)=slp2
tab_show(4,2)=slp3
tab_show(5,2)=slp4
tab_show(6,2)=slp5
tab_show(7,2)=slp6
tab_show(8,2)=slp7


tab_show(1,3)="els_point"   '第3列存放Y对X的点弹性
tab_show(2,3)=els_point1
tab_show(3,3)=els_point2
tab_show(4,3)=els_point3
tab_show(5,3)=els_point4
tab_show(6,3)=els_point5
tab_show(7,3)=els_point6
tab_show(8,3)=els_point7

tab_show(1,4)="els_avr"     '第4列存放Y对X的平均弹性
tab_show(2,4)=els_avr1
tab_show(3,4)=els_avr2
tab_show(4,4)=els_avr3
tab_show(5,4)=els_avr4
tab_show(6,4)=els_avr5
tab_show(7,4)=els_avr6
tab_show(8,4)=els_avr7


' ===========================================================================


