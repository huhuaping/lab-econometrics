'=========================================================================================================
'说明：以下为EViews编程文件my_first_program.prg的代码
'将展示第一章中“宏观经济发展案例”主要分析步骤的“批量式命令驱动”实现方法(:：
'其中，符号'起始的行，为注释行，其他为EViews命令行。
'=========================================================================================================

'创建工作文件（工作文件名=development，子页命名=gdp），季度数据，起始于1980年1季度，结束于2012年1季度
wfcreate(wf=development,page=gdp)  q  1980/1 2012/1

'导入外部数据，路径为d:\github\books\data\lab1-basic-development.xlsx
import d:\github\books\data\lab1-basic-development.xlsx

'构造组对象，名为group_vars，由序列gdp; pce; investment; government构成
group group_vars gdp pce investment government

'得到表格（表格命名为tab_descriptive）<----组对象group_vars.进行描述性统计分析（Descriptive statistics）
freeze(tab_descriptive) group_vars.stats

'进行绘图，图形名为graph_line.线图（堆栈图）<----对组对象group_vars
graph graph_line.line(s)  group_vars

'构建方程，给方程命名为eq_main.执行线性回归，线性方程为
'log(gdp)~ c+ log(pce)+ log(investment)+ log(government)
equation eq_main.ls log(gdp) c log(pce) log(investment) log(government)

'构造向量（2个元素），向量命名为vec_fitness(第1个元素)=提取方程eq_main.的判定系数R^2
'向量vec_fitness(第2个元素)=提取方程eq_main.的调整判定系数\bar{R}^2
vector(2) vec_fitness(1)=eq_main.@r2
vec_fitness(2)=eq_main.@rbar2

'构造标量，标量命名为t_value=利用函数@qtdist(p,f)计算理论t值（a=0.05，f=125）
scalar t_value=@qtdist(0.975,125)

'=========================================================================================================

' 以下三个EViews命令行，将简要展示EViews编程的语法结构
' ===========================================================================
' 命令行1：生成线性回归方程对象eq_main
equation eq_main1.ls gdp c investment pce government

' 命令行2：生成并打开线性回归方程对象
show eq_main.ls gdp c investment pce government

' 命令行3：生成线性回归方程对象eq_main，并将回归报告保存为表格对象tab_output
freeze(tab_output) eq_main.ls gdp c investment pce government
' ===========================================================================

