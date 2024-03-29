'=========================================================================================================
'说明：以下为EViews编程文件longley.prg的代码
'将展示第五章中“郎利就业数据案例”主要分析步骤的“批量式命令驱动”实现方法(:：
'其中，符号'起始的行，为注释行，其他为EViews命令行。
'=========================================================================================================

'创建工作文件（工作文件名=longley，子页命名=employee），无结构无日期，样本数为16
wfcreate(wf=longley,page=employee)  u 16

'导入外部数据，路径为d:\github\books\data\Lab5-longley-short-origin.xlsx
import d:\github\books\data\Lab5-longley-short-origin.xlsx

'生成线性回归模型的方程对象
equation eq_m0.ls Y c x1 x2 x3 x4 x5 x6   '回归方程
scalar r2_m0=eq_m0.@r2

'多重共线性诊断：计算自变量的相关系数表格和散点矩阵图
group varx x1 x2 x3 x4 x5 x6         '构建只含X的group
freeze(tab_cor) varx.cor             '把group的相关系数矩阵表视图保存为表格
graph scatter_matrix.scatmat varx    '绘制散点矩阵图

'多重共线性诊断：构建辅助回归方程
equation eq_a1.ls X1 c X2 X3 X4 X5 X6
equation eq_a2.ls X2 c X1 X3 X4 X5 X6
equation eq_a3.ls X3 c X1 X2 X4 X5 X6
equation eq_a4.ls X4 c X1 X2 X3 X5 X6
equation eq_a5.ls X5 c X1 X2 X3 X4 X6
equation eq_a6.ls X6 c X1 X2 X3 X4 X5 

'多重共线性诊断：辅助方程的判定系数比较
vector(6) r2
r2.fill(o=1) eq_a1.@r2   '提取辅助方程A1的判定系数
r2.fill(o=2) eq_a2.@r2   '提取辅助方程A2的判定系数
r2.fill(o=3) eq_a3.@r2   '提取辅助方程A3的判定系数
r2.fill(o=4) eq_a4.@r2   '提取辅助方程A4的判定系数
r2.fill(o=5) eq_a5.@r2   '提取辅助方程A5的判定系数
r2.fill(o=6) eq_a6.@r2   '提取辅助方程A6的判定系数

'多重共线性诊断：方差膨胀因子
vector(6) unit=1                   '为计算做准备
vector vif=@ediv(unit,(unit-r2))   '利用公式计算VIF
freeze(tab_vif) eq_m0.varinf       '直接冻结主方程的一个view（varinf）为VIF表格

'多重共线性诊断：容忍度
vector tol=unit-r2                 '利用公式计算TOL


'多重共线性诊断：回归系数方差分解法
freeze(tab_vcd) eq_m0.cvardecomp    '

'多重共线性矫正：经济学和实践观察法
series x_rgnp=x2/x1                            ' 构造新变量
equation eq_adj_man.ls y c I(x2/x1) x3 x4 x5      '回归新方程

'多重共线性矫正：逐步最小二乘回归法（后向逐步回归，p值标准为0.05）
equation eq_adj_step.stepls(back,btol=0.05) Y @ c X1 X2 X3 X4 X5 X6


