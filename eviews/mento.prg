'=========================================================================================================
'说明：以下为EViews编程文件mento.prg的代码
'将展示第二章中“蒙特卡洛模拟数据案例”主要分析步骤的“批量式命令驱动”实现方法(:：
'其中，符号'起始的行，为注释行，其他为EViews命令行。
'=========================================================================================================

'创建工作文件（工作文件名=mento，子页命名=spend），无结构无日期，样本数为10
wfcreate(wf=mento,page=spend)  u 10

'导入外部数据，路径为d:\github\books\data\lab2-mento-demon.xlsx
import d:\github\books\data\lab2-mento-demon.xlsx

' 生成线性回归方程对象eq_main
equation eq_m0.ls y c x

' 转换变量名
series x_upr=x                       '序列X
series y_upr=y                       '序列Y

'计算三个重要标量
scalar x_avr_scl=@mean(x_upr)       'X的均值(标量)
scalar y_avr_scl=@mean(y_upr)       'Y的均值(标量)
scalar n=@obs(x_upr)                '样本数n(标量)

'计算均值序列
series x_avr_ser=@mean(x_upr)       'X的均值(序列)
series y_avr_ser=@mean(y_upr)       'Y的均值(序列)

'计算FF，并以group形式打开
series x_upr_sqr=x_upr^2              '序列X^2
series y_upr_sqr=y_upr^2              '序列Y^2
series xy_upr= x_upr*y_upr            '序列XY
group ff_upr x_upr y_upr x_upr_sqr y_upr_sqr xy_upr     '构建FF序列组（group）

'计算ff离差series，并构建group
series x_lwr=x_upr-x_avr_ser          '离差序列x
series y_lwr= y_upr-y_avr_ser         '离差序列y
series x_lwr_sqr= x_lwr ^2            '离差序列x^2
series y_lwr_sqr =y_lwr ^2            '离差序列y^2
series xy_lwr=x_lwr*y_lwr             '离差序列xy
group ff_lwr x_lwr y_lwr x_lwr_sqr y_lwr_sqr xy_lwr     '构建ff离差序列组（group）

' 计算回归系数scalar：b2和b1
scalar b2_hat = @sum(xy_lwr)/@sum(x_lwr_sqr)  '斜率系数
scalar b1_hat =y_avr_scl-b2_hat *x_avr_scl    '截距系数

' 计算回归预测值及其离差series
series y_upr_hat= b1_hat+ b2_hat*x_upr      '回归预测值
series y_lwr_hat= y_upr_hat-y_avr_ser       '回归预测的离差值

' 计算残差及残差平方series
series ei=y_upr-y_upr_hat                   '残差序列
series ei_sqr= ei^2                         '残差平方序列

' 计算回归误差方差及其标准差scalar
scalar sgm_hat_sqr=@sum(ei_sqr)/(n-2)       '方程的回归误差方差
scalar sgm_hat= sgm_hat_sqr^0.5             '方程的回归误差标准差

' 计算回归系数的样本方差和标准差scalar
scalar ss_b2_hat=sgm_hat_sqr/@sum(x_lwr_sqr)       '斜率系数的样本方差
scalar ss_b1_hat=ss_b2_hat*(@sum(x_upr_sqr)/n)     '截距系数的样本方差
scalar s_b2_hat=ss_b2_hat^0.5                      '斜率系数的样本标准差
scalar s_b1_hat=ss_b1_hat^0.5                      '截距系数的样本标准差

' 进行平方和分解并得到相应自由度(标量scalar)
scalar tss=@sum(y_lwr_sqr)     '总平方和TSS
scalar rss=@sum(ei_sqr)        '残差平方和RSS
scalar ess=tss-rss             '回归平方和ESS
scalar df_tss=n-1              ' TSS的自由度
scalar df_rss=n-2              ' RSS的自由度
scalar df_ess=1                ' ESS的自由度

' 计算相关系数r和判定系数(标量scalar)
scalar r=@cor(x_upr,y_upr)      'X和Y变量的相关系数
scalar r_sqr=ess/tss            '回归方程的判定系数

' 回归系数的t检验
scalar t_str_b2_hat =b2_hat/s_b2_hat  '斜率的t统计量
scalar t_str_b1_hat =b1_hat/s_b1_hat  '截距的t统计量
scalar t_value =@qtdist(0.975,8)      'a=0.05下的t查表值（右侧正值）
scalar t_value2 =@qtdist(0.025,8)     'a=0.05下的t查表值（左侧负值）

' 回归方程的F检验
scalar f_str =(ess/df_ess)/(rss/df_rss)   '回归方程的F统计量值
scalar f_value=@qfdist(0.95,df_ess,df_rss)       'a=0.05下的t查表值（右侧大值）
scalar f_value2=@qfdist(0.05,df_ess,df_rss)      'a=0.05下的t查表值（左侧小值）

' 计算样本外预测(标量scalar)
scalar x0 =280                                                                 '样本外X值
scalar y0_hat =b1_hat+b2_hat*x0                                                '样本外估计Y值
scalar s_y0h=(sgm_hat_sqr*(1/n+(x0-x_avr_scl)^2/@sum(x_lwr_sqr)))^0.5          '均值预测的样本标准差
scalar s_mns = (sgm_hat_sqr*(1+1/n+(x0-x_avr_scl)^2/@sum(x_lwr_sqr) ))^0.5        '个值预测的样本标准差
scalar y_exp_lft= y0_hat-t_value*s_y0h                                         '均值预测的置信区间的左界值
scalar y_exp_rht= y0_hat+t_value*s_y0h                                         '均值预测的置信区间的右界值
scalar y_ind_lft= y0_hat-t_value*s_mns                                         '个值预测的置信区间的左界值
scalar y_ind_rht= y0_hat+t_value*s_mns                                         '个值预测的置信区间的右界值

' ===========================================================================


