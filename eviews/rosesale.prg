'=========================================================================================================
'说明：以下为EViews编程文件rosesale.prg的代码
'将展示第三章中“英国家庭食物支出案例”主要分析步骤的“批量式命令驱动”实现方法(:：
'其中，符号'起始的行，为注释行，其他为EViews命令行。
'=========================================================================================================

'创建工作文件（工作文件名=rose，子页命名=sale），无结构无日期，样本数为16
wfcreate(wf=rose,page=sale)  u 16

'导入外部数据，路径为d:\github\books\data\Lab3-family-spends.xlsx
import d:\github\books\data\lab4-rose-demand-origin.xlsx

'生成线性回归模型的方程对象
equation eq_main.ls log(q) c log(x2) log(x3) log(x4) x5    '对数模型

'构造几个重要变量对象
scalar n=@obs(x2)                '样本数n(标量)
series cst=1                     '新建元素全为1的序列对象（用于构造矩阵X）
group xg cst log(x2) log(x3) log(x4) x5    '构造为group，便于观察

'构造X矩阵和Y矩阵对象。
matrix x=xg                  '转换为X矩阵对象
matrix y=log(q)              '构造Y矩阵对象

'计算回归方程的回归系数向量
matrix xtx=@transpose(x)*x  '得到重要矩阵X'X
matrix xtxi=@inverse(xtx)   '得到重要矩阵(X'X)^(-1)
matrix xty=@transpose(x)*y  '得到重要矩阵X'y
matrix beta_hat=xtxi*xty    '得到回归系数矩阵

'计算回归误差方差及标准差
scalar sgm_hat_sqr=1/(n-5)*(@transpose(y)*y-@transpose(beta_hat)*xty)   '回归误差方差
scalar sgm_hat=@sqr(sgm_hat_sqr)                                         '回归误差标准差

'计算回归系数的方差协方差矩阵、系数的样本方差和标准差(列向量)
matrix s2_varcov_beta_hat=sgm_hat_sqr*xtxi               '回归系数的方差协方差矩阵
matrix s2_beta_hat=@getmaindiagonal(s2_varcov_beta_hat)  '回归系数的样本方差
matrix s_beta_hat=@sqr(s2_beta_hat)                      '回归系数的样本标准差

'进行平方和分解，计算TSS、ESS和RSS，以及各自的自由度（标量）
scalar mean_adj=16*(@mean(q))^2                           '均值修正值                             
scalar tss=@transpose(y)*y-mean_adj                       '总平方和TSS
scalar rss=@transpose(y)*y-@transpose(beta_hat)*xty       '剩余平方和RSS
scalar ess=@transpose(beta_hat)*xty-mean_adj              '回归平方和ESS
scalar df_tss=n-1                                                                                    ' TSS的自由度
scalar df_rss=n-5                                         'RSS的自由度
scalar df_ess=4                                           'ESS的自由度

'计算自变量的相关系数表格、回归方程的判定系数和调整判定系数
group varx x2 x3 x4 x5             ' 构建只含X的group
freeze(tab_cor) varx.cor     '把group的相关系数矩阵表视图保存为表格
scalar r2=ess/tss             '回归方程的判定系数
scalar r2_adj=1-(rss/df_rss)/(tss/df_tss)      '回归方程的调整判定系数

'对回归方程的回归系数进行显著性t检验
matrix t_str_beta_hat=@ediv(beta_hat,s_beta_hat)   '计算得到回归系数的样本t统计量
scalar t_value=@qtdist(0.975,df_rss)               '给定α=0.05 水平下的理论t值（右侧正值）

' 对回归方程的整体显著性进行F假设检验
scalar f_str=(ess/4)/(rss/11)                   '计算回归方程的样本F统计量
scalar t_value=@qfdist(0.95,df_ess,df_rss)      '计算给定 α=0.05 水平下的查表的理论F值

' 进行样本外的均值预测、个值预测，并计算置信区间
matrix(1,5) x0                                 '产生1行*5列的空矩阵
x0.fill(b=r) 1,log(20),log(4),log(4),200       '样本外X值为(x2=log(20), x3=log(4), x4=log(4), x5=200)
matrix y0_hat=x0*beta_hat                                          '样本外估计Y值
scalar s_y0h=@sqr(sgm_hat_sqr*x0*xtxi*@transpose(x0))              '均值预测的样本标准差
scalar s_y0h_mns_y0=@sqr(sgm_hat_sqr*(1+x0*xtxi*@transpose(x0)))   '个值预测的样本标准差
scalar y_exp_lft=y0_hat-t_value*s_y0h                              '均值预测的置信区间的左界值
scalar y_exp_rht=y0_hat+t_value*s_y0h                              '均值预测的置信区间的右界值
scalar y_ind_lft=y0_hat-t_value*s_y0h_mns_y0                       '个值预测的置信区间的左界值
scalar y_ind_rht=y0_hat+t_value*s_y0h_mns_y0                       '个值预测的置信区间的右界值


