'=========================================================================================================
'˵��������ΪEViews����ļ�mento.prg�Ĵ���
'��չʾ�ڶ����С����ؿ���ģ�����ݰ�������Ҫ��������ġ�����ʽ����������ʵ�ַ���(:��
'���У�����'��ʼ���У�Ϊע���У�����ΪEViews�����С�
'=========================================================================================================

'���������ļ��������ļ���=mento����ҳ����=spend�����޽ṹ�����ڣ�������Ϊ10
wfcreate(wf=mento,page=spend)  u 10

'�����ⲿ���ݣ�·��Ϊd:\github\books\data\lab2-mento-demon.xlsx
import d:\github\books\data\lab2-mento-demon.xlsx

' �������Իع鷽�̶���eq_main
equation eq_m0.ls y c x

' ת��������
series x_upr=x                       '����X
series y_upr=y                       '����Y

'����������Ҫ����
scalar x_avr_scl=@mean(x_upr)       'X�ľ�ֵ(����)
scalar y_avr_scl=@mean(y_upr)       'Y�ľ�ֵ(����)
scalar n=@obs(x_upr)                '������n(����)

'�����ֵ����
series x_avr_ser=@mean(x_upr)       'X�ľ�ֵ(����)
series y_avr_ser=@mean(y_upr)       'Y�ľ�ֵ(����)

'����FF������group��ʽ��
series x_upr_sqr=x_upr^2              '����X^2
series y_upr_sqr=y_upr^2              '����Y^2
series xy_upr= x_upr*y_upr            '����XY
group ff_upr x_upr y_upr x_upr_sqr y_upr_sqr xy_upr     '����FF�����飨group��

'����ff���series��������group
series x_lwr=x_upr-x_avr_ser          '�������x
series y_lwr= y_upr-y_avr_ser         '�������y
series x_lwr_sqr= x_lwr ^2            '�������x^2
series y_lwr_sqr =y_lwr ^2            '�������y^2
series xy_lwr=x_lwr*y_lwr             '�������xy
group ff_lwr x_lwr y_lwr x_lwr_sqr y_lwr_sqr xy_lwr     '����ff��������飨group��

' ����ع�ϵ��scalar��b2��b1
scalar b2_hat = @sum(xy_lwr)/@sum(x_lwr_sqr)  'б��ϵ��
scalar b1_hat =y_avr_scl-b2_hat *x_avr_scl    '�ؾ�ϵ��

' ����ع�Ԥ��ֵ�������series
series y_upr_hat= b1_hat+ b2_hat*x_upr      '�ع�Ԥ��ֵ
series y_lwr_hat= y_upr_hat-y_avr_ser       '�ع�Ԥ������ֵ

' ����в�в�ƽ��series
series ei=y_upr-y_upr_hat                   '�в�����
series ei_sqr= ei^2                         '�в�ƽ������

' ����ع�������׼��scalar
scalar sgm_hat_sqr=@sum(ei_sqr)/(n-2)       '���̵Ļع�����
scalar sgm_hat= sgm_hat_sqr^0.5             '���̵Ļع�����׼��

' ����ع�ϵ������������ͱ�׼��scalar
scalar ss_b2_hat=sgm_hat_sqr/@sum(x_lwr_sqr)       'б��ϵ������������
scalar ss_b1_hat=ss_b2_hat*(@sum(x_upr_sqr)/n)     '�ؾ�ϵ������������
scalar s_b2_hat=ss_b2_hat^0.5                      'б��ϵ����������׼��
scalar s_b1_hat=ss_b1_hat^0.5                      '�ؾ�ϵ����������׼��

' ����ƽ���ͷֽⲢ�õ���Ӧ���ɶ�(����scalar)
scalar tss=@sum(y_lwr_sqr)     '��ƽ����TSS
scalar rss=@sum(ei_sqr)        '�в�ƽ����RSS
scalar ess=tss-rss             '�ع�ƽ����ESS
scalar df_tss=n-1              ' TSS�����ɶ�
scalar df_rss=n-2              ' RSS�����ɶ�
scalar df_ess=1                ' ESS�����ɶ�

' �������ϵ��r���ж�ϵ��(����scalar)
scalar r=@cor(x_upr,y_upr)      'X��Y���������ϵ��
scalar r_sqr=ess/tss            '�ع鷽�̵��ж�ϵ��

' �ع�ϵ����t����
scalar t_str_b2_hat =b2_hat/s_b2_hat  'б�ʵ�tͳ����
scalar t_str_b1_hat =b1_hat/s_b1_hat  '�ؾ��tͳ����
scalar t_value =@qtdist(0.975,8)      'a=0.05�µ�t���ֵ���Ҳ���ֵ��
scalar t_value2 =@qtdist(0.025,8)     'a=0.05�µ�t���ֵ����ฺֵ��

' �ع鷽�̵�F����
scalar f_str =(ess/df_ess)/(rss/df_rss)   '�ع鷽�̵�Fͳ����ֵ
scalar f_value=@qfdist(0.95,df_ess,df_rss)       'a=0.05�µ�t���ֵ���Ҳ��ֵ��
scalar f_value2=@qfdist(0.05,df_ess,df_rss)      'a=0.05�µ�t���ֵ�����Сֵ��

' ����������Ԥ��(����scalar)
scalar x0 =280                                                                 '������Xֵ
scalar y0_hat =b1_hat+b2_hat*x0                                                '���������Yֵ
scalar s_y0h=(sgm_hat_sqr*(1/n+(x0-x_avr_scl)^2/@sum(x_lwr_sqr)))^0.5          '��ֵԤ���������׼��
scalar s_mns = (sgm_hat_sqr*(1+1/n+(x0-x_avr_scl)^2/@sum(x_lwr_sqr) ))^0.5        '��ֵԤ���������׼��
scalar y_exp_lft= y0_hat-t_value*s_y0h                                         '��ֵԤ���������������ֵ
scalar y_exp_rht= y0_hat+t_value*s_y0h                                         '��ֵԤ�������������ҽ�ֵ
scalar y_ind_lft= y0_hat-t_value*s_mns                                         '��ֵԤ���������������ֵ
scalar y_ind_rht= y0_hat+t_value*s_mns                                         '��ֵԤ�������������ҽ�ֵ

' ===========================================================================


