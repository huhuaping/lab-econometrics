'=========================================================================================================
'˵��������ΪEViews����ļ�foodexp.prg�Ĵ���
'��չʾ�������С�Ӣ����ͥʳ��֧����������Ҫ��������ġ�����ʽ����������ʵ�ַ���(:��
'���У�����'��ʼ���У�Ϊע���У�����ΪEViews�����С�
'=========================================================================================================

'���������ļ��������ļ���=food����ҳ����=expenditure�����޽ṹ�����ڣ�������Ϊ1519
wfcreate(wf=food,page=expenditure)  u 1519

'�����ⲿ���ݣ�·��Ϊd:\github\books\data\Lab3-family-spends.xlsx
import d:\github\books\data\Lab3-family-spends.xlsx

'����ɢ��ͼ
graph scatter_classic.scat x y
graph scatter_loglog.scat log(x) log(y)
graph scatter_reclog.scat 1/x log(y)


' ����7�����Իع�ģ�͵ķ��̶���
equation m1.ls y c x              '����ģ��m1
equation m2.ls y x                '��ԭ��ģ��m2
equation m3.ls log(y) c log(x)    '˫����ģ��m3
equation m4.ls log(y) c x         '��������ģ��m4
equation m5.ls y c log(x)         '���Զ���ģ��m5
equation m6.ls y c 1/x            '����ģ��m6
equation m7.ls log(y) c 1/x       '��������ģ��m7


'�õ�7�����Իع�ģ�͵Ļع�ϵ��(����)
vector(2) coef1=m1.@coefs
vector(1) coef2=m2.@coefs
vector(2) coef3=m3.@coefs
vector(2) coef4=m4.@coefs
vector(2) coef5=m5.@coefs
vector(2) coef6=m6.@coefs
vector(2) coef7=m7.@coefs

' �����������ݵ�(X0, Y0)
scalar x0=100
scalar Y0=30

'�õ�7�����Իع�ģ����y��x��(X0,Y0)����б��
scalar slp1= coef1(2)
scalar slp2= coef2(1)
scalar slp3= coef3(2)*x0/y0
scalar slp4= coef4(2)*y0
scalar slp5= coef5(2)/x0
scalar slp6= coef6(2)/x0^2
scalar slp7= coef7(2)*y0/x0^2

'�õ�7�����Իع�ģ����y��x��(X0,Y0)���ĵ㵯��
scalar els_point1= coef1(2)*x0/y0
scalar els_point2= coef2(1)*x0/y0
scalar els_point3= coef3(2)
scalar els_point4= coef4(2)*x0
scalar els_point5= coef5(2)/y0
scalar els_point6= -coef6(2)/(x0*y0)
scalar els_point7= coef7(2)/x0

'�õ�7�����Իع�ģ����y��x��ƽ������
scalar els_avr1= coef1(2)*@mean(x)/@mean(y)
scalar els_avr2= coef2(1)*@mean(x)/@mean(y)
scalar els_avr3= coef3(2)
scalar els_avr4= coef4(2)*@mean(x)
scalar els_avr5= coef5(2)/@mean(y)
scalar els_avr6= -coef6(2)/(@mean(x)*@mean(y))
scalar els_avr7= coef7(2)/@mean(x)

' �����н�������ɱ��
table(8,4) tab_show      '����8��4�еı��

tab_show(1,1)="model"    '��1�д��ģ�ʹ���
tab_show(2,1)="m1"
tab_show(3,1)="m2"
tab_show(4,1)="m3"
tab_show(5,1)="m4"
tab_show(6,1)="m5"
tab_show(7,1)="m6"
tab_show(8,1)="m7"

tab_show(1,2)="slop"     '��2�д��Y��X�ĵ�б��
tab_show(2,2)=slp1
tab_show(3,2)=slp2
tab_show(4,2)=slp3
tab_show(5,2)=slp4
tab_show(6,2)=slp5
tab_show(7,2)=slp6
tab_show(8,2)=slp7


tab_show(1,3)="els_point"   '��3�д��Y��X�ĵ㵯��
tab_show(2,3)=els_point1
tab_show(3,3)=els_point2
tab_show(4,3)=els_point3
tab_show(5,3)=els_point4
tab_show(6,3)=els_point5
tab_show(7,3)=els_point6
tab_show(8,3)=els_point7

tab_show(1,4)="els_avr"     '��4�д��Y��X��ƽ������
tab_show(2,4)=els_avr1
tab_show(3,4)=els_avr2
tab_show(4,4)=els_avr3
tab_show(5,4)=els_avr4
tab_show(6,4)=els_avr5
tab_show(7,4)=els_avr6
tab_show(8,4)=els_avr7


' ===========================================================================


