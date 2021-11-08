
#————————————————导入数据————————————————————————#

library(tidyverse)

basic <- tibble(Y=c(65,77,89,101,113,125,137,149,161,173), X=seq(80,260, by=20))


options(digits=4)
students<-read.csv(file="c:/r material/class demo/1 econometrics/chapter2 excise/students list 2017 spring.csv")
students<-as.data.frame(students)
id<-students$id
name<-students$name
long<-length(name)-1                  #学生总人数，记得减掉1行空白行
#________产生数据X-Y，u~N(0,3),a=25,b=0.5__________#
generate.Y<-function(m){
  table.XY<-data.frame(X)
  for (i in 1:m){i
    u_n<-rnorm(n=10,mean=0,sd=3)
    u_n<-round(u_n,2)                     #保留两位小数
    Y.data<-25+0.5*X + u_n
    Y.data<-as.data.frame(Y.data)
    table.XY<-cbind(table.XY,Y.data)
    i=i+1
  }
  
  k<-c(1:m)
  Y_label<-paste("Ydata",k,sep="")
  other_name<-c("X")
  all.names<-c(other_name,Y_label)          
  dimnames(table.XY)<-list(1:10,all.names)                  #给出数据表的列变量名
  table_student<-t(students)                                #得到学生信息表
  table_student<-as.data.frame(table_student) 
  names(table_student)<-all.names                      #给出学生信息表的列变量名  
  table.data1<-cbind(t(table_student),t(table.XY))           #合并数据和学生信息
  table.data<-t(table.data1) 
  write.csv(table.data,file="c:/r material/class demo/1 econometrics/chapter2 excise/table_2017.csv")
  write.csv(table.XY,file="c:/r material/class demo/1 econometrics/chapter2 excise/table_Y2017.csv")
}

generate.Y(long)


#————————————计算并得到表格—————————————————#
generate.table<-function(X,Y) {X.square<-X^2; #计算X平方
Y.square<-Y^2; #计算Y平方
X.square<-X^2; #计算大X平方
x<-X-mean(X);  #计算X离差
y<-Y-mean(Y);  #计算Y离差
x.square<-x^2; #计算X离差平方
y.square<-y^2;#计算Y离差平方
xy<-x*y;      #计算X与Y的离差乘积
SSX<-sum(x^2); #计算SSX
SSY<-sum(y^2); #计算SSY——TSS
SSXY<-sum(xy); #计算SSXY
b2<-SSXY/SSX;             #求出回归参数b2                     
b1<-mean(Y)-b2*mean(X);   #求出回归参数b1
Y_reg<-b1+b2*X;         #得出回归后的Y值
ESS_y<-Y_reg-mean(Y)    #得出回归值与Y均值的离差
ei<-Y-Y_reg;            #得出残差值
TSS<-sum(y^2)           #计算TSS
RSS<-sum(ei^2)          #计算RSS
ESS<-sum(ESS_y^2)       #计算ESS
r2<-ESS/TSS             #计算r2

#参数标准差
ss_u<-RSS/(length(X)-2)   #计算干扰项u的方差σ的近似替代值σ^
Se_b2<-(ss_u/SSX)^0.5     #计算参数b2的样本标准差Se_β2
Se_b1<-((ss_u*sum(X.square))/(length(X)*SSX))^0.5   #计算参数b1的样本标准差Se_β1


#生成表1：计算过程值
table1<-data.frame(X,Y,X*Y,X^2,Y^2,x,y,xy,x^2,y^2,Y_reg,ei,ei^2);                     
names(table1)<-c("X","Y","XY","X2","Y2","x","y","xy","x2","y2","Y_reg","ei","ei2");
#生成表2：合计
table2<-data.frame(sum(X),sum(Y),sum(X*Y),sum(X^2),sum(Y^2), sum(x),sum(y),sum(x*y),sum(x^2),sum(y^2),sum(Y_reg),sum(ei),sum(ei^2));
names(table2)<-c("X","Y","XY","X2","Y2","x","y","xy","x2","y2","Y_reg","ei","ei2");
#生成表3：系数、均方差分解、r2
table3.1<-data.frame(list=c(β1=b1,β2=b2,TSS=TSS,RSS=RSS,ESS=ESS,r2=r2,Se_β2=Se_b2,Se_β1=Se_b1));
table3.2<-data.frame(array(data=(rep(x=" ",92)),dim=c(8,12)),row.names=c("b1.(Intercept)","b2.X ","TSS","RSS","ESS","r2","Se_β2","Se_β1"));
table3<-cbind(table3.1,table3.2);
names(table3)<-c("X","Y","XY","X2","Y2","x","y","xy","x2","y2","Y_reg","ei","ei2"); 
#整合3个表格                             
table.agg<-rbind(table1,table2,table3);
} 


#————————根据产生的数据，得到全部分析结果————————————————————#

table.data<-read.csv(file="c:/r material/class demo/1 econometrics/chapter2 excise/table_Y2017.csv")
for (i in c(1:long)){n=i+2
result<-generate.table(table.data[,2],table.data[,n])
Y_name<-paste("Ydata",i,sep="")
file.name.p1<-paste("c:/r material/class demo/1 econometrics/chapter2 excise/result2017/result2017_",Y_name,sep="")
file.name.full<-paste(file.name.p1,".csv",sep="")
write.csv(result,file=file.name.full)
i=i+1
}

#————————————————画动态图————————————————————————————————————————————————————————————————————————

# 2.1 get the data --------------------------------------------------------


table.data<-read.csv(file="c:/r material/class demo/1 econometrics/chapter2 excise/table_Y2017.csv")                  #读取数据
students<-read.csv(file="c:/r material/class demo/1 econometrics/chapter2 excise//students list 2017 spring.csv")   #读取学生信息
students<-students[-1,]
id<-students$id
name<-students$name
class<-students$class
n<-length(name)
X<-table.data[,2]
Y<-as.data.frame(table.data[,3:(n+2)])

# 2.通过循环运算，得到四个变量
model_1<-lm(Y[,1]~X)
Y_reg<-model_1$fitted.values
intercept<-signif(model_1$coefficients[1],digits=4)
slope<-signif(model_1$coefficients[2],digits=4)
title<-paste("Ydata",1,sep="")
for (i in 2:n) { 
  model<-lm(Y[,i]~X)
  Y_reg1<-model$fitted.values
  Y_reg<-cbind(Y_reg,Y_reg1)
  intercept1<-signif(model$coefficients[1],digits=6)
  intercept<-rbind(intercept,intercept1)
  slope1<-signif(model$coefficients[2],digits=4)
  slope<-rbind(slope,slope1)
  title1<-paste("Ydata",i,sep="")
  title<-rbind(title,title1)  
}
color<-rainbow(long)

filename<-paste("c:\\r material\\class demo\\1 econometrics\\chapter2 excise\\result2017\\plot2017\\",title[1],".png",sep="")     #得到文件名的变量
for (i in 2:n){
  filename1<-paste("c:\\r material\\class demo\\1 econometrics\\chapter2 excise\\result2017\\plot2017\\",title[i],".png",sep="")
  filename<-rbind(filename,filename1)
}

filename.every<-paste("c:\\r material\\class demo\\1 econometrics\\chapter2 excise\\result2017\\plotevery2017\\",title[1],".png",sep="")     #得到文件名的变量
for (i in 2:n){
  filename.every1<-paste("c:\\r material\\class demo\\1 econometrics\\chapter2 excise\\result2017\\plotevery2017\\",title[i],".png",sep="")
  filename.every<-rbind(filename.every,filename.every1)
}
# 3.制作动画swf
# 3.1 方案1： 生成静态图片，再制作成动画（所有同学，逐步叠加回归结果）

for (i in 1:n){
  png(filename[i],width = 1024, height =768)
  plot(X,Y[,i],col=color[1],pch=20,                     #原始数据描点
       ylab="家庭支出 Y", xlab="家庭收入 X",       
       xaxt = "n",yaxt = "n",
       xlim=c(80,260),ylim=c(50,170))  
  #绘制SRF
  axis(1, seq(80,260,20), col.axis = "blue")
  axis(2, seq(50,170,20))
  lines(X,Y_reg[,1],col=color[1],lty=2)                      #绘制SRF
  #dev.off()
  name.string<-as.character(name[i])                     #给出姓名和学号
  id.string<-as.character(id[i])
  name.id<-paste(name.string,id.string)
  legend(75,173,paste(title[i],":",name.id),col="green",bg="grey",
         box.lty=0,text.col="blue")
  legend(140,65,paste("SRF1:Y_reg=",intercept[1],"+",slope[1],"Xi",sep=""),    #给出回归直线方程
         box.lty=0,bg="grey",text.col="blue") 
  for (k in 1:i){
    points(X,Y[,k],col=color[k],pch=20)      
    lines(X,Y_reg[,k],col=color[long-k],lty=2)                      #绘制SRF
    name.string<-as.character(name[k])                     #给出姓名和学号
    id.string<-as.character(id[k])
    name.id<-paste(name.string,id.string)
    legend(75,173,paste(title[k],":",name.id),col="green",bg="grey",
           box.lty=0,text.col="blue")
    legend(130,65,paste("SRF",k,":","Y_reg=",intercept[k],"+",slope[k],"Xi",sep=""),    #给出回归直线方程
           box.lty=0,bg="grey",text.col="blue")   
    lines(X,25+0.5*X,col=color[long-k],lty=1)                    #绘制PRF
    legend(172,85,expression("PRF: E(Y|X)"==25+0.5*X[i]),    #给出回归直线方程
           box.lty=0,bg="grey",text.col="blue")      
  }
  dev.off()
}  

# 3.2 方案2： 生成静态图片，再制作成动画（每一个同学，个人的回归结果）
for (i in 1:n){
  jpeg(filename.every[i],width = 1024, height =768)
  plot(X,Y[,i],col=color[i],pch=20,                     #原始数据描点
       ylab="家庭支出 Y", xlab="家庭收入 X",       
       xaxt = "n",yaxt = "n",
       xlim=c(80,260),ylim=c(50,170))  
  #绘制SRF
  axis(1, seq(80,260,20), col.axis = "blue")
  axis(2, seq(50,170,20))
  lines(X,Y_reg[,i],col=color[i],lty=2)                      #绘制SRF
  name.string<-as.character(name[i])                     #给出姓名和学号
  id.string<-as.character(id[i])
  name.id<-paste(name.string,id.string)
  legend(75,173,paste(title[i],":",name.id),col="green",bg="grey",
         box.lty=0,text.col="blue")
  legend(140,65,paste("SRF1:Y_reg=",intercept[i],"+",slope[i],"Xi",sep=""),    #给出回归直线方程
         box.lty=0,bg="grey",text.col="blue") 
  lines(X,25+0.5*X,col="blue",lty=1)                    #绘制PRF
  legend(172,85,expression("PRF: E(Y|X)"==25+0.5*X[i]),    #给出回归直线方程
         box.lty=0,bg="grey",text.col="blue")      
  dev.off()
}


# 2.3：直接动态图2 --------------------------------------------------------------
install.packages("animation")
require("animation")
getOption("device")
draw1<-function() {
  for (i in 1:n) {
    plot.new()
    plot(X,Y[,i],col=color[i],pch=20,                     #原始数据描点
         ylab="家庭支出 Y", xlab="家庭收入 X",       
         xaxt = "n",yaxt = "n",
         xlim=c(80,260),ylim=c(50,170)
    )  #绘制SRF
    axis(1, seq(80,260,20), col.axis = "blue")
    axis(2, seq(50,170,20))
    lines(X,Y_reg[,i],col=color[134-i],lty=2)                      #绘制SRF
    name.string<-as.character(name[i])                     #给出姓名和学号
    id.string<-as.character(id[i])
    name.id<-paste(name.string,id.string)
    legend(75,173,paste(title[i],":",name.id),col="green",bg="grey",
           box.lty=0,text.col="blue")
    legend(140,65,paste("SRF",i,":","Y_reg=",intercept[i],"+",slope[i],"Xi",sep=""),    #给出回归直线方程
           box.lty=0,bg="grey",text.col="blue") 
  }
}


drawall <- function() {
  for (i in 1:n){
    # i<-1                                                #给第一个同学额外绘图
    png(filename[i],width = 1024, height =768)
    plot(X,Y[,i],col=color[1],pch=20,                     #原始数据描点
         ylab="家庭支出 Y", xlab="家庭收入 X",       
         xaxt = "n",yaxt = "n",
         xlim=c(80,260),ylim=c(50,170))  
    #绘制SRF
    axis(1, seq(80,260,20), col.axis = "blue")
    axis(2, seq(50,170,20))
    lines(X,Y_reg[,1],col=color[133],lty=2)                      #绘制SRF
    name.string<-as.character(name[1])                     #给出姓名和学号
    id.string<-as.character(id[1])
    name.id<-paste(name.string,id.string)
    legend(75,173,paste(title[1],":",name.id),col="green",bg="grey",
           box.lty=0,text.col="blue")
    legend(140,65,paste("SRF1:Y_reg=",intercept[1],"+",slope[1],"Xi",sep=""),    #给出回归直线方程
           box.lty=0,bg="grey",text.col="blue") 
    for (k in 2:i){
      points(X,Y[,k],col=color[k],pch=20)      
      lines(X,Y_reg[,k],col=color[134-k],lty=2)                      #绘制SRF
      name.string<-as.character(name[k])                     #给出姓名和学号
      id.string<-as.character(id[k])
      name.id<-paste(name.string,id.string)
      legend(75,173,paste(title[k],":",name.id),col="green",bg="grey",
             box.lty=0,text.col="blue")
      legend(130,65,paste("SRF",k,":","Y_reg=",intercept[k],"+",slope[k],"Xi",sep=""),    #给出回归直线方程
             box.lty=0,bg="grey",text.col="blue")   
      lines(X,25+0.5*X,col=color[163-k],lty=1)                    #绘制PRF
      legend(172,85,expression("PRF: E(Y|X)"==25+0.5*X[i]),    #给出回归直线方程
             box.lty=0,bg="grey",text.col="blue")      
    }
    dev.off()
  } 
}
graphics.off()
#Please download and install the SWFTools before using this function: http://www.swftools.org
saveSWF(draw1(), interval=0.1, swf.name="line20161.swf")
saveSWF(drawall(), interval=0.1, swf.name="line2016all.swf")
#Please install FFmpeg or avconv first: http://ffmpeg.arrozcru.org/autobuilds/
saveVideo(draw1(), swf.name="line2016.mp4")
ani.options('ani.dev')
oopts = ani.options(ani.dev = "png", ani.type = "png")
saveGIF(draw1(), movie.name="line2016.mp4")

oopts = if (.Platform$OS.type == "windows") {
  ani.options(swftools = "C:/Program Files/swftools")
}
ani.options()
