#???????????????????????????????????????ݡ???????????????????????????????????????????????#
excise<-read.table(file="c:/r material/class demo/1 econometrics/chapter2 excise/basic data.txt",header=T,sep="")  
attach(excise)
names(excise)
fix(excise) 
plot(X,Y,pch=16)

options(digits=4)
students<-read.csv(file="c:/r material/class demo/1 econometrics/chapter2 excise/students list 2017 spring.csv")
students<-as.data.frame(students)
id<-students$id
name<-students$name
long<-length(name)-1                  #ѧ???????????ǵü???1?пհ???
#________????????X-Y??u~N(0,3),a=25,b=0.5__________#
generate.Y<-function(m){
  table.XY<-data.frame(X)
  for (i in 1:m){i
    u_n<-rnorm(n=10,mean=0,sd=3)
    u_n<-round(u_n,2)                     #????��λС??
    Y.data<-25+0.5*X + u_n
    Y.data<-as.data.frame(Y.data)
    table.XY<-cbind(table.XY,Y.data)
    i=i+1
  }

  k<-c(1:m)
  Y_label<-paste("Ydata",k,sep="")
  other_name<-c("X")
  all.names<-c(other_name,Y_label)          
  dimnames(table.XY)<-list(1:10,all.names)                  #???????ݱ????б?��??
  table_student<-t(students)                                #?õ?ѧ????Ϣ??
  table_student<-as.data.frame(table_student) 
  names(table_student)<-all.names                      #????ѧ????Ϣ?????б?��??  
  table.data1<-cbind(t(table_student),t(table.XY))           #?ϲ????ݺ?ѧ????Ϣ
  table.data<-t(table.data1) 
write.csv(table.data,file="c:/r material/class demo/1 econometrics/chapter2 excise/table_2017.csv")
write.csv(table.XY,file="c:/r material/class demo/1 econometrics/chapter2 excise/table_Y2017.csv")
}

generate.Y(long)


#???????????????????????????㲢?õ????񡪡???????????????????????????????#
generate.table<-function(X,Y) {X.square<-X^2; #????Xƽ??
  Y.square<-Y^2; #????Yƽ??
  X.square<-X^2; #??????Xƽ??
  x<-X-mean(X);  #????X????
  y<-Y-mean(Y);  #????Y????
  x.square<-x^2; #????X????ƽ??
  y.square<-y^2;#????Y????ƽ??
  xy<-x*y;      #????X??Y???????˻?
  SSX<-sum(x^2); #????SSX
  SSY<-sum(y^2); #????SSY????TSS
  SSXY<-sum(xy); #????SSXY
  b2<-SSXY/SSX;             #?????ع?????b2                     
  b1<-mean(Y)-b2*mean(X);   #?????ع?????b1
  Y_reg<-b1+b2*X;         #?ó??ع?????Yֵ
  ESS_y<-Y_reg-mean(Y)    #?ó??ع?ֵ??Y??ֵ??????
  ei<-Y-Y_reg;            #?ó??в?ֵ
  TSS<-sum(y^2)           #????TSS
  RSS<-sum(ei^2)          #????RSS
  ESS<-sum(ESS_y^2)       #????ESS
  r2<-ESS/TSS             #????r2
  
  #??????׼??
  ss_u<-RSS/(length(X)-2)   #??????????u?ķ????ҵĽ???????ֵ??^
  Se_b2<-(ss_u/SSX)^0.5     #????????b2????????׼??Se_??2
  Se_b1<-((ss_u*sum(X.square))/(length(X)*SSX))^0.5   #????????b1????????׼??Se_??1
  
                               
  #???ɱ?1??????????ֵ
  table1<-data.frame(X,Y,X*Y,X^2,Y^2,x,y,xy,x^2,y^2,Y_reg,ei,ei^2);                     
  names(table1)<-c("X","Y","XY","X2","Y2","x","y","xy","x2","y2","Y_reg","ei","ei2");
  #???ɱ?2???ϼ?
  table2<-data.frame(sum(X),sum(Y),sum(X*Y),sum(X^2),sum(Y^2), sum(x),sum(y),sum(x*y),sum(x^2),sum(y^2),sum(Y_reg),sum(ei),sum(ei^2));
  names(table2)<-c("X","Y","XY","X2","Y2","x","y","xy","x2","y2","Y_reg","ei","ei2");
  #???ɱ?3??ϵ???????????ֽ⡢r2
  table3.1<-data.frame(list=c(??1=b1,??2=b2,TSS=TSS,RSS=RSS,ESS=ESS,r2=r2,Se_??2=Se_b2,Se_??1=Se_b1));
  table3.2<-data.frame(array(data=(rep(x=" ",92)),dim=c(8,12)),row.names=c("b1.(Intercept)","b2.X ","TSS","RSS","ESS","r2","Se_??2","Se_??1"));
  table3<-cbind(table3.1,table3.2);
  names(table3)<-c("X","Y","XY","X2","Y2","x","y","xy","x2","y2","Y_reg","ei","ei2"); 
  #????3??????                             
  table.agg<-rbind(table1,table2,table3);
} 


#???????????????????ݲ????????ݣ??õ?ȫ??????????????????????????????????????????????????#

table.data<-read.csv(file="c:/r material/class demo/1 econometrics/chapter2 excise/table_Y2017.csv")
for (i in c(1:long)){n=i+2
  result<-generate.table(table.data[,2],table.data[,n])
  Y_name<-paste("Ydata",i,sep="")
  file.name.p1<-paste("c:/r material/class demo/1 econometrics/chapter2 excise/result2017/result2017_",Y_name,sep="")
  file.name.full<-paste(file.name.p1,".csv",sep="")
  write.csv(result,file=file.name.full)
  i=i+1
}

#????????????????????????????????????̬ͼ????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????

# 2.1 get the data --------------------------------------------------------


table.data<-read.csv(file="c:/r material/class demo/1 econometrics/chapter2 excise/table_Y2017.csv")                  #??ȡ????
students<-read.csv(file="c:/r material/class demo/1 econometrics/chapter2 excise//students list 2017 spring.csv")   #??ȡѧ????Ϣ
students<-students[-1,]
id<-students$id
name<-students$name
class<-students$class
n<-length(name)
X<-table.data[,2]
Y<-as.data.frame(table.data[,3:(n+2)])

# 2.ͨ??ѭ?????㣬?õ??ĸ???��
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

filename<-paste("c:\\r material\\class demo\\1 econometrics\\chapter2 excise\\result2017\\plot2017\\",title[1],".png",sep="")     #?õ??ļ????ı?��
for (i in 2:n){
  filename1<-paste("c:\\r material\\class demo\\1 econometrics\\chapter2 excise\\result2017\\plot2017\\",title[i],".png",sep="")
  filename<-rbind(filename,filename1)
}

filename.every<-paste("c:\\r material\\class demo\\1 econometrics\\chapter2 excise\\result2017\\plotevery2017\\",title[1],".png",sep="")     #?õ??ļ????ı?��
for (i in 2:n){
  filename.every1<-paste("c:\\r material\\class demo\\1 econometrics\\chapter2 excise\\result2017\\plotevery2017\\",title[i],".png",sep="")
  filename.every<-rbind(filename.every,filename.every1)
}
# 3.????????swf
# 3.1 ????1?? ???ɾ?̬ͼƬ?????????ɶ?????????ͬѧ???𲽵??ӻع???????

for (i in 1:n){
    png(filename[i],width = 1024, height =768)
    plot(X,Y[,i],col=color[1],pch=20,                     #ԭʼ????????
         ylab="??֧ͥ?? Y", xlab="??ͥ???? X",       
         xaxt = "n",yaxt = "n",
         xlim=c(80,260),ylim=c(50,170))  
    #????SRF
    axis(1, seq(80,260,20), col.axis = "blue")
    axis(2, seq(50,170,20))
    lines(X,Y_reg[,1],col=color[1],lty=2)                      #????SRF
    #dev.off()
    name.string<-as.character(name[i])                     #??????????ѧ??
    id.string<-as.character(id[i])
    name.id<-paste(name.string,id.string)
    legend(75,173,paste(title[i],":",name.id),col="green",bg="grey",
           box.lty=0,text.col="blue")
    legend(140,65,paste("SRF1:Y_reg=",intercept[1],"+",slope[1],"Xi",sep=""),    #?????ع?ֱ?߷???
           box.lty=0,bg="grey",text.col="blue") 
    for (k in 1:i){
      points(X,Y[,k],col=color[k],pch=20)      
      lines(X,Y_reg[,k],col=color[long-k],lty=2)                      #????SRF
      name.string<-as.character(name[k])                     #??????????ѧ??
      id.string<-as.character(id[k])
      name.id<-paste(name.string,id.string)
      legend(75,173,paste(title[k],":",name.id),col="green",bg="grey",
             box.lty=0,text.col="blue")
      legend(130,65,paste("SRF",k,":","Y_reg=",intercept[k],"+",slope[k],"Xi",sep=""),    #?????ع?ֱ?߷???
             box.lty=0,bg="grey",text.col="blue")   
      lines(X,25+0.5*X,col=color[long-k],lty=1)                    #????PRF
      legend(172,85,expression("PRF: E(Y|X)"==25+0.5*X[i]),    #?????ع?ֱ?߷???
             box.lty=0,bg="grey",text.col="blue")      
    }
    dev.off()
}  

# 3.2 ????2?? ???ɾ?̬ͼƬ?????????ɶ?????ÿһ??ͬѧ?????˵Ļع???????
for (i in 1:n){
  jpeg(filename.every[i],width = 1024, height =768)
  plot(X,Y[,i],col=color[i],pch=20,                     #ԭʼ????????
       ylab="??֧ͥ?? Y", xlab="??ͥ???? X",       
       xaxt = "n",yaxt = "n",
       xlim=c(80,260),ylim=c(50,170))  
  #????SRF
  axis(1, seq(80,260,20), col.axis = "blue")
  axis(2, seq(50,170,20))
  lines(X,Y_reg[,i],col=color[i],lty=2)                      #????SRF
  name.string<-as.character(name[i])                     #??????????ѧ??
  id.string<-as.character(id[i])
  name.id<-paste(name.string,id.string)
  legend(75,173,paste(title[i],":",name.id),col="green",bg="grey",
         box.lty=0,text.col="blue")
  legend(140,65,paste("SRF1:Y_reg=",intercept[i],"+",slope[i],"Xi",sep=""),    #?????ع?ֱ?߷???
         box.lty=0,bg="grey",text.col="blue") 
  lines(X,25+0.5*X,col="blue",lty=1)                    #????PRF
  legend(172,85,expression("PRF: E(Y|X)"==25+0.5*X[i]),    #?????ع?ֱ?߷???
         box.lty=0,bg="grey",text.col="blue")      
  dev.off()
}


# 2.3??ֱ?Ӷ?̬ͼ2 --------------------------------------------------------------
install.packages("animation")
require("animation")
getOption("device")
draw1<-function() {
  for (i in 1:n) {
    plot.new()
    plot(X,Y[,i],col=color[i],pch=20,                     #ԭʼ????????
       ylab="??֧ͥ?? Y", xlab="??ͥ???? X",       
       xaxt = "n",yaxt = "n",
       xlim=c(80,260),ylim=c(50,170)
  )  #????SRF
  axis(1, seq(80,260,20), col.axis = "blue")
  axis(2, seq(50,170,20))
  lines(X,Y_reg[,i],col=color[134-i],lty=2)                      #????SRF
  name.string<-as.character(name[i])                     #??????????ѧ??
  id.string<-as.character(id[i])
  name.id<-paste(name.string,id.string)
  legend(75,173,paste(title[i],":",name.id),col="green",bg="grey",
         box.lty=0,text.col="blue")
  legend(140,65,paste("SRF",i,":","Y_reg=",intercept[i],"+",slope[i],"Xi",sep=""),    #?????ع?ֱ?߷???
         box.lty=0,bg="grey",text.col="blue") 
   }
}


drawall <- function() {
  for (i in 1:n){
    # i<-1                                                #????һ??ͬѧ??????ͼ
    png(filename[i],width = 1024, height =768)
    plot(X,Y[,i],col=color[1],pch=20,                     #ԭʼ????????
         ylab="??֧ͥ?? Y", xlab="??ͥ???? X",       
         xaxt = "n",yaxt = "n",
         xlim=c(80,260),ylim=c(50,170))  
    #????SRF
    axis(1, seq(80,260,20), col.axis = "blue")
    axis(2, seq(50,170,20))
    lines(X,Y_reg[,1],col=color[133],lty=2)                      #????SRF
    name.string<-as.character(name[1])                     #??????????ѧ??
    id.string<-as.character(id[1])
    name.id<-paste(name.string,id.string)
    legend(75,173,paste(title[1],":",name.id),col="green",bg="grey",
           box.lty=0,text.col="blue")
    legend(140,65,paste("SRF1:Y_reg=",intercept[1],"+",slope[1],"Xi",sep=""),    #?????ع?ֱ?߷???
           box.lty=0,bg="grey",text.col="blue") 
    for (k in 2:i){
      points(X,Y[,k],col=color[k],pch=20)      
      lines(X,Y_reg[,k],col=color[134-k],lty=2)                      #????SRF
      name.string<-as.character(name[k])                     #??????????ѧ??
      id.string<-as.character(id[k])
      name.id<-paste(name.string,id.string)
      legend(75,173,paste(title[k],":",name.id),col="green",bg="grey",
             box.lty=0,text.col="blue")
      legend(130,65,paste("SRF",k,":","Y_reg=",intercept[k],"+",slope[k],"Xi",sep=""),    #?????ع?ֱ?߷???
             box.lty=0,bg="grey",text.col="blue")   
      lines(X,25+0.5*X,col=color[163-k],lty=1)                    #????PRF
      legend(172,85,expression("PRF: E(Y|X)"==25+0.5*X[i]),    #?????ع?ֱ?߷???
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
