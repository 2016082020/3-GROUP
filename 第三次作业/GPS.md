#  基于学生的轨迹行为的成绩预测分析
## 1. 摘要：
#### 1.1.搜集了来自达特茅斯学院48个学生，从2013年3月27日-2013年6月1日的GPS轨迹数据，通过对学生的GPS位置数据进行分析，研究影响学生成绩GPA的相关因素
#### 1.2 在对数据分析过程中，对学生在图书馆学习时间进行分析学习时间的长短对学生成绩的影响
#### 1.3 对学生的在校的密度进行统计，分析对学生GPA的影响的相关程度
## 2. 数据描述：
#### 2.1 数据来源：美国达特茅斯学院48位学生从2013.3.27-2013.6.1的GPS轨迹数据
#### 2.2 数据规模：将近20万条GPS位置数据
#### 2.3 数据字段：如下
![](https://github.com/cuit201608/3-GROUP/blob/master/%E7%AC%AC%E4%B8%89%E6%AC%A1%E4%BD%9C%E4%B8%9A/%E7%85%A7%E7%89%87/%E6%95%B0%E6%8D%AE%E7%BB%84%E6%88%90.JPG)
#### 2.4 数据组成：48位学生GPS数据部分截图
![](https://github.com/cuit201608/3-GROUP/blob/master/%E7%AC%AC%E4%B8%89%E6%AC%A1%E4%BD%9C%E4%B8%9A/%E7%85%A7%E7%89%87/%E5%AD%A6%E7%94%9F%E6%95%B0%E6%8D%AE.JPG)
## 3. 分析框架
#### 3.1 把学生的GPS数据筛选出从早上7点-晚上12点的时间数据和GPS数据经纬度进行提取，从中可以提取在图书馆和校外所处时间，预测在图书馆呆的时间越长成绩3### 可能会好一点，通过对时间统计分析其相关性，得出相关系数
![](https://github.com/cuit201608/3-GROUP/blob/master/%E7%AC%AC%E4%B8%89%E6%AC%A1%E4%BD%9C%E4%B8%9A/%E7%85%A7%E7%89%87/%E6%A8%A1%E5%9E%8B.JPG)
## 4. 数据分析
#### 4.1 下面是对学生的数据经行分析从中对图书馆学习数据数据进行提取，通过对其成绩GPA和平均每天学习时间进行绘图得到的显示结果(在绘图之前将GPA由低到高进行排序)
![](https://github.com/cuit201608/3-GROUP/blob/master/%E7%AC%AC%E4%B8%89%E6%AC%A1%E4%BD%9C%E4%B8%9A/%E7%85%A7%E7%89%87/%E5%8F%8Cy.JPG)
##### 4.1.1 进入图书馆相关度结果
![](https://github.com/cuit201608/3-GROUP/blob/master/%E7%AC%AC%E4%B8%89%E6%AC%A1%E4%BD%9C%E4%B8%9A/%E7%85%A7%E7%89%87/%E7%9B%B8%E5%85%B3%E5%BA%A6.JPG)
#### 4.2 对学生从周一-周五的早上6:00-22:00的次数进行统计，得到该行为的特征值
![](https://github.com/cuit201608/3-GROUP/blob/master/%E7%AC%AC%E4%B8%89%E6%AC%A1%E4%BD%9C%E4%B8%9A/%E7%85%A7%E7%89%87/%E6%A0%A1%E5%A4%96%E7%89%B9%E5%BE%81%E5%80%BC.JPG)
##### 4.2.1 是学生的校外收集次数和成绩GPA的关系
![](https://github.com/cuit201608/3-GROUP/blob/master/%E7%AC%AC%E4%B8%89%E6%AC%A1%E4%BD%9C%E4%B8%9A/%E7%85%A7%E7%89%87/%E6%A0%A1%E5%A4%96%E6%AC%A1%E6%95%B0%E5%9B%BE.JPG)
## 5.关键代码：
#### << 5.1 导入相关的程序包并且读入学生数据
    library(plotrix)
    
    info=48 #48个学生数据，存储数据框totalGPS
    p <- c(paste0('totalGPS ',1:info))
    loc=1
    for(i in 1:59){#59代表第48个学生的csv文件后缀名为59
      a='G:/gps/gps_u'
      c='.csv'
      b=i
      if(i<10){
        b=paste('0',i,sep = '')
      }
      name=paste(a,b,c,sep ='')
      if(file.exists(name)){#name是通过for循环创建的文件路径，循环创建48个存储学生数据的dataframe
        assign(p[loc],read.csv(name,header = TRUE,sep=','))
      }
      else{
        next()
      }
      loc=loc+1
    }
    #mg数据框是存储从学生的原始数据中筛选时间、GPS经纬度坐标数据
    mg <- c(paste0('mg',1:info))
    td <- c(paste0('td',1:info))
    t<-c(paste0('t',1:info))
    for(i in 1:length(p)){
      for(j in get(p[i])[1]){
        assign(t[i],as.POSIXct(j, origin="1970-01-01 00:00:00"))
      }
      assign(mg[i],cbind.data.frame(t=get(t[i]),get(p[i])[5],get(p[i])[6]))#将从原始数据筛选出来的数据进行合并
    }
    
    
    
 #### 5.2 筛选从早上7点--23点的数据并且在对每一个学生的数据进行操作过后打上学生标签
 
    zt<-c()#时间
    zla<-c()#纬度
    zlo<-c()#经度
    zuer<-c()#学生的身份名
    zcount=1
    for (i in 1:info) {#外层循环控制48个学生数据
      for(j in 1:nrow(get(mg[i])[1])){#内层循环控制每个学生数据
        #if(weekdays(as.Date(get(mg[i])[j,1]))=='星期日'||weekdays(as.Date(get(mg[i])[j,1]))=='星期六'){
          if((type.convert(strftime(get(mg[i])[j,1],format = "%H"))>=7&&type.convert(strftime(get(mg[i])[j,1],format = "%H"))<=23)){
            zt[zcount]=get(mg[i])[j,1]#选取时间
            zla[zcount]=get(mg[i])[j,2]#选取维度
            zlo[zcount]=get(mg[i])[j,3]#选取经度
            if(i<10){
              zuer[zcount]=paste('uer0',i,sep = '')
            }
            else{
              zuer[zcount]=paste('uer',i,sep = '')
              }
            zcount=zcount+1
          }else{next()}
      }
    }
    for(i in as.data.frame(zt)){
      zt1=as.POSIXct(i, origin="1970-01-01 00:00:00")
    }
    zdata1<-cbind.data.frame(zt1,zt,zla,zlo,zuer)#学生数据2-23点
    mfre<-as.data.frame(table(zdata1['zuer']))#统计每个学生的数据出现频率table函数
 #### 5.3 关键核心代码：
    #经纬度的最大值是通过谷歌地图进行查找图书馆的位置进行确定的
    mala=43.70597#最大维度
    mila=43.70462#最小维度
    malo=-72.28816#最大jin度
    milo=-72.28950#最小jin度
    
    #算法分析：
    sumtime=c(1:info)
    stime=0
    for(i in 1:nrow(mfre[1])){#控制每个学生的数据搜索，方便后续时间存储，循环48次
      for(j in 1:nrow(zdata1[1])){#控制zdata1数据的新循环搜索总次数约15万次
        if(zdata1[j,5]==mfre[i,1]){
          if(zdata1[j,3]>=mila&&zdata1[j,3]<=mala&&zdata1[j,4]>=milo&&zdata1[j,4]<=malo){#判断学生位置点是否在图书馆范围内
            if(length(zdata1[j-1,1])==1&&length(zdata1[j+1,1])==1){#如果在，并且当前位置和下一位置数据存在
              if(zdata1[j+1,2]-zdata1[j,2]>480&&zdata1[j+1,2]-zdata1[j,2]<720||zdata1[j+1,2]-zdata1[j,2]>1080&&zdata1[j+1,2]-zdata1[j,2]<1320){#取后者的时间-前者时间是否在10-20分钟的范围内，为了减小误差把差值范围增大
                stime=zdata1[j+1,2]-zdata1[j,2]
              }
              else{
                stime=1200#如果不满足条件就附一个固定的时间
              }
              # if(zdata1[j+1,2]-zdata1[j,2]>1080&&zdata1[j+1,2]-zdata1[j,2]<1260){
              #   stime=zdata1[j+1,2]-zdata1[j,2]
              # }
            }
            else{stime=1200}
            sumtime[i]=sumtime[i]+stime#将单个学生的时间进行单独的累加和
            stime=0
          }
          else{next()}
        }
        else{next()}
      }
    }
    
    
 #### 5.4 对得出的数据进行处理合并
    uid<-read.csv('G:/id.csv',header = TRUE,sep=',')#读取id编号数据
    GPA<-read.csv('G:/GPA.csv',header = TRUE,sep=',')#读取成绩数据

    st=c()
    strnum=1
    for(i in 1:length(sumtime)){#对累加的时间进行缩小到每一天
      st[strnum]=round(sumtime[i]/216000,3)
      strnum=strnum+1
    }

    dsum<-cbind.data.frame(st,uid)
    finall<-merge(GPA,dsum,by.x = 'id',by.y = 'id',all.x = TRUE)
    write.csv(finall,'G:/finall.csv',row.names=FALSE)#输出到文件，便于存储
    
 #### 5.5通过学生的GPA与轨迹特征值，分析特征与GPA的相关性
    #计算Sx（GPA标准差）
    newd<-data[2]
    sum<-0
    for(i in 1:nrow(newd)){
      sum = sum + newd[i,1]
    }
   
    x1=sum/nrow(data[1]) #GPA均值

    x2=0
    for(i in 1:nrow(newd)){
      x2= x2+(newd[i,1]-x1)*(newd[i,1]-x1)
    }
    Sx=sqrt(x2/(nrow(data[1])-1)) #GPA标准差
    
    #计算Sy（轨迹特征值标准差）
    newd1<-data[3]
    sum1<-0
    for(i in 1:nrow(newd1)){
      sum1 = sum1 + newd1[i,1]
    }
    
    y1=sum1/nrow(data[1]) #轨迹特征值均值

    y2=0
    for(i in 1:nrow(newd1)){
      y2= y2+(newd1[i,1]-y1)*(newd1[i,1]-y1)
    }
    Sy=sqrt(y2/(nrow(data[1])-1)) #轨迹特征值标准差
    
    #计算Sxy（样本协方差）
    xy <- 0
    for(i in 1:nrow(data)){
      xy = xy + (data[i,2] - x1) * (data[i,3] - y1)
    }
    Sxy = xy/(nrow(data[1])-1)#样本协方差

    #计算Rxy（样本相关系数）
    rxy = Sxy/(Sx*Sy)
    print(rxy)
    
## 6. 贝叶斯分类及预测
#### 6.1 从特征值中构成训练集，数据组成如下
![](https://github.com/cuit201608/3-GROUP/blob/master/%E7%AC%AC%E4%B8%89%E6%AC%A1%E4%BD%9C%E4%B8%9A/%E7%85%A7%E7%89%87/%E8%AE%AD%E7%BB%83%E9%9B%86%E6%A0%B7%E6%9C%AC.JPG)
#### 6.2 构建训练集
    #数据具有特殊格式
    ccc<-read.csv('G:/1.csv',header = TRUE,sep=',')
    bys<-naiveBayes(ccc[,1:3],ccc[,4])#第一参数：类别中的数据，第二参数：结果（GPA）
    test<-data.frame(librarytime='short',librarycount='little',outshcoolcount='frequen')
    predict(bys,test)

#### 6.3 正确性检验(将每条数据记录在训练集中当做测试数据)
    #result=c(1:30)
    pre <- c(paste0('pre',1:30))#生成数据框
      for(j in 1:nrow(ccc[1])){
       assign(pre[j],data.frame(librarytime=ccc[j,1],librarycount=ccc[j,2],outshcoolcount=ccc[j,3]))

      }
      for (i in 1:30) {
        result[i]=predict(bys,get(pre[i]))#输入的格式比较固定，第一个参数是数据框
      }
    contrast<-cbind.data.frame(specialldata1[1],ccc[4],result)
    colnames(contrast)<-c('编号','原始','预测')
#### 6.4 精确度计算（将预测结果与真实结果对比得到精准度）
    #统计正确率
    jishu=0
    for(i in 1:nrow(contrast[1])){
      if(contrast[i,2]==contrast[i,3]){
        jishu=jishu+1
      }else{next()}
    }
    successpredict=jishu/nrow(contrast[1])
    print('精度检验预测成功率')
    print(successpredict)
 ![](https://github.com/cuit201608/3-GROUP/blob/master/%E7%AC%AC%E4%B8%89%E6%AC%A1%E4%BD%9C%E4%B8%9A/%E7%85%A7%E7%89%87/%E7%B2%BE%E5%87%86%E5%BA%A6.JPG)
## 7. 实验结论
#### 相关度说明：相关度越接近于1说明特征行为越会影响成绩，特征值越接近于0说明特征行为与成绩的好坏关系越低，甚至没有关系
####  比较特征值和成绩与图书馆学习时间图可以看出：成绩越差的确实与待在图书馆的时间有关，而成绩较好的其成绩与待在图书馆的时间关系不大
