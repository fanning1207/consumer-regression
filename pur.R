setwd('C:\\Users\\fanning1207\\Documents\\consumerDeng')

#----load packages------
library(readxl)
library(plm)
library(lmtest)
library(sandwich)
library(Hmisc)
library(reshape2)
library(MASS)
library(nnet)
library(stargazer)
library(systemfit)
#-----------
consumer=read_excel('consumer.xlsx')
#有部分乱码
consumer=as.data.frame(consumer)
#------主要变量
#是否听说过谣言1：know1
#是否相信谣言1：rumor1_3
#态度:attoil_3,attoil_o
#购买行为:puroil_3,puroil_o
#态度和购买行为：action
#知道程度：know_total
#相信程度：rumor_total
#知识测试；k1-k5,a32-a36
#态度和行为一致：same=|pur-att|
#same_1=1/0
#control = list(maxit = 500)
#----------------------------
#对相信程度赋分
consumer$a25_2[consumer$a25==1]=1
consumer$a25_2[consumer$a25==2]=-1
consumer$a25_2[consumer$a25==3]=0

consumer$a26_2[consumer$a26==1]=1
consumer$a26_2[consumer$a26==2]=-1
consumer$a26_2[consumer$a26==3]=0

consumer$a27_2[consumer$a27==1]=1
consumer$a27_2[consumer$a27==2]=-1
consumer$a27_2[consumer$a27==3]=0

consumer$a28_2[consumer$a28==1]=1
consumer$a28_2[consumer$a28==2]=-1
consumer$a28_2[consumer$a28==3]=0

consumer$rumor1_3=as.factor(consumer$a25_2)
consumer$rumor2_3=as.factor(consumer$a26_2)
consumer$rumor3_3=as.factor(consumer$a27_2)
consumer$rumor4_3=as.factor(consumer$a28_2)

consumer$a21_2=consumer$a21
consumer$a21_2[consumer$a21==2]=0

consumer$a22_2=consumer$a22
consumer$a22_2[consumer$a22==2]=0

consumer$a23_2=consumer$a23
consumer$a23_2[consumer$a23==2]=0

consumer$a24_2=consumer$a24
consumer$a24_2[consumer$a24==2]=0

consumer$know1=as.factor(consumer$a21_2)
consumer$know2=as.factor(consumer$a22_2)
consumer$know3=as.factor(consumer$a23_2)
consumer$know4=as.factor(consumer$a24_2)

consumer$know_total=consumer$a21_2+consumer$a22_2+consumer$a23_2+consumer$a24_2
consumer$know_total2=consumer$a21_2+consumer$a22_2+consumer$a24_2
consumer$rumor_total=consumer$a25_2+consumer$a26_2+consumer$a27_2+consumer$a28_2
consumer$rumor_total2=consumer$a25_2+consumer$a26_2+consumer$a28_2

consumer$attoil_3[consumer$attoil==1]=1
consumer$attoil_3[consumer$attoil==2]=0
consumer$attoil_3[consumer$attoil==3]=-1

consumer$puroil_3[consumer$puroil==1]=1
consumer$puroil_3[consumer$puroil==2]=0
consumer$puroil_3[consumer$puroil==3]=-1

consumer$same=abs(as.integer(consumer$puroil_3)-as.integer(consumer$attoil_3))
consumer$attoil_3=as.factor(consumer$attoil_3)
consumer$puroil_3=as.factor(consumer$puroil_3)

consumer$know_trust1=rep(0,length(consumer$know1))
consumer$know_trust1[consumer$know1==1&consumer$rumor1_3==1]=1
consumer$know_trust1[consumer$know1==1&consumer$rumor1_3==-1]=-1

consumer$know_trust2=rep(0,length(consumer$know2))
consumer$know_trust2[consumer$know2==1&consumer$rumor2_3==1]=1
consumer$know_trust2[consumer$know2==1&consumer$rumor2_3==-1]=-1

consumer$know_trust3=rep(0,length(consumer$know3))
consumer$know_trust3[consumer$know3==1&consumer$rumor3_3==1]=1
consumer$know_trust3[consumer$know3==1&consumer$rumor3_3==-1]=-1

consumer$know_trust4=rep(0,length(consumer$know4))
consumer$know_trust4[consumer$know4==1&consumer$rumor4_3==1]=1
consumer$know_trust4[consumer$know4==1&consumer$rumor4_3==-1]=-1

consumer$know_trust=consumer$know_trust1+consumer$know_trust2+consumer$know_trust3+consumer$know_trust4
#---------------clean data-----------------------
consumer$puroil[consumer$puroil==22]=2
consumer$puroil[is.na(consumer$puroil)]=3
consumer$purrice[consumer$purrice==12]=3
consumer$purflavor[consumer$purflavor==11]=1

consumer$puroil_b=rep(0,length(consumer$puroil))
consumer$puroil_b[consumer$puroil==1]=1

consumer$puroil_b_no=rep(0,length(consumer$puroil))
consumer$puroil_b_no[consumer$puroil==2]=1

consumer$attoil_b=rep(0,length(consumer$attoil))
consumer$attoil_b[consumer$attoil==1]=1

consumer$rumor_np=consumer$rumor2+consumer$rumor3+consumer$rumor4

consumer$education[consumer$major=='4学前教育' & is.na(consumer$education)]=5
consumer$education[is.na(consumer$major)==FALSE & is.na(consumer$education)]=3
consumer$education[is.na(consumer$education)]=5

consumer$edu_year[consumer$education==1]=23
consumer$edu_year[consumer$education==2]=19
consumer$edu_year[consumer$education==3]=16
consumer$edu_year[consumer$education==4]=16
consumer$edu_year[consumer$education==5]=12
consumer$edu_year[consumer$education==6]=12
consumer$edu_year[consumer$education==7]=9

consumer$attoil_o=rep('anti',length(consumer$attoil))
consumer$attoil_o[consumer$attoil==1]='pro'
consumer$attoil_o[consumer$attoil==2]='neu'
consumer$attoil_o=as.factor(consumer$attoil_o)

consumer$puroil_o=rep('dont know',length(consumer$puroil))
consumer$puroil_o[consumer$puroil==1]='buy'
consumer$puroil_o[consumer$puroil==2]='not buy'
consumer$puroil_o=as.factor(consumer$puroil_o)

consumer$attoil_anti=rep(0,length(consumer$puroil))
consumer$attoil_anti[consumer$attoil==3]=1
consumer$attoil_neu=rep(0,length(consumer$puroil))
consumer$attoil_neu[consumer$attoil==1]=1
consumer$attoil_pro=rep(0,length(consumer$puroil))
consumer$attoil_pro[consumer$attoil==2]=1

consumer$scorea=(consumer$scorea)/20

consumer$action[consumer$attoil==1 & consumer$puroil==1]='接受购买'
consumer$action[consumer$attoil==2 & consumer$puroil==1]='中立购买'
consumer$action[consumer$attoil==3 & consumer$puroil==1]='反对购买'

consumer$action[consumer$attoil==1 & consumer$puroil==2]='接受不购买'
consumer$action[consumer$attoil==2 & consumer$puroil==2]='中立不购买'
consumer$action[consumer$attoil==3 & consumer$puroil==2]='反对不购买'

consumer$action[consumer$attoil==1 & consumer$puroil==3]='接受不知道'
consumer$action[consumer$attoil==2 & consumer$puroil==3]='中立不知道'
consumer$action[consumer$attoil==3 & consumer$puroil==3]='反对不知道'

consumer$same_1=rep(0,1460)
consumer$same_1[consumer$attoil==3 & consumer$puroil==1]=1
consumer$same_1[consumer$attoil==1 & consumer$puroil==2]=1

consumer$age_level=rep('age<=20',1460)
consumer$age_level[consumer$age<=40 & consumer$age>20]='20<age<=40'
consumer$age_level[consumer$age<=60 & consumer$age>40]='40<age<=60'
consumer$age_level[consumer$age<=80 & consumer$age>60]='60<age<=80'
consumer$age_level= relevel(factor(consumer$age_level), ref = 'age<=20')

consumer$puroil_3[consumer$puroil==1]=1
consumer$puroil_3[consumer$puroil==2]=0
consumer$puroil_3[consumer$puroil==3]=-1

consumer$a5[consumer$a5=='不好说']=''
consumer$a5[consumer$a5=='中立']='3'
consumer$a5=as.integer(consumer$a5)
consumer$a5[consumer$a5==2]=-1
consumer$a5[consumer$a5==3]=0

consumer$a9=as.integer(consumer$a9)
consumer$a9[consumer$a9==2]=-1
consumer$a9[consumer$a9==3]=0

consumer$bio_score=consumer$k1+consumer$k2+consumer$k3+consumer$k4+consumer$k5

consumer$attoil_3=as.integer(consumer$attoil_3)
consumer$attoil_3=consumer$attoil_3-2
consumer$rumor_total=as.integer(consumer$rumor_total)

consumer$puroil_3=as.integer(consumer$puroil_3)
consumer$puroil_3[consumer$puroil_3==2]=0
consumer$puroil_3[consumer$puroil_3==1]=NA
consumer$puroil_3[consumer$puroil_3==3]=1

consumer$major_2=rep(0,1460)
consumer$major_2[consumer$major_clean<4]=1

consumer$job_gov=rep(0,1460)
consumer$job_gov[consumer$job_clean==1|consumer$job_clean==3]=1

consumer$job_pri=rep(0,1460)
consumer$job_pri[consumer$job_clean==4|consumer$job_clean==5|consumer$job_clean==6]=1

consumer$industry_bio=rep(0,1460)
consumer$industry_bio[consumer$industry_clean<5]=1
consumer$industry_food=rep(0,1460)
consumer$industry_food[consumer$industry_clean==5]=1
consumer$industry_bio2=rep(0,1460)
consumer$industry_bio2[consumer$industry_clean<=5]=1

consumer$edu_12=rep(0,1460)
consumer$edu_12[consumer$education<3]=1
consumer$edu_3=rep(0,1460)
consumer$edu_3[consumer$education==3]=1
consumer$edu_4=rep(0,1460)
consumer$edu_4[consumer$education==4]=1
consumer$edu_56=rep(0,1460)
consumer$edu_56[consumer$education==5|consumer$education==6]=1

#--------------
#国家食品安全法规定含有转基因的食品都必须标识，您认为政府能做好该方面的监管工作吗？1=能；2=不能
#a10

#对于专业从事转基因领域研究的科学家得出的转基因是安全的，而一些非专业人士和个别非专业从事该研究的学者持有转基因是不安全的观点，您更相信谁？
#1=转基因科学家；2=非专业人士；3=都不相信；4=不知道
#a31

#-------------
attach(consumer)
detach()

consumer$edu_7=rep(0,1460)
consumer$edu_7[consumer$education==7]=1

aggregate(consumer$television,by=list(consumer$edu_12),FUN=mean)
aggregate(consumer$television,by=list(consumer$edu_3),FUN=mean)
aggregate(consumer$television,by=list(consumer$edu_4),FUN=mean)
aggregate(consumer$television,by=list(consumer$edu_56),FUN=mean)
aggregate(consumer$television,by=list(consumer$edu_7),FUN=mean)

#consumer$a25_2+consumer$a26_2+consumer$a28_2


#----------------问卷注释-----------------
#rumor1=(a25==1)
#rumor2=(a26==1)
#rumor3=(a27==1)
#rumor4=(a28==1)

#attfood1=(attsoybean+attoil+attrice)/3
#sourcea=(internet==1 | television==1)
#sourceb=(sourcea==1 | weichat==1)
#sourcec=(sourceb==1 | newspaper==1)
#教育水平：1=博士；2=硕士；3=本科；4=大专；5=中专；6=高中；7=初中及以下
#您听说吗？(1=是；2=否)
#您的看法是？(即使没听说过，也请填写您的看法)
#(1=相信；  2=不相信；3=不好回答)
#runmor1:美国人生产的转基因粮食自己不吃，却让中国人吃
#rumor=rumor1+rumor2+rumor3+rumor4
#rumor_np=rumor2+rumor3+rumor4
#pursoybean=转基因大豆做的豆腐
#puroil=转基因大豆油
#purpork=转基因饲料喂养的猪、鸡等蓄产品
#purflavor=转基因大豆等做的酱油、醋等调料
#consumer$grow_soybean=consumer$a5，是否支持我国转基因大豆商业化种植？
#a2:是否相信科学家
#a3:转基因安全性信息的获取来源(uncleaned)
#-----------------共线性检验------------
library(psych)
corr.test(x=consumer[,c('scorea',"female",'age','edu_year',"torestrant","rumor")])

x_cor=cor(x=consumer[,c('scorea',"female",'age','edu_year',"torestrant","rumor")])
kappa(x_cor,exact=TRUE)

#----regression-----------------------

attoil_logit000=glm(attoil_b~female+age+edu_year+
                      rumor_total+know_total+
                      internet+newspaper+television+weichat+relative
                 , data=consumer[consumer$attoil_3!=0,],family=binomial(link='logit'),control = list(maxit = 500))
or0=coeftest(attoil_logit000, vcov = vcovCL(attoil_logit000, vcov = vcovHC(lmAPI, "HC3")))

#交叉项
attoil_logit_c=glm(attoil_b~doc*rumor+scorea+age+edu_year+torestrant
                 , data=consumer,family=binomial(link='logit'),control = list(maxit = 500))
coeftest(attoil_logit_c, vcov = vcovCL(attoil_logit_c, vcov = vcovHC(lmAPI, "HC3")))


#分谣言
attoil_logit=glm(attoil_b~scorea+female+age+edu_year
                 +torestrant+rumor1+rumor2+rumor3+rumor4
                 , data=consumer,family=binomial(link='logit'),control = list(maxit = 500))
coeftest(attoil_logit, vcov = vcovCL(attoil_logit, vcov = vcovHC(lmAPI, "HC3")))

#---购买---
puroil_logit=glm(puroil_3~female+age+edu_year+
                   rumor_total+know_total+
                   internet+newspaper+television+weichat+relative
                , data=consumer[consumer$puroil_3!=0,],family=binomial(link='logit'),control = list(maxit = 500))
or1=coeftest(puroil_logit, vcov = vcovCL(puroil_logit, vcov = vcovHC(lmAPI, "HC3")))

#分谣言
puroil_logit=glm(puroil_b~scorea+female+age+edu_year
                 +torestrant+rumor1+rumor2+rumor3+rumor4
                 , data=consumer,family=binomial(link='logit'),control = list(maxit = 500))
coeftest(puroil_logit, vcov = vcovCL(puroil_logit, vcov = vcovHC(lmAPI, "HC3")))

stargazer(or0,or1,type = 'text')
#------------mlogit--------------------------------
library(nnet)
consumer$action=as.factor(consumer$action)
#----------3 term bug----------
consumer$puroil2= relevel(consumer$puroil_o, ref = "dont know")
buy_mlogit=multinom(puroil2 ~ scorea+female+age+edu_year+torestrant+rumor, data = consumer)
z11=summary(buy_mlogit)$coefficients/summary(buy_mlogit)$standard.errors
p11=(1 - pnorm(abs(z11), 0, 1))*2
p11<0.1

#----------9 terms-------------
action_mlogit=multinom(action ~ rumor, data = consumer)
z=summary(action_mlogit)$coefficients/summary(action_mlogit)$standard.errors
p=(1 - pnorm(abs(z), 0, 1))*2
p<0.1

sigi0=rep('',length(p))
sigi0[p<0.01]="***"
sigi0[p<0.05 & p>=0.01]="**"
sigi0[p<0.1 & p>=0.05]="*"

table_ml=cbind(round(summary(action_mlogit)$coefficients,digits = 3),round(summary(action_mlogit)$standard.errors,digits = 3),round(p,digits = 3),sigi0)
table_ml=as.data.frame(table_ml)
print(table_ml)
#-------------------------------------------------
consumer$pur_sum=as.factor(consumer$pur_sum)

pur_sum_ologit= polr(pur_sum~ trustscientist+female+age+as.factor(education)
                     +torestrant+rumor_np, data = consumer
                     , Hess=TRUE)
p=pnorm(abs(coef(summary(pur_sum_ologit))[, "t value"]), lower.tail = FALSE) * 2

table_r=cbind(round(coef(summary(pur_sum_ologit))[, 1:3],digits = 3),round(p,digits = 3),sigi)
table_r=as.data.frame(table_r)
names(table_r)[4:5]=c('p value','')
print(table_r)




#---------Jan--------
#scorea~female+age+edu_year+collegestudents
#torestrant~female+age+edu_year
#attoil_o~female+age+edu_year+collegestudents+scorea+torestrant

library(systemfit)
#seemingly unrelated regression
r1=rumor_total~scorea+female+age+edu_year+factor(attoil_o)+internet+newspaper+television+weichat+relative
r2=attoil_o~scorea+female+age+edu_year+rumor_total+internet+newspaper+television+weichat+relative
fitsur= systemfit(list(rumor = r1, attoil= r2), data=consumer)
summary(fitsur)

consumer$scorea_fit=fitted(fitsur)[,'scorea']
consumer$torestrant_fit=fitted(fitsur)[,'torestrant']

#----------Feb---------------------------
fit_believe=glm(rumor_total+4~scorea+female+age+edu_year+internet+newspaper+television+weichat+relative ,
                family = poisson, data=consumer)
a=coeftest(fit_believe, vcov = vcovCL(fit_believe, vcov = vcovHC(lmAPI, "HC3")))

fit_believe=glm.nb(rumor_total+4~scorea+female+age+edu_year+internet+newspaper+television+weichat+relative,
                 data=consumer)
b=coeftest(fit_believe, vcov = vcovCL(fit_believe, vcov = vcovHC(lmAPI, "HC3")))

fit_believe= polr(factor(rumor_total+4)~ scorea+female+age+edu_year+internet+newspaper+television+weichat+relative
                     , data = consumer
                     , Hess=TRUE)
c=coeftest(fit_believe, vcov = vcovCL(fit_believe, vcov = vcovHC(lmAPI, "HC3")))

stargazer(a,b,c,type = 'text')
#-----------ordered logit------------------------------------
#态度
#教育水平：1=博士；2=硕士；3=本科；4=大专；5=中专；6=高中；7=初中及以下
#scorea+female+age+edu_year+rumor_total+internet+newspaper+television+weichat+relative+trustgovernment+trustscientist

consumer$education=as.factor(consumer$education)
consumer$education=relevel(consumer$education,ref="1")
levels(consumer$education)=c( "7" ,"1","2", "3", "4", "5", "6")

consumer$education=as.integer(consumer$education)
consumer$edu_12=rep(0,1460)
consumer$edu_12[consumer$education<=2]=1
consumer$edu_3=rep(0,1460)
consumer$edu_3[consumer$education==3]=1
consumer$edu_456=rep(0,1460)
consumer$edu_456[consumer$education>3]=1

consumer$edu_level=rep('mas&phd',1460)
consumer$edu_level[consumer$edu_3==1]='bac'
consumer$edu_level[consumer$edu_456==1]='lowbac'

attoil_omlogit= polr(attoil_o~ female+age+edu_year+
                       rumor_total+know_total+
                       internet+newspaper+television+weichat+relative
                     , data = consumer
                     , Hess=TRUE)
a0=coeftest(attoil_omlogit, vcov = vcovCL(attoil_omlogit, vcov = vcovHC(lmAPI, "HC3")))

#-------平行假设检验-----
sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)))
}

consumer$attoil_3_0=as.numeric(consumer$attoil_3)

s <- with(consumer, summary(attoil_3_0 ~ 
                              female+age_level+edu_level+rumor_total+know_total+internet+newspaper+television+weichat+relative, fun=sf))

plot(s, which=1:3, pch=1:3, xlab='logit', main=' ', xlim=range(s[,3:4]))
#--------------

exp(cbind(OR = coef(attoil_omlogit), confint(attoil_omlogit)))

attoil_omlogit= polr(attoil_o~ female+age+edu_year+
                       rumor_total*know_total+
                       internet+newspaper+television+weichat+relative
                     , data = consumer
                     , Hess=TRUE)
a=coeftest(attoil_omlogit, vcov = vcovCL(attoil_omlogit, vcov = vcovHC(lmAPI, "HC3")))

attoil_omlogit= polr(attoil_o~ female+age+edu_12+edu_3+know_total+rumor_total
                     +internet+newspaper+television+weichat+relative
                     , data = consumer
                     , Hess=TRUE)
b0=coeftest(attoil_omlogit, vcov = vcovCL(attoil_omlogit, vcov = vcovHC(lmAPI, "HC3")))

attoil_omlogit= polr(attoil_o~ female+age+edu_12*know_total+edu_3*know_total+rumor_total
                     +internet+newspaper+television+weichat+relative
                     , data = consumer
                     , Hess=TRUE)
b=coeftest(attoil_omlogit, vcov = vcovCL(attoil_omlogit, vcov = vcovHC(lmAPI, "HC3")))

attoil_omlogit= polr(attoil_o~ female+age+know_total+edu_12*rumor_total+edu_3*rumor_total
                     +internet+newspaper+television+weichat+relative
                     , data = consumer
                     , Hess=TRUE)
b1=coeftest(attoil_omlogit, vcov = vcovCL(attoil_omlogit, vcov = vcovHC(lmAPI, "HC3")))

attoil_omlogit= polr(attoil_o~ female+edu_year+age_level+know_total+rumor_total
                     +internet+newspaper+television+weichat+relative
                     , data = consumer
                     , Hess=TRUE)
c0=coeftest(attoil_omlogit, vcov = vcovCL(attoil_omlogit, vcov = vcovHC(lmAPI, "HC3")))

attoil_omlogit= polr(attoil_o~ female+edu_year+factor(age_level)*know_total+rumor_total
                     +internet+newspaper+television+weichat+relative
                     , data = consumer
                     , Hess=TRUE)
c=coeftest(attoil_omlogit, vcov = vcovCL(attoil_omlogit, vcov = vcovHC(lmAPI, "HC3")))

attoil_omlogit= polr(attoil_o~ female+edu_year+know_total+factor(age_level)*rumor_total
                     +internet+newspaper+television+weichat+relative
                     , data = consumer
                     , Hess=TRUE)
c1=coeftest(attoil_omlogit, vcov = vcovCL(attoil_omlogit, vcov = vcovHC(lmAPI, "HC3")))

stargazer(a,b0,b,b1,c0,c,c1,type = 'text')
#--------------print ologit------------
p2=pnorm(abs(coef(summary(attoil_omlogit))[, "t value"]), lower.tail = FALSE) * 2

sigi2=rep('',length(p2))
sigi2[p2<0.01]="***"
sigi2[p2<0.05 & p2>=0.01]="**"
sigi2[p2<0.1 & p2>=0.05]="*"

table_r1=cbind(round(coef(summary(attoil_omlogit))[, 1:3],digits = 3),round(p2,digits = 3),sigi2)
table_r1=as.data.frame(table_r1)
names(table_r1)[4:5]=c('p value','')
print(table_r1)

#----------------------相关性----------
library(psych)
consumer$attoil_3=as.integer(consumer$attoil_3)
consumer$puroil_3=as.integer(consumer$puroil_3)
dta_t=consumer[,c('attoil_3','puroil_3','scorea','female','age','edu_year','rumor_total','know_total','trustscientist','trustgovernment','internet','newspaper','television','weichat','relative')]
corr.test(dta_t)
#由于scorea和态度不相关，但与age 、edu_year 、rumor_total都高度相关，故没有放

library(car)
vif()

library(pls)
gas1=plsr(rumor_total+4~scorea+female+age+edu_year+
       internet+newspaper+television+weichat+relative
     ,  data = consumer, validation = "LOO")
summary(gas1)
plot(RMSEP(gas1), legendpos = "topright")
plot(gas1,ncomp = 2,asp = 1,line = TRUE)
plot(gas1, ncomp = 2, asp = 1, line = TRUE)
plot(gas1, plottype = "scores", comps = 1:3)

library(psych)
dta_t=consumer[,c('trustscientist','trustgovernment','scorea','female','age','edu_year','rumor_total','internet','newspaper','television','weichat','relative')]
pc=principal(dta_t,nfactors = 2,scores = TRUE)
#--------------mlogit-------------------------------------
consumer$puroil_o= relevel(consumer$puroil_o, ref = "dont know")

proil_omlogit=multinom(puroil_o ~female+age+edu_year+
                         rumor_total+know_total+
                         internet+newspaper+television+weichat+relative
                       , 
                       data = consumer)

#---a-----
proil_omlogit= multinom(attoil_o~ female+age+edu_year+
                       rumor_total*know_total+
                       internet+newspaper+television+weichat+relative
                     , data = consumer
                     )
#---b0-----
proil_omlogit= multinom(attoil_o~ female+age+edu_12+edu_3+know_total+rumor_total
                     +internet+newspaper+television+weichat+relative
                     , data = consumer
                     )
#---b-----
proil_omlogit= multinom(attoil_o~ female+age+edu_12*know_total+edu_3*know_total+rumor_total
                     +internet+newspaper+television+weichat+relative
                     , data = consumer
                     )
#---b1-----
proil_omlogit= multinom(attoil_o~ female+age+know_total+edu_12*rumor_total+edu_3*rumor_total
                     +internet+newspaper+television+weichat+relative
                     , data = consumer
                     )
#---c0-----
proil_omlogit= multinom(attoil_o~ female+edu_year+age_level+know_total+rumor_total
                     +internet+newspaper+television+weichat+relative
                     , data = consumer
                     )
#---c-----
proil_omlogit= multinom(attoil_o~ female+edu_year+factor(age_level)*know_total+rumor_total
                     +internet+newspaper+television+weichat+relative
                     , data = consumer
                     )
#---c1-----
proil_omlogit= multinom(attoil_o~ female+edu_year+know_total+factor(age_level)*rumor_total
                     +internet+newspaper+television+weichat+relative
                     , data = consumer
                     )

#-----------logit buy------------
puroil_logit= glm(puroil_b~ female+age+edu_year+
                    rumor_total+know_total+
                    internet+newspaper+television+weichat+relative
                  ,data=consumer[consumer$puroil_3!=0,],family=binomial(link='logit'),control = list(maxit = 500))
a0=coeftest(puroil_logit, vcov = vcovCL(puroil_logit, vcov = vcovHC(lmAPI, "HC3")))

exp(cbind(OR = coef(puroil_logit), confint(puroil_logit)))

puroil_logit=glm(puroil_b~female+age+edu_year+
                   rumor_total+know_total+
                   internet+newspaper+television+weichat+relative
                 , data=consumer[consumer$puroil_3!=0,],family=binomial(link='logit'),control = list(maxit = 500))
a=coeftest(puroil_logit, vcov = vcovCL(puroil_logit, vcov = vcovHC(lmAPI, "HC3")))

puroil_logit= glm(puroil_b~ female+age+edu_year+
                    rumor_total*know_total+
                    internet+newspaper+television+weichat+relative
                  , data=consumer[consumer$puroil_3!=0,],family=binomial(link='logit'),control = list(maxit = 500)
)
a1=coeftest(puroil_logit, vcov = vcovCL(puroil_logit, vcov = vcovHC(lmAPI, "HC3")))

puroil_logit= glm(puroil_b~ female+age+edu_12+edu_3+know_total+rumor_total
                  +internet+newspaper+television+weichat+relative
                  , data=consumer[consumer$puroil_3!=0,],family=binomial(link='logit'),control = list(maxit = 500)
)
b0=coeftest(puroil_logit, vcov = vcovCL(puroil_logit, vcov = vcovHC(lmAPI, "HC3")))

puroil_logit= glm(puroil_b~ female+age+edu_12*know_total+edu_3*know_total+rumor_total
                  +internet+newspaper+television+weichat+relative
                  , data=consumer[consumer$puroil_3!=0,],family=binomial(link='logit'),control = list(maxit = 500)
)
b=coeftest(puroil_logit, vcov = vcovCL(puroil_logit, vcov = vcovHC(lmAPI, "HC3")))

puroil_logit= glm(puroil_b~ female+age+know_total+edu_12*rumor_total+edu_3*rumor_total
                  +internet+newspaper+television+weichat+relative
                  , data=consumer[consumer$puroil_3!=0,],family=binomial(link='logit'),control = list(maxit = 500)
)
b1=coeftest(puroil_logit, vcov = vcovCL(puroil_logit, vcov = vcovHC(lmAPI, "HC3")))

puroil_logit= glm(puroil_b~ female+edu_year+age_level+know_total+rumor_total
                  +internet+newspaper+television+weichat+relative
                  , data=consumer[consumer$puroil_3!=0,],family=binomial(link='logit'),control = list(maxit = 500)
)
c0=coeftest(puroil_logit, vcov = vcovCL(puroil_logit, vcov = vcovHC(lmAPI, "HC3")))

puroil_logit= glm(puroil_b~ female+edu_year+factor(age_level)*know_total+rumor_total
                  +internet+newspaper+television+weichat+relative
                  , data=consumer[consumer$puroil_3!=0,],family=binomial(link='logit'),control = list(maxit = 500)
)
c=coeftest(puroil_logit, vcov = vcovCL(puroil_logit, vcov = vcovHC(lmAPI, "HC3")))

puroil_logit= glm(puroil_b~ female+edu_year+know_total+factor(age_level)*rumor_total
                  +internet+newspaper+television+weichat+relative
                  , data=consumer[consumer$puroil_3!=0,],family=binomial(link='logit'),control = list(maxit = 500)
)
c1=coeftest(puroil_logit, vcov = vcovCL(puroil_logit, vcov = vcovHC(lmAPI, "HC3")))


stargazer(b0,b1,type = 'text')

#--------------print mlogit buy-----------------------
z=summary(proil_omlogit)$coefficients/summary(proil_omlogit)$standard.errors
p=(1 - pnorm(abs(z), 0, 1))*2
p<0.1

#buy
table_ml=cbind(round(summary(proil_omlogit)$coefficients[1,],digits = 3),round(summary(proil_omlogit)$standard.errors[1,],digits = 3),round(p[1,],digits = 3))
table_ml=as.data.frame(table_ml)
asigi=table_ml[,3]
sigi=rep('',length(asigi))
sigi[asigi<0.01]="***"
sigi[asigi<0.05 & asigi>=0.01]="**"
sigi[asigi<0.1 & asigi>=0.05]="*"
table_ml[,4]=sigi
names(table_ml)=c('Value','SE','p','')
print(table_ml)
#--------------print mlogit not buy-----------------------
z=summary(proil_omlogit)$coefficients/summary(proil_omlogit)$standard.errors
p=(1 - pnorm(abs(z), 0, 1))*2
p<0.1

#buy
table_ml2=cbind(round(summary(proil_omlogit)$coefficients[2,],digits = 3),round(summary(proil_omlogit)$standard.errors[2,],digits = 3),round(p[2,],digits = 3))
table_ml2=as.data.frame(table_ml2)
asigi2=table_ml2[,3]
sigi22=rep('',length(asigi2))
sigi22[asigi2<0.01]="***"
sigi22[asigi2<0.05 & asigi2>=0.01]="**"
sigi22[asigi2<0.1 & asigi2>=0.05]="*"
table_ml2[,4]=sigi22
names(table_ml2)=c('Value','SE','p','')
print(table_ml2)
#-----------ordered logit---same------4-5---------------------------
#scorea+female+age+edu_year+rumor_total+internet+newspaper+television+weichat+relative+trustgovernment+trustscientist
consumer2=consumer[is.na(consumer$puroil_3)==FALSE,]
consumer2$puroil_3[consumer2$puroil_3==0]=-1
consumer2$same=abs(as.integer(consumer2$puroil_3)-as.integer(consumer2$attoil_3))
consumer2$same=factor(consumer2$same)

same_omlogit= polr(same~ female+age+edu_12+edu_3+edu_4+edu_56+bio_score+
                       a20+know_total+major_2+industry_food
                     , data = consumer2
                     , Hess=TRUE)
a=coeftest(same_omlogit, vcov = vcovCL(same_omlogit, vcov = vcovHC(lmAPI, "HC3")))

#a20 如果饭店的食材转基因标识后，采用非转基因食材的饭店价格增加，上涨幅度_______%，您就不去饭店吃饭了？

same_nb=glm.nb(as.integer(consumer2$same)~ female+age+edu_12+edu_3+edu_4+edu_56+bio_score+
                     know_total+major_2+industry_food
                   , data = consumer2,control=glm.control(maxit=1000))
b=coeftest(same_nb, vcov = vcovCL(same_nb, vcov = vcovHC(lmAPI, "HC3")))

same_p=glm(as.integer(consumer2$same)~ female+age+edu_12+edu_3+edu_4+edu_56+bio_score+
             know_total+major_2+industry_food
           , data = consumer2, family = poisson,control=glm.control(maxit=1000))
c=coeftest(same_p, vcov = vcovCL(same_p, vcov = vcovHC(lmAPI, "HC3")))

stargazer(a,b,c,type = 'text')

library(lmtest)
consumer2$same=as.integer(consumer2$same)
bptest(same~female+age+edu_12+edu_3+edu_4+edu_56+bio_score+
         know_total+major_2
       , data = consumer2)
#存在异方差,计数数据的必然

#edu_12+edu_3+edu_4+edu_56+major_2+industry_bio+industry_food

sf <- function(y) {
  c('Y>=0' = qlogis(mean(y >= 0)),
    'Y>=1' = qlogis(mean(y >=1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    '(Y>=1)-(Y>=2)'= qlogis(mean(y >=1))- qlogis(mean(y >= 2) ))
}

s <- with(consumer2, summary(as.numeric(consumer2$same)-1 ~ 
                              female+age+edu_12+edu_3+edu_4+edu_56+bio_score+
                              know_total+major_2+industry_bio+industry_food, fun=sf))
#y=summary(b~a)代表a取各值时,对应的b的均值

consumer2$pro_level=as.integer(consumer2$puroil_3)+as.integer(consumer2$attoil_3)
consumer2$pro_level=factor(consumer2$pro_level)

pro_level_ologit= polr(pro_level~ female+age+edu_12+edu_3+edu_4+edu_56+bio_score+
                     rumor_total+major_2+industry_bio+industry_food
                   , data = consumer2
                   , Hess=TRUE)
coeftest(pro_level_ologit, vcov = vcovCL(pro_level_ologit, vcov = vcovHC(lmAPI, "HC3")))

#------------leaning------sem----------------
library(sem)
fullsem<-specify.model()

library(lavaan)
fit <- cfa(myModel, data = consumer,
           ordered='attoil_o')

# latent variable definitions 
f1 =~ y1 + y2 + y3 
f2 =~ y4 + y5 + y6 
f3 =~ y7 + y8 + y9 + y10

# variances and covariances 
y1 ~~ y1 
y1 ~~ y2 
f1 ~~ f2

# intercepts 
y1 ~ 1 
f1 ~ 1

head(HolzingerSwineford1939)
HS.model <- ' visual  =~ x1 + x2 + x3 
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9 '
#潜变量=~观测指标
fit <- cfa(HS.model, data=HolzingerSwineford1939)
summary(fit, fit.measures=TRUE)

head(PoliticalDemocracy)
model <- '
  # measurement model
ind60 =~ x1 + x2 + x3
dem60 =~ y1 + y2 + y3 + y4
dem65 =~ y5 + y6 + y7 + y8
# regressions
dem60 ~ ind60
dem65 ~ ind60 + dem60
# residual correlations
y1 ~~ y5
y2 ~~ y4 + y6
y3 ~~ y7
y4 ~~ y8
y6 ~~ y8
'
fit <- sem(model, data=PoliticalDemocracy)
summary(fit, standardized=TRUE)

#---------my sem-----------------
library(lavaan)

#attoil_o =~ edu_year
#rumor_total =~ scorea+female+age
#know_total =~ internet
#rumor_total~scorea+female+age+edu_year+attoil_o+know_total+internet+newspaper+television+weichat+relative

model <- '
# measurement model
rumor_total=~scorea
attoil_o=~age+edu_year

# residual correlations
attoil_o ~~ rumor_total
scorea ~~ edu_year
age~~age
'
fit_lan=lavaan(model, data = consumer,auto.var=TRUE)


fit_my <- cfa(model, data = consumer,
              ordered = c('attoil_o','internet','newspaper','television','weichat','relative'))

summary(fit_my, standardized=TRUE)

modA <- '
attoil_3 ~ rumor_total+know_total+scorea+age+edu_year+internet+newspaper+television+weichat
rumor_total ~ attoil_3+scorea
'
fit<-sem(modA,data=consumer,meanstructure=T)
summary(fit)

# polychoric (ordered – ordered)
# polyserial (ordered – numeric)

#--------------------------
consumer3=consumer[consumer$attoil_3==1 & is.na(consumer$puroil_3)==FALSE,]
consumer4=consumer[consumer$attoil_3==-1& is.na(consumer$puroil_3)==FALSE,]

#接受中购买的
puroil_logit= glm(puroil_3~ female+age+edu_12+edu_3+edu_4+edu_56+
                    know_total+major_2+bio_score
                  ,data=consumer3,family=binomial(link='logit'),control = list(maxit = 500))
a0=coeftest(puroil_logit, vcov = vcovCL(puroil_logit, vcov = vcovHC(lmAPI, "HC3")))

puroil_logit= glm(puroil_3~ female+age+edu_12+edu_3+edu_4+edu_56+
                    know_total+industry_bio+industry_food+bio_score
                  ,data=consumer3,family=binomial(link='logit'),control = list(maxit = 500))
a1=coeftest(puroil_logit, vcov = vcovCL(puroil_logit, vcov = vcovHC(lmAPI, "HC3")))

#反对中购买的
puroil_logit= glm(puroil_3~ female+age+edu_12+edu_3+edu_4+edu_56+
                    know_total+major_2+bio_score
                  ,data=consumer4,family=binomial(link='logit'),control = list(maxit = 500))
b0=coeftest(puroil_logit, vcov = vcovCL(puroil_logit, vcov = vcovHC(lmAPI, "HC3")))

puroil_logit= glm(puroil_3~ female+age+edu_12+edu_3+edu_4+edu_56+
                    know_total+industry_bio+industry_food+bio_score
                  ,data=consumer4,family=binomial(link='logit'),control = list(maxit = 500))
b1=coeftest(puroil_logit, vcov = vcovCL(puroil_logit, vcov = vcovHC(lmAPI, "HC3")))


#--------------mlogit----action---------------------------------
consumer2=consumer[is.na(consumer$puroil_3)==FALSE,]
#consumer2$action[consumer2$action=='中立不购买']='中立'
#consumer2$action[consumer2$action=='中立购买']='中立'
###consumer2$action[consumer2$action!='中立购买']='其他'
consumer2$action= relevel(factor(consumer2$action), ref = '中立不购买')

proil_omlogit=multinom(action ~female+age+edu_year, 
                       data = consumer2)
summary(proil_omlogit)$coefficients
summary(proil_omlogit)$standard.errors
z=summary(proil_omlogit)$coefficients/summary(proil_omlogit)$standard.errors
p=(1 - pnorm(abs(z), 0, 1))*2
p<0.1
#--------------print action--------反对购买---------------
z=summary(proil_omlogit)$coefficients/summary(proil_omlogit)$standard.errors
p=(1 - pnorm(abs(z), 0, 1))*2
p<0.1

#buy
table_ml2=cbind(round(summary(proil_omlogit)$coefficients[2,],digits = 3),round(summary(proil_omlogit)$standard.errors[2,],digits = 3),round(p[2,],digits = 3))
table_ml2=as.data.frame(table_ml2)
asigi2=table_ml2[,3]
sigi22=rep('',length(asigi2))
sigi22[asigi2<0.01]="***"
sigi22[asigi2<0.05 & asigi2>=0.01]="**"
sigi22[asigi2<0.1 & asigi2>=0.05]="*"
table_ml2[,4]=sigi22
names(table_ml2)=c('Value','SE','p','')
print(table_ml2)
bbb1=table_ml2
#--------------print action--------接受不购买---------------
z=summary(proil_omlogit)$coefficients/summary(proil_omlogit)$standard.errors
p=(1 - pnorm(abs(z), 0, 1))*2
p<0.1

#buy
table_ml2=cbind(round(summary(proil_omlogit)$coefficients[3,],digits = 3),round(summary(proil_omlogit)$standard.errors[3,],digits = 3),round(p[3,],digits = 3))
table_ml2=as.data.frame(table_ml2)
asigi2=table_ml2[,3]
sigi22=rep('',length(asigi2))
sigi22[asigi2<0.01]="***"
sigi22[asigi2<0.05 & asigi2>=0.01]="**"
sigi22[asigi2<0.1 & asigi2>=0.05]="*"
table_ml2[,4]=sigi22
names(table_ml2)=c('Value','SE','p','')
print(table_ml2)
bbb2=table_ml2
#------------------
print(bbb1)
print(bbb2)
#------------一致性----------------------
attoil_logit000=glm(same_1~female+age+edu_year+
                      rumor_total+know_total+
                      internet+newspaper+television+weichat+relative
                    , data=consumer,family=binomial(link='logit'),control = list(maxit = 500))
or0=coeftest(attoil_logit000, vcov = vcovCL(attoil_logit000, vcov = vcovHC(lmAPI, "HC3")))

#-----------------联立方程模型------------------

R<-rumor_total~female+age+edu_year+know_total
A<-attoil_3~rumor_total+female+age+edu_year
B<-puroil_3~attoil_3+female+age+edu_year
sys <- list(R,A,B)
truff.sys <- systemfit(sys,data=consumer)
summary(truff.sys)

#
R<-rumor_total~female+edu_year+know_total*age_level
A<-attoil_3~rumor_total*age_level+female+edu_year
B<-puroil_3~attoil_3*age_level+female+edu_year
sys <- list(R,A,B)
truff.sys <- systemfit(sys,data=consumer)
summary(truff.sys)

#age_edu_level
R0<-rumor_total~female+age_level+edu_12+edu_3+know_total
A0<-attoil_3~rumor_total+female+age_level+edu_12+edu_3
B0<-puroil_o~attoil_3+female+age_level+edu_12+edu_3
sys0 <- list(R0,A0,B0)
truff.sys0 <- systemfit(sys0,data=consumer)
summary(truff.sys0)

#internet etc.
I<-internet~female+age+edu_12+edu_3+edu_4+edu_56
I2<-newspaper~female+age+edu_12+edu_3+edu_4+edu_56
I3<-television~female+age+edu_12+edu_3+edu_4+edu_56
I4<-weichat~female+age+edu_12+edu_3+edu_4+edu_56
I5<-relative~female+age+edu_12+edu_3+edu_4+edu_56
E<-bio_score~edu_12+edu_3+edu_4+edu_56+major_2
S<-know_total2~female+age+edu_12+edu_3+edu_4+edu_56+major_2+internet+newspaper+television+weichat+relative
R<-rumor_total2~attoil_3+bio_score+female+age+edu_12+edu_3+edu_4+edu_56+major_2+know_total2+internet+newspaper+television+weichat+relative
A<-attoil_3~rumor_total2+female+age+edu_12+edu_3+edu_4+edu_56+major_2
B<-puroil_3~attoil_3+female+age+edu_12+edu_3+edu_4+edu_56+major_2
sys <- list(I,I2,I3,I4,I5,E,S,R,A,B)
truff.sys1 <- systemfit(sys,data=consumer)
summary(truff.sys1)

I<-internet~female+age+edu_12+edu_3+edu_4+edu_56
I2<-newspaper~female+age+edu_12+edu_3+edu_4+edu_56
I3<-television~female+age+edu_12+edu_3+edu_4+edu_56
I4<-weichat~female+age+edu_12+edu_3+edu_4+edu_56
I5<-relative~female+age+edu_12+edu_3+edu_4+edu_56
E<-bio_score~edu_12+edu_3+edu_4+edu_56+industry_bio+industry_food
S<-know_total~female+age+edu_12+edu_3+edu_4+edu_56+industry_bio+industry_food+internet+newspaper+television+weichat+relative
R<-rumor_total~attoil_3+bio_score+female+age+edu_12+edu_3+edu_4+edu_56+industry_bio+industry_food+know_total+internet+newspaper+television+weichat+relative
A<-attoil_3~rumor_total+female+age+edu_12+edu_3+edu_4+edu_56+industry_bio+industry_food
B<-puroil_3~attoil_3+female+age+edu_12+edu_3+edu_4+edu_56+industry_bio+industry_food
sys <- list(I,I2,I3,I4,I5,E,S,R,A,B)
truff.sys1 <- systemfit(sys,data=consumer)
summary(truff.sys1)

I<-internet~female+age+edu_12+edu_3+edu_4+edu_56
I2<-newspaper~female+age+edu_12+edu_3+edu_4+edu_56
I3<-television~female+age+edu_12+edu_3+edu_4+edu_56
I4<-weichat~female+age+edu_12+edu_3+edu_4+edu_56
I5<-relative~female+age+edu_12+edu_3+edu_4+edu_56
E<-bio_score~edu_12+edu_3+edu_4+edu_56+industry_bio2+internet+newspaper+television+weichat+relative
S<-know_total~female+age+edu_12+edu_3+edu_4+edu_56+industry_bio2+internet+newspaper+television+weichat+relative
R<-rumor_total~attoil_3+bio_score+female+age+edu_12+edu_3+edu_4+edu_56+industry_bio2+know_total+internet+newspaper+television+weichat+relative
A<-attoil_3~rumor_total+female+age+edu_12+edu_3+edu_4+edu_56+industry_bio2
B<-puroil_3~attoil_3+female+age+edu_12+edu_3+edu_4+edu_56+industry_bio2
sys <- list(I,I2,I3,I4,I5,E,S,R,A,B)
truff.sys1 <- systemfit(sys,data=consumer)
summary(truff.sys1)

#已知attitude=1
I<-internet~female+age+edu_12+edu_3+edu_4+edu_56
I2<-newspaper~female+age+edu_12+edu_3+edu_4+edu_56
I3<-television~female+age+edu_12+edu_3+edu_4+edu_56
I4<-weichat~female+age+edu_12+edu_3+edu_4+edu_56
I5<-relative~female+age+edu_12+edu_3+edu_4+edu_56
E<-bio_score~edu_12+edu_3+edu_4+edu_56+industry_bio2+internet+newspaper+television+weichat+relative+bio_score+know_total
S<-know_total~female+age+edu_12+edu_3+edu_4+edu_56+industry_bio2+internet+newspaper+television+weichat+relative
B<-puroil_3~female+age+edu_12+edu_3+edu_4+edu_56+industry_bio+industry_food
sys <- list(I,I2,I3,I4,I5,E,S,B)
truff.sys1 <- systemfit(sys,data=consumer3)
summary(truff.sys1)



## OLS estimation
##递归模型的估计方法是OLS法

## 3SLS estimation with GMM-3SLS formula
## inst <- ~ income + farmPrice + trend
## fit3sls <- systemfit( system, "3SLS", inst = inst, data = Kmenta, method3sls = "GMM" )
#--------似不相关-------------------
library(systemfit)
#seemingly unrelated regression
I<-internet~female+age+edu_12+edu_3+edu_4+edu_56
I2<-newspaper~female+age+edu_12+edu_3+edu_4+edu_56
I3<-television~female+age+edu_12+edu_3+edu_4+edu_56
I4<-weichat~female+age+edu_12+edu_3+edu_4+edu_56
I5<-relative~female+age+edu_12+edu_3+edu_4+edu_56
fitsur= systemfit(list(internet = I, newspaper= I2,television=I3,weichat=I4,relative=I5), "SUR", data=consumer)
summary(fitsur)

sys0 <- list(I,I2,I3,I4,I5)
fitsur_ols= systemfit(sys0,data=consumer)
#------变量确定--------------------------
library(leaps)
leaps=regsubsets(know_total~female+age+edu_year+major_2+industry_bio+industry_food+collegestudents+familysize+internet+newspaper+television+weichat+relative,
                 data=consumer,nbest = 4)
plot(leaps,scale='adjr2')
#全子集回归

fa= polr(factor(know_total)~ female+age+edu_year+
           collegestudents+familysize+
           internet+newspaper+television+weichat+relative,
                     data = consumer
                     , Hess=TRUE)
coeftest(fa, vcov = vcovCL(fa, vcov = vcovHC(lmAPI, "HC3")))

fa1= polr(factor(rumor_total)~ attoil_3+know_total+
            female+age+edu_year+
            major_2+
            industry_bio+industry_food+
            collegestudents+
            internet+newspaper+television+weichat+relative,
         data = consumer
         , Hess=TRUE)

#相对权重方法
relweights <- function(fit,...){
  R <- cor(fit$model)
  nvar <- ncol(R)
  rxx <- R[2:nvar, 2:nvar]
  rxy <- R[2:nvar, 1]
  svd <- eigen(rxx)
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))
  lambda <- evec %*% delta %*% t(evec)
  lambdasq <- lambda ^ 2
  beta <- solve(lambda) %*% rxy
  rsquare <- colSums(beta ^ 2)
  rawwgt <- lambdasq %*% beta ^ 2
  import <- (rawwgt / rsquare) * 100
  lbls <- names(fit$model[2:nvar])
  rownames(import) <- lbls
  colnames(import) <- "Weights"
  barplot(t(import),names.arg=lbls,
          ylab="% of R-Square",
          xlab="Predictor Variables",
          main="Relative Importance of Predictor Variables",
          sub=paste("R-Square=", round(rsquare, digits=3)),
          ...)
  return(import)
}

fa=lm(rumor_total~ attoil_3+know_total+
  female+age+edu_year+
  major_2+
  industry_bio+industry_food+
  collegestudents+
  internet+newspaper+television+weichat+relative,
data = consumer)

relweights(fa,col='lightgrey')

#k重交叉验证
shrinkage <- function(fit, k=10){
  require(bootstrap)
  theta.fit <- function(x,y){lsfit(x,y)}
  theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef}
  x <- fit$model[,2:ncol(fit$model)]
  y <- fit$model[,1]
  results <- crossval(x, y, theta.fit, theta.predict, ngroup=k)
  r2 <- cor(y, fit$fitted.values)^2
  r2cv <- cor(y, results$cv.fit)^2
  cat("Original R-square =", r2, "\n")
  cat(k, "Fold Cross-Validated R-square =", r2cv, "\n")
  cat("Change =", r2-r2cv, "\n")
}
shrinkage(fa)

fa1= polr(factor(rumor_total)~ major_2+
            industry_bio+industry_food
           ,
          data = consumer
          , Hess=TRUE)

coeftest(fa1, vcov = vcovCL(fa1, vcov = vcovHC(lmAPI, "HC3")))

#-----------------multivariate probit model------------------
library(mvProbit)

sigma <- symMatrix( c( 1, 0.27, 1 ) )

mvp <- mvProbit( cbind(attoil_b,puroil_b) ~ female+age+edu_12+edu_3+edu_4+edu_56+major_2+industry_bio+industry_food,
                 startSigma = sigma,
                  data = consumer, iterlim = 1, nGHK = 50 )
summary(mvp)

## generate a simulated data set
set.seed( 123 )
# number of observations
nObs <- 50
# generate explanatory variables
xMat <- cbind(
  const = rep( 1, nObs ),
  x1 = as.numeric( rnorm( nObs ) > 0 ),
  x2 = rnorm( nObs ) )
# model coefficients
beta <- cbind( c( 0.8, 1.2, -0.8 ),
               c( -0.6, 1.0, -1.6 ),
               c( 0.5, -0.6, 1.2 ) )
# covariance matrix of error terms
library( miscTools )
sigma <- symMatrix( c( 1, 0.2, 0.4, 1, -0.1, 1 ) )
# generate dependent variables
yMatLin <- xMat %*% beta
yMat <- ( yMatLin + rmvnorm( nObs, sigma = sigma ) ) > 0
colnames( yMat ) <- paste( "y", 1:3, sep = "" )
# estimation (BHHH optimizer and GHK algorithm)
estResult <- mvProbit( cbind( y1, y2, y3 ) ~ x1 + x2,
                       data = as.data.frame( cbind( xMat, yMat ) ), iterlim = 1, nGHK = 50 )
summary( estResult )
# same estimation with user-defined starting values
estResultStart <- mvProbit( cbind( y1, y2, y3 ) ~ x1 + x2,
                            start = c( beta ), startSigma = sigma,
                            data = as.data.frame( cbind( xMat, yMat ) ), iterlim = 1, nGHK = 50 )
summary( estResultStart )
dat=as.data.frame( cbind( xMat, yMat ) )
#-------nested logit---------------------
library(mlogit)

nested.logit <- mlogit(action ~ female+age+edu_year+know_total, data=consumer, shape='long', alt.var='attoil_b', 
                       nests=list(consumer$attoil_b))

data("HC", package = "mlogit")
HC <- mlogit.data(HC, varying = c(2:8, 10:16), choice = "depvar",
                     shape = "wide")
cooling.modes <- attr(HC, "index")$alt %in% c("gcc", "ecc", "erc",
                                                 "hpc")
room.modes <- attr(HC, "index")$alt %in% c("erc", "er")
HC$icca[!cooling.modes] <- 0
HC$occa[!cooling.modes] <- 0
HC$icca <- HC$icca/100
HC$occa <- HC$occa/100
HC$ich <- HC$ich/100
HC$och <- HC$och/100
HC$inc.cooling <- HC$inc.room <- 0
HC$inc.cooling[cooling.modes] <- HC$income[cooling.modes]
HC$inc.room[room.modes] <- HC$income[room.modes]
HC$int.cooling <- as.numeric(cooling.modes)
nl <- mlogit(depvar ~ ich + och + icca + occa + inc.room + inc.cooling +
                  + int.cooling | 0, HC, nests = list(cooling = c("gcc", "ecc",
                                                                  "erc", "hpc"), other = c("gc", "ec", "er")), un.nest.el = TRUE)
summary(nl)

#R/Weka Classifier Trees------------------
library(RWeka)

fit2=M5P(attoil_3~rumor_total+bio_score+female+age+edu_12+edu_3+edu_4+edu_56+major_2+industry_bio+industry_food+collegestudents,data=consumer)
fit2

# Regression Tree
library(rpart)
fit <- rpart(attoil_3~rumor_total+bio_score+female+age+edu_12+edu_3+edu_4+edu_56+major_2+industry_bio+industry_food+collegestudents,data=consumer, 
             method="anova")
fit

#
library(tree)
fit0 <- tree(attoil_3~rumor_total+bio_score+female+age+edu_12+edu_3+edu_4+edu_56+major_2+industry_bio+industry_food+collegestudents,data=consumer
             )
summary(fit0)

# Neural Network model
set.seed(500)
library(MASS)
data <- Boston
apply(data,2,function(x) sum(is.na(x)))
index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]
lm.fit <- glm(medv~., data=train)
#
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test)
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
train_ <- scaled[index,]
test_ <- scaled[-index,]
library(neuralnet)
n <- names(train_)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=T)
plot(nn)

#-------------
## generate a simulated data set
set.seed( 123 )
# number of observations
nObs <- 50

# generate explanatory variables
xMat <- cbind( 
  const = rep( 1, nObs ),
  x1 = as.numeric( rnorm( nObs ) > 0 ),
  x2 = rnorm( nObs ) )

# model coefficients
beta <- cbind( c(  0.8,  1.2, -0.8 ),
               c( -0.6,  1.0, -1.6 ),
               c(  0.5, -0.6,  1.2 ) )

# covariance matrix of error terms
library( miscTools )
sigma <- symMatrix( c( 1, 0.2, 0.4, 1, -0.1, 1 ) )

# generate dependent variables
yMatLin <- xMat %*% beta 
yMat <- ( yMatLin + rmvnorm( nObs, sigma = sigma ) ) > 0
colnames( yMat ) <- paste( "y", 1:3, sep = "" )

# estimation (BHHH optimizer and GHK algorithm)
dat=as.data.frame( cbind( xMat, yMat ) )
aaa=cbind( dat$y1, dat$y2, dat$y3 )
estResult <- mvProbit( cbind( y1, y2, y3 ) ~ x1 + x2,
                       data = dat, iterlim = 1, nGHK = 50 )
summary( estResult )

# same estimation with user-defined starting values
estResultStart <- mvProbit( cbind( y1, y2, y3 ) ~ x1 + x2,
                            start = c( beta ), startSigma = sigma, 
                            data = as.data.frame( cbind( xMat, yMat ) ), iterlim = 1, nGHK = 50 )
summary( estResultStart )
#----------------Reliability Analysis-------------
library(dplyr)
#use dplyr::select if it is masked from other packages

consumer$attoil_3=as.integer(consumer$attoil_3)
consumer$attoil_3=consumer$attoil_3-2
consumer$attoil_3=as.integer(consumer$attoil_3)
consumer$a5=as.integer(consumer$a5)
consumer$a9=as.integer(consumer$a9)

consumer$rumor1_3=as.integer(consumer$rumor1_3)
consumer$rumor1_3=consumer$rumor1_3-2
consumer$rumor1_3=as.integer(consumer$rumor1_3)

consumer$rumor2_3=as.integer(consumer$rumor2_3)
consumer$rumor2_3=consumer$rumor2_3-2
consumer$rumor2_3=as.integer(consumer$rumor2_3)

consumer$rumor3_3=as.integer(consumer$rumor3_3)
consumer$rumor3_3=consumer$rumor3_3-2
consumer$rumor3_3=as.integer(consumer$rumor3_3)

consumer$rumor4_3=as.integer(consumer$rumor4_3)
consumer$rumor4_3=consumer$rumor4_3-2
consumer$rumor4_3=as.integer(consumer$rumor4_3)

dat_RA=consumer[,c('attoil_3','a5','a9',
                   'rumor1_3','rumor2_3','rumor3_3','rumor4_3')]
tbl_df(dat_RA)

attitude_oil=select(dat_RA, 1,2)
know_oil=select(dat_RA, 4,5,6,7)

psych::alpha(attitude_oil)
psych::alpha(know_oil)
#----------------路径分析--------------------
#source('http://openmx.psyc.virginia.edu/getOpenMx.R')
library(lavaan)
library(semPlot)
library(OpenMx)
library(tidyverse)
library(knitr)
library(kableExtra)
library(GGally)

model <- '
attoil_3 ~ scorea+female+age+edu_year+rumor_total+know_total+internet+newspaper+television+weichat+relative
rumor_total ~ scorea+female+age+edu_year+attoil_3+know_total+internet+newspaper+television+weichat+relative
scorea~female+age+edu_year+rumor_total+know_total+internet+newspaper+television+weichat+relative
internet ~female+age+edu_year
weichat~female+age+edu_year
know_total~female+age+edu_year+internet+newspaper+television+weichat+relative
'

model <- '
attoil_3 ~ age+edu_year+rumor_total+know_total+internet
rumor_total~scorea+age+attoil_3 
scorea~age+edu_year+internet
internet ~age+edu_year
'

fit <- cfa(model, data = consumer,ordered = c('attoil_3'))
summary(fit, fit.measures = TRUE, standardized=T,rsquare=T)
#,ordered = c('attoil_o','internet','newspaper','television','weichat','relative')

semPaths(fit, 'std', layout = 'circle')
semPaths(fit,"std",layout = 'tree', edge.label.cex=.9, curvePivot = TRUE)