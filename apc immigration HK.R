library(knitr)
library(apc)
library(Epi)
library(ggpubr)
library(ggplot2)
library(reshape)
library(tidyverse)

# part of code was credited to Dr. Qianyin Lin

rm(list=ls())

from <- 1998
to <- 2030
gender <- "F"
npar <- 15
ref.c <- 1945
disease <- "Lung Cancer"

directory <- paste0("data_group/",disease,"/")
if(!file.exists(directory)) {
  dir.create(directory)
}
names <- c("death.M.hk", "death.M.cn.l", "death.M.cn.s",
           "death.F.hk", "death.F.cn.l", "death.F.cn.s")
pop.names <- c("male.hk", "male.cn.l", "male.cn.s",
               "female.hk", "female.cn.l", "female.cn.s")



## load death and population data

load("death1998_2020_updated.RData")
load("birthplace_gender_pop(85)_1995-2030_grouped.RData")


######
read.csv("lookup_tab.csv") -> lookup

lookup %>%
  filter(Name==disease) %>%
  .$Code %>%
  strsplit(split=", ") %>% 
  `[[`(1) -> code

death %>%
  filter(essential.cause %in% code) %>%
  select(-essential.cause) -> death

caselist <- list()
indices <- c()
sex <- c("M", "F")
born <- c("hk", "cn.l", "cn.s")
case.threshold <- .03
age.threshold <- .1

# table(death$duration.inHK)

# confirm the case threshold
for (i in 1:2) {
  #i <- 1
  death %>%
    filter(gender==sex[i] & hk.born) %>% 
    select(-duration.inHK) -> caselist[[3*i-2]]
  indices <- 
    c(indices, 
      nrow(caselist[[3*i-2]]) / 
        ((max(caselist[[3*i-2]]$age)-min(caselist[[3*i-2]]$age)+1)*
           (to-from+1)) > case.threshold)
  
  death %>%
    filter(gender==sex[i] & cn.born & duration.inHK > 10) %>% 
    select(-duration.inHK) -> caselist[[3*i-1]]
  indices <- 
    c(indices, nrow(caselist[[3*i-1]]) / 
        ((max(caselist[[3*i-1]]$age)-min(caselist[[3*i-1]]$age)+1)*
           (to-from+1)) > case.threshold)
  
  death %>%
    filter(gender==sex[i] & cn.born & duration.inHK <= 10) %>% 
    select(-duration.inHK) -> caselist[[3*i]]
  indices <- 
    c(indices, nrow(caselist[[3*i]]) / 
        ((max(caselist[[3*i-2]]$age)-min(caselist[[3*i]]$age)+1)*
           (to-from+1)) > case.threshold)
}

# caselist: 1. hk.male, 2.cn.male.l, 3.cn.male.s, 
#           4. hk.female, 5. cn.female.l, 6. cn.female.s

# rm(death, lookup, code)

# elimate small samples & strim matrix and population
source("makeup_cut.R")

age.min <- NULL
age.max <- NULL

source("seq_range.R")

# indices
for (i in 1:6) {
  #i = 1
  if(indices[i]) {
    assign(paste0("death.",rep(sex,each=3)[i],".",
                  rep(born,times=2)[i]), caselist[[i]])
    'result <- seq_range(table(caselist[[i]]$age), 
                        (to-from+1)*age.threshold, 
                        lasting=10, type=">=")
    age.min <- c(age.min, 
                 as.numeric(names(result))[1])
    age.max <- c(age.max, 
                 as.numeric(names(result))[2])'
  } else {
    rm(list=c(pop.names[i]))
  }
}
rm(caselist)

min.age <- 34
max.age <- 85
# rm(age.min, age.max, case.threshold, age.threshold)

for (i in 1:6) {
  #i=4
  if(exists(names[i])) {
    assign(paste0("mat.",names[i]), 
           makeup_cut(as.data.frame(get(names[i])), 
                      var1="age", var2="year", 
                      min.age=min.age, max.age=max.age)[-1,])
    assign(paste0("mat.pop.",pop.names[i]), 
           get(pop.names[i]) %>% filter(age %in% min.age:max.age) %>%
             select((one_of(paste0("y",from:to)))) %>% as.matrix %>% .[-1,])
    save(list=c(paste0("mat.",names[i]),
                paste0("mat.pop.",pop.names[i]), "min.age", "max.age"),
         file=paste0(directory, "mat.",pop.names[i],".",disease,".RData"))
    write.csv(get(paste0("mat.",names[i])), 
              file=paste0(directory, "mat.",pop.names[i],".",disease,".csv"))
    write.csv(get(paste0("mat.pop.",pop.names[i])), 
              file=paste0(directory, "mat.pop.",pop.names[i],".",disease,".csv"))
    rm(list=c(names[i], pop.names[i]))
  }
}


if(gender=="M"){
  exd1<-cbind(melt(mat.death.M.cn.l),melt(mat.pop.male.cn.l[,c(1:23)])[,3])
  exd2<-cbind(melt(mat.death.M.hk),melt(mat.pop.male.hk[,c(1:23)])[,3])
  exd3<-cbind(melt(mat.death.M.cn.s),melt(mat.pop.male.cn.s[,c(1:23)])[,3])
}else{
  exd1<-cbind(melt(mat.death.F.cn.l),melt(mat.pop.female.cn.l[,c(1:23)])[,3])
  exd2<-cbind(melt(mat.death.F.hk),melt(mat.pop.female.hk[,c(1:23)])[,3])
  exd3<-cbind(melt(mat.death.F.cn.s),melt(mat.pop.female.cn.s[,c(1:23)])[,3])
}

names(exd1)<-c('A','P','D','Y')
names(exd2)<-c('A','P','D','Y')
names(exd3)<-c('A','P','D','Y')

ex.1 <- apc.fit( exd1, npar= npar, 
                 model="ns", dr.extr="Y", 
                 parm="ACP", scale=10^5,
                 ref.c=ref.c,dist = "poisson")
ex.2 <- apc.fit( exd2, npar= npar, 
                 model="ns", dr.extr="Y", 
                 parm="ACP", scale=10^5,
                 ref.c=ref.c,dist = "poisson")
ex.3 <- apc.fit( exd3, npar= npar, 
                 model="ns", dr.extr="Y", 
                 parm="ACP", scale=10^5,
                 
                 ref.c=ref.c,dist = "poisson")

par(mfrow=c(1,1))
apc.plot(ex.3, col="blue", lty=c(3,3,3), r.txt="Annual deaths/100,000 population")
matshade(ex.3$Age[,1], ex.3$Age[,-1], col="blue")
pc.matshade(ex.3$Per[,1], ex.3$Per[,-1], col="blue")
pc.matshade(ex.3$Coh[,1], ex.3$Coh[,-1], col="blue")

apc.lines(ex.1, lty=c(3,3,3),col='black')
matshade(ex.1$Age[,1], ex.1$Age[,-1])
pc.matshade(ex.1$Per[,1], ex.1$Per[,-1])
pc.matshade(ex.1$Coh[,1], ex.1$Coh[,-1])

apc.lines(ex.2, col="red", lty=c(3,3,3))
matshade(ex.2$Age[,1], ex.2$Age[,-1], col='red')
pc.matshade(ex.2$Per[,1], ex.2$Per[,-1], col='red')
pc.matshade(ex.2$Coh[,1], ex.2$Coh[,-1], col='red')


legend("topright",lty=1, lwd=2, col=c('blue','black','red'),
       legend=c('Female immigrants<= 10 years','Female immigrants> 10 years','Female locals'))
mtext(side = 3, line = 0, 
      "  (a)Age effects                      (b)Cohort effects                (c)Period effects", 
      las = 0, adj = 0, cex = 1.4)


plot(1998:2020,colSums(mat.death.F.cn.l)/colSums(mat.pop.female.cn.l))
plot(1998:2020,colSums(mat.death.F.cn.s)/colSums(mat.pop.female.cn.s))
plot(1998:2020,colSums(mat.death.F.hk)/colSums(mat.pop.female.hk))








################# Prediction#########################
library(BAPC)
library(INLA)
data(whostandard)

my.weights = whostandard[8:18,2]/sum(whostandard[8:18,2])
agegroup = c("35-39", "40-44",
             "45-49", "50-54", "55-59", "60-64",
             "65-69", "70-74", "75-79", "80-84","85+")
row=c("1998", "1999", "2000", "2001", "2002", "2003", "2004", 
      "2005", "2006", "2007", "2008","2009", "2010", "2011", "2012",
      "2013", "2014", "2015", "2016", "2017", "2018", "2019","2020", 
      "2021","2022", "2023", "2024","2025","2026", "2027","2028","2029", "2030"  )

group.death.F.l=data.frame(t(rbind(rowsum(mat.death.F.cn.l[-51,], rep(seq_len(nrow(mat.death.F.cn.l[-51,]) / 5), each=5)),mat.death.F.cn.l[51,])))
group.death.F.l[24:33,]=NA
rownames(group.death.F.l)=row

group.death.F.s=data.frame(t(rbind(rowsum(mat.death.F.cn.s[-51,], rep(seq_len(nrow(mat.death.F.cn.s[-51,]) / 5), each=5)),mat.death.F.cn.s[51,])))
group.death.F.s[24:33,]=NA
rownames(group.death.F.s)=row

group.death.F.hk=data.frame(t(rbind(rowsum(mat.death.F.hk[-51,], rep(seq_len(nrow(mat.death.F.hk[-51,]) / 5), each=5)),mat.death.F.hk[51,])))
group.death.F.hk[24:33,]=NA
rownames(group.death.F.hk)=row

group.death.M.l=data.frame(t(rbind(rowsum(mat.death.M.cn.l[-51,], rep(seq_len(nrow(mat.death.M.cn.l[-51,]) / 5), each=5)),mat.death.M.cn.l[51,])))
group.death.M.l[24:33,]=NA
rownames(group.death.M.l)=row

group.death.M.s=data.frame(t(rbind(rowsum(mat.death.M.cn.s[-51,], rep(seq_len(nrow(mat.death.M.cn.s[-51,]) / 5), each=5)),mat.death.M.cn.s[51,])))
group.death.M.s[24:33,]=NA
rownames(group.death.M.s)=row

group.death.M.hk=data.frame(t(rbind(rowsum(mat.death.M.hk[-51,], rep(seq_len(nrow(mat.death.M.hk[-51,]) / 5), each=5)),mat.death.M.hk[51,])))
group.death.M.hk[24:33,]=NA
rownames(group.death.M.hk)=row

group.pop.F.l=data.frame(t(rbind(rowsum(mat.pop.female.cn.l[-51,], rep(seq_len(nrow(mat.pop.female.cn.l[-51,]) / 5), each=5)),mat.pop.female.cn.l[51,])))
rownames(group.pop.F.l)=row

group.pop.F.s=data.frame(t(rbind(rowsum(mat.pop.female.cn.s[-51,], rep(seq_len(nrow(mat.pop.female.cn.s[-51,]) / 5), each=5)),mat.pop.female.cn.s[51,])))
rownames(group.pop.F.s)=row

group.pop.F.hk=data.frame(t(rbind(rowsum(mat.pop.female.hk[-51,], rep(seq_len(nrow(mat.pop.female.hk[-51,]) / 5), each=5)),mat.pop.female.hk[51,])))
rownames(group.pop.F.hk)=row

group.pop.M.l=data.frame(t(rbind(rowsum(mat.pop.male.cn.l[-51,], rep(seq_len(nrow(mat.pop.male.cn.l[-51,]) / 5), each=5)),mat.pop.male.cn.l[51,])))
rownames(group.pop.M.l)=row

group.pop.M.s=data.frame(t(rbind(rowsum(mat.pop.male.cn.s[-51,], rep(seq_len(nrow(mat.pop.male.cn.s[-51,]) / 5), each=5)),mat.pop.male.cn.s[51,])))
rownames(group.pop.M.s)=row

group.pop.M.hk=data.frame(t(rbind(rowsum(mat.pop.male.hk[-51,], rep(seq_len(nrow(mat.pop.male.hk[-51,]) / 5), each=5)),mat.pop.male.hk[51,])))
rownames(group.pop.M.hk)=row


APC.F.l = APCList(group.death.F.l, group.pop.F.l, gf=5, agelab=agegroup)
res.F.l = BAPC(APC.F.l, predict=list(npredict=10, retro=FALSE),
                 stdweight=my.weights)

APC.F.s = APCList(group.death.F.s, group.pop.F.s, gf=5, agelab=agegroup)
res.F.s = BAPC(APC.F.s, predict=list(npredict=10, retro=FALSE),
               stdweight=my.weights)

APC.F.hk = APCList(group.death.F.hk, group.pop.F.hk, gf=5, agelab=agegroup)
res.F.hk = BAPC(APC.F.hk, predict=list(npredict=10, retro=FALSE),
               stdweight=my.weights)

APC.M.l = APCList(group.death.M.l, group.pop.M.l, gf=5, agelab=agegroup)
res.M.l = BAPC(APC.M.l, predict=list(npredict=10, retro=FALSE),
               stdweight=my.weights)

APC.M.s = APCList(group.death.M.s, group.pop.M.s, gf=5, agelab=agegroup)
res.M.s = BAPC(APC.M.s, predict=list(npredict=10, retro=FALSE),
               stdweight=my.weights)

APC.M.hk = APCList(group.death.M.hk, group.pop.M.hk, gf=5, agelab=agegroup)
res.M.hk = BAPC(APC.M.hk, predict=list(npredict=10, retro=FALSE),
               stdweight=my.weights)


par(mfrow=c(3,2), mar=c(4,4,4,1), las=1, cex.lab=1.1)

plotBAPC(res.M.l, scale=10^5, type="ageStdProj",
         probs= seq(0.05, 0.95, by=0.05),obs.lwd=1.5,  
         showdata=TRUE, main="Male immigrants> 10 years", ylim=c(0,200))
plotBAPC(res.F.l, scale=10^5, type="ageStdProj",  
         probs= seq(0.05, 0.95, by=0.05), obs.lwd=1.5, 
         showdata=TRUE, main="Female immigrants> 10 years", ylim=c(0,200))

plotBAPC(res.M.s, scale=10^5, type="ageStdProj", 
         probs= seq(0.05, 0.95, by=0.05),obs.lwd=1.5, 
         showdata=TRUE, main="Male immigrants<= 10 years", ylim=c(0,200))
plotBAPC(res.F.s, scale=10^5, type="ageStdProj", 
         probs= seq(0.05, 0.95, by=0.05),obs.lwd=1.5, 
         showdata=TRUE, main="Female immigrants<= 10 years", ylim=c(0,200))

plotBAPC(res.M.hk, scale=10^5, type="ageStdProj", 
         probs= seq(0.05, 0.95, by=0.05),obs.lwd=1.5, 
         showdata=TRUE, main="Male locals", ylim=c(0,140))
plotBAPC(res.F.hk, scale=10^5, type="ageStdProj", 
         probs= seq(0.05, 0.95, by=0.05),obs.lwd=1.5, 
         showdata=TRUE, main="Female locals", ylim=c(0,140))



