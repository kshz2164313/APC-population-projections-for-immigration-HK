library(tidyverse)
library(readxl)
library(splines)
rm(list=ls())
#sample.year <- c(2001, 2006, 2011, 2016)
all.year <- c(1995:2020)
spar <- 0.125

#male
matrix_male <- read.csv("age_matrix_male_85.csv", header =T)
rownames(matrix_male) <- matrix_male[,1]
matrix_male <- matrix_male[,-1]
matrix_male[,-c(7,12,17,22)] <- matrix_male[,-c(7,12,17,22)] * 1000
colnames(matrix_male) = paste("year.", all.year, sep = "")
rownames(matrix_male) = paste("age.", c(0:85), sep = "")

#female
matrix_female <- read.csv("age_matrix_female_85.csv", header =T)
rownames(matrix_female) <- matrix_female[,1]
matrix_female <- matrix_female[,-1]
matrix_female[,-c(7,12,17,22)] <- matrix_female[,-c(7,12,17,22)] * 1000
colnames(matrix_female) = paste("year.", all.year, sep = "")
rownames(matrix_female) = paste("age.", c(0:85), sep = "")

hk.source.data.1996 = read_excel("birth_place_age_gender_1996.xlsx")
hk.source.data.2001 = read_excel("birth_place_age_gender_2001.xlsx")
hk.source.data.2006 = read_excel("birth_place_age_gender_2006.xlsx")
hk.source.data.2011 = read_excel("birth_place_age_gender_2011.xlsx")
hk.source.data.2016 = read_excel("birth_place_age_gender_2016.xlsx")

trans.age.range = function(source.data = the.data, year = year){
  # Female: 4-5
  if(gender=="F"){
    age.info = matrix_female
    this.data = source.data[,c(4:5)] %>% as.data.frame
    #print("F")
  } 
  # Male: 2-3
  if(gender=="M"){
    age.info = matrix_male
    this.data = source.data[,c(2:3)] %>% as.data.frame
    #print("M")
  } 
  this.age.info = c(age.info[,(year-1995+1)])
  the.length = dim(source.data)[1]
  ##
  temp.index = NULL
  temp.list = NULL
  temp.sum = NULL
  temp.prop = NULL
  temp.data = NULL
  data.matrix = matrix(data = NA, nrow = length(this.age.info), 
                       ncol = dim(this.data)[2])
  for(j in 1:the.length){
    temp.index = c((j*5-4):(j*5))
    if(j == the.length)
      temp.index = c((j*5-4):(86))
    #
    temp.list = this.age.info[temp.index]
    temp.list = c(unlist(temp.list))
    temp.sum = sum(temp.list)
    temp.prop = temp.list/temp.sum
    
    #
    temp.data = this.data[j,1]*temp.prop
    data.matrix[temp.index,1] = temp.data
    temp.data = this.data[j,2]*temp.prop
    data.matrix[temp.index,2] = temp.data
  }
  colnames(data.matrix) = paste(c("HongKong.", "OtherCN."), year, sep = "")
  rownames(data.matrix) = paste("age.", c(0:85), sep = "")
  return(data.matrix)
}

sample.year <- c(1996, 2001, 2006, 2011, 2016)

gender = "F"

## make the matrix
hk.local.matrix = matrix(data = NA, nrow = 86, ncol = length(sample.year))
hk.immigrant.matrix = matrix(data = NA, nrow = 86, ncol = length(sample.year))

# 1996
temp.matrix = trans.age.range(source.data = hk.source.data.1996,year = 1996)
hk.local.matrix[,1] = temp.matrix[,1]
hk.immigrant.matrix[,1] = temp.matrix[,2]

# 2001
temp.matrix = trans.age.range(source.data = hk.source.data.2001, year = 2001)
hk.local.matrix[,2] = temp.matrix[,1]
hk.immigrant.matrix[,2] = temp.matrix[,2]

# 2006
temp.matrix = trans.age.range(source.data = hk.source.data.2006, year = 2006)
hk.local.matrix[,3] = temp.matrix[,1]
hk.immigrant.matrix[,3] = temp.matrix[,2]

# 2011
temp.matrix = trans.age.range(source.data = hk.source.data.2011, year = 2011)
hk.local.matrix[,4] = temp.matrix[,1]
hk.immigrant.matrix[,4] = temp.matrix[,2]

# 2016
temp.matrix = trans.age.range(source.data = hk.source.data.2016, year = 2016)
hk.local.matrix[,5] = temp.matrix[,1]
hk.immigrant.matrix[,5] = temp.matrix[,2]

## start loop for the local
all.year=c(1995:2030)
temp.age.data = NULL
processed.hk.local.matrix = matrix(data = NA, nrow = 86, ncol = length(all.year))
#dim(processed.hk.local.matrix)
#      i = 2
for(i in 1:86){
  temp.age.data = c((unlist(hk.local.matrix[i,c(1:5)])))
  temp.smooth.func = smooth.spline(x = sample.year, 
                                   y = temp.age.data %>% log(), spar = spar)
  temp.age.pred = predict(temp.smooth.func, all.year)$y %>% exp() %>% round()
  temp.age.pred[which(temp.age.pred<0)] <- 0
  #
  processed.hk.local.matrix[i,] = temp.age.pred
}
colnames(processed.hk.local.matrix) = paste0("y", all.year)
rownames(processed.hk.local.matrix) = c(1:86)
processed.hk.local.matrix = as.data.frame(processed.hk.local.matrix)
#  check plot
plot(all.year, colSums(processed.hk.local.matrix))
##

## for the other chinese
#
temp.age.data = NULL
processed.hk.immigrant.matrix = matrix(data = NA, nrow = 86, ncol = length(all.year))
#dim(processed.hk.immigrant.matrix)
#      i = 2
for(i in 1:86){
  temp.age.data = c((unlist(hk.immigrant.matrix[i,c(1:5)])))
  temp.smooth.func = smooth.spline(x = sample.year, 
                                   y = temp.age.data %>% log(), spar = spar)
  temp.age.pred = predict(temp.smooth.func, all.year)$y %>% exp() %>% round()
  temp.age.pred[which(temp.age.pred<0)] <- 0
  #
  processed.hk.immigrant.matrix[i,] = temp.age.pred
}
colnames(processed.hk.immigrant.matrix) = paste0("y", all.year)
rownames(processed.hk.immigrant.matrix) = c(1:86)
processed.hk.immigrant.matrix = as.data.frame(processed.hk.immigrant.matrix)
#  check plot
plot(all.year, colSums(processed.hk.immigrant.matrix))
plot(all.year, colSums(processed.hk.local.matrix))
plot(all.year, colSums(matrix_female))
##
female.hk <- processed.hk.local.matrix
female.cn <- processed.hk.immigrant.matrix

gender = "M"
## make the matrix
hk.local.matrix = matrix(data = NA, nrow = 86, ncol = length(sample.year))
hk.immigrant.matrix = matrix(data = NA, nrow = 86, ncol = length(sample.year))

# 1996
temp.matrix = trans.age.range(source.data = hk.source.data.1996,year = 1996)
hk.local.matrix[,1] = temp.matrix[,1]
hk.immigrant.matrix[,1] = temp.matrix[,2]

# 2001
temp.matrix = trans.age.range(source.data = hk.source.data.2001, year = 2001)
hk.local.matrix[,2] = temp.matrix[,1]
hk.immigrant.matrix[,2] = temp.matrix[,2]

# 2006
temp.matrix = trans.age.range(source.data = hk.source.data.2006, year = 2006)
hk.local.matrix[,3] = temp.matrix[,1]
hk.immigrant.matrix[,3] = temp.matrix[,2]

# 2011
temp.matrix = trans.age.range(source.data = hk.source.data.2011, year = 2011)
hk.local.matrix[,4] = temp.matrix[,1]
hk.immigrant.matrix[,4] = temp.matrix[,2]

# 2016
temp.matrix = trans.age.range(source.data = hk.source.data.2016, year = 2016)
hk.local.matrix[,5] = temp.matrix[,1]
hk.immigrant.matrix[,5] = temp.matrix[,2]

## start loop for the local
temp.age.data = NULL
processed.hk.local.matrix = matrix(data = NA, nrow = 86, ncol = length(all.year))
#dim(processed.hk.local.matrix)
#      i = 2
for(i in 1:86){
  temp.age.data = c((unlist(hk.local.matrix[i,c(1:5)])))
  temp.smooth.func = smooth.spline(x = sample.year, 
                                   y = temp.age.data %>% log(), spar = spar)
  temp.age.pred = predict(temp.smooth.func, all.year)$y %>% exp() %>% round()
  temp.age.pred[which(temp.age.pred<0)] <- 0
  #
  processed.hk.local.matrix[i,] = temp.age.pred
}
colnames(processed.hk.local.matrix) = paste0("y", all.year)
rownames(processed.hk.local.matrix) = c(1:86)
processed.hk.local.matrix = as.data.frame(processed.hk.local.matrix)
#  check plot
plot(all.year, colSums(processed.hk.local.matrix))
##

## for the other chinese
#
temp.age.data = NULL
processed.hk.immigrant.matrix = matrix(data = NA, nrow = 86, ncol = length(all.year))
#dim(processed.hk.immigrant.matrix)
#      i = 2
for(i in 1:86){
  temp.age.data = c((unlist(hk.immigrant.matrix[i,c(1:5)])))
  temp.smooth.func = smooth.spline(x = sample.year, 
                                   y = temp.age.data %>% log(), spar = spar)
  temp.age.pred = predict(temp.smooth.func, all.year)$y %>% exp() %>% round()
  temp.age.pred[which(temp.age.pred<0)] <- 0
  #
  processed.hk.immigrant.matrix[i,] = temp.age.pred
}
colnames(processed.hk.immigrant.matrix) = paste0("y", all.year)
rownames(processed.hk.immigrant.matrix) = c(1:86)
processed.hk.immigrant.matrix = as.data.frame(processed.hk.immigrant.matrix)
#  check plot
plot(all.year, colSums(processed.hk.immigrant.matrix))
##
male.hk <- processed.hk.local.matrix
male.cn <- processed.hk.immigrant.matrix


####### subgroup
subgroup_ref <- read_xlsx("processed_length_group.xlsx")
subgroup_ref$num[which(subgroup_ref$num=="-")] <- 0
subgroup_ref$num <- as.numeric(subgroup_ref$num)

for(sex in c("male", "female")){
  #sex <- "male"
  for(group in c("s", "l")){
    #group <- "l"
    assign(paste0("cn.",sex,".",group,".matrix"), NULL)
    for(i in seq(2001,2016,5)){
      #i <- 2011
      ref_set <- get(paste0(sex,".cn"))
      #ref_set <- get(paste0("matrix_", sex))
      
      start <- seq(0,85,5)+1
      end <- c(seq(4,84,5), 85)+1
      duration <- end-start+1
      if(i==2011 & group=="s"){
        duration[1] <- sum(duration[1:3])
        duration <- duration[-c(2:3)]
      }
      
      data.frame(num=ref_set[,grepl(i,names(ref_set))],
                 ind=rep(1:length(duration), times = duration)) %>%
        group_by(ind) %>%
        summarise(n=sum(num)) -> temp
      if(i==2011 & group=="l") temp <- temp[-c(1,2), ]
      
      subgroup_ref %>%
        filter(gender==sex, grepl(group, length), year==i) -> temp_group
      
      temp_pop <- temp_group$num/temp$n
      if(i==2011 & group=="l") temp_pop <- c(0,0,temp_pop)
      rep(temp_pop, times = duration) *
        ref_set[,grepl(i, names(ref_set))] -> temp_pop
      
      assign(paste0("cn.",sex,".",group,".matrix"), 
             cbind(get(paste0("cn.",sex,".",group,".matrix")),
                   temp_pop))
    }
  }
}

sample.year <- seq(2001, 2016, 5)
## start loop for the local
temp.age.data = NULL
male.cn.s = matrix(data = NA, nrow = 86, ncol = length(all.year))
#      i = 2
for(i in 1:86){
  temp.age.data = c((unlist(cn.male.s.matrix[i,c(1:4)])))
  temp.smooth.func = smooth.spline(x = sample.year, 
                                   y = temp.age.data %>% log1p(), spar = spar)
  temp.age.pred = predict(temp.smooth.func, all.year)$y %>% expm1() %>% round()
  temp.age.pred[which(temp.age.pred<0)] <- 0
  #
  male.cn.s[i,] = temp.age.pred
}
colnames(male.cn.s) = paste0('y',all.year)
rownames(male.cn.s) = c(1:86)
male.cn.s = as.data.frame(male.cn.s)
#  check plot
plot(all.year, colSums(male.cn.s))
##

temp.age.data = NULL
male.cn.l = matrix(data = NA, nrow = 86, ncol = length(all.year))
#      i = 2
for(i in 1:86){
  temp.age.data = c((unlist(cn.male.l.matrix[i,c(1:4)])))
  temp.smooth.func = smooth.spline(x = sample.year, 
                                   y = temp.age.data %>% log1p(), spar = spar)
  temp.age.pred = predict(temp.smooth.func, all.year)$y %>% expm1() %>% round()
  temp.age.pred[which(temp.age.pred<0)] <- 0
  #
  male.cn.l[i,] = temp.age.pred
}
colnames(male.cn.l) = paste0('y', all.year)
rownames(male.cn.l) = c(1:86)
male.cn.l = as.data.frame(male.cn.l)
#  check plot
plot(all.year, colSums(male.cn.l))
##

### Female

## start loop for the local
temp.age.data = NULL
female.cn.s = matrix(data = NA, nrow = 86, ncol = length(all.year))
#      i = 2
for(i in 1:86){
  temp.age.data = c((unlist(cn.female.s.matrix[i,c(1:4)])))
  temp.smooth.func = smooth.spline(x = sample.year, 
                                   y = temp.age.data %>% log1p(), spar = spar)
  temp.age.pred = predict(temp.smooth.func, all.year)$y %>% expm1() %>% round()
  temp.age.pred[which(temp.age.pred<0)] <- 0
  #
  female.cn.s[i,] = temp.age.pred
}
colnames(female.cn.s) = paste0('y', all.year)
rownames(female.cn.s) = c(1:86)
female.cn.s = as.data.frame(female.cn.s)
#  check plot
plot(all.year, colSums(female.cn.s))
##

temp.age.data = NULL
female.cn.l = matrix(data = NA, nrow = 86, ncol = length(all.year))
#      i = 2
for(i in 1:86){
  temp.age.data = c((unlist(cn.female.l.matrix[i,c(1:4)])))
  temp.smooth.func = smooth.spline(x = sample.year, 
                                   y = temp.age.data %>% log1p(), spar = spar)
  temp.age.pred = predict(temp.smooth.func, all.year)$y %>% expm1() %>% round()
  temp.age.pred[which(temp.age.pred<0)] <- 0
  #
  female.cn.l[i,] = temp.age.pred
}
colnames(female.cn.l) = paste0( 'y',all.year)
rownames(female.cn.l) = c(1:86)
female.cn.l = as.data.frame(female.cn.l)
#  check plot
plot(all.year, colSums(female.cn.l))
##

male.cn.l <- round(male.cn.l/(male.cn.l+male.cn.s)*male.cn)
female.cn.l <- round(female.cn.l/(female.cn.l+female.cn.s)*female.cn)
male.cn.s <- round(male.cn.s/(male.cn.l+male.cn.s)*male.cn)
female.cn.s <- round(female.cn.s/(female.cn.l+female.cn.s)*female.cn)

plot(all.year, colSums(female.cn.l+female.cn.s+male.cn.l+male.cn.s))
plot(all.year, colSums(female.cn.l+female.cn.s))
plot(all.year, colSums(male.cn.l+male.cn.s))
plot(all.year, colSums(female.cn.l+male.cn.l))
plot(all.year, colSums(female.cn.s+male.cn.s))
plot(all.year, colSums(male.cn+female.cn))

female.hk$age <- 0:85
female.cn$age <- 0:85
male.hk$age <- 0:85
male.cn$age <- 0:85

save(female.hk, female.cn, male.hk, male.cn,
     file = "birthplace_gender_pop(85)_1995-2030.RData")

male.cn.s$age <- 0:85
male.cn.l$age <- 0:85
female.cn.s$age <- 0:85
female.cn.l$age <- 0:85

save(female.hk, female.cn.s, female.cn.l, male.hk, male.cn.s, male.cn.l,
     file = "birthplace_gender_pop(85)_1995-2030_grouped.RData")

