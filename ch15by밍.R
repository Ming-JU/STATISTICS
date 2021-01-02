
# #예제 ---------------------------------------------------------------------


# 예제1 ---------------------------------------------------------------------
# A, B, C 
# 1:2:1
# 18, 55, 27

o = c(18,55,27)
k = length(o)
p = c(1/4,1/2,1/4)
n = sum(o)
e = p*n
d = (o-e)^2/e
chisq0 = sum(d)
alpha = 0.05
chisq_alpha = qchisq(1-alpha,k-1)
pvalue = 1-pchisq(chisq0,k-1)
m = matrix(c(o,e,d),nrow = 3, byrow = T
           ,dimnames = list(c("O","E","D"),c("A","B","C")))

s = apply(m,1,sum)
M = cbind(m,s)
m
M



# 예제2 ---------------------------------------------------------------------
oA = c(37,24,19)
oB = c(17,33,20)
namelist = list(c("A","B"),c("양호","보통","불량"))
o = matrix(c(oA,oB),nrow = 2,ncol = 3,byrow = T,
           dimnames = namelist)
print(o)
row_sum = apply(o,1,sum)
col_sum = apply(o,2,sum)
e = (row_sum%*%t(col_sum))/sum(o)
dimnames(e) = namelist

d = (o-e)^2/e
chisq0 = sum(d)
chisq0

alpha = 0.05

df = (length(row_sum)-1)*(length(col_sum)-1)
chisq_alpha = qchisq(1-alpha,df)
print(c(chisq0,chisq_alpha))
p_value = 1-pchisq(chisq0,df)
p_value

r = qchisq(1-alpha,df)
r

print(c(r,chisq0))
# 예제5 ---------------------------------------------------------------------


# 예제6 ---------------------------------------------------------------------

oA = c(32,268)
oB = c(51,199)
oC = c(67,233)
oD = c(83,267)

namelist = list(c("사무원","교육자","기업인","상인"),c("알콜중독","정상"))
o = matrix(c(oA,oB,oC,oD),nrow = 4,ncol = 2,byrow = T,
           dimnames = namelist)
print(o)

row_sum = apply(o,1,sum)
col_sum = apply(o,2,sum)
e = (row_sum%*%t(col_sum))/sum(o)
dimnames(e) = namelist

d = (o-e)^2/e

chisq0 = sum(d)

alpha = 0.05

df = (length(row_sum)-1)*(length(col_sum)-1)
chisq_alpha = qchisq(1-alpha,df)

p_value = 1-pchisq(chisq0,df)
p_value

r = qchisq(1-alpha,df)
r
print(c(chisq_alpha,chisq0))
o


# 예제6 ---------------------------------------------------------------------

oA = c(32,268)
oB = c(51,199)
oC = c(67,233)
oD = c(83,267)
namelist = list(c("사무원","교육자","기업인","상인"),c("알콜중독","정상"))
o = matrix(c(oA,oB,oC,oD),nrow = 4,ncol = 2,byrow = T,
           dimnames = namelist)

row_sum = apply(o,1,sum)
col_sum = apply(o,2,sum)

e = row_sum%*%t(col_sum)/sum(o)

d = (o-e)^2/e
df = (length(row_sum)-1)*(length(col_sum)-1)

chisq0 = sum(d)

alpha = 0.05

chisq_alpha = qchisq(1-alpha,df)
pvalue = 1-pchisq(chisq0,df)


# 예제7 ---------------------------------------------------------------------

oA = c(84,16)
oB = c(132,18)

namelist = list(c("처리함","처리안함"),c("싹이틈","싹이 트지않음"))
o = matrix(c(oA,oB),nrow=2,ncol = 2,byrow =T, dimnames = namelist)

row_sum = apply(o,1,sum)
col_sum = apply(o,2,sum)

e = ((row_sum)%*%t(col_sum))/sum(o)
dimnames(e) = namelist

d = (o-e)^2/e

chisq0 = sum(d)

alpha = 0.05
df = (length(row_sum-1))*(length(col_sum-1))
chisq_alpha = qchisq(1-alpha,df)

pvalue = 1-pchisq(chisq0,df)
r = qchisq(1-alpha,df)


r
o
chisq0
pvalue


# 예제8 ---------------------------------------------------------------------

oA = c(378,237,26)
oB = c(388,196,25)

namelist = list(c("남자","여자"),c("너무많다","적당하다","너무적다"))

o = matrix(c(oA,oB),nrow = 2, ncol =3, byrow = T,dimnames = namelist)

row_sum = apply(o,1,sum)
col_sum = apply(o,2,sum)

e = row_sum%*%t(col_sum)/sum(o)
d = (o-e)^2/e
chisq0 = sum(d)

alpha = 0.05

df = (length(row_sum)-1)*(length(col_sum)-1)

chisq_alpha = qchisq(1-alpha,df)
pvalue = 1 - pchisq(chisq0,df)

print(c(chisq_alpha,chisq0,pvalue))

r = qchisq(1-alpha,df)
r


# 연습문제 --------------------------------------------------------------------

# 2.1 ---------------------------------------------------------------------
o = c(38,61,54,65,55,37)
p = c(1/6,1/6,1/6,1/6,1/6,1/6)
n = sum(o)
e = n*p
d = (o-e)^2/e

chisq0 = sum(d)
k = length(o)
alpha= 0.05

chisq_alpha = qchisq(1-alpha,k-1)

if(chisq0>=chisq_alpha){
  print("reject")

}else{
  print("dontreject")
}

pvalue = 1-pchisq(chisq0,k-1)
pvalue






# 2.7 ---------------------------------------------------------------------


o <- c(52.09,54.46,52.68,51.68,53.83,47.21,44.36)
p <- c(1/7, 1/7,1/7,1/7,1/7,1/7,1/7)
n = sum(o)
e = n*p
d = (o-e)^2/e
k = length(o)
chisq0 = sum(d)

alpha = 0.01
chisq_alpha = qchisq(1-alpha,k-1)

if (chisq0 >= chisq_alpha) {
  print("H0기각")
} else {
  print("H0 기각못함")
}
pvalue <- 1 - pchisq(chisq0, k-1)
print(c(chisq0, pvalue))


# #2.9교수님 ------------------------------------------------------------------

cumulative = c(0,pnorm(c(48,60,72),60,15),1)
p = cumulative[2:5] - cumulative[1:4]
print(p)
o = c(16,28,36,20)
n = sum(o)
e = n*p
d = (o-e)^2/e
k =4
chisq0 = sum(d)
alpha = 0.05
chisq_alpha = qchisq(1-alpha,k-1)
print(c(chisq0,chisq_alpha))
pvalue = 1-pchisq(chisq0,k-1)
print(pvalue)


cumulative


# 2.9밍 --------------------------------------------------------------------

cumulative = c(0,pnorm(c(48,60,72),60,15),1)
p = cumulative[2:5]-cumulative[1:4]
print(p)
o = c(16,28,36,20)
n = sum(o)
e = n*p
d = (o-e)^2/e
k = 4
chisq0 = sum(d)
alpha = 0.05
chisq_alpha = qchisq(1-alpha,k-1)
chisq0
chisq_alpha

# #2.9연습 ------------------------------------------------------------------

cumulative = c(0,pnorm(c(48,60,72),60,15),1)
p = cumulative[2:5]-cumulative[1:4]

print(p)

o = c(16,28,36,20)
n = sum(o)

e = p*n

d = (o-e)^2/e

chisq0 = sum(d)
k = length(p)
alpha = 0.05
chisq_alpha = qchisq(1-alpha,k-1)

print(c(chisq_alpha,chisq0))

# 2.11 --------------------------------------------------------------------
n = 3
p0 = 0.4
p = dbinom(c(0,1,2,3),n,p0)
print(p)

o = c(19,32,22,7)
n = sum(o)
e = 80*p
d = (o-e)^2/e

chisq0 = sum(d)

#print(c(chisq0,chisq_alpha))
pvalue = 1-pchisq(chisq0,k-1)
print(pvalue)
print(c(chisq0,pvalue))


alpha = 0.05
alpha = 0.05

df = (length(row_sum) - 1) * (length(col_sum) - 1)
chisq_alpha = qchisq(1 - alpha, df)
print(c(chisq0, chisq_alpha))

pvalue = 1 - pchisq(chisq0, df)


#이항분포를 따르는가?의 모수추정

#자유도2인 chi분포

d = (e-o)^2/e
chisq0 = sum(d)
print(chisq0)
k = length(p)

pvalue = 1- pchisq(chisq0,2)
print(c(chisq0,pvalue))


# 2.11 --------------------------------------------------------------------

n = 3
p0 = 0.4
p = dbinom(c(0,1,2,3),n,p0)
print(p)

o = c(19,32,22,7)

n = sum(o)

e = n*p
d = (o-e)^2/e

chisq0 = sum(d)

k = length(p)

alpha = 0.05
chisq_alpha = qchisq(1-alpha,k-1)


# #### --------------------------------------------------------------------


# 3.1 ---------------------------------------------------------------------

oA = c(32,8)
oB = c(28,12)
oC = c(19,21)

namelist = list(c("양","시","토"),c("심각","없거나 심각안함"))
o = matrix(c(oA,oB,oC),nrow = 3,ncol = 2,byrow = T,
           dimnames = namelist)

row_sum = apply(o,1,sum)
col_sum = apply(o,2,sum)
e = (row_sum)%*%t(col_sum)/sum(o)
dimnames(e)=namelist

d = (o-e)^2/e
chisq0 = sum(d)
alpha = 0.1
df = (length(row_sum)-1)*(length(col_sum)-1)
chisq_alpha = qchisq(1-alpha,df)


pvalue = 1- pchisq(chisq0,df)

print(c(chisq_alpha,pvalue))
# 3.5 ---------------------------------------------------------------------
oA = c(18,17,6,4)
oB = c(11,14,16,6)

namelist = list(c("A","B"),c("없","약","중","심"))
o = matrix(c(oA,oB),nrow= 2,ncol = 4,byrow=T
           ,dimnames = namelist)
row_sum = apply(o,1,sum)
col_sum = apply(o,2,sum)

df = (length(row_sum)-1)*(length(col_sum)-1)

e = row_sum %*% t(col_sum)/sum(o)

d = (o-e)^2/e

chisq0 = sum(d)

alpha = 0.05

chisq_alpha= qchisq(1-alpha,df)

pvalue = 1-pchisq(chisq0,df)

if (chisq0>=chisq_alpha){
  print("reject")
}else{
  print("dont")
}
print(c(chisq_alpha,pvalue))

# 3.6 ---------------------------------------------------------------------

oA = c(58,57)
#oB = c(43,77)
oC = c(56,42)
#oD = c(45,75)

namelist = list(c("a","c"),c("죽은수","산수"))
o = matrix(c(oA,oC),nrow = 2,ncol =2,byrow = T,
           dimnames=namelist)

row_sum = apply(o,1,sum)
col_sum = apply(o,2,sum)

e = row_sum%*%t(col_sum)/sum(o)
dimnames(e) = namelist


df = (length(row_sum)-1)*(length(col_sum)-1)

alpha = 0.05

d = (o-e)^2/e

chisq0 = sum(d)

chisq_alpha = qchisq(1-alpha,df)
pvalue = 1-pchisq(chisq0,df)

if (chisq0>=chisq_alpha){
  print("reject")
}else{
  print("dont")
}


print(c(chisq_alpha,pvalue))
o

#(2)z검정

p1 <- 58/(58+57)
p2 <- 56/(56+42)
p <- (58+56)/(58+57+56+42)
n1 <- 58+57
n2 <- 56+42
alpha <- 0.05
z <- (p1-p2)/(sqrt(p*(1-p))*sqrt(1/n1 + 1/n2))
pvalue <- 2*(1-pnorm(abs(z)))
print(c(z, pvalue))
z_alpha = qnorm(1-alpha/2)

if(z>=z_alpha){
  print("reject")
}else{
  print("dont")
}
# 3.7 ---------------------------------------------------------------------
oA = c(38,15,7)
oB = c(22,32,16)
oC = c(15,30,25)

namelist = list(c("조치x","물리","운동"),c("상당손실","작은변화","상당한증가"))
o = matrix(c(oA,oB,oC),nrow = 3,ncol = 3,byrow = T,
           dimnames = namelist)


row_sum = apply(o,1,sum)
col_sum = apply(o,2,sum)
e = (row_sum%*%t(col_sum))/sum(o)
dimnames(e) = namelist

d = (o-e)^2/e

chisq0 = sum(d)

alpha = 0.05

df = (length(row_sum)-1)*(length(col_sum)-1)
chisq_alpha = qchisq(1-alpha,df)

p_value = 1-pchisq(chisq0,df)
p_value

r = qchisq(1-alpha,df)
r
print(c(chisq_alpha,chisq0))
o
if (chisq0>=chisq_alpha){
  print("reject")
}else{
  print("dont")
}

# 4.1 ---------------------------------------------------------------------

oA = c(59,108,17)
oB = c(70,63,3)

namelist = list(c("직접","위임"),c("증가","유지","감소"))

o = matrix(c(oA,oB),nrow= 2,ncol = 3 ,byrow = T,dimnames = namelist)

row_sum = apply(o,1,sum)
col_sum = apply(o,2,sum)

e = row_sum%*%t(col_sum)/sum(o)
d = (o-e)^2/e
chisq0 = sum(d)

alpha = 0.05

df = (length(row_sum)-1)*(length(col_sum)-1)

chisq_alpha = qchisq(1-alpha,df)
pvalue = 1 - pchisq(chisq0,df)

print(c(chisq_alpha,chisq0,pvalue))

r = qchisq(1-alpha,df)
r
if (chisq0>=chisq_alpha){
  print("reject")
}else{
  print("dont")
}


# 4.5 ---------------------------------------------------------------------

oA = c(65,118-65)
oB = c(59,135-59)
oC = c(48,90-48)
oD = c(43,92-43)

namelist = list(c("서","동","남","중"),c("선호","비선호"))
o = matrix(c(oA,oB,oC,oD),nrow = 4,ncol =2,byrow = T,
           dimnames=namelist)

row_sum = apply(o,1,sum)
col_sum = apply(o,2,sum)

e = row_sum%*%t(col_sum)/sum(o)
dimnames(e) = namelist


df = (length(row_sum)-1)*(length(col_sum)-1)

alpha = 0.05

d = (o-e)^2/e

chisq0 = sum(d)

chisq_alpha = qchisq(1-alpha,df)
pvalue = 1-pchisq(chisq0,df)

if (chisq0>=chisq_alpha){
  print("reject")
}else{
  print("dont")
}


print(c(chisq_alpha,pvalue))
o

if (chisq0>=chisq_alpha){
  print("reject")
}else{
  print("dont")
}