
# 예제 ----------------------------------------------------------------------


# 예제1 ---------------------------------------------------------------------


y1 = c(10, 15, 8, 12,15)
y2 = c(14, 18, 21, 15)
y3 = c(17, 16, 14, 15, 17, 15, 18)
y4 = c(12, 15, 17, 15, 16, 15)

k = 4

ni = c(length(y1), length(y2), length(y3), length(y4))
ni
yibar = c(mean(y1), mean(y2), mean(y3), mean(y4))
n = sum(ni)
y = c(y1, y2, y3, y4)
ybar = mean(y)


sst = sum((y - ybar)**2)
sst2 = var(y)*(n-1)

sstr = sum(ni*(yibar - ybar)**2)

sse = sum((ni - 1)*c(var(y1), var(y2), var(y3), var(y4)))

ss = c(sstr, sse, sst)

Df = c(k-1, n-k, n-1) # 자유도

ms = ss/Df

F0 = ms[1]/ms[2]

anovaTbl = data.frame("제곱" = c(sstr, sse, sst),
                      "자유도" = Df, "평균제곱" = ms, 
                      "F값" = c(F0, "", ""))
rownames(anovaTbl) = c("처리", "오차", "합계")
print(anovaTbl)

F0
y1 = c(10,15,8,12,15)
y2 = c(14,18,21,15)
y3 = c(17,16,14,15,17,15,18)
y4 = c(12,15,17,15,16,15)

y = c(y1,y2,y3,y4)
ni = c(length(y1),length(y2),length(y3),length(y4))
n = sum(ni)
k = 4
yibar = c(mean(y1),mean(y2),mean(y3),mean(y4))
ybar = mean(y)

sst = sum((y- ybar)^2)
sse = sum((ni-1)*c(var(y1),var(y2),var(y3),var(y4)))
sstr = sum(ni*(yibar - ybar)^2)
ss = c(sstr,sse,sst)
Df = c(k-1,n-k,n-1)

ms = ss/Df

F0 = ms[1]/ms[2]

aTb1 = data.frame("제곱합"=ss,"자유도"=Df,"평균제곱"=ms,
                  "F값"=c(F0,"",""))

rownames(aTb1) = c("처리","오차","합계")
#fit = lm(rownames,data)
#aTb1 = anova(fit)
print(aTb1)




# ㄴㄴ ----------------------------------------------------------------------



Fvalue <- ms[1]/ms[2]
Pvalue <- 1 - pf(Fvalue, k-1, n-k)
anovaTbl <- data.frame('제곱합'=ss, '자유도'=Df, '평균제곱'=ms, 'F값'=c(Fvalue,'',''), 'P-값'=c(Pvalue,'','') )
rownames(anovaTbl) <- c('처리', '오차', '합계')
print(anovaTbl)

ni <- c(32, 16, 16)
yibar <- c(81.06, 78.56, 87.81)
ybar <- sum(yibar*ni)/64
sstr <- sum(ni*(yibar-ybar)^2)


ni <- c(length(y1), length(y2), length(y3), length(y4))
group <- rep(c('A', 'B', 'C', 'D'), ni)
data <- data.frame( '마모도' = c(y1, y2, y3, y4), '코팅' = group )
print(data)

fit <- lm(마모도 ~ 코팅, data)
aTbl <- anova(fit)
print(aTbl)



# 2절 ----------------------------------------------------------------------


# #2.1 --------------------------------------------------------------------
y1<-c(6,10)
y2<-c(9,5)
y3<-c(9,7)
y4<-c(4,6)

k <- 4
ni <- c(length(y1), length(y2), length(y3), length(y4))
yibar <-c(mean(y1), mean(y2), mean(y3), mean(y4))
n <- sum(ni)
y <- c(y1, y2, y3, y4)
ybar <- mean(y)

sst <- sum((y-ybar)^2)
sst2 <- var(y)*(n-1)
sstr <- sum(ni*(yibar-ybar)^2)
sse <- sum((ni-1)*c(var(y1), var(y2), var(y3), var(y4)))


ss<- c(sstr, sse, sst)
Df <- c(k-1, n-k, n-1)
ms <- ss/Df
F0 <-ms[1]/ms[2]

anovaTb1 <- data.frame("제곱합"= ss, "자유도"= Df, "평균"=ms, "오차"=c(F0,"",""))
rownames(anovaTb1) <- c("처리", "오차", "합계")
print(anovaTb1)


# #2.1 --------------------------------------------------------------------
y1 = c(6,10)
y2 = c(9,5)
y3 = c(9,7)
y4 = c(4,6)

y = c(y1,y2,y3,y4)
ni = c(length(y1),length(y2),length(y3),length(y4))
yibar = c(mean(y1),mean(y2),mean(y3),mean(y4))
k = 4
ybar = mean(y)

sst = sum((y-ybar)^2)
sse = sum((ni-1)*(c(var(y1),var(y2),var(y3),var(y4))))
sstr = sum(ni*(yibar-ybar)^2)

ss = c(sstr,sse,sst)
df = c(k-1,n-k,n-1)

ms = ss/df

F0 = ms[1]/ms[2]

aTb1 = data.frame("제곱합"=ss,"자유도"=df,"평균"=ms,"오차"=c(F0,"",""))
rownames(aTb1) = c("처리","오차","합계")
print(aTb1)


# #2.2 --------------------------------------------------------------------
y1<-c(35, 24, 28, 21)
y2<-c(19, 14, 14, 13)
y3<-c(21, 16, 21, 14)
k = 3

ni = c(length(y1),length(y2),length(y3))
y = c(y1, y2, y3)
yibar = c(mean(y1),mean(y2),mean(y3))
ybar = mean(y)

n = sum(ni)


sst = sum((yn-ybar)^2)
sstr = sum(ni*(yibar-ybar)^2)
sse = sum((ni-1)*c(var(y1),var(y2),var(y3)))

ss = c(sstr,sse,sst)
df = c(k-1,n-k,n-1)

ms = ss/df

F0 = ms[1]/ms[2]

aTb1 = data.frame("제곱합"=ss,"자유도"=df,"평균"=ms,"오차"=c(F0,"",""))
rownames(aTb1) = c("처리","오차","합계")
print(aTb1)


# #2.3 --------------------------------------------------------------------

y1 = c(5,3,2,2)
y2 = c(5,0,1)
y3 = c(2,1,0,1)

y = c(y1,y2,y3)
yibar = c(mean(y1),mean(y2),mean(y3))
ni = c(length(y1),length(y2),length(y3))
ybar = mean(y)
k = 3
n = sum(ni)


sst = sum((y-ybar)^2)
sstr = sum(ni*(yibar-ybar)^2)
sse = sum((ni-1)*c(var(y1),var(y2),var(y3)))

ss = c(sstr,sse,sst)
df = c(k-1,n-k,n-1)

ms = ss/df

F0 = ms[1]/ms[2]

aTb1 = data.frame("제곱합"=ss,"자유도"=df,"평균"=ms,"오차"=c(F0,"",""))

rownames(aTb1) = c("처리","오차","합계")

print(aTb1)
                  

# 2.5---------------------------------------------------------------------

y1 = c(2,1,3)
y2 = c(1,5)
y3 = c(9,5,6,4)
y4 = c(3,4,5)

y = c(y1,y2,y3,y4)
yibar = c(mean(y1),mean(y2),mean(y3),mean(y4))
ni = c(length(y1),length(y2),length(y3),length(y4))
ybar = mean(y)
k = 4
n = sum(ni)

sst = sum((y-ybar)^2)
sstr = sum(ni*(yibar-ybar)^2)
sse = sum((ni-1)*c(var(y1),var(y2),var(y3),var(y4)))

ss = c(sstr,sse,sst)
df = c(k-1,n-k,n-1)

ms = ss/df

F0 = ms[1]/ms[2]

aTb1 = data.frame("제곱합"=ss,"자유도"=df,"평균제곱"=ms,"오차"=c(F0,"",""))
rownames(aTb1) = c("처리","오차","합계")
print(aTb1)

# #2.5 --------------------------------------------------------------------

y1 = c(2,1,3)
y2 = c(1,5)
y3 = c(9,5,6,4)
y4 = c(3,4,5)

y = c(y1,y2,y3,y4)
ni = c(length(y1),length(y2),length(y3),length(y4))
n = sum(ni)
yibar = c(mean(y1),mean(y2),mean(y3),mean(y4))

sst = sum((y-ybar)^2)
sstr = sum(ni*(yibar-ybar)^2)
sse = sum((ni-1)*c(var(y1),var(y2),var(y3),var(y4)))

ss = c(sstr,sse,sst)
df = c(k-1,n-k,n-1)

ms = ss/df

F0 = ms[1]/ms[2]

aTb1 = data.frame("제곱합"=ss,"자유도"=df,"평균제곱"=ms,"오차"=c(F0,"",""))
rownames(aTb1) = c("처리","오차","합")

print(aTb1)


# #2.6 --------------------------------------------------------------------

ni = c(10,6,9)
yibar = c(5,2,7)
n = sum(ni)
ybar = sum(ni*yibar)/n

k = 3

sse = 30+16+25
sstr = sum(ni*(yibar-ybar)^2)
sst = sse+sstr

ss = c(sstr,sse,sst)
df = c(k-1, n-k ,n-1)

ms = ss/df

F0 = ms[1]/ms[2]

aTb1 = data.frame("제곱합"=ss,"자유도"=df,"평균"=ms,"오차"=c(F0,"",""))
rownames(aTb1) = c("오차","처리","합")

print(aTb1)



# #2.7 --------------------------------------------------------------------

yibar = c(81.06,78.56,87.81)
ni = c(32,16,16)
s = c(17.05,15.43,14.36)
n = sum(ni)
ybar = sum(yibar*ni)/n
k = 3

sse = sum((ni-1)*s^2)
sstr = sum(ni*(yibar-ybar)^2)
sst = sse+sstr

ss = c(sstr,sse,sst)
df = c(k-1,n-k,n-1)

ms = ss/df

F0= ms[1]/ms[2]
aTb1 = data.frame("제곱합"=ss,"자유도"=df,"평균"=ms,"오차"=c(F0,"",""))
rownames(aTb1) = c("처리","오차","합")

print(aTb1)


# #3.1 --------------------------------------------------------------------

qf(1-0.05,5,10)
qf(1-0.05,10,5)



# #3.2 --------------------------------------------------------------------

qf(1-0.1,3,5)
qf(1-0.1,3,10)



# #3.3 --------------------------------------------------------------------

qf(1-0.1,5,20)

F0 = (104/5)/(109/20)
F0


# #3.4 --------------------------------------------------------------------

F0 =(24/5)/(57/35)

qf(1-0.05,5,35)
F0


# 3.8 ---------------------------------------------------------------------

y1 = c(0.95,0.86,0.71,0.72,0.74)
y2 = c(0.71,0.85,0.62,0.72,0.64)
y3 = c(0.69,0.68,0.51,0.73,0.44)


y = c(y1,y2,y3)
yibar = c(mean(y1),mean(y2),mean(y3))
ni = c(length(y1),length(y2),length(y3))
ybar = mean(y)
k = 3
n = sum(ni)

sst = sum((y-ybar)^2)
sstr = sum(ni*(yibar-ybar)^2)
sse = sum((ni-1)*c(var(y1),var(y2),var(y3)))

ss = c(sstr,sse,sst)
df = c(k-1,n-k,n-1)

ms = ss/df

F0 = ms[1]/ms[2]

aTb1 = data.frame("제곱합"=ss,"자유도"=df,"평균제곱"=ms,"오차"=c(F0,"",""))
rownames(aTb1) = c("처리","오차","합계")
print(aTb1)

pvalue = 1-pf(F0,2,12)
pvalue


