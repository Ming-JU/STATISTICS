
# #예제2 -------------------------------------------------------------------
x = c(3,3,4,5,6,6,7,8,8,9)
y = c(9,5,12,9,14,16,22,18,24,22)

n = length(x)

xbar = mean(x)
ybar = mean(y)

Sxx = sum(x^2)-n*xbar^2
Sxy = sum(x*y)-n*xbar*ybar

beta1_hat = Sxy/Sxx
beta0_hat = ybar - beta1_hat*xbar

yhat = beta0_hat + beta1_hat*x
yhat
e = y-yhat
e

SSE = sum(e^2)
MSE = SSE/(n-2)
s = sqrt(MSE)
print(c(beta1_hat,beta0_hat,SSE,MSE))


#예제3

alpha = 0.05

L = beta1_hat - qt(1-alpha/2,n-2)*s/sqrt(Sxx)
U = beta1_hat + qt(1-alpha/2,n-2)*s/sqrt(Sxx)

print(c(L,U))

#예제4

t = beta1_hat/(s/sqrt(Sxx))

r = qt(1-alpha,n-2)
t
r
if(t>=r){print("reject")
}else{print("don't reject")}

#예제5

l = beta0_hat - qt(1-alpha/2,n-2)*s*sqrt(1/n+xbar^2/Sxx)
u = beta0_hat + qt(1-alpha/2,n-2)*s*sqrt(1/n+xbar^2/Sxx)

print(c(l,u))

t_2 = beta0_hat/(s*sqrt(1/n+xbar^2/Sxx))

if(abs(t_2)>=qt(1-alpha/2,n-2)){print("reject")
}else{print("don't reject")}


#예제6
xstar1 = 6
xstar2 = 9.5
L2 = (beta0_hat+beta1_hat*xstar1) - qt(1-alpha/2,n-2)*s*sqrt(1/n+(xstar1-xbar)^2/Sxx)
U2 = (beta0_hat+beta1_hat*xstar1) + qt(1-alpha/2,n-2)*s*sqrt(1/n+(xstar1-xbar)^2/Sxx)

L3 = (beta0_hat+beta1_hat*xstar2) - qt(1-alpha/2,n-2)*s*sqrt(1/n+(xstar2-xbar)^2/Sxx)
U3 = (beta0_hat+beta1_hat*xstar2) + qt(1-alpha/2,n-2)*s*sqrt(1/n+(xstar2-xbar)^2/Sxx)

print(c(L2,U2))
print(c(L3,U3))


#예제7

y_predict1 = beta0_hat + beta1_hat*xstar1

L4 = y_predict1 - qt(1-alpha/2,n-2)*s*sqrt(1+1/n+(xstar1-xbar)^2/Sxx)
U4 = y_predict1 + qt(1-alpha/2,n-2)*s*sqrt(1+1/n+(xstar1-xbar)^2/Sxx)

print(c(L4,U4))

#예제8

R_sq = Sxy^2/(Sxx*Syy)
R_sq


# 13장3절 -------------------------------------------------------------------

# #3.1 --------------------------------------------------------------------

x = c(1,2,3,4,5)
y = c(0.9,2.1,2.5,3.3,3.8)

xbar = mean(x)
ybar = mean(y)

n = length(x)

Sxx = sum(x^2)-n*xbar^2
Syy = sum(y^2)-n*ybar^2
Sxy = sum(x*y)-n*xbar*ybar

beta1_hat = Sxy/Sxx
beta0_hat = ybar - beta1_hat*xbar
beta0_hat
beta1_hat
yhat = beta0_hat + beta1_hat*x
x2 = seq(0,10,by = 0.1)
plot(x,yhat)
lines(x,yhat,col = 'blue')

#3.7

e = y - yhat
e
sum(e^2)

SSE = Syy - Sxy^2/Sxx
SSE

MSE = SSE/(n-2)
MSE


# #3.9 --------------------------------------------------------------------

n = 18
xbar = 1.2
ybar = 5.1
Sxx = 14.10
Sxy = 2.31
Syy = 2.01

beta1_hat = Sxy/Sxx
beta0_hat = ybar - beta1_hat*xbar

print(c(beta0_hat,beta1_hat))

yhat = beta0_hat + beta1_hat*x

e = y- yhat
SSE = sum(e^2)
SSE = Syy - Sxy^2/Sxx
MSE = SSE/(n-2)

SSE
MSE


# #3.13 -------------------------------------------------------------------

x = c(6,7,5,21,13,5,13,14)
y = c(28,23,29,22,20,19,28,19)

xbar = mean(x)
ybar = mean(y)

n = length(x)

Sxx = sum(x^2)-n*xbar^2
Syy = sum(y^2)-n*ybar^2
Sxy = sum(x*y)-n*xbar*ybar

beta1_hat = Sxy/Sxx
beta0_hat = ybar - beta1_hat*xbar
beta0_hat
beta1_hat
yhat = beta0_hat + beta1_hat*x


# 13장 4절 ------------------------------------------------------------------

# 4.1 ---------------------------------------------------------------------
x = c(0,1,6,3,5)
y = c(4,3,0,2,1)

xbar = mean(x)
ybar = mean(y)

n = length(x)

Sxx = sum(x^2)-n*xbar^2
Syy = sum(y^2)-n*ybar^2
Sxy = sum(x*y)-n*xbar*ybar

#(1)
beta1_hat = Sxy/Sxx
beta0_hat = ybar - beta1_hat*xbar
beta0_hat
beta1_hat
yhat = beta0_hat + beta1_hat*x



e = y - yhat
e
sum(e^2)

SSE = Syy - Sxy^2/Sxx
SSE

MSE = SSE/(n-2)
MSE
s = sqrt(MSE)
#(2)
alpha = 0.05

t = beta1_hat/sqrt(s^2/Sxx)
r = qt(1-alpha/2,n-2)
t
if(abs(t)>=r){print("reject")
  }else{print("dont reject")}

#(3)
a = print(beta0_hat + beta1_hat*2.5)
a
alpha2 = 0.1

L = a - qt(1-alpha2/2,n-2)*s*sqrt(1/n+(2.5-xbar)^2/Sxx)
U = a + qt(1-alpha2/2,n-2)*s*sqrt(1/n+(2.5-xbar)^2/Sxx)
print(c(L,U))

#(4)

L2 = beta0_hat - qt(1-alpha2/2,n-2)*s*sqrt(1/n+xbar^2/Sxx)
U2 = beta0_hat + qt(1-alpha2/2,n-2)*s*sqrt(1/n+xbar^2/Sxx)

print(c(L2,U2))



# #4.3 --------------------------------------------------------------------
x = c(1,2,3,4,5)
y = c(0.9,2.1,2.4,3.3,3.8)

xbar = mean(x)
ybar = mean(y)

n = length(x)

Sxx = sum(x^2)-n*xbar^2
Syy = sum(y^2)-n*ybar^2
Sxy = sum(x*y)-n*xbar*ybar

#(1)
beta1_hat = Sxy/Sxx
beta0_hat = ybar - beta1_hat*xbar
beta0_hat
beta1_hat
yhat = beta0_hat + beta1_hat*x



e = y - yhat
e
sum(e^2)

SSE = Syy - Sxy^2/Sxx
SSE

MSE = SSE/(n-2)
MSE
s = sqrt(MSE)

#(2)
alpha = 0.05

t = (beta1_hat - 1)/(s/sqrt(Sxx))
r = qt(1-alpha/2,n-2)
r ##확인

if(abs(t)>=r){print("o")
  }else{print("x")}

#(3)

a = beta0_hat + beta1_hat*3.5
a

L = a - qt(1-alpha/2,n-2)*s*sqrt(1+1/n+(3.5-xbar)^2/Sxx)
U = a + qt(1-alpha/2,n-2)*s*sqrt(1+1/n+(3.5-xbar)^2/Sxx)

print(c(L,U))

#(4)

alpha2 = 0.1

L2 = beta0_hat - qt(1-alpha2/2,n-2)*s*sqrt(1/n+xbar^2/Sxx)
U2 = beta0_hat + qt(1-alpha2/2,n-2)*s*sqrt(1/n+xbar^2/Sxx)

print(c(L2,U2))


# #4.9 --------------------------------------------------------------------
x = c(0,1,2,3,4)
y = c(195,216,244,260,284)

xbar = mean(x)
ybar = mean(y)

n = length(x)
Sxx = sum(x^2) - n*xbar^2
Syy = sum(y^2) - n*ybar^2
Sxy = sum(x*y) - n*xbar*ybar

beta1_hat = Sxy/Sxx
beta0_hat = ybar - beta1_hat*xbar

yhat = beta0_hat + beta1_hat*x

plot(yhat)

e = y - yhat
SSE = Syy-Sxy^2/Sxx
MSE = SSE/(n-2)
s = sqrt(MSE)
#(2)
alpha = 0.05

t = beta1_hat/(s/sqrt(Sxx))
r = qt(1-alpha,n-2)
t
r
if(t>=r){print("o")
  }else{print("x")}

#(3)

a = beta0_hat + beta1_hat*9
a
beta0_hat
beta1_hat


# 13장 5절 ------------------------------------------------------------------


# 5.1 ---------------------------------------------------------------------
n =14
xbar = 1.2
ybar = 5.1
Sxx = 14.10
Sxy = 2.31
Syy =2.01

SSE = Syy - Sxy^2/Sxx
SST = Syy
SSR = Sxy^2/Sxx

R2 = SSR/SST
R2
R = Sxy^2/Sxx/Syy
R


# #5.3 --------------------------------------------------------------------
Sxx = 92
Syy = 457
Sxy = 160

SST = Syy
SSE = Syy-Sxy^2/Sxx
SSR = Sxy^2/Sxx

R2 = SSR/SST
R2


# #5.5 --------------------------------------------------------------------

x = c(200.8,194.6,183.5,190.5,210.2,170.5,220.0)
y = c(207.0,199.0,188.0,191.2,211.0,176.2,218.0)

xbar = mean(x)
ybar = mean(y)

n = length(x)

Sxx = sum(x^2) - n*xbar^2
Syy - sum(y^2) - n*ybar^2
Sxy = sum(x*y) - n*xbar*ybar

SSE = Syy - Sxy^2/Sxx
SSR = Sxy^2/Sxx
SST = Syy

R2= SSR/SST
R2
r = sqrt(R2)

Sxy^2/Sxx/Syy



# 13장6절 -------------------------------------------------------------------


# #6.1 --------------------------------------------------------------------

y <-c(11.3, 14.8, 18.4, 22.0, 25.5, 27.0, 31.2, 32.7, 34.1, 36.2, 39.8, 43.4, 45.5, 46.2, 46.9, 46.9)
e <-c(-0.3, 0.2, -5.4, 2.0, 0.5, 5.0, -9.2, 6.3, 12.9, -4.2, -14.8, 7.6, -1.5, 15.8, 1.1, -16.9)

plot(y,e)


# #6.3 --------------------------------------------------------------------

x <-c(2.2, 3.1, 2.5, 3.3, 2.3, 3.6, 2.6, 2.5, 3.0, 3.2, 2.9, 3.3, 2.7, 3.2)
y <-c(9, 6, 13, 1, 7, 14, 8, 3, 12, 4, 11, 2, 10, 5)
e <-c(-1, -2, 3, -3, -1, 5, 0, 0, 3, -2, 2, -5, 0, 1)

plot(x,e)
plot(y,e)

