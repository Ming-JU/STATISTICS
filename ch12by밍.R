
# 예제9 ---------------------------------------------------------------------
x = c(70,80,72,76,76,76,72,78,82,64,74,92,74,68,84)
y = c(68,72,62,70,58,66,68,52,64,72,74,60,74,72,74)
d = x - y
dbar = mean(d)
n = length(d)
sd = sd(d)
alpha1 = 0.05

L = dbar - qt(1-alpha1/2,n-1)*sd/sqrt(n)
U = dbar + qt(1-alpha1/2,n-1)*sd/sqrt(n)
print(c(L,U))

alpha2 = 0.01
t = (dbar-0)/(sd/sqrt(n))
r = qt(1-alpha2,n-1)

if(t>=r){print("reject")
  }else{print("don't reject")}


# 예제10 --------------------------------------------------------------------

n1 = 100
n2 = 150
x = 88
y  = 126

p1_hat = x/n1
p2_hat = y/n2

delta = p1_hat - p2_hat
sd = sqrt((p1_hat*(1-p1_hat)/n1)+(p2_hat*(1-p2_hat)/n2))
phat = (x+y)/(n1+n2)

alpha = 0.05 

L = delta - qnorm(1-alpha/2)*sd
U = delta + qnorm(1-alpha/2)*sd
print(c(L,U))

#예제11

Z = delta/sqrt(phat*(1-phat))/sqrt(1/n1+1/n2)
Z
r = qnorm(1-alpha)
r

if(Z>=r){print("reject")
  }else{print("don't reject")}

p_value = 1-pnorm(Z)
p_value


# 12장3절 -------------------------------------------------------------------


# #3.1 --------------------------------------------------------------------

x = c(6,10,8,13)
y = c(7,9,11,11)
d = x-y
n = length(d)
dbar = mean(d)
sd = sd(d)
t = dbar/(sd/sqrt(n))
t


# #3.3 --------------------------------------------------------------------
d = c(2,5,6,8,-6,4,18,-12,17,-7,16,12)
dbar = mean(d)
sd = sd(d)
alpha = 0.05
n = length(d)
r = qt(1-alpha,n-1)
t = dbar/(sd/sqrt(n))
if(t>=r){print("reject")
}else{print("don't reject")}

L = dbar - qt(1-alpha/2,n-1)*sd/sqrt(n)
U = dbar + qt(1-alpha/2,n-1)*sd/sqrt(n)

print(c(L,U))


# #3.5 --------------------------------------------------------------------

x = c(12,29,16,37,28,15)
y = c(10,28,17,35,25,16)

d = x-y

dbar = mean(d)
sd = sd(d)
n = length(d)
alpha = 0.05

r = qt(1-alpha,n-1)

t = dbar/(sd/sqrt(n))

if(t>=r){print("reject")
}else{print("don't reject")}

print(c(t,r))


# #3.13 -------------------------------------------------------------------

x = c(140,90,125,130,95,121,85,97,131,110)
y = c(138,87,110,132,96,120,86,90,129,100)

d = x-y
n = length(d)
dbar = mean(d)
sd = sd(d)

t = dbar/(sd/sqrt(n))
alpha1 = 0.05
r = qt(1-alpha1,n-1)

if(t>=r){print("reject")
}else{print("don't")}

alpha2 = 0.1

L = dbar - qt(1-alpha2/2,n-1)*sd/sqrt(n)
U = dbar + qt(1-alpha2/2,n-1)*sd/sqrt(n)

print(c(L,U))
pvalue = 1-pt(t,n-1)
pvalue
# 12장4절 -------------------------------------------------------------------


# #4.1 --------------------------------------------------------------------

n1 = 100
n2 = 200

p1_hat = 0.5
p2_hat = 0.7

delta = p1_hat - p2_hat
alpha = 0.05

p = ((n1*p1_hat+n2*p2_hat)/(n1+n2))
s = sqrt(p1_hat*(1-p1_hat)/n1+p2_hat*(1-p2_hat)/n2)

L = delta - qnorm(1-alpha/2)*s
U = delta + qnorm(1-alpha/2)*s

print(c(L,U))

Z = delta / sqrt(p*(1-p)*(1/n1+1/n2))

r = qnorm(1-alpha/2)

if(Z<=r){print("reject")
}else{print("don't")}


# #4.3 --------------------------------------------------------------------

n1 =120
n2 =150
x = 52
y = 88

p1_hat = x/n1
p2_hat = y/n2

delta = p1_hat - p2_hat
p = (x+y)/(n1+n2)

se =sqrt(p1_hat*(1-p1_hat)/n1+p2_hat*(1-p2_hat)/n2)

alpha = 0.05

L = delta - qnorm(1-alpha/2)*se
U = delta + qnorm(1-alpha/2)*se

print(c(L,U))

Z = delta/sqrt(p*(1-p)*(1/n1+1/n2))
pvalue = pnorm(Z)
pvalue

# #4.7 --------------------------------------------------------------------

x = 21 
n1 = 85
y = 11
n2 = 120

p1_hat = x/n1
p2_hat = y/n2
p = (x+y)/(n1+n2)
alpha = 0.01

delta = p1_hat- p2_hat

Z = delta / sqrt(p*(1-p)*(1/n1+1/n2))
Z

r = qnorm(1-alpha)
r

if(Z>=r){print("reject")
}else{print("don't reject")}


# #4.15 -------------------------------------------------------------------

n1 = 549
n2 = 534
x = 11
y = 70

p1_hat = x/n1
p2_hat = y/n2

delta = p1_hat - p2_hat
alpha1 = 0.01
p = (x+y)/(n1+n2)

Z = delta/sqrt(p*(1-p)*(1/n1+1/n2))

r = qnorm(1-alpha1)

if(Z<=r){print("reject")
}else{print("dont reject")}

Z
r

alpha2 = 0.05

L = delta - qnorm(1-alpha2/2)*sqrt(p1_hat*(1-p1_hat)/n1+p2_hat*(1-p2_hat)/n2)

U = delta + qnorm(1-alpha2/2)*sqrt(p1_hat*(1-p1_hat)/n1+p2_hat*(1-p2_hat)/n2)

print(c(L,U))
