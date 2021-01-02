
# #예제9 --------------------------------------------------------------------

datax = c(70,80,72,76,76,76,72,78,82,64,74,92,74,68,84)
datay = c(68,72,62,70,58,66,68,52,64,72,74,60,74,72,74)

d = datax - datay
d

mu = mean(d)
mu
s = sd(d)
s
alpha = 0.05


L = mu - qt(1-alpha/2,length(d)-1)*(s/sqrt(length(d)))
U = mu + qt(1-alpha/2,length(d)-1)*(s/sqrt(length(d)))

print(c(L,U))


t = mu/(s/sqrt(length(d)))
r = qt(1-0.01,length(d)-1)
print(c(t,r))

if(t>=r){print("reject")
}else{print("dont reject")}


pvalue = 1-pt(t,14)
pvalue


# 예제 10 -------------------------------------------------------------------

n1 = 100
n2 = 150
x = 88
y = 126
p1 = 88/100
p2 = 126/150

alpha = 0.05
delta = p1-p2
d = qnorm(1-alpha/2)*sqrt(p1*(1-p1)/n1+p2*(1-p2)/n2)

L = delta - d
U = delta + d

print(c(L,U))

p = (x+y)/(n1+n2)
Z = (p1-p2)/sqrt(p*(1-p)*(1/n1+1/n2))
Z
pvalue =1- pnorm(Z)
r = qnorm(1-alpha)
r
pvalue

if(Z>=r){print("reject")
}else{print("dont reject")}

