
# 2.2 ---------------------------------------------------------------------

n1 = 90
n2 = 100
xbar = 76.4
ybar = 81.2
s1= 8.2
s2 = 7.6
alpha = 0.02
alpha_2 = alpha/2
z= qnorm(1-alpha/2)
delta = xbar - ybar

L = delta - z*sqrt(s1^2/n1+s2^2/n2)
U = delta + z*sqrt(s1^2/n1+s2^2/n2)

print(c(L,U))
-4.8-2.68




# 2.6(2) ---------------------------------------------------------------------

n1 = 66
n2 = 38
xbar = 305
ybar = 311
s1= 29
s2 = 40
alpha = 0.02
alpha_2 = alpha/2
z= qnorm(1-alpha/2)
delta = xbar - ybar
d = sqrt(s1^2/n1 + s2^2/n2)
L = delta - z*d
U = delta + z*d

print(c(L,U))


# 2.6(1) ------------------------------------------------------------------

n1 = 66
n2 = 38
xbar = 305
ybar = 311
s1= 29
s2 = 40
delta = xbar - ybar
alpha = 0.01
alpha_2 = alpha/2
r= qnorm(alpha/2)
r
Z= delta /sqrt(s1^2/n1+s2^2/n2)
Z



d = sqrt(s1^2/n1 + s2^2/n2)
L = delta - z*d
U = delta + z*d
print(c(L,U))
print(c(Z,r))  

if(Z<=r){print("기각")
}else{print("기각ㄴㄴ")}

# #2.13 -------------------------------------------------------------------

xbar = 10.6
ybar = 14.9
n1 = 5
n2 = 8
s1 = 3.62
s2 = 4.17
spsq = ((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2)
spsq
alpha = 0.1
delta = xbar - ybar

L = delta - qt(1-alpha/2,n1+n2-2)*sqrt(spsq)*sqrt(1/n1+1/n2)
U = delta + qt(1-alpha/2,n1+n2-2)*sqrt(spsq)*sqrt(1/n1+1/n2)
print(c(L,U))

r = qt(0.025,n1+n2-2)
t = delta/(sqrt(spsq)*sqrt(1/n1+1/n2))
if(t<=r){print("기각")
}else{print("기각ㄴㄴ")}
r


# #2.21 -------------------------------------------------------------------
lg = 76
ug = 102
delta = (lg+ug)/2
d = (ug-lg)/2
sqrt = d/qt(1-0.05,9)

print(c(delta-qt(1-0.01,9)*sqrt,delta+qt(1-0.01,9)*sqrt))

delta0 = 100

t = (delta - delta0)
