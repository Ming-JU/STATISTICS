
# 예제#3 --------------------------------------------------------------------

xbar = 453
ybar = 401
s1 = 80
s2 = 60
alpha = 0.05
alpha_2 = alpha/2
delta = xbar - ybar
n1 = 50
n2 = 100
L = delta - qnorm(1-alpha_2)*sqrt(s1^2/n1+s2^2/n2)
U = delta + qnorm(1-alpha_2)*sqrt(s1^2/n1+s2^2/n2)
print(c(L,U))

delta0 = 0
z0 = (delta - delta0) /sqrt(s1^2/n1+s2^2/n2)
z0

alpha_1 = 0.01
alpha_1_2 = alpha_1 / 2
qnorm(1-alpha_1_2)

pvalue = (1-pnorm(z0))*2
pvalue



# 예제#5 --------------------------------------------------------------------

data1 = c(8,5,7,6,9,7)
data2 = c(2,6,4,7,6)

n1 = length(data1)
n2 = length(data2)
s1 = sd(data1)
s2 = sd(data2)
sp_square = ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1 + n2 -2)
sp_square


# 예제6 ---------------------------------------------------------------------
x = c(44,44,56,46,47,38,58,53,49,35,46,30,41)
y = c(35,47,55,29,40,39,32,41,42,57,51,39)
n1 = length(x)
n2 = length(y)
xbar = mean(x)
ybar = mean(y)
s1 = sd(x)

s2 = sd(y)
s1
s2
delta = xbar - ybar
spsq = ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1 + n2 -2)
alpha = 0.05
d = spsq*sqrt(1/n1+1/n2)

t = qt(1-alpha_2,n1+n2-2)
t
alpha_2 = alpha/2
L = delta -qt(1-alpha_2,n1+n2-2)*sqrt(spsq*(1/n1+1/n2))
U = delta +qt(1-alpha_2,n1+n2-2)*sqrt(spsq*(1/n1+1/n2))
print(c(L,U))

delta0 = 0

t0 = delta/sqrt(spsq*(1/n1+1/n2))
t0
t_c = qt(1-0.05,n1+n2-2)
t_c

print(c(t0,t_c))  #차이가 난다고 할수없다


# 예제8 ---------------------------------------------------------------------

n1 = 13
n2 = 11
xbar = 2.4
ybar = 2.15
s1 = 0.72
s2 = 0.35
delta0 = 0
delta = xbar - ybar
alpha = 0.05
alpha_2 = alpha/2
t_star = delta/(s1^2/n1+s2^2/n2)
t_star
t0 = qt(1-alpha_2,10)
t0
print(c(t_star,t0))