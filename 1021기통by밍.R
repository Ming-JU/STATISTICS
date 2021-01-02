
# 예제6 ---------------------------------------------------------------------

x = c(3,3,4,5,6,6,7,8,8,9)
y = c(9,5,12,9,14,16,22,18,24,22)

xstar1 = 6
xstar2 = 9.5

xbar = mean(x)
ybar = mean(y)
Sxx =sum(x^2) - length(x)*mean(x)^2
Sxy = sum(x*y)-length(x)*mean(x)*mean(y)
Syy = sum(y^2) - length(y)*mean(y)^2
n = length(x)
beta1_hat = Sxy /Sxx
beta0_hat = ybar - beta1_hat*xbar
Y = beta0_hat +beta1_hat*xstar1
alpha = 0.05

SSE = Syy - Sxy^2/Sxx
s = sqrt(SSE/(n-2))

se1 = s*sqrt(1/n +(xstar1-xbar)^2/Sxx)
B1 = beta0_hat+beta1_hat*xstar1
U1 = B1 + qt(1-alpha/2,n-2)*se1
L1 = B1 - qt(1-alpha/2,n-2)*se1

se2 = s*sqrt(1/n +(xstar2-xbar)^2/Sxx)
B2 = beta0_hat+beta1_hat*xstar1
U2 = B2 + qt(1-alpha/2,n-2)*se2
L2 = B2 - qt(1-alpha/2,n-2)*se2

print(c(L1,U1))
print(c(L2,U2))


# 예제7 ---------------------------------------------------------------------

x = c(3,3,4,5,6,6,7,8,8,9)
y = c(9,5,12,9,14,16,22,18,24,22)

xstar1 = 6


xbar = mean(x)
ybar = mean(y)
Sxx =sum(x^2) - length(x)*mean(x)^2
Sxy = sum(x*y)-length(x)*mean(x)*mean(y)
Syy = sum(y^2) - length(y)*mean(y)^2
n = length(x)
beta1_hat = Sxy /Sxx
beta0_hat = ybar - beta1_hat*xbar
Y = beta0_hat +beta1_hat*xstar1
alpha = 0.05

SSE = Syy - Sxy^2/Sxx
s = sqrt(SSE/(n-2))

se1 = s*sqrt(1+ 1/n +(xstar1-xbar)^2/Sxx)
B1 = beta0_hat+beta1_hat*xstar1
U1 = B1 + qt(1-alpha/2,n-2)*se1
L1 = B1 - qt(1-alpha/2,n-2)*se1

print(c(L1,U1))

