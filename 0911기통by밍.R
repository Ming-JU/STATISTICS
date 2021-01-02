
# #카이분포  예제6 --------------------------------------------------------------

qchisq(0.95,17)
qchisq(1-0.95,17)


# 예제7 ---------------------------------------------------------------------
n= 10
s = 0.4
alpha = 0.1

chisq_alpha_2 = qchisq(1-alpha/2,n-1)
chisq_1_alpha_2 = qchisq(alpha/2,n-1)

U = (n-1)*s^2/chisq_1_alpha_2
L = (n-1)*s^2/chisq_alpha_2
print(c(L,U))
print(c(sqrt(L),sqrt(U)))


# #예제8 ------------------------------------------------------------------

n = 10
s = 0.4
sigma0 = 0.2
alpha =  0.05

chisq_alpha = qchisq(1-alpha,n-1)
chisq_alpha

chisq0 = (n-1)*s^2/sigma0^2
chisq #기각역 R:chisq>= chisq(0.05,9)


if(chisq0 >= chisq_alpha){
  print("reject")
} else{
  print("do not reject")
}
pvalue = 1 - pchisq(chisq0,n-1)

print(c(chisq_alpha,chisq,pvalue))
