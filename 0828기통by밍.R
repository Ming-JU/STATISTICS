
# #10 ---------------------------------------------------------------------
n = 70
mu = 20
sigma = 5.6
xbar = 21.31
z = (xbar - mu)/(sigma/sqrt(n))
prob = 1 - pnorm(z)
print(prob)


# #10_(2) ---------------------------------------------------------------------
a= qnorm(0.95)
d = (a*5.6/sqrt(70))+20
print(d) 

# #11 ---------------------------------------------------------------------

n = 80
mu0 = 1100
xbar = 1060
s = 210

alpha = 0.01

z = (xbar - mu0)/(s/sqrt(n))

z_alpha_half = qnorm(1-alpha/2)

print(c(z,z_alpha_half))

print(z)


pvalue = 2*(1- pnorm(abs(z)))

if (abs(z) >= z_alpha_half) { 
  print("H0±â°¢")
} else{
    print("H0±â°¢ ³ñ")
  }

print(c(z,z_alpha_half,pvalue))


# #12 ---------------------------------------------------------------------



n = 226
p0 = 0.3
p = 80/226


alpha = 0.05

z = (p - p0)/sqrt(p0*(1-p0)/n)
z
z_alpha = qnorm(1-alpha)

print(c(z,z_alpha))

print(z)


pvalue = (1- pnorm(abs(z)))

if (abs(z) >= z_alpha) { 
  print("H0±â°¢")
} else{
  print("H0±â°¢ ³ñ")
}

print(c(z,z_alpha,pvalue))
