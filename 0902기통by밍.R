
# #12 ---------------------------------------------------------------------

mu = 15
n = 70
xbar = 14.6
s = 3.0
alpha = 0.025

z = (xbar - mu)/(s/sqrt(n))

z_alpha_half = qnorm(1-alpha/2)

print(c(z,z_alpha_half))

print(z)


pvalue = (1- pnorm(abs(z)))
print(pvalue)


# #13 ---------------------------------------------------------------------

n = 750
p = 0.598
alpha = 0.05
z_alpha_half = qnorm(1-alpha/2)
print(z_alpha_half)
p - z_alpha_half*sqrt(p*(1-p)/n)
p + z_alpha_half*sqrt(p*(1-p)/n)

# #13-2 -------------------------------------------------------------------

n = 750
p = 0.042
alpha = 0.1
z_alpha_half = qnorm(1-alpha/2)
print(z_alpha_half)
p - z_alpha_half*sqrt(p*(1-p)/n)
p + z_alpha_half*sqrt(p*(1-p)/n)


# #14 ---------------------------------------------------------------------
n = 500
p = 228/500
p0 = 0.5
alpha = 0.05
z_alpha_half = qnorm(1-alpha/2)
print(z_alpha_half)
p - z_alpha_half*sqrt(p0*(1-p0)/n)
p + z_alpha_half*sqrt(p0*(1-p0)/n)
z = (p - p0)/sqrt((p0*(1-p0)/n))
z
pvalue = (pnorm(z))

pvalue


