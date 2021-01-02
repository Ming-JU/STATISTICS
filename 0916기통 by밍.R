
# #똥글뱅이1 ---------------------------------------------------------------------

n = 30
mu = 3
sigma = 4
N = 10000

data = rnorm(n*N,mu,sigma) #표준 정규분포를 모집단으로 해서 표본 추출
data = matrix(data,nrow = N ,ncol = n )
xbar = apply(data,1,mean) #평균을 1(행에따라서) 구해줌 우리는 10000개 계산할거야
x = seq(-2,8,by = 0.1)
hist(xbar,breaks = x ,probability = T)
lines(x,dnorm(x,mu,sigma/sqrt(n)),col = 'blue')


# 똥글뱅이2 -------------------------------------------------------------------

n = 30
mu = 3
sigma = 4
N = 10000


data = rnorm(n*N,mu,sigma) #표준 정규분포를 모집단으로 해서 표본 추출
data = matrix(data,nrow = N ,ncol = n )
xbar = apply(data,1,mean)
z =(xbar-mu)/(sigma/sqrt(n))
x = seq(-4,4,by = 0.1)
hist(z,breaks = x,probability = T)
lines(x,dnorm(x,0,1),col = 'red')


# 똥글뱅이3 -------------------------------------------------------------------
n = 30
mu = 3
sigma = 4
N = 10000


data = rnorm(n*N,mu,sigma) #표준 정규분포를 모집단으로 해서 표본 추출
data = matrix(data,nrow = N ,ncol = n )
xbar = apply(data,1,mean)
s = apply(data,1,sd)

x = seq(-5,5,by = 0.2)
hist(s,breaks = x,probability = T)
lines(x,dnorm(x,0,1),col = 'red')
lines(x,dt(x,n-1),col = 'green')

