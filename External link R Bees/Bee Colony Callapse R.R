#This text file contains R scripts for reproducing Figures 1-7 in
#Dennis, B. and Kemp, W. P. How hives collapse:  Allee effects,
#ecological resilience, and the honey bee. PLoS One.

#==============================
# Figure 1 script.
lambda=2000
theta=13333.33
alpha=.2
beta=1000
mu=.1
k=20000

nints=20
n0=seq(500,18500,1000)
n=matrix(0,nints+1,length(n0))
time=seq(0,nints,1)
n[1,]=n0
for (i in 1:nints) {
  n[i+1,]=(lambda/mu)+(n0-(lambda/mu))*exp(-mu*i)
}
matplot(time,n,type="l",col=1,lty=1,
        ylab="population size")
abline(h=k,lty=2)

#================================
# Figure 2 script.
lambda=2000

theta=13333.33
alpha=.2
beta=1000
mu=.05

N1=seq(0,60000,1)
N2=seq(0,20000,1)
recruit=lambda*N1/(theta+N1)
recruit.per.N=lambda/(theta+N1)
mortality=(alpha*beta/(beta+N2)+mu)*N2
mortality.per.N=alpha*beta/(beta+N2)+mu

layout(matrix(c(1,2,3,4),2,2,byrow=TRUE))
plot(N1,recruit,type="l",xlab="population size",ylab="recruitment")
plot(N1,recruit.per.N,type="l",ylim=c(0,.20),
     xlab="population size",ylab="recruitment per indiv")
plot(N2,mortality,type="l",xlab="population size",ylab="mortality")
plot(N2,mortality.per.N,type="l",ylim=c(0,.20),
     xlab="population size",ylab="mortality per indiv")
abline(h=.05,lty=2)

#==============================
# Figure 3 script.
lambda=2000
theta=13333.33
alpha=.2
beta=1000
mu=.05

N=seq(0,25000,1)
recruit.per.N=lambda/(theta+N)
mortality.per.N=alpha*beta/(beta+N)+mu

layout(matrix(c(1,2,3,4),2,2,byrow=TRUE))

lambda=4000
recruit.per.N=lambda/(theta+N)
mortality.per.N=alpha*beta/(beta+N)+mu
plot(N,recruit.per.N,type="l",lty=1,ylab="rate per individual",
     xlab="population size",ylim=c(0,.3))
points(N,mortality.per.N,type="l",lty=2)

lambda=2000
recruit.per.N=lambda/(theta+N)
mortality.per.N=alpha*beta/(beta+N)+mu
plot(N,mortality.per.N,type="l",lty=2,ylab="rate per individual",
     xlab="population size",ylim=c(0,.3))
points(N,recruit.per.N,type="l",lty=1)

beta=4000
recruit.per.N=lambda/(theta+N)
mortality.per.N=alpha*beta/(beta+N)+mu
plot(N,mortality.per.N,type="l",lty=2,ylab="rate per individual",
     xlab="population size",ylim=c(0,.3))
points(N,recruit.per.N,type="l",lty=1)

beta=1000

nints=100
n0=seq(200,18200,500)
n=matrix(0,nints+1,length(n0))
time=seq(0,nints)
n[1,]=n0
for (i in 1:nints) {
  n[i+1,]=n[i,]+lambda*n[i,]/(theta+n[i,]) -
    (alpha*beta/(beta+n[i,])+mu)*n[i,]
}
matplot(time,n,type="l",col=1,lty=1,
        ylab="population abundance")
# abline(h=k,lty=2)

#==============================
# Figure 4 script.
lambda=2000
theta=13333.33
alpha=.2
beta=1000
mu=.05

N=seq(0,25000,1)
recruit.per.N=lambda/(theta+N)
mortality.per.N=alpha*beta/(beta+N)+mu
rate.per.N=recruit.per.N-mortality.per.N
plot(N,rate.per.N,type="l",lty=1,ylim=c(-.1,.15),
     ylab="net rate per individual",
     xlab="population size")
abline(h=0,lty=3)

lambda=4000
beta=1000
recruit.per.N=lambda/(theta+N)
mortality.per.N=alpha*beta/(beta+N)+mu
rate.per.N=recruit.per.N-mortality.per.N
points(N,rate.per.N,type="l",lty=5)

lambda=2000
beta=4000
recruit.per.N=lambda/(theta+N)
mortality.per.N=alpha*beta/(beta+N)+mu
rate.per.N=recruit.per.N-mortality.per.N
points(N,rate.per.N,type="l",lty=2)

#=============================
# Figure 5 script.
lambda=2000
theta=13333.33
alpha=.2
beta=1000
mu=.05
ref=3

N=seq(0,22000,1)

layout(matrix(c(1,2,3,4),2,2,byrow=TRUE))

# (A) lambda.
lambda=2000
theta=13333.33
alpha=.2
beta=1000
mu=.05
a=-mu
b=lambda-alpha*beta-mu*theta-mu*beta
c=beta*(lambda-alpha*theta-mu*theta)
y=a*N^2+b*N+c
plot(N,y,type="l",lty=ref,ylab=expression(a*N^2+b*N+c),lwd=2)
abline(h=0,lty=2)
text(800,4e+06,"A")
for (ii in 1:3) {
  lambda=lambda-200
  a=-mu
  b=lambda-alpha*beta-mu*theta-mu*beta
  c=beta*(lambda-alpha*theta-mu*theta)
  y=a*N^2+b*N+c
  points(N,y,type="l",lty=1)
}

# (B) mu.
lambda=2000
theta=13333.33
alpha=.2
beta=1000
mu=.05
a=-mu
b=lambda-alpha*beta-mu*theta-mu*beta
c=beta*(lambda-alpha*theta-mu*theta)
y=a*N^2+b*N+c
plot(N,y,type="l",lty=ref,ylab=expression(a*N^2+b*N+c),lwd=2)
abline(h=0,lty=2)
text(800,4e+06,"B")
for (ii in 1:3) {
  mu=mu+.01
  a=-mu
  b=lambda-alpha*beta-mu*theta-mu*beta
  c=beta*(lambda-alpha*theta-mu*theta)
  y=a*N^2+b*N+c
  points(N,y,type="l",lty=1)
}

# (C) theta.
lambda=2000
theta=13333.33
alpha=.2
beta=1000
mu=.05
a=-mu
b=lambda-alpha*beta-mu*theta-mu*beta
c=beta*(lambda-alpha*theta-mu*theta)
y=a*N^2+b*N+c
plot(N,y,type="l",lty=ref,ylab=expression(a*N^2+b*N+c),lwd=2)
abline(h=0,lty=2)
text(800,4e+06,"C")
for (ii in 1:3) {
  theta=theta+3000
  a=-mu
  b=lambda-alpha*beta-mu*theta-mu*beta
  c=beta*(lambda-alpha*theta-mu*theta)
  y=a*N^2+b*N+c
  points(N,y,type="l",lty=1)
}

# (D) beta.
lambda=2000
theta=13333.33
alpha=.2
beta=1000
mu=.05
a=-mu
b=lambda-alpha*beta-mu*theta-mu*beta
c=beta*(lambda-alpha*theta-mu*theta)
y=a*N^2+b*N+c
plot(N,y,type="l",lty=ref,ylab=expression(a*N^2+b*N+c),lwd=2)
abline(h=0,lty=2)
text(800,4e+06,"D")
for (ii in 1:3) {
  beta=beta+500
  a=-mu
  b=lambda-alpha*beta-mu*theta-mu*beta
  c=beta*(lambda-alpha*theta-mu*theta)
  y=a*N^2+b*N+c
  points(N,y,type="l",lty=1)
}

#==============================
# Figure 6 script.
lambda=2000
theta=13333.33
alpha=.2
beta=1000
mu=.05

N=seq(0,25000,1)
u=alpha*beta*N-alpha*beta*beta*log(beta+N)+(mu*N/2)*N-
  lambda*N+lambda*theta*log(theta+N)
u=u/1000000
par(mar=c(5.1,5.1,4.1,2.1))
plot(N,u,type="l",lty=1,
     ylab=expression(paste("potential function","   "%*%10^-6)),
     xlab="population size")

#==============================
# Figure 7 script.
lambda=2000
theta=13333.33
alpha=.2
beta=1000
mu0=.05

N=seq(0,25000,1)
u=matrix(0,length(N),4)
for (ii in 1:4) {
  mu=mu0+(ii-1)*.01
  u[,ii]=alpha*beta*N-alpha*beta*beta*log(beta+N)+(mu*N/2)*N-
    lambda*N+lambda*theta*log(theta+N)
}
u=u/1000000

hi=max(u)
lo=min(u)
par(mar=c(5.1,5.1,4.1,2.1))
plot(N,u[,1],type="l",lty=1,ylim=c(lo,hi),
     ylab=expression(paste("potential functions","   "%*%10^-6)),
     xlab="population size")

for (ii in 2:4) {
  points(N,u[,ii],type="l",lty=1)
}
write.csv(n,"honeybee_collapse.csv")

