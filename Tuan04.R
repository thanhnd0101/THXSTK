#Bai 1
fsb=function(x,n,M,N){
 return (choose(M,x)*choose(N-M,n-x)/choose(N,n));}
plot(0:15,fsb(0:15,15,25,100),type="h",ylab="H(x,M,N)")

#Bai 2
sum(fsb(5:12,15,25,100));
pfsb=function(x,n,M,N){
return (sum(fsb(0:x,15,25,100)));}

#Bai 3
#a
curve(dexp(x,0.6),from=0,to=10)
#b
curve(dexp(x,0.3),from=0,to=10,add=T)
#n
pexp(10,0.6) 
pexp(10,0.3) 

#Bai 4
plot(0:8,dpois(0:8,1),type="h",ylab="P(1)")

#Bai 5
plot(0:10,dchisq(1,df=3,ncp=0:10),ylab="X^2(3)")

#Bai 6
#par(mfrow=c(1,3))

x=0:50
plot(stepfun(x,c(0,dbinom(x,50,0.08))),ylim=c(0,0.15),ylab="B(50,0.08)")
x=0:50
plot(stepfun(x,c(0,dpois(x,4))),ylim=c(0,0.2),ylab="P(4)")

#Bai 7
#par(mfrow=c(2,2))
layout(matrix(c(1,2),nrow=2))

curve(dbinom(x,50,0.4),from=0,to=50,ylab="B(50,0.4)")
curve(dnorm(x,20,sqrt(12)),from=0,to=50)
