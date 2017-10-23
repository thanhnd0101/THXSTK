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
curve(pexp(x,0.3),from=0,to=10)
#c
sum(dexp(seq(from=0,to=10,by=0.1),0.6))
sum(dexp(seq(from=0,to=10,by=0.1),0.3))


#Bai 4
plot(0:8,dpois(0:8,1),type="h",ylab="P(1)")

#Bai 5
plot(0:10,dchisq(1,df=3),ylav="X^2(3)")