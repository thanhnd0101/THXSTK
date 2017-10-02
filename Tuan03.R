#Bai2
x=sample(1:5,size=100,replace=TRUE,prob=c(.1,.2,.3,.2,.2))
table(x)/100;
plot(table(x)/100,type="h")

#1a
k=0:8;
p=function(k){
	return (choose(8,k)*0.3^k*0.7^(8-k));}
F=function(k){
	return (sum(p(0:k)));}
F2=function(k){
	return (integrate(f,lower=-Inf,upper=k));}  

#1b
f=function(x,mu=0,sigma=1){
	return (1/sqrt(2*pi*sigma^2)*exp(-((x-mu)^2)/(2*sigma^2)));}
curve(f,from=-4,to=4,ylab="fX(x)");

#2a
#plot(k,p(k),type="h",ylab="P(X=x)")


#Bai1
f=function(p){
	return (0.07*p^(-0.93));
}
integrate(f,lower=0,upper=1);


