#Bai6
UocLuong<-function(KichCo,C1,C2,R){
	N=(C1+1)*(C2+1)/(R+1)-1;
	varN=((C1+1)*(C2+1)*(C1-R)*(C2-R))/((R+1)^2*(R+2));
	return (c(N-1.96*sqrt(varN),N+1.96*sqrt(varN)));
}

#Bai4
data=read.csv(file.choose(),header=TRUE);
data11=data.frame(data)

thongke<-function(data){
	x=t(Bai4(data[1]));
	result=data.frame(x);
	for(i in 2:length(data)){
		x=t(Bai4(data[i]));
		result=data.frame(result,x);
}
	for(i in 1:length(data)){
		colnames(result)[i]=names(data[i]);
}
	rownames(result)[4]="Var";
	return (result);}

Bai4<-function(data){
	Max=max(data);
	Min=min(data);
	Mean=mean(t(as.vector(data)));
	Var=round(var(data),digits=2);
	return (data.frame(Max,Min,Mean,Var));}
#Bai5
PhanVi<-function(X,P){
	X=sort(X,decreasing=FALSE);
	if(P==100) return (X[length(X)]);
	if(P<100 && P>=1){
		i=(P/100)*length(X);
		
		if((P*length(X))%%100>0){
			i=round(i,digits=0);
			if(i==0){ return (X[1]);}
			return (X[i]);			
		}
		return ((X[i]+X[i+1])/2);
}
}