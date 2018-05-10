#Part 1 Reading the file and assigning values
x<-read.csv(file=file.choose(),header=TRUE,sep=",")
x1<-x[,1]
y1<-x[,2]
x1<-x1[!is.na(x1)]
y1<-y1[!is.na(y1)]
n1<-length(x1)
n2<-length(y1)
CSK<-as.character(x[1,3])
DI<-as.character(x[2,3])
x1y1<-c(x1,y1)

#Part 2 Setup
IQRx<-IQR(x1,type = 6)
Q1x<-summary(x1)[2]
Q1x
Q3x<-summary(x1)[5]
Q3x
IQRy<-IQR(y1,type = 6)
Q1y<-summary(y1)[2]
Q1y
Q3y<-summary(y1)[5]
Q3y

hasXOut<-FALSE
hasYOut<-FALSE
for(i in x1){
	if((i < Q1x - (1.5*IQRx)) || (i > Q3x + (1.5*IQRx))){
		print(paste0(i , ": Is an outliar"))
		hasXOut = TRUE
	}
}
for(j in y1){
	if((j < Q1y - (1.5*IQRy)) || (j > Q3y + (1.5*IQRy))){
		print(paste0(j , ": Is an outliar"))
		hasYOut = TRUE
	}
}

#Normality should be accepted if the Shapiro Wilk test returns a p-value>0.05
normalX<-shapiro.test(x1)[2]
normalX
normalY<-shapiro.test(y1)[2]
normalY
shapiro.test
signT<-x1-y1
signTNormal<-shapiro.test(signT)[2]
signTNormal

summary(y1)
sdx<-sd(x1)
sdy<-sd(y1)
ratio<-0
equalVar <- TRUE

if(sdx > sdy){
	ratio = sdx / sdy
}else{
	ratio = sdy / sdx
}

if(ratio > 2){
	equalVar <-FALSE
}

#All hypothesis testing is assuming not equal to each other
#Significance level for our decision will be 0.05

CSK<-as.character(x[1,3])
DI<-as.character(x[2,3])

DI

if(CSK == 'K' && n1 > 10 && n2 >10){
	print("Two Proportion Z Test")
}

if((CSK == 'S') && (normalX >0.05) && (normalY >0.05)){
	print("F Test")
}

if(CSK == 'C'){
	if(DI == 'I'){
		if((equalVar == true) && (normalX >0.05) && (normalY >0.05)){
			print("pooled two sample")
		}
		else if((normalX >0.05) && (normalY >0.05)){
			print("2 sample t test")
		}
		else{
			print("abort")
		}
	}
	else if((DI == 'D') && (n1==n2)){
		if(signTNormal){
			print("paired t")
		}
		else{
			print("sign")
		}
	}
}
else{
	print("abort")
}





