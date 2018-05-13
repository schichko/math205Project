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



if(CSK == 'K' && n1 > 10 && n2 >10){
	#The Two Proportion test
	xSuccess<-0
	ySuccess<-0
	for(i in 1:length(x1)){
		if(x1[i] == 1){
			xSuccess=xSuccess + 1
		}
	}
	for(i in 1:length(y1)){
		if(y1[i] == 1){
			ySuccess=ySuccess + 1
		}
	}
	p1<-xSuccess/n1
	p2<-ySuccess/n2
	p<-(xSuccess+ySuccess)/(n1+n2)
	z<-(((p1-p2)-0)/sqrt(p*(1-p)*((1/n1)+(1/n2))))
	z
	pnorm(z)
	print("Proportions test")
}else{
	if(CSK == 'K'){
	print("The two-proportion test could not be run because n1 or n2 is less than 10")
	}else{
	print("The two-proportion test could not be run because the value of CSK was not equal to K, meaning our data is not a count of successes and failures")
	}
}

if((CSK == 'S') && (normalX >0.05) && (normalY >0.05)){
	#The F Test
	var.test(x1,y1,alternative = "two.sided")
}else{
	if(CSK == 'S'){
	print("The F test could not be run because the x and y values are not normal")
	}else{
	print("The F test could not be run because the CSK value is not 'S', meaning our data is not spread")
	}
}

if(CSK == 'C'){
	if(DI == 'I'){
		if((equalVar == TRUE) && (normalX >0.05) && (normalY >0.05)){
			print("We can preform the pooled two sample t test because we have approximately equal variance and our data is approximately normal")
			t.test(x1,y1,alternative = "two.sided", var.equal = TRUE,conf.level = 0.95)
		}else if((normalX >0.05) && (normalY >0.05)){
			print("We cannot perform  the pooled two-sample t test because our variance is not approximately equal")
		      print("We can perform the two-sample t test because even though our variance is not approximately equal,our data is approximately normal")
			p<-t.test(x1,y1,alternative = "two.sided", var.equal = FALSE,conf.level = 0.95)[3]
			if(p>=0.05){
				print(paste0("Fail to reject Ho: p=", p))
			}else{
				print(paste0("Reject Ho: p=", p))
			}
		}else{
			print("Even though our data is Centerd and Independed, we can not preform either two sample t test because our data is not approximately normal")
		}
	}else{
		print("We cannot perform the pooled two-sample t test or the two-sample t test because our data is not independent")
	}

	if((DI == 'D') && (n1==n2)){
		if(signTNormal > 0.05){
			print("Since our data is paired, Depended and our signTNormal, which was generated from the shapiro test is greater than 0.05 we can use the paired t test")
			p<-t.test(x1,y1,alternative = "two.sided", paired = TRUE, conf.level = 0.95)[3]
			if(p>=0.05){
				print(paste0("Fail to reject Ho: p=", p))
			}else{
				print(paste0("Reject Ho: p=", p))
			}
		}else{
			print("We can not use the paired t test because our signTNormal, gathered from the shapiro test, is less than 0.05")
			print("Since our data is dependet and the size of x and y are equal we can use the Sign test")
			##Sign test, first we initlize an empty array called d
			d<-list()
			for(i in 1:length(x1)){
			#Then we go through the x1 and y1 and subtract them from eachother
				if(x1[i] - y1[i] == 0){
				#We ignore if the difference is 0
				}
				else{
					#if the difference is not 0 we store it in d[i]
					d[i]<-x1[i] - y1[i]
				}
			}
			d
			print("sign")
		}
	}
}else{
	print("None of the tests,pooled two sample test, two sample t test, paired t or sign test could be run because CSK was not equal to C")
}




 

