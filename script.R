require(data.table)
DiabetesData=data.table(read.csv('diabetes.csv'))
##Quick data summary
summary(DiabetesData)
##Scatter plot matrix
require(GGally)
ggpairs(DiabetesData,lower = list(continuous='smooth'))
###first model
glm1=glm(Outcome~.,DiabetesData, family=binomial(link="logit"))
summary(glm1)
require(stargazer)
stargazer(glm1,type='text')

###second model
glm2=glm(Outcome~.,data=DiabetesData[,c(1:3,6:7,9),with=F], family=binomial(link="logit"))
summary(glm2)

###Correctly classified observations
mean((glm2$fitted.values>0.5)==DiabetesData$Outcome)
###Confusion matrix count
RP=sum((glm2$fitted.values>=0.5)==DiabetesData$Outcome & DiabetesData$Outcome==1)
FP=sum((glm2$fitted.values>=0.5)!=DiabetesData$Outcome & DiabetesData$Outcome==0)
RN=sum((glm2$fitted.values>=0.5)==DiabetesData$Outcome & DiabetesData$Outcome==0)
FN=sum((glm2$fitted.values>=0.5)!=DiabetesData$Outcome & DiabetesData$Outcome==1)
confMat<-matrix(c(RP,FP,FN,RN),ncol = 2)
colnames(confMat)<-c("Pred Diabetes",'Pred no diabetes')
rownames(confMat)<-c("Real Diabetes",'Real no diabetes')
confMat

###Confusion matrix proportion
RPR=RP/sum(DiabetesData$Outcome==1)*100
FNR=FN/sum(DiabetesData$Outcome==1)*100
FPR=FP/sum(DiabetesData$Outcome==0)*100
RNR=RN/sum(DiabetesData$Outcome==0)*100
confMat<-matrix(c(RPR,FPR,FNR,RNR),ncol = 2)
colnames(confMat)<-c("Pred Diabetes",'Pred no diabetes')
rownames(confMat)<-c("Real Diabetes",'Real no diabetes')
confMat

####Plot and decision boundaries
require(ggplot2)
DiabetesData$Predicted<-glm2$fitted.values
ggplot(DiabetesData,aes(x=BMI,y=Glucose,color=Predicted>0.5))+geom_point(size=2,alpha=0.5)

###BMI vs predicted
ggplot(DiabetesData,aes(x=BMI,y=Glucose,color=Outcome==(Predicted>0.5)))+geom_point(size=2,alpha=0.5)
BMI_plot=data.frame(BMI=((min(DiabetesData$BMI-2)*100):(max(DiabetesData$BMI+2)*100))/100,
                    Glucose=mean(DiabetesData$Glucose),
                    Pregnancies=mean(DiabetesData$Pregnancies),
                    BloodPressure=mean(DiabetesData$BloodPressure),
                    DiabetesPedigreeFunction=mean(DiabetesData$DiabetesPedigreeFunction))
BMI_plot$Predicted=predict(glm2,BMI_plot,type = 'response')
ggplot(BMI_plot,aes(x=BMI,y=Predicted))+geom_line()

###Glucose vs predicted
Glucose_plot=data.frame(Glucose=((min(DiabetesData$Glucose-2)*100):(max(DiabetesData$Glucose+2)*100))/100,
                    BMI=mean(DiabetesData$BMI),
                    Pregnancies=mean(DiabetesData$Pregnancies),
                    BloodPressure=mean(DiabetesData$BloodPressure),
                    DiabetesPedigreeFunction=mean(DiabetesData$DiabetesPedigreeFunction))
Glucose_plot$Predicted=predict(glm2,Glucose_plot,type = 'response')
ggplot(Glucose_plot,aes(x=Glucose,y=Predicted))+geom_line()
