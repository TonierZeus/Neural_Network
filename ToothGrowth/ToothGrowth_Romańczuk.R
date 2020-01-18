library(AMORE)
data(ToothGrowth)
ToothGrowth
table(ToothGrowth$supp)

l.danych=nrow(ToothGrowth)
set.seed(8)
idxTren<-sample(1:l.danych,40)    
idxTest<-setdiff(1:l.danych,idxTren) 

target<-function(x)
{
  n<-length(x)
  wartosci<-levels(x)
  l<-length(wartosci)
  T<-matrix(0,nrow=n,ncol=l)
  for(i in 1:l)
    T[,i]<-(x==wartosci[i])
  colnames(T)<-wartosci
  return(T)
}
wZadane<-target(ToothGrowth$supp)
wZadane

set.seed(8)

siec<-newff(n.neurons=c(2,2,2),
            learning.rate.global=0.03,
            momentum.global=0.5,
            hidden.layer="sigmoid",
            output.layer="purelin",
            method="ADAPTgdwm",
            error.criterium="LMS")

wynik<-train(siec,
             ToothGrowth[idxTren,c(1,3)],
             wZadane[idxTren,],
             error.criterium="LMS",
             report=TRUE,
             show.step=10,
             n.shows=800)


plot(wynik$Merror,type="l",xlab="Ileracja (x10)",
     ylab="Błąd", col="darkred")

y<-sim(wynik$net,ToothGrowth[idxTest, c(1,3)])
y

test.klasyf<-function(zad,wy)
{
  zadane<-max.col(zad)
  rozpoznane<-max.col(wy)
  print(table(zadane,rozpoznane))
}
wynik<-test.klasyf(wZadane[idxTest,],y)

cat("Dokładność klasyfikacji:",
    sum(diag(wynik))/sum(wynik)*100, "%\n")

