library(AMORE)
data("infert")
infert
table(infert$education)

l.danych=nrow(infert)
set.seed(8)

idxTren<-sample(1:l.danych,2*l.danych/3)    
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
wZadane<-target(infert$education)


set.seed(128)

siec<-newff(n.neurons=c(6,6,3),
            learning.rate.global=0.03,
            momentum.global=0.5,
            hidden.layer="sigmoid",
            output.layer="purelin",
            method="ADAPTgdwm",
            error.criterium="LMS")


wynik<-train(siec,
             infert[idxTren,c(2,3,4,5,6,7,8)],
             wZadane[idxTren,],
             error.criterium="LMS",
             report=TRUE,
             show.step=10,
             n.shows=800)


plot(wynik$Merror,type="l",xlab="Ileracja (x10)",
     ylab="Błąd", col="darkred")

y<-sim(wynik$net,infert[idxTest, c(2,3,4,5,6,7,8)])
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
