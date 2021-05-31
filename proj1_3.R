#install.packages("dgof")
library(dgof)
ks.test()

odchylenia <- seq(0.5, 20, 0.5)
wektor=NULL
odsetek=NULL
for(j in 1:38){
  
  for(i in 1:1000){
    x<-rnorm(200,5,odchylenia[j])
    y<-rnorm(200,5,10)
    testKS<-ks.test(x,y)
    wektor[i]<-testKS$p.value
  }
  odsetek[j] <- mean(wektor<0.05)
}


########################################

odchylenia2 <- seq(0.5, 20, 0.5)
wektor2=NULL
odsetek2=NULL
for(j in 1:38){
  
  for(i in 1:1000){
    x2<-rnorm(200,14,odchylenia2[j])
    y2<-rnorm(200,14,10)
    testKS2<-ks.test(x2,y2)
    wektor2[i]<-testKS2$p.value
  }
  odsetek2[j] <- mean(wektor2<0.05)
}
plot(odsetek, xaxt = "n", xlab = "Odchylenie standardowe", ylab = "Odsetek odrzuceń", main = "Moc testu w zależności od\n odchylenia standardowego", type = "l", col="green")
axis(1,at = seq(0,50,10),labels = c(0,odchylenia[10],odchylenia[20],odchylenia[30],odchylenia[40],odchylenia[50]))
#plot(odsetek2, xaxt = "n", xlab = "Odchylenie standardowe", ylab = "Odsetek odrzuceń", main = "Moc testu w zależności od\n odchylenia standardowego,\nm = 14", type="l")
#axis(1,at = seq(0,50,10),labels = c(0,odchylenia2[10],odchylenia2[20],odchylenia2[30],odchylenia2[40],odchylenia2[50]))
lines(odsetek2,col="blue")
legend("bottomleft",legend=c("m = 5", "m = 14"),col=c("green","blue"), lty=1, cex=0.8)

##########################################

liczba_danych3 <- seq(20, 250, 5)
wektor3=NULL
odsetek3=NULL
for(j in 1:45){
  
  for(i in 1:1000){
    x3<-rnorm(liczba_danych3[j],5,4)
    y3<-rnorm(liczba_danych3[j],5,2.25)
    testKS3<-ks.test(x3,y3)
    wektor3[i]<-testKS3$p.value
  }
  odsetek3[j] <- mean(wektor3<0.05)
}


############################################

liczba_danych4 <- seq(20, 250, 5)
wektor4=NULL
odsetek4=NULL
for(j in 1:45){
  
  for(i in 1:1000){
    x4<-rnorm(liczba_danych4[j],14,4)
    y4<-rnorm(liczba_danych4[j],14,2.25)
    testKS4<-ks.test(x4,y4)
    wektor4[i]<-testKS4$p.value
  }
  odsetek4[j] <- mean(wektor4<0.05)
}

plot(odsetek3, xaxt = "n", xlab = "Liczba danych", ylab = "Odsetek odrzuceń", main = "Moc testu w zależności od\n liczby danych", type="l", col = "green")
axis(1,at = seq(0,50,10),labels = c(0,liczba_danych3[10],liczba_danych3[20],liczba_danych3[30],liczba_danych3[40],liczba_danych3[50]))

#plot(odsetek4, xaxt = "n", xlab = "Liczba danych", ylab = "Odsetek odrzuceń", main = "Moc testu w zależności od\n liczby danych,\nm = 14", type = "l", col = "red")
#axis(1,at = seq(0,50,10),labels = c(0,liczba_danych4[10],liczba_danych4[20],liczba_danych4[30],liczba_danych4[40],liczba_danych4[50]))

lines(odsetek4,col="blue")
legend("bottomright",legend=c("m = 5", "m = 14"),col=c("green","blue"), lty=1, cex=0.8)
#par(mfrow=c(2,2))

#plot(odsetek, xaxt = "n", xlab = "Odchylenie standardowe", ylab = "Odsetek odrzuceń", main = "Moc testu w zależności od\n odchylenia standardowego,\nm = 5")
#axis(1,at = seq(0,50,10),labels = c(0,odchylenia[10],odchylenia[20],odchylenia[30],odchylenia[40],odchylenia[50]))

#plot(odsetek2, xaxt = "n", xlab = "Odchylenie standardowe", ylab = "Odsetek odrzuceń", main = "Moc testu w zależności od\n odchylenia standardowego,\nm = 14")
#axis(1,at = seq(0,50,10),labels = c(0,odchylenia2[10],odchylenia2[20],odchylenia2[30],odchylenia2[40],odchylenia2[50]))

#plot(odsetek3, xaxt = "n", xlab = "Liczba danych", ylab = "Odsetek odrzuceń", main = "Moc testu w zależności od\n liczby danych,\nm = 5")
#axis(1,at = seq(0,50,10),labels = c(0,liczba_danych3[10],liczba_danych3[20],liczba_danych3[30],liczba_danych3[40],liczba_danych3[50]))

#plot(odsetek4, xaxt = "n", xlab = "Liczba danych", ylab = "Odsetek odrzuceń", main = "Moc testu w zależności od\n liczby danych,\nm = 14")
#axis(1,at = seq(0,50,10),labels = c(0,liczba_danych4[10],liczba_danych4[20],liczba_danych4[30],liczba_danych4[40],liczba_danych4[50]))


##Drugi przypadek##

liczba_danych3_1 <- seq(20, 250, 5)
wektor3_1=NULL
odsetek3_1=NULL
for(j in 1:45){
  
  for(i in 1:1000){
    x3_1<-rnorm(liczba_danych3_1[j],5,4)
    y3_1<-rnorm(20,5,2.25)
    testKS3_1<-ks.test(x3_1,y3_1)
    wektor3_1[i]<-testKS3_1$p.value
  }
  odsetek3_1[j] <- mean(wektor3_1<0.05)
}


############################################

liczba_danych4_1 <- seq(20, 250, 5)
wektor4_1=NULL
odsetek4_1=NULL
for(j in 1:45){
  
  for(i in 1:1000){
    x4_1<-rnorm(liczba_danych4_1[j],14,4)
    y4_1<-rnorm(20,14,2.25)
    testKS4_1<-ks.test(x4_1,y4_1)
    wektor4_1[i]<-testKS4_1$p.value
  }
  odsetek4_1[j] <- mean(wektor4_1<0.05)
}

plot(odsetek3_1, xaxt = "n", xlab = "Liczba danych", ylab = "Odsetek odrzuceń", main = "Moc testu w zależności od\n liczby danych", type="l", col = "green")
axis(1,at = seq(0,50,10),labels = c(0,liczba_danych3_1[10],liczba_danych3_1[20],liczba_danych3_1[30],liczba_danych3_1[40],liczba_danych3_1[50]))
lines(odsetek4_1,col="blue")
legend("bottomright",legend=c("m = 5", "m = 14"),col=c("green","blue"), lty=1, cex=0.8)


##Trzeci przypadek##

############################################

liczba_danych3_2 <- seq(20, 250, 5)
wektor3_2=NULL
odsetek3_2=NULL
for(j in 1:45){ 
  
  for(i in 1:1000){
    x3_2<-rnorm(liczba_danych3_2[j],5,4)
    y3_2<-rnorm(250,5,2.25)
    testKS3_2<-ks.test(x3_2,y3_2)
    wektor3_2[i]<-testKS3_2$p.value
  }
  odsetek3_2[j] <- mean(wektor3_2<0.05)
}


############################################

liczba_danych4_2 <- seq(20, 250, 5)
wektor4_2=NULL
odsetek4_2=NULL
for(j in 1:45){
  
  for(i in 1:1000){
    x4_2<-rnorm(liczba_danych4_2[j],14,4)
    y4_2<-rnorm(250,14,2.25)
    testKS4_2<-ks.test(x4_2,y4_2)
    wektor4_2[i]<-testKS4_2$p.value
  }
  odsetek4_2[j] <- mean(wektor4_2<0.05)
}

plot(odsetek3_2, xaxt = "n", xlab = "Liczba danych", ylab = "Odsetek odrzuceń", main = "Moc testu w zależności od\n liczby danych", type="l", col = "green")
axis(1,at = seq(0,50,10),labels = c(0,liczba_danych3_2[10],liczba_danych3_2[20],liczba_danych3_2[30],liczba_danych3_2[40],liczba_danych3_2[50]))
lines(odsetek4_2,col="blue")
legend("bottomright",legend=c("m = 5", "m = 14"),col=c("green","blue"), lty=1, cex=0.8)



######################################################################
######################################################################

odchylenia5 <- seq(0.5, 12, 0.1)
wektor5=NULL
odsetek5=NULL
for(j in 1:120){
  
  for(i in 1:1000){
    x5<-rnorm(200,5,odchylenia5[j]/2.25)
    y5<-rnorm(200,5,2.25)
    testKS5<-ks.test(x5,y5)
    wektor5[i]<-testKS5$p.value
  }
  odsetek5[j] <- mean(wektor5<0.05)
}

############################################

odchylenia6 <- seq(0.5, 12, 0.1)
wektor6=NULL
odsetek6=NULL
for(j in 1:100){
  
  for(i in 1:1000){
    x6<-rnorm(200,14,odchylenia6[j]/2.25)
    y6<-rnorm(200,14,2.25)
    testKS<-ks.test(x6,y6)
    wektor6[i]<-testKS$p.value
  }
  odsetek6[j] <- mean(wektor6<0.05)
}

plot(odsetek5, xaxt = "n", xlab = "Odchylenie standardowe", ylab = "Odsetek odrzuceń", main = "Moc testu w zależności od\n odchylenia standardowego", type = "l", col="green")
axis(1,at = seq(0,50,10),labels = c(0,round(odchylenia5[10]/2.25,2),round(odchylenia5[20]/2.25,2),round(odchylenia5[30]/2.25,2),round(odchylenia5[40]/2.25,2),round(odchylenia5[50]/2.25,2)))
#plot(odsetek2, xaxt = "n", xlab = "Odchylenie standardowe", ylab = "Odsetek odrzuceń", main = "Moc testu w zależności od\n odchylenia standardowego,\nm = 14", type="l")
#axis(1,at = seq(0,50,10),labels = c(0,odchylenia2[10],odchylenia2[20],odchylenia2[30],odchylenia2[40],odchylenia2[50]))
lines(odsetek6,col="blue")
legend("bottomright",legend=c("m = 5", "m = 14"),col=c("green","blue"), lty=1, cex=0.8)

##################################################

odchylenia7 <- seq(0.5, 10, 0.1)
wektor7=NULL
odsetek7=NULL
for(j in 1:96){
  
  for(i in 1:1000){
    x7<-rnorm(200,14,odchylenia7[j])
    y7<-rnorm(200,14,1)
    testKS7<-ks.test(x7,y7)
    wektor7[i]<-testKS7$p.value
  }
  odsetek7[j] <- mean(wektor7<0.05)
}

####################################################

odchylenia8 <- seq(0.5, 10, 0.1)
wektor8=NULL
odsetek8=NULL
for(j in 1:96){
  
  for(i in 1:1000){
    x8<-rnorm(200,14,odchylenia8[j])
    y8<-rnorm(200,14,2)
    testKS8<-ks.test(x8,y8)
    wektor8[i]<-testKS8$p.value
  }
  odsetek8[j] <- mean(wektor8<0.05)
}

o7 = odchylenia7/1
plot(o7,odsetek7,type="l",col="green", xlim=c(0,3), xlab="")

o8 = odchylenia8/2
lines(o8,odsetek8,col="blue")

plot(odsetek7, xaxt = "n", xlab = "Odchylenie standardowe", ylab = "Odsetek odrzuceń", main = "Moc testu w zależności od\n odchylenia standardowego", type = "l", col="green", xlim = c(0,50))
axis(1,at = seq(0,80,10),labels = c(0,odchylenia7[10],odchylenia7[20],odchylenia7[30],odchylenia7[40],odchylenia7[50], odchylenia7[60], odchylenia7[70], odchylenia7[80]))
#plot(odsetek2, xaxt = "n", xlab = "Odchylenie standardowe", ylab = "Odsetek odrzuceń", main = "Moc testu w zależności od\n odchylenia standardowego,\nm = 14", type="l")
#axis(1,at = seq(0,50,10),labels = c(0,odchylenia2[10],odchylenia2[20],odchylenia2[30],odchylenia2[40],odchylenia2[50]))
lines(odsetek8,col="blue")
legend("bottomright",legend=c("sigma_0 = 1", "sigma_0 = 2"),col=c("green","blue"), lty=1, cex=0.8)
