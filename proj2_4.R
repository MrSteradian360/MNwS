#1.Moc ANOVA w zależnosci od liczebnosci grup dla różnych odchylen standardowych
# (liczebnosc zmienia sie we wszystkich probkach jednoczesnie):

# Odchylenie standardowe = 6
liczba_danych<-seq(21,255,3)
wektor=NULL
odsetek1=NULL
for(j in 1:79){
  for(i in 1:100){
    test1<-data.frame(
      wartosci1<-rnorm(liczba_danych[j],1,6),
      grupy1<-c("A")
    )
    test2<-data.frame(
      wartosci2<-rnorm(liczba_danych[j],2,6),
      grupy2<-c("B")
    )
    test3<-data.frame(
      wartosci3<-rnorm(liczba_danych[j],3,6),
      grupy3<-c("C")
    )
    
    colnames(test1)<-c("wartosci","grupy")
    colnames(test2)<-c("wartosci","grupy")
    colnames(test3)<-c("wartosci","grupy")
    
    test<-rbind(test1,test2,test3)
    
    anova_results<-aov(wartosci ~ grupy, data = test)
    wynik<-summary(anova_results)
    wektor[i]<-wynik[[1]]$`Pr(>F)`[1]
  }
  odsetek1[j] <- mean(wektor<0.05)
}
plot(odsetek1, type = "l", col="blue", xlab = "Liczebność grupy", ylab = "Moc testu",
     main = "Moc ANOVA w zależności od liczebności grup
     dla różnych odchyleń standardowych",xaxt = "n", ylim=c(0,1))

# Odchylenie standardowe = 3
wektor=NULL
odsetek2=NULL
for(j in 1:79){
  for(i in 1:100){
    test1<-data.frame(
      wartosci1<-rnorm(liczba_danych[j],1,3),
      grupy1<-c("A")
    )
    test2<-data.frame(
      wartosci2<-rnorm(liczba_danych[j],2,3),
      grupy2<-c("B")
    )
    test3<-data.frame(
      wartosci3<-rnorm(liczba_danych[j],3,3),
      grupy3<-c("C")
    )
    
    colnames(test1)<-c("wartosci","grupy")
    colnames(test2)<-c("wartosci","grupy")
    colnames(test3)<-c("wartosci","grupy")
    
    
    test<-rbind(test1,test2,test3)
    
    anova_results<-aov(wartosci ~ grupy, data = test)
    wynik<-summary(anova_results)
    wektor[i]<-wynik[[1]]$`Pr(>F)`[1]
  }
  odsetek2[j] <- mean(wektor<0.05)
}
lines(odsetek2, col = "green")

# Odchylenie standardowe = 9
wektor=NULL
odsetek3=NULL
for(j in 1:79){
  for(i in 1:100){
    test1<-data.frame(
      wartosci1<-rnorm(liczba_danych[j],1,9),
      grupy1<-c("A")
    )
    test2<-data.frame(
      wartosci2<-rnorm(liczba_danych[j],2,9),
      grupy2<-c("B")
    )
    test3<-data.frame(
      wartosci3<-rnorm(liczba_danych[j],3,9),
      grupy3<-c("C")
    )
    
    colnames(test1)<-c("wartosci","grupy")
    colnames(test2)<-c("wartosci","grupy")
    colnames(test3)<-c("wartosci","grupy")
    
    test<-rbind(test1,test2,test3)
    
    anova_results<-aov(wartosci ~ grupy, data = test)
    wynik<-summary(anova_results)
    wektor[i]<-wynik[[1]]$`Pr(>F)`[1]
  }
  odsetek3[j] <- mean(wektor<0.05)
}
lines(odsetek3, col = "red")
legend("bottomright",legend=c("sigma_0 = 3", "sigma_0 = 6","sigma_0 = 9"),
       col=c("green","blue","red"), lty=1, cex=0.8)
axis(1,at = seq(0,79,11),labels = c(21,liczba_danych[12],liczba_danych[23],liczba_danych[34],liczba_danych[45],liczba_danych[56],liczba_danych[67],liczba_danych[78]))



#2.Moc ANOVA w zależnosci od postaci falszywej hipotezy glownej 
#(wartosci oczekiwane zmieniaja sie w jednej grupie):

# Odchylenie standardowe = 6
srednie <- c(seq(0.1,6.1,0.1))
wektor=NULL
odsetek4=NULL
for(j in 1:60){
  for(i in 1:100){
    test1<-data.frame(
      wartosci1<-rnorm(255,3,6),
      grupy1<-c("A")
    )
    test2<-data.frame(
      wartosci2<-rnorm(255,3,6),
      grupy2<-c("B")
    )
    test3<-data.frame(
      wartosci3<-rnorm(255,srednie[j],6),
      grupy3<-c("C")
    )
    
    colnames(test1)<-c("wartosci","grupy")
    colnames(test2)<-c("wartosci","grupy")
    colnames(test3)<-c("wartosci","grupy")
    
    
    test<-rbind(test1,test2,test3)
    
    anova_results<-aov(wartosci ~ grupy, data = test)
    wynik<-summary(anova_results)
    wektor[i]<-wynik[[1]]$`Pr(>F)`[1]
  }
  odsetek4[j] <- mean(wektor<0.05)
}
plot(odsetek4, type = "l", col="blue", xlab = "Odstająca wartość oczekiwana", ylab = "Moc testu",
     main = "Moc ANOVA w zależności od\npostaci fałszywej hipotezy glównej",xaxt = "n", ylim=c(0,1))

# Odchylenie standardowe = 9
wektor=NULL
odsetek5=NULL
for(j in 1:60){
  for(i in 1:100){
    test1<-data.frame(
      wartosci1<-rnorm(255,3,9),
      grupy1<-c("A")
    )
    test2<-data.frame(
      wartosci2<-rnorm(255,3,9),
      grupy2<-c("B")
    )
    test3<-data.frame(
      wartosci3<-rnorm(255,srednie[j],9),
      grupy3<-c("C")
    )
    
    colnames(test1)<-c("wartosci","grupy")
    colnames(test2)<-c("wartosci","grupy")
    colnames(test3)<-c("wartosci","grupy")
    
    test<-rbind(test1,test2,test3)
    
    anova_results<-aov(wartosci ~ grupy, data = test)
    wynik<-summary(anova_results)
    wektor[i]<-wynik[[1]]$`Pr(>F)`[1]
  }
  odsetek5[j] <- mean(wektor<0.05)
}

lines(odsetek5, col = "red")

# Odchylenie standardowe = 3
wektor=NULL
odsetek6=NULL
for(j in 1:60){
  for(i in 1:100){
    test1<-data.frame(
      wartosci1<-rnorm(255,3,3),
      grupy1<-c("A")
    )
    test2<-data.frame(
      wartosci2<-rnorm(255,3,3),
      grupy2<-c("B")
    )
    test3<-data.frame(
      wartosci3<-rnorm(255,srednie[j],3),
      grupy3<-c("C")
    )
    
    colnames(test1)<-c("wartosci","grupy")
    colnames(test2)<-c("wartosci","grupy")
    colnames(test3)<-c("wartosci","grupy")
    
    test<-rbind(test1,test2,test3)
    
    anova_results<-aov(wartosci ~ grupy, data = test)
    wynik<-summary(anova_results)
    wektor[i]<-wynik[[1]]$`Pr(>F)`[1]
  }
  odsetek6[j] <- mean(wektor<0.05)
}
lines(odsetek6, col = "green")
legend("bottomright",legend=c("sigma_0 = 3", "sigma_0 = 6","sigma_0 = 9"),
       col=c("green","blue","red"), lty=1, cex=0.8)
axis(1,at = seq(0,60,10),labels = c(0,srednie[10],srednie[20],srednie[30],srednie[40],srednie[50],srednie[60]))


#3.Moc testu Kruskala-Wallisa w zależnosci od liczebnosci grup dla różnych odchylen standardowych
# (liczebnosc zmienia sie we wszystkich probkach jednoczesnie):

# Odchylenie standardowe = 6
liczba_danych<-seq(21,255,3)
wektor=NULL
odsetek7=NULL
for(j in 1:79){
  for(i in 1:100){
    test1<-data.frame(
      wartosci1<-rnorm(liczba_danych[j],1,6),
      grupy1<-c("A")
    )
    test2<-data.frame(
      wartosci2<-rnorm(liczba_danych[j],2,6),
      grupy2<-c("B")
    )
    test3<-data.frame(
      wartosci3<-rnorm(liczba_danych[j],3,6),
      grupy3<-c("C")
    )
    
    colnames(test1)<-c("wartosci","grupy")
    colnames(test2)<-c("wartosci","grupy")
    colnames(test3)<-c("wartosci","grupy")
    
    test<-rbind(test1,test2,test3)
    
    results<-kruskal.test(wartosci ~ grupy, data = test)
    wektor[i]<-results$p.value
  }
  odsetek7[j] <- mean(wektor<0.05)
}
plot(odsetek7, type = "l", col="blue", xlab = "Liczebność próbki", ylab = "Moc testu",
     main = "Moc testu Kruskala-Walisa\n w zależnosci od liczebności próbek 
     dla różnych odchyleń standardowych",xaxt = "n", ylim=c(0,1))

# Odchylenie standardowe = 3
wektor=NULL
odsetek8=NULL
for(j in 1:79){
  for(i in 1:100){
    test1<-data.frame(
      wartosci1<-rnorm(liczba_danych[j],1,3),
      grupy1<-c("A")
    )
    test2<-data.frame(
      wartosci2<-rnorm(liczba_danych[j],2,3),
      grupy2<-c("B")
    )
    test3<-data.frame(
      wartosci3<-rnorm(liczba_danych[j],3,3),
      grupy3<-c("C")
    )
    
    colnames(test1)<-c("wartosci","grupy")
    colnames(test2)<-c("wartosci","grupy")
    colnames(test3)<-c("wartosci","grupy")
    
    
    test<-rbind(test1,test2,test3)
    
    results<-kruskal.test(wartosci ~ grupy, data = test)
    wektor[i]<-results$p.value
  }
  odsetek8[j] <- mean(wektor<0.05)
}
lines(odsetek8, col = "green")

# Odchylenie standardowe = 9
wektor=NULL
odsetek9=NULL
for(j in 1:79){
  for(i in 1:100){
    test1<-data.frame(
      wartosci1<-rnorm(liczba_danych[j],1,9),
      grupy1<-c("A")
    )
    test2<-data.frame(
      wartosci2<-rnorm(liczba_danych[j],2,9),
      grupy2<-c("B")
    )
    test3<-data.frame(
      wartosci3<-rnorm(liczba_danych[j],3,9),
      grupy3<-c("C")
    )
    
    colnames(test1)<-c("wartosci","grupy")
    colnames(test2)<-c("wartosci","grupy")
    colnames(test3)<-c("wartosci","grupy")
    
    
    test<-rbind(test1,test2,test3)
    
    results<-kruskal.test(wartosci ~ grupy, data = test)
    wektor[i]<-results$p.value
  }
  odsetek9[j] <- mean(wektor<0.05)
}
lines(odsetek9, col = "red")
legend("bottomright",legend=c("sigma_0 = 3", "sigma_0 = 6","sigma_0 = 9"),
       col=c("green","blue","red"), lty=1, cex=0.8)
axis(1,at = seq(0,79,11),labels = c(21,liczba_danych[12],liczba_danych[23],liczba_danych[34],liczba_danych[45],liczba_danych[56],liczba_danych[67],liczba_danych[78]))


#4.Moc testu Kruskala-Wallisa w zależnosci od postaci falszywej hipotezy glownej:
#(wartosci oczekiwane zmieniaja sie w jednej grupie):

# Odchylenie standardowe = 6
srednie <- c(seq(0.1,6.1,0.1))
wektor=NULL
odsetek10=NULL
for(j in 1:60){
  for(i in 1:100){
    test1<-data.frame(
      wartosci1<-rnorm(255,3,6),
      grupy1<-c("A")
    )
    test2<-data.frame(
      wartosci2<-rnorm(255,3,6),
      grupy2<-c("B")
    )
    test3<-data.frame(
      wartosci3<-rnorm(255,srednie[j],6),
      grupy3<-c("C")
    )
    
    colnames(test1)<-c("wartosci","grupy")
    colnames(test2)<-c("wartosci","grupy")
    colnames(test3)<-c("wartosci","grupy")
    
    test<-rbind(test1,test2,test3)
    
    results<-kruskal.test(wartosci ~ grupy, data =test)
    wektor[i]<-results$p.value
  }
  odsetek10[j] <- mean(wektor<0.05)
}
plot(odsetek10, type = "l", col="blue", xlab = "Odstająca wartość oczekiwana", ylab = "Moc testu",
     main = "Moc testu Kruskala-Wallisa w zależności od\npostaci fałszywej hipotezy głównej",xaxt = "n", ylim=c(0,1))

# Odchylenie standardowe = 3
wektor=NULL
odsetek11=NULL
for(j in 1:60){
  for(i in 1:100){
    test1<-data.frame(
      wartosci1<-rnorm(255,3,3),
      grupy1<-c("A")
    )
    test2<-data.frame(
      wartosci2<-rnorm(255,3,3),
      grupy2<-c("B")
    )
    test3<-data.frame(
      wartosci3<-rnorm(255,srednie[j],3),
      grupy3<-c("C")
    )
    
    colnames(test1)<-c("wartosci","grupy")
    colnames(test2)<-c("wartosci","grupy")
    colnames(test3)<-c("wartosci","grupy")
    
    
    test<-rbind(test1,test2,test3)
    
    results<-kruskal.test(wartosci ~ grupy, data = test)
    wektor[i]<-results$p.value
  }
  odsetek11[j] <- mean(wektor<0.05)
}

lines(odsetek11, col = "green")

# Odchylenie standardowe = 9
wektor=NULL
odsetek12=NULL
for(j in 1:60){
  for(i in 1:100){
    test1<-data.frame(
      wartosci1<-rnorm(255,3,9),
      grupy1<-c("A")
    )
    test2<-data.frame(
      wartosci2<-rnorm(255,3,9),
      grupy2<-c("B")
    )
    test3<-data.frame(
      wartosci3<-rnorm(255,srednie[j],9),
      grupy3<-c("C")
    )
    
    colnames(test1)<-c("wartosci","grupy")
    colnames(test2)<-c("wartosci","grupy")
    colnames(test3)<-c("wartosci","grupy")
    
    test<-rbind(test1,test2,test3)
    
    results<-kruskal.test(wartosci ~ grupy, data = test)
    wektor[i]<-results$p.value
  }
  odsetek12[j] <- mean(wektor<0.05)
}
lines(odsetek12, col = "red")
legend("bottomright",legend=c("sigma_0 = 3", "sigma_0 = 6","sigma_0 = 9"),
       col=c("green","blue","red"), lty=1, cex=0.8)
axis(1,at = seq(0,60,10),labels = c(0,srednie[10],srednie[20],srednie[30],srednie[40],srednie[50],srednie[60]))


#5.Moc ANOVA w zależnosci od postaci falszywej hipotezy glownej
#(wartosci oczekiwane zmieniaja sie w dwóch grupach):

# Odchylenie standardowe = 6

srednie <- c(seq(0.1,6.1,0.1))
srednie2 <- c(seq(1.1,7.1,0.1))
wektor=NULL
odsetek13=NULL
for(j in 1:60){
  for(i in 1:100){
    test1<-data.frame(
      wartosci1<-rnorm(255,3,6),
      grupy1<-c("A")
    )
    test2<-data.frame(
      wartosci2<-rnorm(255,srednie[j],6),
      grupy2<-c("B")
    )
    test3<-data.frame(
      wartosci3<-rnorm(255,srednie[j],6),
      grupy3<-c("C")
    )
    
    colnames(test1)<-c("wartosci","grupy")
    colnames(test2)<-c("wartosci","grupy")
    colnames(test3)<-c("wartosci","grupy")
    
    test<-rbind(test1,test2,test3)
    
    anova_results<-aov(wartosci ~ grupy, data = test)
    wynik<-summary(anova_results)
    wektor[i]<-wynik[[1]]$`Pr(>F)`[1]
  }
  odsetek13[j] <- mean(wektor<0.05)
}
plot(odsetek13, type = "l", col="blue", xlab = "Odstająca wartość oczekiwana", ylab = "Moc testu",
     main = "Moc ANOVA w zależności od\npostaci fałszywej hipotezy glównej",xaxt = "n", ylim=c(0,1))

# Odchylenie standardowe = 9
wektor=NULL
odsetek14=NULL
for(j in 1:60){
  for(i in 1:100){
    test1<-data.frame(
      wartosci1<-rnorm(255,3,9),
      grupy1<-c("A")
    )
    test2<-data.frame(
      wartosci2<-rnorm(255,srednie[j],9),
      grupy2<-c("B")
    )
    test3<-data.frame(
      wartosci3<-rnorm(255,srednie[j],9),
      grupy3<-c("C")
    )
    
    colnames(test1)<-c("wartosci","grupy")
    colnames(test2)<-c("wartosci","grupy")
    colnames(test3)<-c("wartosci","grupy")
    
    
    test<-rbind(test1,test2,test3)
    
    anova_results<-aov(wartosci ~ grupy, data = test)
    wynik<-summary(anova_results)
    wektor[i]<-wynik[[1]]$`Pr(>F)`[1]
  }
  odsetek14[j] <- mean(wektor<0.05)
}

lines(odsetek14, col = "red")

# Odchylenie standardowe = 3
wektor=NULL
odsetek15=NULL
for(j in 1:60){
  for(i in 1:100){
    test1<-data.frame(
      wartosci1<-rnorm(255,3,3),
      grupy1<-c("A")
    )
    test2<-data.frame(
      wartosci2<-rnorm(255,srednie[j],3),
      grupy2<-c("B")
    )
    test3<-data.frame(
      wartosci3<-rnorm(255,srednie[j],3),
      grupy3<-c("C")
    )
    
    colnames(test1)<-c("wartosci","grupy")
    colnames(test2)<-c("wartosci","grupy")
    colnames(test3)<-c("wartosci","grupy")
    
    
    test<-rbind(test1,test2,test3)
    
    anova_results<-aov(wartosci ~ grupy, data = test)
    wynik<-summary(anova_results)
    wektor[i]<-wynik[[1]]$`Pr(>F)`[1]
  }
  odsetek15[j] <- mean(wektor<0.05)
}
lines(odsetek15, col = "green")
legend("bottomright",legend=c("sigma_0 = 3", "sigma_0 = 6","sigma_0 = 9"),
       col=c("green","blue","red"), lty=1, cex=0.8)
axis(1,at = seq(0,60,10),labels = c(0,srednie[10],srednie[20],srednie[30],srednie[40],srednie[50],srednie[60]))

#6.Moc testu Kruskala-Wallisa w zależnosci od postaci falszywej hipotezy glownej
#(wartosci oczekiwane zmieniaja sie w dwóch grupach):

srednie <- c(seq(0.1,6.1,0.1))
wektor=NULL

# Odchylenie standardowe = 6
odsetek16=NULL
for(j in 1:60){
  for(i in 1:100){
    test1<-data.frame(
      wartosci1<-rnorm(255,3,6),
      grupy1<-c("A")
    )
    test2<-data.frame(
      wartosci2<-rnorm(255,srednie[j],6),
      grupy2<-c("B")
    )
    test3<-data.frame(
      wartosci3<-rnorm(255,srednie[j],6),
      grupy3<-c("C")
    )
    
    colnames(test1)<-c("wartosci","grupy")
    colnames(test2)<-c("wartosci","grupy")
    colnames(test3)<-c("wartosci","grupy")
    
    test<-rbind(test1,test2,test3)
    
    results<-kruskal.test(wartosci ~ grupy, data =test)
    wektor[i]<-results$p.value
  }
  odsetek16[j] <- mean(wektor<0.05)
}
plot(odsetek16, type = "l", col="blue", xlab = "Odstająca wartość oczekiwana", ylab = "Moc testu",
     main = "Moc testu Kruskala-Wallisa w zależności od\npostaci fałszywej hipotezy glównej",xaxt = "n", ylim=c(0,1))

# Odchylenie standardowe = 3
wektor=NULL
odsetek17=NULL
for(j in 1:60){
  for(i in 1:100){
    test1<-data.frame(
      wartosci1<-rnorm(255,3,3),
      grupy1<-c("A")
    )
    test2<-data.frame(
      wartosci2<-rnorm(255,srednie[j],3),
      grupy2<-c("B")
    )
    test3<-data.frame(
      wartosci3<-rnorm(255,srednie[j],3),
      grupy3<-c("C")
    )
    
    colnames(test1)<-c("wartosci","grupy")
    colnames(test2)<-c("wartosci","grupy")
    colnames(test3)<-c("wartosci","grupy")
    
    
    test<-rbind(test1,test2,test3)
    
    results<-kruskal.test(wartosci ~ grupy, data = test)
    wektor[i]<-results$p.value
  }
  odsetek17[j] <- mean(wektor<0.05)
}

lines(odsetek17, col = "green")

# Odchylenie standardowe = 9
wektor=NULL
odsetek18=NULL
for(j in 1:60){
  for(i in 1:100){
    test1<-data.frame(
      wartosci1<-rnorm(255,3,9),
      grupy1<-c("A")
    )
    test2<-data.frame(
      wartosci2<-rnorm(255,srednie[j],9),
      grupy2<-c("B")
    )
    test3<-data.frame(
      wartosci3<-rnorm(255,srednie[j],9),
      grupy3<-c("C")
    )
    
    colnames(test1)<-c("wartosci","grupy")
    colnames(test2)<-c("wartosci","grupy")
    colnames(test3)<-c("wartosci","grupy")
    
    test<-rbind(test1,test2,test3)
    
    results<-kruskal.test(wartosci ~ grupy, data = test)
    wektor[i]<-results$p.value
  }
  odsetek18[j] <- mean(wektor<0.05)
}
lines(odsetek18, col = "red")
legend("bottomright",legend=c("sigma_0 = 3", "sigma_0 = 6","sigma_0 = 9"),
       col=c("green","blue","red"), lty=1, cex=0.8)
axis(1,at = seq(0,60,10),labels = c(0,srednie[10],srednie[20],srednie[30],srednie[40],srednie[50],srednie[60]))

#Porównanie mocy testow ANOVA i Kruskala-Wallisa w zależnosci od liczebnosci grup

plot(odsetek1, type = "l", col="blue", xlab = "Liczebność próbki", ylab = "Moc testu",
     main = "Porównanie mocy testów ANOVA i Kruskala-Wallisa\nw zależnosci od liczebnosci próbek\nsigma_0 = 6",xaxt = "n", ylim=c(0,1))
lines(odsetek7, col = "red")
legend("bottomright",legend=c("ANOVA", "Kruskal-Wallis"),
       col=c("blue","red"), lty=1, cex=0.8)
axis(1,at = seq(0,79,11),labels = c(21,liczba_danych[12],liczba_danych[23],liczba_danych[34],liczba_danych[45],liczba_danych[56],liczba_danych[67],liczba_danych[78]))

#Porównanie mocy testow ANOVA i Kruskala-Wallisa w zależnosci od postaci falszywej hipotezy glownej (zmiana w jednej grupie)

plot(odsetek4, type = "l", col="blue", xlab = "Odstająca wartość oczekiwana", ylab = "Moc testu",
     main = "Porównanie mocy testów ANOVA i Kruskala-Wallisa\nw zależności od postaci fałszywej hipotezy glównej\nsigma_0 = 6",xaxt = "n", ylim=c(0,1))
lines(odsetek10, col = "red")
legend("bottomright",legend=c("ANOVA", "Kruskal-Wallis"),
       col=c("blue","red"), lty=1, cex=0.8)
axis(1,at = seq(0,60,10),labels = c(0,srednie[10],srednie[20],srednie[30],srednie[40],srednie[50],srednie[60]))

#Porównanie mocy testow ANOVA i Kruskala-Wallisa w zależnosci od postaci falszywej hipotezy glownej (zmiana w dwóch grupach)

plot(odsetek13, type = "l", col="blue", xlab = "Odstająca wartość oczekiwana", ylab = "Moc testu",
     main = "Porównanie mocy testów ANOVA i Kruskala-Wallisa\nw zależności od postaci fałszywej hipotezy glównej\nsigma_0 = 6",xaxt = "n", ylim=c(0,1))
lines(odsetek16, col = "red")
legend("bottomright",legend=c("ANOVA", "Kruskal-Wallis"),
       col=c("blue","red"), lty=1, cex=0.8)
axis(1,at = seq(0,60,10),labels = c(0,srednie[10],srednie[20],srednie[30],srednie[40],srednie[50],srednie[60]))





