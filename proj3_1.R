library(caret)
library(np)
library(nortest)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(plm)

#dane<-dane[-c(1,2),]
#colnames(dane)=c("Nazwa", "bezrobocie", "ludnosc", "wynagrodzenie", "dodatki.mieszkaniowe")
#dane$bezrobocie<-as.numeric(dane$bezrobocie)
#dane$ludnosc<-as.numeric(dane$ludnosc)
#dane$wynagrodzenie<-as.numeric(dane$wynagrodzenie)
#dane$dodatki.mieszkaniowe<-as.numeric(dane$dodatki.mieszkaniowe)

#bezr1 = sample(dane$bezrobocie, size = 140, replace = FALSE)

#Statystyki opisowe

summary(dane$bezrobocie)
summary(dane$ludnosc)
summary(dane$wynagrodzenie)
summary(dane$dodatki.mieszkaniowe)

sd(dane$bezrobocie)
sd(dane$ludnosc)
sd(dane$wynagrodzenie)
sd(dane$dodatki.mieszkaniowe)

boxplot(dane$bezrobocie, outline = FALSE)
title(main = "Wykres pudelkowy dla bezrobocia", ylab = "Liczba osób bezrobotnych")
boxplot(dane$ludnosc,outline = FALSE)
title(main = "Wykres pudelkowy liczby ludnosci", ylab = "Liczba ludnosci")
boxplot(dane$wynagrodzenie, outline =FALSE)
title(main = "Wykres pudelkowy dla wynagrodzen", ylab = "Srednie miesieczne wynagrodzenie")
boxplot(dane$dodatki.mieszkaniowe, outline = FALSE)
title(main = "Wykres pudelkowy dla dodatków mieszkaniowych", ylab = "Wyplacone dodatki mieszkaniowe")

#Badanie normalnosci rozkladu

lillie.test(dane$bezrobocie)
lillie.test(dane$ludnosc)
lillie.test(dane$wynagrodzenie)
lillie.test(dane$dodatki.mieszkaniowe)

hist(dane$bezrobocie)
hist(dane$ludnosc)
hist(dane$wynagrodzenie)
hist(dane$dodatki.mieszkaniowe)

ggqqplot(dane$bezrobocie)
ggqqplot(dane$ludnosc)
ggqqplot(dane$wynagrodzenie)
ggqqplot(dane$dodatki.mieszkaniowe)


#wykres estymatorów gęstości rozkładu uzyskany z zastosowaniem trzech różnych jąder:

sigma_hat = min(IQR(dane$bezrobocie), sd(dane$bezrobocie))
h_hat = 1.06 * sigma_hat * length(dane$bezrobocie)^(-1/5)

plot(density(dane$bezrobocie,kernel="gaussian", bw = h_hat), col="green", main = "Estymatory gestosci rozkladu")
lines(density(dane$bezrobocie,kernel="epanechnikov", bw = h_hat), col="blue")
lines(density(dane$bezrobocie,kernel="cosine", bw = h_hat), col="red")

legend("topright",legend=c("gaussian", "epanechnikov","cosine"),
       col=c("green","blue","red"), lty=1, cex=0.8)

# MNK (ludnosc):

X_l = matrix(data=1, ncol=1, nrow=length(dane$ludnosc))
X_l = cbind(X_l, dane$ludnosc)

y = matrix(data=dane$bezrobocie, ncol=1)
a = solve(t(X_l) %*% X_l) %*% t(X_l) %*% y

plot(dane$ludnosc,dane$bezrobocie, 
     xlab="Liczba ludnosci w powiecie",
     
     ylab="Poziom bezrobocia w powiecie")
abline(a[1,1],a[2,1], col="green")
title("Wykres zależnosci liczby ludnosci od bezrobocia")

wsp_det_1= 1 - (t(y) %*% y - t(a) %*% t(X_l) %*% y)/
  (t(y) %*% y - length(dane$bezrobocie)*mean(y)^2)

# MNK (wynagrodzenie):

X_w = matrix(data=1, ncol=1, nrow=length(dane$wynagrodzenie))
X_w = cbind(X_w, dane$wynagrodzenie)

b = solve(t(X_w) %*% X_w) %*% t(X_w) %*% y

plot(dane$wynagrodzenie,dane$bezrobocie, xlim=c(3000,5000), 
     xlab="Średnie wynagrodzenie w powiecie",
     ylab="Poziom bezrobocia w powiecie")

abline(b[1,1],b[2,1], col="red")
#abline(coefficients(lm(dane$bezrobocie~dane$wynagrodzenie)), col = "pink", lwd=2, lty=2,)

title("Wykres zależnosci wynagrodzenia od bezrobocia")

wsp_det_2= 1 - (t(y) %*% y - t(b) %*% t(X_w) %*% y)/
  (t(y) %*% y - length(dane$bezrobocie)*mean(y)^2)

# MNK (dodatki mieszkaniowe):

X_d = matrix(data=1, ncol=1, nrow=length(dane$dodatki.mieszkaniowe))
X_d = cbind(X_d, dane$dodatki.mieszkaniowe)

c = solve(t(X_d) %*% X_d) %*% t(X_d) %*% y

plot(dane$dodatki.mieszkaniowe,dane$bezrobocie,  
     xlab="Wysokosc dodatków mieszkaniowych w powiecie",
     ylab="Poziom bezrobocia w powiecie")
abline(c[1,1],c[2,1], col="blue")
title("Wykres zależnosci wysokosci dodatkow mieszkaniowych od bezrobocia")

wsp_det_3= 1 - (t(y) %*% y - t(c) %*% t(X_d) %*% y)/
  (t(y) %*% y - length(dane$bezrobocie)*mean(y)^2)

# Regresja nieparametryczna

bw1 = npregbw(formula = bezrobocie ~ ludnosc, data = dane, regtype = "lc", 
              nmulti = 2)
bw1$bw

regresja1 = npreg(txdat = dane$ludnosc, tydat = dane$bezrobocie, bws = bw1)
plot(regresja1, plot.errors.method = "asymptotic")
summary(regresja1)

bw2 = npregbw(formula = bezrobocie ~ wynagrodzenie, data = dane, regtype = "lc",
              nmulti = 2)
bw2$bw

regresja2 = npreg(txdat = dane$wynagrodzenie, tydat = dane$bezrobocie, bws = bw2)
plot(regresja2, plot.errors.method = "asymptotic")
summary(regresja2)

bw3 = npregbw(formula = bezrobocie ~ dodatki.mieszkaniowe, data = dane, regtype = "lc", 
              nmulti = 2)
bw3$bw

regresja3 = npreg(txdat = dane$dodatki.mieszkaniowe, tydat = dane$bezrobocie, bws = bw3)
plot(regresja3, plot.errors.method = "asymptotic")
summary(regresja3)

# K-krotny sprawdzian krzyzowy dla MNK:

train_control <- trainControl(method = "cv", number = 10)
model <- train(bezrobocie ~ ludnosc, data = dane, method = "lm", 
                trControl = train_control)
print(model)


# K-krotny sprawdzian krzyzowy dla regresji nieparametrycznej:

bwb=npregbw(formula=bezrobocie~ludnosc, data = dane[-c(1:38),], regtype="lc", multi=2)
reg0=npreg(bwb)
reg=npreg(bwb,exdat=dane[1:38,3], txdat = dane[-c(1:38),3], tydat = unlist(dane[-c(1:38),2]))
m_hat=reg$mean
y=unlist(dane[1:38,2])
bledy1=abs(y-m_hat)


#2
bwb=npregbw(formula=bezrobocie~ludnosc, data = dane[-c(39:76),], regtype="lc", multi=2)
reg0=npreg(bwb)
reg=npreg(bwb,exdat=dane[39:76,3], txdat = dane[-c(39:76),3], tydat = unlist(dane[-c(39:76),2]))
m_hat=reg$mean
y=unlist(dane[39:76,2])
bledy2=abs(y-m_hat)


#3
bwb=npregbw(formula=bezrobocie~ludnosc, data = dane[-c(77:114),], regtype="lc", multi=2)
reg0=npreg(bwb)
reg=npreg(bwb,exdat=dane[77:114,3], txdat = dane[-c(77:114),3], tydat = unlist(dane[-c(77:114),2]))
m_hat=reg$mean
y=unlist(dane[77:114,2])
bledy3=abs(y-m_hat)

#4
bwb=npregbw(formula=bezrobocie~ludnosc, data = dane[-c(115:152),], regtype="lc", multi=2)
reg0=npreg(bwb)
reg=npreg(bwb,exdat=dane[115:152,3], txdat = dane[-c(115:152),3], tydat = unlist(dane[-c(115:152),2]))
m_hat=reg$mean
y=unlist(dane[115:152,2])
bledy4=abs(y-m_hat)

#5
bwb=npregbw(formula=bezrobocie~ludnosc, data = dane[-c(153:190),], regtype="lc", multi=2)
reg0=npreg(bwb)
reg=npreg(bwb,exdat=dane[153:190,3], txdat = dane[-c(153:190),3], tydat = unlist(dane[-c(153:190),2]))
m_hat=reg$mean
y=unlist(dane[153:190,2])
bledy5=abs(y-m_hat)

#6
bwb=npregbw(formula=bezrobocie~ludnosc, data = dane[-c(191:228),], regtype="lc", multi=2)
reg0=npreg(bwb)
reg=npreg(bwb,exdat=dane[191:228,3], txdat = dane[-c(191:228),3], tydat = unlist(dane[-c(191:228),2]))
m_hat=reg$mean
y=unlist(dane[191:228,2])
bledy6=abs(y-m_hat)

#7
bwb=npregbw(formula=bezrobocie~ludnosc, data = dane[-c(229:266),], regtype="lc", multi=2)
reg0=npreg(bwb)
reg=npreg(bwb,exdat=dane[229:266,3], txdat = dane[-c(229:266),3], tydat = unlist(dane[-c(229:266),2]))
m_hat=reg$mean
y=unlist(dane[229:266,2])
bledy7=abs(y-m_hat)

#8
bwb=npregbw(formula=bezrobocie~ludnosc, data = dane[-c(267:304),], regtype="lc", multi=2)
reg0=npreg(bwb)
reg=npreg(bwb,exdat=dane[267:304,3], txdat = dane[-c(267:304),3], tydat = unlist(dane[-c(267:304),2]))
m_hat=reg$mean
y=unlist(dane[267:304,2])
bledy8=abs(y-m_hat)

#9
bwb=npregbw(formula=bezrobocie~ludnosc, data = dane[-c(305:342),], regtype="lc", multi=2)
reg0=npreg(bwb)
reg=npreg(bwb,exdat=dane[305:342,3], txdat = dane[-c(305:342),3], tydat = unlist(dane[-c(305:342),2]))
m_hat=reg$mean
y=unlist(dane[305:342,2])
bledy9=abs(y-m_hat)

#10
bwb=npregbw(formula=bezrobocie~ludnosc, data = dane[-c(343:380),], regtype="lc", multi=2)
reg0=npreg(bwb)
reg=npreg(bwb,exdat=dane[343:380,3], txdat = dane[-c(343:380),3], tydat = unlist(dane[-c(343:380),2]))
m_hat=reg$mean
y=unlist(dane[343:380,2])
bledy10=abs(y-m_hat)


bledy_razem=c(bledy1,bledy2,bledy3,bledy4,bledy5,bledy6,bledy7,bledy8,bledy9,bledy10)
unlist(bledy_razem)
bledy_razem_kw = bledy_razem^2

mae_np=mean(bledy_razem)
rmse_np=sqrt(mean(bledy_razem_kw))



