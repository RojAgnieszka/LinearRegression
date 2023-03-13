

# Importowanie bibliotek:

library(dlookr)
library(raster)
library(ggplot2)

# Importowanie danych:

df <- read.table('dane1.txt', header=TRUE)
View(df)
names(df)[3] <- 'plec' # uproszczenie nazwy dla wygody
names(df)
View(df)

# Analiza zbioru danych:

head(df) 
tail(df) 

str(df)  
dim(df)  

colnames(df) 

# podstawowe statystyki
summary(df) 
describe(df)

# sprawdzenie ilosci unikatowych wartosci
apply(df, 2, function(x){length(unique(x))}) 
# plec przyjuje tylko 2 wartosci

table(df$plec) 
# w badaniu wzielo udzial wiecej mezczyzn niz kobiet

colSums(is.na(df)) # brak wartosci NA w zbiorze danych
colSums(df=='') # brak pustych komorek w zbiorze danych

nrow(df[df$wiek > 100 | df$wiek < 0,]) # liczba rekordow z wiekiem ponad 100
# i mniej niz 0
# brak takich rekordow - dane przyjmuja realistyczne wartosci

nrow(df[df$wiek<18,]) # wszyscy respondenci sa pelnoletni

nrow(df[df$wydatki<0,]) # wydatki nie przyjmuja wartosci ponizej 0 -
# realistyczne wartosci

# Zad.1.

# wartosci przecietne:
# a) srednie:

mean(df$wydatki) # srednia wydatkow wsrod kobiet i mezczyzn
mean(df[df$plec=='0',]$wydatki) # srednie wydatki wsrod kobiet
mean(df[df$plec=='1',]$wydatki) # srednie wydatki wsrod mezczyzn
# srednie miesieczne wydatkow na kulture wsrod mezczyzn byly wieksze, a wsrod kobiet
# mniejsze niz srednie wydatki w obydwu grupach

# b) modalne:

modal(df$wydatki) # modalna wydatkow wsrod kobiet i mezczyzn
nrow(df[df$wydatki==185,]) # wystapily 4 takie przypadki
sum(table(df$wydatki)=='4') # dwie wartosci wystepuja 4 razy w obu grupach
names(which(table(df$wydatki)=='4')) # modalne calej badanej populacji
modal(df[df$plec=='0',]$wydatki) # modalna wydatkow kobiet, taka sama jak w obu grupach
nrow(df[df$plec=='0' & df$wydatki==177,]) # wystapily 3 takie przypadki
modal(df[df$plec=='1',]$wydatki) # modalna wydatkow mezczyzn
nrow(df[df$plec=='1' & df$wydatki==187,]) # dwa wystapienia
sum(table(df[df$plec=='1',]$wydatki)=='2') # 5 innych wartosci tez wystepuje dwa razy
sum(table(df[df$plec=='0',]$wydatki)=='3') # tylko jedna wartosc pojawia sie 3 razy
# u kobiet
names(which(table(df[df$plec=='1',]$wydatki)=='2')) # modalne mezczyzn

# kwartyle:
quantile(df$wydatki, c(0.25, 0.5, 0.75)) # kwartyle wydatkow badanej populacji
quantile(df[df$plec=='0',]$wydatki, c(0.25, 0.5, 0.75)) # kwartyle wydatkow wsrod
# kobiet
quantile(df[df$plec=='1',]$wydatki, c(0.25, 0.5, 0.75)) # kwartyle wydatkow wsrod 
# mezczyzn

# odchylenie standardowe:

sd(df$wydatki) # odchylenie standardowe wydatkow w calej  grupie
sd(df[df$plec=='0',]$wydatki) # odchylenie standardowe wydatkow u kobiet
sd(df[df$plec=='1',]$wydatki) # odchylenie standardowe wydatkow u mezczyzn
# wydatki na kulture byly bardziej zblizone do siebie w przypadku kobiet


# Zad. 2.:

# analiza wystepowania wartosci odstajacych:
ggplot(df, aes(x=as.factor(plec), y=wydatki)) +
  geom_boxplot(fill='slateblue') +
  xlab("płeć: 0 - kobieta, 1- meżczyzna") +
  ylab('wydatki [zł]') +
  labs(title = "Miesięczne wydatki na kulturę wśród meżczyzn i kobiet")
# na powyzszym wykresie pudelkowym uwidoczniono jedna wartosc odstajaca w grupie 
# kobiet

boxplot(df$wydatki, main = "Miesięczne wydatki na kulturę", ylab = 'wydatki [zł]')

ggplot(df, aes(x=as.factor(plec), y=wiek)) +
  geom_boxplot(fill = 'red') +
  xlab("plec: 0 - kobieta, 1- meżczyzna") +
  ylab("wiek [lata]") +
  labs(title = 'Wiek mężczyzn i kobiet')

boxplot(df$wiek, main = "Wiek", ylab = 'wiek [lata]')

# sprawdzenie, czy badane dane maja rozklad normalny
# z uzyciem testu Shapiro-Wilka, na poziomie istotnosci 0,05:

shapiro.test(df$wydatki)
# rozkład wydatkow jest normalny

# test Shapiro-Wilka dla wydatkow z podzialem na plec

shapiro.test(df[df$plec=='0',]$wydatki) # normalny u kobiet
shapiro.test(df[df$plec=='1',]$wydatki) # normalny u mezczyzn

# nie ma podstaw do odrzucenia hipotezy zerowej, mowiacej o tym
# ze rozklad jest normlany 

# wykres obrazujacy rozklad wydatkow w badanej grupie
wykr <- hist(df$wydatki, breaks = 15, col='blue', xlab = 'wydatki [zł]', 
             ylab = 'ilość wystąpień', main = 'Wydatki')
xfit <- seq(min(df$wydatki), max(df$wydatki), length=40)
yfit <- dnorm(xfit, mean=mean(df$wydatki), sd= sd(df$wydatki))
yfit <- yfit*diff(wykr$mids[1:2])*length(df$wydatki)
lines(xfit, yfit, col = 'red', lwd=2)

# wykres obrazujacy rozklad wydatkow w grupie kobiet
wykr <- hist(df[df$plec=='0',]$wydatki, breaks = 15, col='blue', xlab = 'wydatki [zł]', 
             ylab = 'ilość wystąpień', main = 'Wydatki w grupie kobiet')
xfit <- seq(min(df[df$plec=='0',]$wydatki), max(df[df$plec=='0',]$wydatki), length=40)
yfit <- dnorm(xfit, mean=mean(df[df$plec=='0',]$wydatki), sd= sd(df[df$plec=='0',]$wydatki))
yfit <- yfit*diff(wykr$mids[1:2])*length(df[df$plec=='0',]$wydatki)
lines(xfit, yfit, col = 'red', lwd=2)

# wykres obrazujacy rozklad wydatkow w grupie mezczyzn
wykr <- hist(df[df$plec=='1',]$wydatki, breaks = 15, col='blue', xlab = 'wydatki [zł]', 
             ylab = 'ilość wystąpień', main = 'Wydatki w grupie mężczyzn')
xfit <- seq(min(df[df$plec=='1',]$wydatki), max(df[df$plec=='1',]$wydatki), length=40)
yfit <- dnorm(xfit, mean=mean(df[df$plec=='1',]$wydatki), sd= sd(df[df$plec=='1',]$wydatki))
yfit <- yfit*diff(wykr$mids[1:2])*length(df[df$plec=='1',]$wydatki)
lines(xfit, yfit, col = 'red', lwd=2)

# tworzenie modeli regresji liniowej:


lmK <- lm(wydatki~wiek, df[df$plec=='0',]) # reresja liniowa zaleznosci wydatkow
# od wieku wsrod kobiet
summary(lmK)
coef(lmK)
# rownanie ma postac y = 236.005 + x*(-1.502)
# mozna odrzucic hipoteze zerowa, na poziomie istotności alfa = 0.05.
# Wraz z wiekiem wydatki na kulture wsrod kobiet maleja. 
# Zaleznosc ta jest istotna statystycznie

lmM <- lm(wydatki~wiek, df[df$plec=='1',]) # regresja liniowa zaleznosci wydatkow
# od wieku wsrod mezczyzn
summary(lmM)
coef(lmM)
# rownanie ma postac y = 252.633 + x*(-1.922)
# mozna odrzucic hipoteze zerowa, na poziomie istotności alfa = 0.05.
# wraz z wiekiem wydatki na kulture wsrod mezczyzn maleja.
# Zaleznosc ta jest istotna statystycznie

lmAll <- lm(wydatki~wiek, df) # regresja liniowa zaleznosci wydatkow od wieku calej
# grupy
summary(lmAll)
coef(lmAll)
# rownanie ma postac y = 245.85 + x*(-1.75)
# mozna odrzucic hipoteze zerowa, na poziomie istotności alfa = 0.05.
# Wraz z wiekiem wydatki na kulture w badanej grupie maleja.
# Zaleznosc ta jest istotna statystycznie.


# przyklad wykresu prezentujacego miesieczne wydatki na kulture w zaleznosci od wieku

firstPlot <- function(model, wiek_g, wydatki_g, group){
  plot(x= wiek_g, y=wydatki_g, 
       main = c('Miesieczne wydatki na kulture w zaleznosci od wieku ', group),
       xlab = 'wiek [lata]', ylab= 'wydatki [zl]')
  abline(model, col = 'red')
  points(wiek_g, fitted(model), col = 'blue', pch=16)
}

# wykres calosci

firstPlot(lmAll, df$wiek, df$wydatki, "")

# wykres dla kobiet

firstPlot(lmK, df[df$plec=='0',]$wiek, df[df$plec=='0',]$wydatki, "wsrod kobiet")

# wykres dla mezczyzn

firstPlot(lmM, df[df$plec=='1',]$wiek, df[df$plec=='1',]$wydatki, "wsrod mezczyzn")


# Zad. 3


modelEvaluation <- function (model, y){

  # Blad standardowy wspolczynnikow modelu:
  # y = b*x + a
  # blad standardowy a
  cat("Blad standardowy wspolczynnika a: ", summary(model)$coefficients[1,2], "\n") 
  # blad standardowy b
  cat("Blad standardowy wspolczynnika b: ", summary(model)$coefficients[2,2], "\n") 

  # Blad standardowy lini regresji:
  n_all <- length(y)
  cat("Wielkosc zbioru y: ", n_all, "\n")

  se1 <- sqrt(sum((y-model$fitted.values)^2)/(n_all-2))
  cat("Blad standardowy lini regresji: ", se1, "\n")

  # rezydualna suma kwadratow
  sse_all <- sum((y-model$fitted.values)^2)
  cat("Rezydualna suma kwadratow: ", sse_all, "\n")

  # blad sredniokwadratowy
  mse <- sse_all / (n_all-2)
  cat("Blad sredniokwadratowy: ", mse, "\n")

  # wspolczynnik determinacji
  r2_adj_all <- summary(model)$adj.r.squared
  cat("Wspolczynnik determinacji: ", r2_adj_all, "\n") 
  plot(model$residuals, pch=16, col='red')
}

# a) obliczenia dla modelu opisujacego cala grupe:

modelEvaluation(lmAll, df$wydatki)

# Otrzymany model jest średno dokładny. 
# Tłumaczy zmienność wartości w zbiorze danych na poziomie 62%. 
# Jego błąd średniokwadratowy wynosi 226.22, co jest dużą wartością.
# Wykazano istotną statystycznie zależność między wydatkami, a wiekiem. 
# Wraz z wiekiem maleją wydatki na kulturę.

# b) obliczenia dla modelu opisujacego kobiety:

modelEvaluation(lmK, df[df$plec=='0',]$wydatki)

# Otrzymany model jest średnio dokładny.
# Tłumaczy zmienność wartości w zbiorze danych na poziomie 53,5%. 
# Jego błąd średniokwadratowy wynosi 218.29, co jest dużą wartością.
# Wykazano istotną statystycznie zależność między wydatkami, a wiekiem. 
# Wraz z wiekiem maleją wydatki na kulturę w tej grupie.

# c) obliczenia dla modelu opisujacego mezczyzn:

modelEvaluation(lmM, df[df$plec=='1',]$wydatki)

# Otrzymany model jest średnio dokładny.
# Tłumaczy zmienność wartości w zbiorze danych na poziomie 67%. 
# Jego błąd średniokwadratowy wynosi 236,39, co jest dużą wartością.
# Wykazano istotną statystycznie zależność między wydatkami, a wiekiem. 
# Wraz z wiekiem maleją wydatki na kulturę w tej grupie.



# Zad. 4.
# Wykres przedstawiajacy miesieczne wydatki na kulture w zaleznosci od wieku
# z naniesiona linia regresji, z zaznaczonymi pasmami predykcji na 
# poziomie ufnosci 95%, a takze przedzialami ufnosci na poziomie 95%.

finalPlot <- function(model, wiek_g, wydatki_g, group) {

  new_wiek <- seq(min(wiek_g), max(wiek_g), by = 1)

  conf_interval <- predict(model, newdata = data.frame(wiek = new_wiek), 
                         interval = "confidence", level = 0.95) 
  # przedzial ufnosci na poziomie 95%

  pred_interval <- predict(model, newdata = data.frame(wiek = new_wiek), 
                         interval = "prediction", level = 0.95) 
  # pasma predykcji na poziomie ufnosci 95%

  plot(x= wiek_g, y=wydatki_g, 
       main = c('Miesięczne wydatki na kulture w zależności od wieku ', group),
       xlab = 'wiek [lata]', ylab= 'wydatki [zl]')
  abline(model, col = 'black')
  lines(new_wiek, conf_interval[,2], col="blue", lty=2)
  lines(new_wiek, conf_interval[,3], col="blue", lty=2)
  lines(new_wiek, pred_interval[,2], col = "red", lty=2)
  lines(new_wiek, pred_interval[,3], col="red", lty=2)
}

# a) Wykres dla calej badanej grupy:

finalPlot(lmAll, df$wiek, df$wydatki, "")

# b) Wykres dla grupy kobiet:

finalPlot(lmK, df[df$plec=='0',]$wiek, df[df$plec=='0',]$wydatki, "wsród kobiet")

# c) Wykres dla grupy mezczyzn:

finalPlot(lmM, df[df$plec=='1',]$wiek, df[df$plec=='1',]$wydatki, "wsród mężczyzn")















