library(ggplot2)
library(psych)
library(e1071)

dane1 <- read.csv("https://stooq.pl/q/d/l/?s=11b&d1=20220101&d2=20231231&i=d&o=1000000")
dane2 <- read.csv("https://stooq.pl/q/d/l/?s=ten&d1=20220101&d2=20231231&i=d&o=1000000")


dodanie_stopy <- function(dane){
  logarytmiczne_stopy <- c(NA)
  
  for(i in 2:(nrow(dane))) {
    logarytmiczne_stopy <- c(logarytmiczne_stopy, log(dane$Zamkniecie[i] / dane$Zamkniecie[i-1]))
  }
  
  dane$logarytmiczne_stopy <- logarytmiczne_stopy
  return(dane)
}


obliczenie_statystyk<- function(dane){
  srednia <- mean(dane$logarytmiczne_stopy, na.rm = TRUE)
  odchylenie_stand <- sd(dane$logarytmiczne_stopy, na.rm = TRUE)
  skosnosc <- skewness(dane$logarytmiczne_stopy, na.rm = TRUE)
  kurtoza <- kurtosis(dane$logarytmiczne_stopy, na.rm = TRUE)
  
  cat("Średnia:", srednia, "\n")
  cat("Odchylenie standardowe:", odchylenie_stand, "\n")
  cat("Skośność:", skosnosc, "\n")
  cat("Kurtoza:", kurtoza, "\n")
}

histogram <- function(dane) {
  
  ggplot(dane, aes(x = logarytmiczne_stopy)) +
    geom_histogram(binwidth = 0.01, fill = "lightblue", color = "darkblue", alpha = 0.7) +
    labs(title = "Histogram logarytmicznych stóp zwrotu", x = "Logarytmiczne stopy zwrotu") +
    theme_minimal()
}


wykres_pudelkowy <- function(dane) {
  
  ggplot(dane, aes(y = logarytmiczne_stopy)) +
    geom_boxplot(fill = "lightblue", color = "darkblue", alpha = 0.7) +
    labs(title = "Wykres pudełkowy logarytmicznych stóp zwrotu", y = "Logarytmiczne stopy zwrotu") +
    theme_minimal()
}

wsp_korelacji <- function(dane1, dane2) {
  
  dane1 <- na.omit(dane1)
  dane2 <- na.omit(dane2)
  
  cat( 
    "Współczynnik korelacji Pearsona: ",
    cor( dane1$logarytmiczne_stopy, dane2$logarytmiczne_stopy ),
    "Współczynnik korelacji Spearmana: ",
    cor( dane1$logarytmiczne_stopy, dane2$logarytmiczne_stopy, method = "spearman" ),
    sep = "\n"
  )
}

ex_spolek_rozn <- function(dane1, dane2) {
  
  print( t.test(
    dane1$logarytmiczne_stopy,
    dane2$logarytmiczne_stopy
  ) )
  
  # cat("Różnica wartości oczekiwanych: \n", 
  #     mean(  ) - 
  #       mean(  )
  # )
}


dane1 <- dodanie_stopy(dane1)
obliczenie_statystyk(dane1)
histogram(dane1)
wykres_pudelkowy(dane1)

dane2 <- dodanie_stopy(dane2)
obliczenie_statystyk(dane2)
histogram(dane2)
wykres_pudelkowy(dane2)

wsp_korelacji(dane1,dane2)
ex_spolek_rozn(dane1,dane2)