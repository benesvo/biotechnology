# 2l reaktor:
# 100 ml inokula + 900 ml batch media (10 g glc/l) + 500 ml feedu
##### OPTIMALIZACE DOBY FEEDOVANI #####

#funkce pro optimalizaci doby kultivace (feedu) 


mu_poc <- 0.2   # maximální rùstová rychlost (na zaèátku kultivace)
mu_kon <- 0.058   # minimální rùstová rychlost (na konci kultivace)
par_k <- 0.2    # prohnutí køivky prùbìhu rùstové rychlosti

OD <- 8.65      # OD inokula

{
  #funkce pro optimalizaci doby kultivace (feedu) !!! v tele funkce zadej OD inokula!
  opt_fct <- function(delka_kultivace){
    # delka kultivace v hodinach
    
    # cas po 10 s
    data <- data.frame(cas = c(0:(delka_kultivace*360)))
    
    # cas v hod
    data$cas_h <- data$cas / 360
    
    # odhadovane mnozstvi susiny na zacatku feedu
    # zadej OD; vypocte se DWT jako OD / 1.8136 * objem inokula + odhadovaný zisk z batch
    DWT_0 <- OD / 1.8136 * 0.1 + 0.9
    
    
    #rustova rychlost se snizi z 0.2 na zacatku kultivace na 0.058 na konci (~ doubling time 3.5 - 12 h)
    mu_min <- mu_poc - 2* (mu_poc - mu_kon) # rozdil beru 2*, protoze chci jen pulku sigmoidy
    mu_max <- mu_poc
    par_k <- par_k
    data$mu <- mu_min + (mu_max - mu_min) / (1 + exp(par_k * (data$cas_h - delka_kultivace)))
    
    
    
    data$DWT <- DWT_0  # pocatecni mnozstvi biomasy
    
    for(i in c(2: nrow(data))){
      data$DWT[i] <- data$DWT[i-1] * ((1 + data$mu[i]) ^ (1/360))
    }
    
    # mnozstvi feedu od zacatku (vypocteno z DWT)
    data$feed_vol_total <- (data$DWT - data$DWT[1]) / 0.5 / 0.22
    
    
    
    # vratit rozdil mezi feed_vol_total a cilovym objemem 0.5 l
    return(abs(500 - data$feed_vol_total[nrow(data)]))
  }
  
  # optimalizace: vysledek je doba feedovani pro nadavkovani 500 ml
  opt <- optim(24, opt_fct, 
               control = list(maxit = 100), 
               method = "Brent", lower = 0, upper = 100)
  
  
  ##### SIMULACE KULTIVACE #####
  delka_kultivace <- opt$par
  
  
  {
    data <- list()
    data$cas_h <- (c(0:(delka_kultivace*3600))) / 3600  #cas kultivace v hod
    data <- data.frame(data)
    DWT_0 <- OD / 1.8136 * 0.1 + 0.9  #odhadovane mnozstvi susiny na zacatku feedu
    
    mu_min <- mu_poc - 2* (mu_poc - mu_kon) # rozdil beru 2*, protoze chci jen pulku sigmoidy
    mu_max <- mu_poc
    par_k <- par_k
    data$mu <- mu_min + (mu_max - mu_min) / (1 + exp(par_k * (data$cas_h - delka_kultivace)))
    
    data$DWT <- DWT_0  # pocatecni mnozstvi biomasy
    for(i in c(2: nrow(data))){ #samotna iterace rustu biomasy
      data$DWT[i] <- data$DWT[i-1] * ((1 + data$mu[i]) ^ (1/3600))
    }
    data$feed_vol_total <- (data$DWT - data$DWT[1]) / 0.5 / 0.22  #mnozstvi feedu od zacatku (vypocteno z DWT)
    data$feed_rot <- data$feed_vol_total * 19.56 # a prepocet na otacky pumpy
    data$feed_speed <- c(0, diff(data$feed_vol_total)) * 60 #rychlost feedu [ml/min]
    #data$pump_value <- data$feed_speed / xxxxx  #rychlost pumpy [%]
  }
  
  # graf rychlost feedu
  plot(feed_speed~cas_h, data, type = "l",
       xlab = "cas [h]", ylab = "rychlost feedu [ml/min]")
  
  # fitting objemu
  model <- nls(feed_rot ~ a*cas_h^3 + b*cas_h^2 + c*cas_h + d, data, #chybi linearni clen!!! (naschval)
               start = list(a=0, b=0, c=0, d=0))
  
  # graf
  plot(feed_rot~cas_h, data, type = "l",
       xlab = "cas [h]", ylab = "celkovy objem nacerpaneho feedu [otacky]")+
    lines(data$cas_h, predict(model), lty = "dashed", col = "red")
  
  cat("doba davkovani feedu: ", opt$par, " hod\n")
  print(coef(model))
}