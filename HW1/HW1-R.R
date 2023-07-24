#---
#title: "HW1-R"
#author: "Nima Kelidari"
#date: "2023-05-02"
#---
  

Sys.setlocale(locale = "persian")

?ggplot

#install.packages("data.table")
#install.packages("ggplot2")
#install.packages("tidyverse")
#install.packages("heatmaply")                                      
library(heatmaply)
library(data.table)
library(ggplot2)
library(tidyverse)
library(ggplot2)
mortality = fread("D:/Terme8/Regression/HW1/iranprovs_mortality_monthly.csv",encoding="UTF-8")


ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}



provs_per_mounth <- mortality %>% 
  group_by(y,prov,m) %>% 
  summarise(n = sum(n))

provs_per_year <- mortality %>% 
  group_by(y,prov) %>% 
  summarise(n = sum(n))

provs <- provs_per_year

normal <- c()
mort = mat<-matrix(list(), nrow=31, ncol=43,)
year_weights <- c(1,2,4,7,10,14,18,23,25)


for(i in 1 : 31){
  #print(provs$prov[i])
  m <- provs_per_year %>%
    filter(prov %in% provs$prov[i])
  mm <- provs_per_mounth %>%
    filter(prov %in% provs$prov[i])
  earlier <- m %>%
    filter(y < 1398)
  fit <- earlier %>% 
    lm(n~y,data = .,weights = year_weights)
  normal <- c(normal,sum(earlier$n)/10000)
  for(j in 10 : 13){
    predict(fit,newdata = m)[j]
    predicted <- as.integer(predict(fit,newdata = m)[j])
    start <- j*12-11
    end <- j*12
    ex <- mm$n[start:end]-round(predicted/12)
    ex[ex<0] = 0
    ex <- ex[!is.na(ex)]
    start <- (j-10)*12 + 1
    end <- start + length(ex) -1
    mort[i,start:end]  <- ex
  }
}

mort <- matrix(as.numeric(mort), nrow=31, ncol=43,)
ggplotRegression(fit)
summary(fit)



x <- c("98/1")
for (i in 2 : 43){
  x <- c(x,paste(98+floor(i/12),"/",i%%12+1))
}

p <- heatmaply(mort, 
               dendrogram = "none",
               xlab = "ماه", 
               ylab = "استان", 
               main = "نقشه فوت اضافه بر حسب استان",
               labCol = x,
               labRow = provs$prov[1:31],
               grid_color = "white",
               grid_width = 0.000001,
               
)
p

normalized <- mort / normal[row(mort)]

p <- heatmaply(normalized, 
               dendrogram = "none",
               xlab = "ماه", 
               ylab = "استان", 
               main = " نقشه فوت اضافه بر حسب استان (نرمال شده)",
               labCol = x,
               labRow = provs$prov[1:31],
               grid_color = "white",
               grid_width = 0.000001
)
p



print(paste("کل فوت شده های کشور:",sum(mort)))


print("کل فوت شده ها به تفکیک استان:")
for(i in 1:31){
  print(paste("تعداد فوت شده های استان",provs$prov[i],":",rowSums(mort)[i]))
}


rowSums(normalized)


mort_age <- array(dim = c(31,21,43))


provs_per_mounth <- mortality %>% 
  group_by(y,prov,m,age_group) %>% 
  summarise(n = sum(n))

provs_per_year <- mortality %>% 
  group_by(y,prov,age_group) %>% 
  summarise(n = sum(n))


for(k in 1 : 21){
  for(i in 1 : 31){
    #print(provs$prov[i])
    m <- provs_per_year %>%
      filter(age_group %in% provs_per_year$age_group[k]) %>% 
      filter(prov %in% provs$prov[i])
    
    mm <- provs_per_mounth %>%
      filter(age_group %in% provs_per_year$age_group[k]) %>% 
      filter(prov %in% provs$prov[i])
    earlier <- m %>%
      filter(y < 1398)
    fit <- earlier %>% 
      lm(n~y,data = .)
    for(j in 10 : 13){
      predict(fit,newdata = m)[j]
      predicted <- as.integer(predict(fit,newdata = m)[j])
      start <- j*12-11
      end <- j*12
      ex <- mm$n[start:end]-round(predicted/12)
      ex[ex<0] = 0
      ex <- ex[!is.na(ex)]
      ex <- ex/(sum(earlier$n)/10000)
      start <- (j-10)*12 + 1
      end <- start + length(ex) -1
      mort_age[i,k,start:end]  <- ex
    }
  }
  print(paste("محاسبات گروه سنی",provs_per_year$age_group[k],"پایان یافت"))
}


weights <- c(2,1,3,6,8,9,10,10,9,9,8,8,7,6,5,4,3,2,2,1,1)
weights <- weights / sum(weights)

factors <- matrix(ncol = 43,nrow=31)
for (i in 1 : 43){
  for (j in 1 : 31){
    factors[j,i] <- sum(mort_age[j,,i] * weights)
  }
}


apply(factors,1,t.test)

overall <- rowSums(factors)

min_prove <- which.min(overall)
max_prove <- which.max(overall)

print(paste("استان با بهترین عملکرد، استان", provs$prov[min_prove],"است با", rowSums(mort)[min_prove],"فوت شده و فاکتور مرگ و میر",overall[min_prove]))
print(paste("استان با بدترین عملکرد، استان", provs$prov[max_prove],"است با", rowSums(mort)[max_prove],"فوت شده و فاکتور مرگ و میر",overall[max_prove]))
