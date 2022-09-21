way = 'C:\\Козлов Марк\\Микси\\Анализ'
setwd(way)
x <- dir()
y <- x[ grepl('Магазин', x, fixed = TRUE) ]

COUNT_SHOPS <- length(y) %/% 2

char_list <- c("in", "out")
num_list <- as.character(c(1:COUNT_SHOPS))

for (char in char_list) {
  for (num in num_list) {   
    i <- read.table(paste0("Магазин", num, "_Микси.", char), head=TRUE)
    
    assign(paste(char,num, sep = ''), i)
  }
}

# file_p <- read.table("закуп.txt", head=FALSE, encoding="UTF-8")
# const_prices <- c()
# named <- c(names(get('in1')))[2:length(file_p)]
# for (i in 1:length(file_p)){
#   col <- file_p[, i]
#   vectorr <- list(name = named[i], P_SUPPLY = col[1], P_UTIL = col[2], P_SELL = col[3])
#   const_prices <- c(const_prices, vectorr)
# }
# print(const_prices[name])
# 
# создание итоговой таблицы

const_prices <- list(
  list(name = 'Молоко, уп', P_SUPPLY = 40, P_UTIL = 10, P_SELL = 55),
  list(name = 'Хлеб, шт', P_SUPPLY = 25, P_UTIL = 5, P_SELL = 30),
  list(name = 'Тушёнка, банка', P_SUPPLY = 100, P_UTIL = 40, P_SELL = 165),
  list(name = 'Сок, уп', P_SUPPLY = 80, P_UTIL = 15, P_SELL = 95)
  )

#############################################  основная таблица
res_tabl_all <- function(){
  rev <- rep(0, COUNT_SHOPS+2)
  
  row_names <- c()
  for (i in 1:COUNT_SHOPS){
    row_names[i] <- paste0('Магазин ', as.character(i))
  }
  
  res.tabl <- data.frame("Выручка" = rev,
      row.names = c(row_names, 'Итого', 'Среднее')
      )
  
  column_names <- c('Прибыль, тыс. руб.', 'Реализация, конт.', "Списание, конт.", "Равномерность продаж", "День, макс", "Продажи макс, тыс. руб.", "День, мин", "Продажи мин, тыс. руб.", "День списания, макс", "Списание макс, тыс. руб.")
  
  for (i in column_names){
    res.tabl[i] <- 0
  }
  
  names(res.tabl)[names(res.tabl) == "Выручка"] <- "Выручка, тыс. руб."
  
  ################################### Расчёты
  
  for (num in num_list) {                   # количество магазинов
    now_in <- get(paste0('in', num))
    now_out <- get(paste0('out', num))
    
    revenue <- 0
    containers_was_sell <- 0
    containers_was_delete <- 0
    profit <- 0
    for (j in 1:length(const_prices)){
      prices_to_sell <- const_prices[[j]]$P_SELL
      count_cont <- sum(now_out[, j+1])
      count_buy_cont <- sum(now_in[, j+1])
      revenue <- revenue + count_cont*prices_to_sell
      containers_was_sell <- containers_was_sell + count_cont
      containers_was_delete <- containers_was_delete + (count_buy_cont - count_cont)
      
      profit <- profit + (count_cont*prices_to_sell - count_buy_cont*const_prices[[j]]$P_SUPPLY - (count_buy_cont - count_cont)*const_prices[[j]]$P_UTIL)
    }
    res.tabl[as.integer(num), "Выручка, тыс. руб."] <- revenue %/% 1000
    res.tabl[as.integer(num), "Реализация, конт."] <- containers_was_sell
    res.tabl[as.integer(num), "Списание, конт."] <- containers_was_delete
    res.tabl[as.integer(num), "Прибыль, тыс. руб."] <- profit %/% 1000
    
    rev_per_days <- c()
    count_del_per_days <- c()
    for (j in 1:length(now_out[, 1])){
      bet_rev <- 0
      bet_del <- 0
      for (k in 1:length(const_prices)){
        bet_rev <- bet_rev + now_out[j, k+1]*const_prices[[k]]$P_SUPPLY
        bet_del <- bet_del + (now_in[j, k+1]-now_out[j, k+1])
      rev_per_days[j] <- bet_rev
      count_del_per_days[j] <- bet_del
      }
    }
    res.tabl[as.integer(num), "Равномерность продаж"] <- round(sd(rev_per_days))
    res.tabl[as.integer(num), "Продажи макс, тыс. руб."] <- max(rev_per_days) %/% 1000
    res.tabl[as.integer(num), "Продажи мин, тыс. руб."] <- min(rev_per_days) %/% 1000
    res.tabl[as.integer(num), "День, макс"] <- which.max(rev_per_days)
    res.tabl[as.integer(num), "День, мин"] <- which.min(rev_per_days)
    res.tabl[as.integer(num), "День списания, макс"] <- which.max(count_del_per_days)
    res.tabl[as.integer(num), "Списание макс, тыс. руб."] <- which.min(count_del_per_days)
  }

  ####### Подсчёт итоговых результатов
  res.tabl["Итого", "Выручка, тыс. руб."] <- sum(res.tabl[, "Выручка, тыс. руб."])
  res.tabl["Среднее", "Выручка, тыс. руб."] <- mean(res.tabl[, "Выручка, тыс. руб."])
  res.tabl["Итого", "Прибыль, тыс. руб."] <- sum(res.tabl[, "Прибыль, тыс. руб."])
  res.tabl["Среднее", "Прибыль, тыс. руб."] <- mean(res.tabl[, "Прибыль, тыс. руб."])
  res.tabl["Итого", "Реализация, конт."] <- sum(res.tabl[, "Реализация, конт."])
  res.tabl["Среднее", "Реализация, конт."] <- mean(res.tabl[, "Реализация, конт."])
  res.tabl["Итого", "Списание, конт."] <- sum(res.tabl[, "Списание, конт."])
  res.tabl["Среднее", "Списание, конт."] <- mean(res.tabl[, "Списание, конт."])
  write.table(x = res.tabl, file = paste0(way, "\\Итоговая_таблица.csv"), sep = ',')
  return(res.tabl)
}
###################################   По всем товарам одного магазина
res_tabl_for_one_shop <- function(number){ 
  now_in = get(paste0('in', as.character(number)))
  now_out = get(paste0('out', as.character(number)))
  
  rev = rep(0, length(now_in))
  named <- c(names(now_in))[2:length(now_in)]
  res.tabl <- data.frame("Выручка" = rev,
  row.names = c(named, 'Итого')
  )
  
  column_names <- c('Прибыль, руб.', 'Реализация, конт.', "Списание, конт.", 
                    "Равномерность продаж", "День, макс", "Продажи макс, руб.", 
                    "День, мин", "Продажи мин, руб.", "День списания, макс", 
                    "Списание макс, руб.")
  
  for (i in column_names){
    res.tabl[i] <- 0
  }
  
  names(res.tabl)[names(res.tabl) == "Выручка"] <- "Выручка, руб."
  
  for (i in 1:length(const_prices)){
    res.tabl[i, 'Выручка, руб.'] <- sum(now_out[, i+1]) * const_prices[[i]]$P_SELL
    res.tabl[i, 'Реализация, конт.'] <- sum(now_out[, i+1])
    res.tabl[i, 'Списание, конт.'] <- sum(now_in[, i+1]) - sum(now_out[, i+1])
    res.tabl[i, 'Прибыль, руб.'] <- sum(now_out[, i+1]) * const_prices[[i]]$P_SELL - (sum(now_in[, i+1]) - sum(now_out[, i+1]))*const_prices[[i]]$P_UTIL - sum(now_out[, i+1])*const_prices[[i]]$P_SUPPLY
    res.tabl[i, "Продажи макс, руб."] <- max(now_out[, i+1]) * const_prices[[i]]$P_SELL
    res.tabl[i, "День, макс"] <- which.max(now_out[, i+1])
    res.tabl[i, "Продажи мин, руб."] <- min(now_out[, i+1]) * const_prices[[i]]$P_SELL
    res.tabl[i, "День, мин"] <- which.min(now_out[, i+1])
    res.tabl[i, "Списание макс, руб."] <- max(now_in[,i+1]-now_out[,i+1]) * const_prices[[i]]$P_UTIL
    res.tabl[i, "День списания, макс"] <- which.max(now_in[,i+1]-now_out[,i+1])
    res.tabl[i, "Равномерность продаж"] <- round(sd(now_out[,i+1]))
  }
  res.tabl["Итого", "Выручка, руб."] <- sum(res.tabl[, "Выручка, руб."])
  res.tabl["Итого", "Прибыль, руб."] <- sum(res.tabl[, "Прибыль, руб."])
  res.tabl["Итого", "Реализация, конт."] <- sum(res.tabl[, "Реализация, конт."])
  res.tabl["Итого", "Списание, конт."] <- sum(res.tabl[, "Списание, конт."])
  res.tabl['Номер магазина'] <- number
  write.table(x = res.tabl, file = paste0(way, "\\По_всем_товарам_магазина", as.character(number), ".csv"), sep = ',')
  return(res.tabl)
}

###################################    для одного товара по всем магазинам (структура: строки это магазины, столбцы это обычные наши показатели )
res_tabl_for_one_product <- function(name_prod){
  rev <- rep(0, COUNT_SHOPS+2)
  
  row_names <- c()
  for (i in 1:COUNT_SHOPS){
    row_names[i] <- paste0('Магазин ', as.character(i))
  }
  
  res.tabl <- data.frame("Выручка" = rev,
                         row.names = c(row_names, 'Итого', 'Среднее')
  )
  
  column_names <- c('Прибыль, руб.', 'Реализация, конт.', "Списание, конт.", "Равномерность продаж", "День, макс", "Продажи макс, руб.", "День, мин", "Продажи мин, руб.", "День списания, макс", "Списание макс, руб.")
  
  for (i in column_names){
    res.tabl[i] <- 0
  }
  
  names(res.tabl)[names(res.tabl) == "Выручка"] <- "Выручка, руб."
  
  index_tovara <- 1
  for (i in const_prices){
    if (i$name == name_prod){
      index_tovara <- index_tovara + 1
      break
    } else{
      index_tovara <- index_tovara + 1
    }
  }
  P_sup <- const_prices[[index_tovara-1]]$P_SUPPLY
  P_sell <- const_prices[[index_tovara-1]]$P_SELL
  P_util <- const_prices[[index_tovara-1]]$P_UTIL
  
  for (i in 1:COUNT_SHOPS){
    now_in <- get(paste0('in', as.character(i)))
    now_out <- get(paste0('out', as.character(i)))
    
    res.tabl[i, 'Выручка, руб.'] <- sum(now_out[, index_tovara]) * P_sell
    res.tabl[i, 'Реализация, конт.'] <- sum(now_out[, index_tovara])
    res.tabl[i, 'Списание, конт.'] <- sum(now_in[, index_tovara]) - sum(now_out[, index_tovara])
    res.tabl[i, 'Прибыль, руб.'] <- sum(now_out[, index_tovara]) * P_sell - (sum(now_in[, index_tovara]) - sum(now_out[, index_tovara]))*P_util - sum(now_in[, index_tovara])*P_sup 
    
    res.tabl[i, "Продажи макс, руб."] <- max(now_out[, index_tovara])*P_sell
    res.tabl[i, "Продажи мин, руб."] <- min(now_out[, index_tovara])*P_sell
    res.tabl[i, "День, макс"] <- which.max(now_out[, index_tovara])
    res.tabl[i, "День, мин"] <- which.min(now_out[, index_tovara])
    
    res.tabl[i, "Списание макс, руб."] <- max(now_in[, index_tovara] - now_out[, index_tovara])*P_util
    res.tabl[i, "День списания, макс"] <- which.max(now_in[, index_tovara] - now_out[, index_tovara])
    res.tabl[i, "Равномерность продаж"] <- round(sd(now_out[, index_tovara]))
  }
  res.tabl["Итого", "Выручка, руб."] <- sum(res.tabl[, "Выручка, руб."])
  res.tabl["Итого", "Прибыль, руб."] <- sum(res.tabl[, "Прибыль, руб."])
  res.tabl["Итого", "Реализация, конт."] <- sum(res.tabl[, "Реализация, конт."])
  res.tabl["Итого", "Списание, конт."] <- sum(res.tabl[, "Списание, конт."])
  res.tabl["Среднее", "Выручка, руб."] <- mean(res.tabl[, "Выручка, руб."])
  res.tabl["Среднее", "Прибыль, руб."] <- mean(res.tabl[, "Прибыль, руб."])
  res.tabl["Среднее", "Реализация, конт."] <- mean(res.tabl[, "Реализация, конт."])
  res.tabl["Среднее", "Списание, конт."] <- mean(res.tabl[, "Списание, конт."])
  res.tabl['Наименование товара'] <- name_prod
  write.table(x = res.tabl, file = paste0(way, "\\Таблица_для_товара_", name_prod, ".csv"), sep = ',')
  return(res.tabl)
}

##################################  

X <- res_tabl_all()     # итоговая таблица по всем магазинам
View(X)

for (i in 1:COUNT_SHOPS){
  Y <- res_tabl_for_one_shop(i)     # таблица по продажам всех товаров в одном магазине
  View(Y)
}

for (i in 1:length(const_prices)){
  name_prod <- const_prices[[i]]$name
  Z <- res_tabl_for_one_product(name_prod)    # таблица для конкретного товара во всех магазинах
  View(Z)
}




############################################# графики

png(file = "Продажи молока в упаковках, магазин 1.jpg")
now_out1 = get(paste0('out', '1'))
y = now_out1[, 2]
x = c(1:length(y))
plot(x = x, y = y, main = 'Продажи молока в упаковках, магазин 1', xlab = 'Дни', 
     ylab = 'Упаковки', pch = 21, lty = 1,
     lwd = 6)
lines(x = x, y = y, col = 'green')
points(x = x, y = y, col = 'black')
dev.off()


png(file = "Продажи молока в упаковках, магазин 2.jpg")
now_out2 = get(paste0('out', '2'))
y = now_out2[, 2]
x = c(1:length(y))
plot(x = x, y = y, main = 'Продажи молока в упаковках, магазин 2', xlab = 'Дни', 
     ylab = 'Упаковки', pch = 21, lty = 1,
     lwd = 6)
lines(x = x, y = y, col = 'green')
points(x = x, y = y, col = 'black')
dev.off()
              

png(file = "Продажи молока, магазин 1.jpg")
y = now_out1[, 2] * const_prices[[1]]$P_SELL
x = c(1:length(y))
plot(x = x, y = y, main = 'Продажи молока, магазин 1', xlab = 'Дни', 
     ylab = 'Рубли', pch = 21, lty = 1,
     lwd = 6)
lines(x = x, y = y, col = 'green')
points(x = x, y = y, col = 'black')
dev.off()


png(file = "Продажи молока, магазин 2.jpg")
y = now_out2[, 2] * const_prices[[1]]$P_SELL
x = c(1:length(y))
plot(x = x, y = y, main = 'Продажи молока, магазин 2', xlab = 'Дни', 
     ylab = 'Рубли', pch = 21, lty = 1,
     lwd = 6)
lines(x = x, y = y, col = 'green')
points(x = x, y = y, col = 'black')
dev.off()

png(file = "Прибыль по нескольким товарам.jpg")
now_in1 = get(paste0('in', '1'))
y = now_in1[, 2]*const_prices[[1]]$P_SELL - (now_in1[, 2] - now_out1[, 2])*const_prices[[1]]$P_UTIL - now_in1[, 2]*const_prices[[1]]$P_SUPPLY
x = c(1:length(y))
y1 = now_in1[, 3]*const_prices[[2]]$P_SELL - (now_in1[, 3] - now_out1[, 3])*const_prices[[2]]$P_UTIL - now_in1[, 3]*const_prices[[2]]$P_SUPPLY
x1 = c(1:length(y1))
plot(x = x, y = y)
lines(x = x, y = y, col = 'green')
points(x = x, y = y, col = 'black')
lines(x = x1, y = y1, col = 'green')
points(x = x1, y = y1, col = 'black')
dev.off()


png(file = "Прибыль по продаже хлеба в магазинах.jpg")
y = Z$`Прибыль, руб.`[1:COUNT_SHOPS]
x = c(1:COUNT_SHOPS)
barplot(height=y,xlab = 'Магазины', ylab = 'Рубли', main = 'Прибыль по продаже хлеба в магазинах')
dev.off()


