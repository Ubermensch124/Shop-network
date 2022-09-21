# функция будет генерировать данные по поставкам некоторого товара

generate.supply <-
  function(way ='C:\\Козлов Марк\\Микси',
           file.name = 'Микси',
           days = 7,
           goods = list( list(name = 'Сахар, уп', min = 100, max = 120, price_in = 70)) ) {
    
    # проверим, существует ли папка. Если её нет, создами её
    if (way != ''){
      isFoundDir <- dir.exists(way)
      if (isFoundDir == FALSE){
        dir.create(path=way, showWarnings = FALSE)
        if (isFoundDir == FALSE){
          print('Папка не существует')
          return(NULL)
        }
      }
    }
    x <- dir(way)
    y <- x[ grepl('Магазин', x, fixed = TRUE) ]
    
    COUNT_OF_SHOPS <- length(y)
    
    #if (COUNT_OF_SHOPS == 0){
     # for (i in 1:10){
      #  
     # }
    #}

    for (j in 1:COUNT_OF_SHOPS){
      # в среде R создадим таблицы из двух столбцов длиной days
      tabl <- data.frame('Дни' = 1:days)    # для закупок
      new.table <- data.frame('Дни' = 1:days) # для продажи
      
      
      # в цикле добавим столбцы под каждый товар. Данные для продажи рассчитываются исходя из данных таблицы закупок
      for (i in 1:length(goods)){
        buy_data <- c(sample(x = goods[[i]]$min:goods[[i]]$max, size = days, replace = TRUE))
        sell_data <- c()
        for (k in 1:days){
          sell_data[k] <- sample(x = goods[[i]]$min:buy_data[k], size = 1)
        }

        tabl[1+i] <- buy_data 
        new.table[i+1] <- sell_data
        colnames(x=tabl)[1+i] <- goods[[i]]$name
        colnames(x=new.table)[1+i] <- goods[[i]]$name
      }
      print(new.table)
      # запишем таблицы в файлы
      write.table(x = tabl, file = paste0(way, '\\Магазин ', as.character(j), '\\', file.name, '.in'), col.names = TRUE, row.names = FALSE)
      write.table(x = new.table, file = paste0(way, '\\Магазин ', as.character(j), '\\', file.name, '.out'), col.names = TRUE, row.names = FALSE)
    }
      
  }


goods <- list(
  list(name = 'Молоко, уп', min = 100, max = 120, price_in = 40),
  list(name = 'Хлеб, шт', min = 200, max = 300, price_in = 20),
  list(name = 'Тушёнка, банка', min = 20, max = 26, price_in = 100),
  list(name = 'Сок, уп', min = 70, max = 95, price_in = 100)
)

z <- generate.supply(goods = goods, days = 7, file.name = 'Микси')
z

