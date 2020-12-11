preprocess = function(data){
  data = data[, -c(1, 2, 9)]
  
  data$Order.Date = as.Date(data$Order.Date, '%d-%m-%y')
  data$Ship.Date = as.Date(data$Ship.Date, '%d-%m-%y')
  data$Days = data$Ship.Date - data$Order.Date
  
  # Save a date that is 29/02 which needs to be adjusted when translating the dates
  date = data[3758, 'Order.Date'] 
  day(date) = day(date) - 1
  year(date) = year(date) + 2
  
  year(data$Order.Date) = year(data$Order.Date) + 2
  year(data$Ship.Date) = year(data$Ship.Date) + 2
  data[is.na(data$Order.Date), 'Order.Date'] = date
  
  return(data)
}

get_products = function(data){
  prices = data.frame(Product.ID = data$Product.ID, 
                      Product.Name = data$Product.Name, 
                      Product.Price = (data$Sales - data$Profit)/data$Quantity)
  prices = prices[unique(data$Product.ID), ]
  profits = vector(length = nrow(prices))
  for (i in 1:nrow(prices)){
    profits[i] = sum(data[data$Product.ID == prices$Product.ID[i], 'Profit'])
  }
  prices$Total_Profit = profits
  
  return(prices)
}

aggr = function(data, by, filtr = NA){ 
  
  # Returns time series of sales and profits aggregated by week or months. 
  # A filter or two filters can be selected, so two or three aggregations are made: 
  # first by categorical variable/s and then by week/month. 
  # In this case, multiple time series are returned (as many as categories in the filter) which are all containes in one ts object
  # Options: 
  #   by = c('Week', 'Month')
  #   filtr = c('Ship.Mode': 4, 'Segment': 3, 'State': 49, 'Region': 4, 'Category': 3, 'Sub.Category': 17) (Numbers represent unique values)
  # If you select two variables for filters, then they must be stored in a vector. 
  if (by == 'Day'){
    
    if (is.na(filtr)){
      data %>%
        group_by(Order.Date) %>%
        summarize(Sales = sum(Sales), Profit = sum(Profit)) -> result
      final = data.frame(Day = seq(as.Date("2016/01/03"), as.Date("2019/12/30"), "days"))
      final = merge(x = final, y = result, by.x = 'Day', by.y = 'Order.Date', all.x = TRUE)
      final = ts(result[, 2:3], start = c(2016, 3), end = c(2019, 364), frequency = 365)
      final[is.na(final)] = 0
      return(final)}
  
  } else if (by == 'Week'){
    
    if (is.na(filtr)){
      data %>%
        mutate(Week = yearweek(Order.Date)) %>%
        group_by(Week) %>%
        summarize(Sales = sum(Sales), Profit = sum(Profit)) -> result
      final = ts(result[, 2:3], start = c(2016, 1), end = c(2019, 52), frequency = 52)
      final[is.na(final)] = 0
      return(final)
      
    } else {
      data %>%
        mutate(Week = yearweek(Order.Date)) %>%
        group_by(.data[[filtr[1]]], Week) %>%
        summarize(Total = sum(Sales), Profit = sum(Profit)) -> result
      final_S = data.frame(Week = seq(yearweek('2016 W01'), yearweek('2019 W52'), 1))
      final_P = data.frame(Week = seq(yearweek('2016 W01'), yearweek('2019 W52'), 1))
      for (r in unique(data[[filtr[1]]])){
        selected = result[result[[filtr[1]]] == r, c(2:4)]
        colnames(selected) = c('Week', paste('S', r, sep = '_'), paste('P', r, sep = '_'))
        final_S = merge(x = final_S, y = selected[c(1, 2)], by = 'Week', all.x = TRUE)
        final_P = merge(x = final_P, y = selected[c(1, 3)], by = 'Week', all.x = TRUE)}
      final = merge(x = final_S, y = final_P, by = 'Week')
      final = ts(final[, 2:ncol(final)], start = c(2016, 1), end = c(2019, 52), frequency = 52)
      final[is.na(final)] = 0
      return(final)} 
  }
  
  if (is.na(filtr)){
    data %>%
      mutate(Month = yearmonth(Order.Date)) %>%
      group_by(Month) %>%
      summarize(Sales = sum(Sales), Profit = sum(Profit)) -> result
    final = ts(result[, 2:3], start = c(2016, 1), end = c(2019, 12), frequency = 12)
    final[is.na(final)] = 0
    return(final)
    
  } else if (length(filtr) == 1){
    data %>%
      mutate(Month = yearmonth(Order.Date)) %>%
      group_by(.data[[filtr[1]]], Month) %>%
      summarize(Total = sum(Sales), Profit = sum(Profit)) -> result
    final_S = data.frame(Month = seq(yearmonth('2016-01'), yearmonth('2019-12'), 1))
    final_P = data.frame(Month = seq(yearmonth('2016-01'), yearmonth('2019-12'), 1))
    for (r in unique(data[[filtr[1]]])){
      selected = result[result[[filtr[1]]] == r, c(2:4)]
      colnames(selected) = c('Month', paste('S', r, sep = '_'), paste('P', r, sep = '_'))
      final_S = merge(x = final_S, y = selected[c(1, 2)], by = 'Month', all.x = TRUE)
      final_P = merge(x = final_P, y = selected[c(1, 3)], by = 'Month', all.x = TRUE)}
    final = merge(x = final_S, y = final_P, by = 'Month')
    final = ts(final[, 2:ncol(final)], start = c(2016, 1), end = c(2019, 12), frequency = 12)
    final[is.na(final)] = 0
    return(final)
    
  } else {
    data %>%
      mutate(Month = yearmonth(Order.Date)) %>%
      group_by(.data[[filtr[1]]], .data[[filtr[2]]], Month) %>%
      summarize(Total = sum(Sales), Profit = sum(Profit)) -> result
    final_S = data.frame(Month = seq(yearmonth('2016-01'), yearmonth('2019-12'), 1))
    final_P = data.frame(Month = seq(yearmonth('2016-01'), yearmonth('2019-12'), 1))
    for (r2 in unique(data[[filtr[2]]])){
      selected1 = result[result[[filtr[2]]] == r2, ]
      for (r1 in unique(data[[filtr[1]]])){
        selected2 = selected1[selected1[[filtr[1]]] == r1, c(3:5)]
        colnames(selected2) = c('Month', paste('S', substr(r1, 1, 4), r2, sep = '_'), paste('P', substr(r1, 1, 4), r2, sep = '_'))
        final_S = merge(x = final_S, y = selected2[c(1, 2)], by = 'Month', all.x = TRUE)
        final_P = merge(x = final_P, y = selected2[c(1, 3)], by = 'Month', all.x = TRUE)}}
    final = merge(x = final_S, y = final_P, by = 'Month')
    final = ts(final[, 2:ncol(final)], start = c(2016, 1), end = c(2019, 12), frequency = 12)
    final[is.na(final)] = 0
    return(final)
  }
}

get_populations = function(){
  pop = read.csv('./population.csv')
  pop = pop[, -c(1, 3, 4, 6:12, 17:ncol(pop))]
  colnames(pop) = as.character(c('Region', 'Location', 2016:2019))
  locations = c('East', 'Central', 'South', 'West')
  pop = pop[-c(2:5, 57), ]
  
  d_16 = read.csv('./2016.csv', sep = ';')[1:53, ]
  d_17 = read.csv('./2017.csv', sep = ';')[1:53, ]
  d_18 = read.csv('./2018.csv', sep = ';')[1:53, ]
  d_19 = read.csv('./2019.csv', sep = ';')[1:53, ]
  no_work = merge(x = d_16, y = d_17, by = 'Location', all.y = TRUE)
  no_work = merge(x = no_work, y = d_18, by = 'Location', all.y = TRUE)
  no_work = merge(x = no_work, y = d_19, by = 'Location', all.y = TRUE)
  colnames(no_work) = as.character(c('Location', 2016:2019))
  no_work[, 2:5] = apply(no_work[, 2:5], 2, as.numeric)
  divide = function(x){
    return(x/1000)
  }
  no_work[, 2:5] = apply(no_work[, 2:5], 2, divide)
  no_work = no_work[-40, ]
  
  result = cbind(pop[, c(1, 2)], round(pop[-c(1, 2)]*(1 - no_work[-1]), 0))
  result[1, 3:6] = apply(result[-1, 3:6], 2, sum) # Modify USA Total by excluding Puerto Rico (no shipping there)
  regions = matrix(nrow = 4, ncol = 4)
  for (i in 1:4){
    regions[i, ] = apply(result[result$Region == i, 3:6], 2, sum)}
  regions = cbind(1:4, locations, regions)
  colnames(regions) = colnames(result)
  result = rbind(result, regions)
  result = result[, -1]
  row.names(result) = NULL
  
}