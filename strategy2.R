rm(list=ls())
library(randomForest)

# ----------------- PARAMETERS ----------------------
{
  ### Choose pretrained forests ---------------------
  
  ### If there's no pre-trained model, specify codes.
  tickers = unique(c(
    "WMT", "MSFT", "VUSA.AS", "META"
  ))
  load(file = "all_data_2.RData")
  tickers = training_tickers
  
  n = Inf
  new_model = FALSE
  start_from = 1 # use 1 for today. Use ~ 360 for 25% test data 
  model_name = "all_data_2"
  reset_volume = TRUE
}

# ----------------- RAW DATASETS --------------------
{
  tickers = sort(tickers)
  
  print("Downloading data.")
  df_from_ticker = function(ticker){
    df = read.csv(paste("https://query1.finance.yahoo.com/v7/finance/download/",
                        ticker,
                        "?period1=1483228800&period2=1767225600&interval=1d&events=history&includeAdjustedClose=true",
                        sep = ""))
    return(df[nrow(df):1,])
  }
  raw_dfs = lapply(tickers, df_from_ticker)
  
  new_price = function(ticker, price){
    i = which(tickers == ticker)
    raw_dfs[[i]]$Adj.Close[1] = price
    raw_dfs <<- raw_dfs
    print(paste("Setting price of", ticker, "to", price))
  }
  # new_price("VUSA.AS", 88.48)
}


# ----------------- PREPROCESSING --------------------
{
  get_dates = function(df){
    return(df$Date)
  }
  common_dates = sort(Reduce(intersect, lapply(raw_dfs,get_dates)), decreasing = TRUE)
  if (length(common_dates) > n){
    common_dates = common_dates[1:n]
  }
  print(paste("Using", length(common_dates) + 1 - start_from, "days."))
  print(paste("Until ", common_dates[start_from], ".", sep = ""))
  
  dfs = list()
  X = c()
  for (i in 1:length(tickers)){
    df = raw_dfs[[i]]
    dfs[[i]] = df[which(is.element(df$Date,common_dates)),]
    dfs[[i]]$Close[1:(nrow(dfs[[i]])-1)] = log(dfs[[i]]$Close[1:(nrow(dfs[[i]])-1)]) -
                                           log(dfs[[i]]$Close[2:nrow(dfs[[i]])])
    dfs[[i]] = dfs[[i]][start_from:(nrow(dfs[[i]])-1),c("Close", "Volume", "Date")]
    
    # ---- Create X -----
    df = dfs[[i]]
    
    delay2 = delay4 = delay8 = delay16 = delay32 = rep(df$Close[nrow(df)],nrow(df))
    volume_avg = rep(df$Volume[nrow(df)],nrow(df))
    var_avg = rep(df$Close[nrow(df)]^2,nrow(df))
    for (day in (nrow(df)-1):1){
      a = 1 - 1/2
      delay2[day] = a*delay2[day+1] + (1-a)*df$Close[day]
      a = 1 - 1/4
      delay4[day] = a*delay4[day+1] + (1-a)*df$Close[day]
      a = 1 - 1/8
      delay8[day] = a*delay8[day+1] + (1-a)*df$Close[day]
      a = 1 - 1/16
      delay16[day] = a*delay16[day+1] + (1-a)*df$Close[day]
      a = 1 - 1/32
      delay32[day] = a*delay32[day+1] + (1-a)*df$Close[day]
      a = 1 - 1/10
      volume_avg[day] = a*volume_avg[day+1] + (1-a)*df$Volume[day]
      a = 1 - 1/10
      var_avg[day] = a*var_avg[day+1] + (1-a)*df$Close[day]^2
    }
    avance = rep(df$Close[1],nrow(df))
    for (day in 2:nrow(df)){
      a = 1 - 1/5
      avance[day] = a*avance[day-1] + (1-a)*df$Close[day]
    }
    X = rbind(X, cbind(
      "Y" = avance,
      "d1" = df$Close,
      "d2" = delay2,
      "d4" = delay4,
      "d8" = delay8,
      "d16" = delay16,
      "d32" = delay32,
      "volume" = log(df$Volume) - log(volume_avg),
      "var" = var_avg,
      "date" = 1:nrow(df)
    ))
  }
  X = data.frame(X)
}

if (new_model){
  print("Growing the random forest...")
  model = randomForest(Y ~ ., data = X,
                       ntree = 1000,
                       mtry = 3)
  print("Saving...")
  training_tickers = tickers
  save(start_from, training_tickers, model, file = paste(model_name, ".RData", sep = ""))
} else {
  print("Loading model.")
  load(file = paste(model_name, ".RData", sep = ""))
  test_inds = which(X$date < start_from)
  X$date[test_inds] = 1
}

get_predictions = function(reset_volume = FALSE){
  if (reset_volume){
    X$volume = 0
  }
  predictions = predict(model, X[which(X$date == 1),-1])
  predictions = matrix(predictions, ncol = length(tickers))
  predictions = data.frame(predictions)
  names(predictions) = tickers
  return(predictions)
}

get_signals = function(prediction){
  return(prediction / sqrt(X[which(X$date == 1),"var"]))
}

get_portfolios = function(prediction, sharpe = TRUE){
  if (sharpe){
    Sigma = cov(matrix(X$d1, ncol = length(tickers)))
    portfolio = as.matrix(prediction) %*% solve(Sigma)
  } else {
    portfolio = as.matrix(prediction)
  }
  
  normalize = function(row){
    return(row / sum(abs(row)))
  }
  portfolio = data.frame(t(apply(portfolio, 1, normalize)))
  names(portfolio) = tickers
  return(portfolio)
}

get_returns = function(portfolio){
  actual = matrix(X$Y[test_inds], ncol = length(tickers))
  returns = apply(actual * portfolio, 1, sum)
  return(data.frame(
    "mean" = mean(returns),
    "sd" = sd(returns)
  ))
}

get_summary = function(reset_volume = FALSE){
  if (!new_model){
    p = get_predictions(reset_volume)
    portfolio = get_portfolios(p)
    signals = get_signals(p)
    print(head(signals))
    par(mfrow = c(1,2))
    barplot(as.numeric(portfolio[1,]),
            names.arg = tickers,
            main = "Portfolio",
            las = 2)
    barplot(as.numeric(signals[1,]),
            names.arg = tickers,
            main = "Signals",
            las = 2)
    returns = get_returns(portfolio)
    ret1 = format(round(100 * returns$mean, 2), nsmall = 2)
    ret2 = format(round(100 * returns$sd, 2), nsmall = 2)
    print(paste("Test returns: (", ret1, " +/- ", ret2, ")%.", sep = ""))
  } else {
    par(mfrow = c(1,1))
    plot(model)
    print("Feature importance:")
    print(model$importance)
  }
}
get_summary(reset_volume)
