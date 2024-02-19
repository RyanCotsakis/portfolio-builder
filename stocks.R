# Analyze stock proces using most recent information
library(randomForest)

# ----------------- PARAMETERS ----------------------
{
  ### Choose pretrained forests ---------------------
  
  load_model = "bluechip0217"
  
  ### If there's no pre-trained model, specify codes.
  new_codes = unique(c(
    "AMZN", "IBM", "MSFT", "GOOG", "NVDA", "META", "SPY", "ABBV", "VWDRY", "PG"
  ))
  
  n = Inf # total number of days. Set to Inf to use all
  # filter = c("GOOG", "IBM") # optional filter
  test_size = 0 # set to 0 to use all data. Parameter in [0,1)
  test_block_size = 10 # test data comes in blocks of size test_block_size
  n_strategies = 5 # number of days to forecast
  p = 100 # number of features before RFE. Set to Inf to use all.
  iterate_times = 1 # use recursive feature elimination (RFE) if > 1.
  ntree = 1200
  regress_on_date = TRUE # use the date as a covariate
  risk_param = 0.5 # amount of mitigatable risk. Parameter in [0,1].
  
  visualize = 1
  tryCatch(visualize_strategy(visualize), error = function(e){message(paste("Warning:", e))})
}


# ----------------- RAW DATASETS --------------------
{
  if (exists("load_model") && !exists("result.train")){
    setwd("C:/Users/Ryan/Documents/misc_code/stocks/")
    print(paste("Loading ", load_model, ".", sep = ""))
    load(paste(load_model, ".RData", sep = ""))
  }
  
  if(exists("result.train")){
    codes = result.train$codes
    stopifnot(n_strategies <= max(result.train$strategies))
  } else {
    codes = new_codes
  }
  codes = sort(codes)
  
  print("Downloading data.")
  df_from_code = function(code){
    df = read.csv(paste("https://query1.finance.yahoo.com/v7/finance/download/",
                          code,
                          "?period1=1483228800&period2=1767225600&interval=1d&events=history&includeAdjustedClose=true",
                          sep = ""))
    return(df[nrow(df):1,])
  }
  raw_dfs = lapply(codes, df_from_code)
  
  new_price = function(code, price){
    i = which(codes == code)
    raw_dfs[[i]]$Adj.Close[1] = price
    raw_dfs <<- raw_dfs
    print(paste("Setting price of", code, "to", price))
  }
  # new_price("VUSA.AS", 88.48)
}

# ----------------- PREPROCESSING --------------------
{
  if (exists("filter") && length(filter) > 1){
    filter = sort(filter)
    stopifnot(all(is.element(filter,codes)))
  } else {
    print("Not filtering.")
    filter = codes
  }
  if (exists("result.train")){
    stopifnot(all(is.element(filter, result.train$stock_names)))
  }
  
  get_dates = function(df){
    return(df$Date)
  }
  common_dates = sort(Reduce(intersect, lapply(raw_dfs,get_dates)), decreasing = TRUE)
  if (length(common_dates) > n){
    common_dates = common_dates[1:n]
  }
  print(paste("Using", length(common_dates), "days."))
  print(paste("Until ", common_dates[1], ".", sep = ""))
}

# ----------------- TRAIN / TEST INDICIES --------------------
{
  all_indices = (n_strategies+1):(length(common_dates)-10)
  if(test_size > 0 && !exists("result.train")){
    set.seed(111)
    print(paste("Using a test / train split of ", 100*test_size, "% test data.", sep = ""))
    test_days = sample(all_indices[-(1:(test_block_size - 1))],
                       max(floor(length(all_indices)*test_size/test_block_size), 1),
                       replace = FALSE)
    for (i in 1:(test_block_size - 1)){
      test_days = unique(c(test_days, test_days - 1))
    }
    train_days = all_indices[which(!is.element(all_indices,test_days))]
  } else if (!exists("result.train")){
    print("Not using test data.")
    # test_days = all_indices
    train_days = all_indices
  }
  dfs = list()
  for (i in 1:length(codes)){
    df = raw_dfs[[i]]
    dfs[[i]] = df[which(is.element(df$Date,common_dates)),]
  }
}

# ------------------------------------------------------------

if(regress_on_date){
  print("Regressing on the date.")
} else {
  print("Not regressing on the date.")
}

create_X = function(days){
  X = c()
  for (day in days){
    new_row_X = c()
    for (i in 1:length(dfs)){
      df = dfs[[i]]
      code = codes[i]
      close = log(df$Adj.Close)
      new_row_X = cbind(new_row_X,
                        t(close[day] - close[(day+10):(day+1)])
      )
    }
    X = rbind(X, new_row_X)
  }
  if (regress_on_date){
    X = cbind(X, as.numeric(days))
  }
  return(X)
}

create_Y = function(days, strategies, stock_names){
  Ys = list()
  for (i in 1:length(dfs)){
    code = codes[i]
    if (is.element(code, filter)){
      df = dfs[[i]]
      close = log(df$Adj.Close)
      for (strategy in 1:n_strategies){
        j = which(strategies == strategy & stock_names == code)
        stopifnot(length(j) == 1)
        Ys[[j]] = close[days-strategy] - close[days]
      }
    }
  }
  return(Ys)
}

### TRAIN ###

#' @param p: number of features given to the random forest.
#'             if 0, then use all features.
#' @return: a list, of lists, each having the same length. $codes is the codes used to train.
train = function(p = Inf, ntree = 1200){
  print(paste("Using", ntree, "trees per model."))
  X = create_X(train_days) # common to all ensembles 
  if (p > ncol(X)){
    p = ncol(X)
  }
  print(paste("p =", p))
  
  # CREATE THE Y VECTORS
  strategies = rep(1:n_strategies, length(filter))
  stock_names = rep(filter, each = n_strategies)
  Ys = create_Y(train_days, strategies, stock_names)
  stopifnot(length(Ys) == length(strategies))
  
  # DIMENSION REDUCTION
  if (p < ncol(X)){
    print("Reducing dimensionality.")
    normalize = diag(1/apply(X, MARGIN = 2, sd))
    dim_reduction = normalize %*% prcomp(X %*% normalize)$rotation[,1:p]
  } else {
    dim_reduction = NULL
  }
  
  # CREATE RANDOM FOREST FOR EACH STOCK + STRATEGY
  print("Growing the random forest.")
  if (iterate_times > 1){
    print("Using feature elimination.")
  }
  get_forest_from_index = function(idx){
    if (!is.null(dim_reduction)){
      features = X %*% dim_reduction
    } else {
      features = X
    }
    inds_to_keep = 1:ncol(features)
    for (i in 1:iterate_times){
      model = randomForest(x = features[,inds_to_keep], y = Ys[[idx]],
                           ntree = ntree,
                           mtry = 5)
      model$rfe = inds_to_keep
      inds_to_keep = inds_to_keep[
        order(model$importance, decreasing = TRUE)[1:ceiling(length(model$importance)*2/3)]
        ]
    }
    return(model)
  }
  models = lapply(1:length(Ys), get_forest_from_index)
  return(list("models" = models,
              "dim_reduction" = dim_reduction,
              "strategies" = strategies,
              "stock_names" = stock_names,
              "codes" = codes))
}

### PORTFOLIO CONSTRUCTION ###

get_portfolio = function(days, strategy, risk = 0){
  filter = sort(filter)
  projections = c()
  X = create_X(days)
  Ys = create_Y(all_indices, result.train$strategies, result.train$stock_names)
  Y_vectors = c()
  pcs = result.train$dim_reduction
  for (idx in 1:length(result.train$models)){
    if(result.train$strategies[[idx]] != strategy){
      next
    }
    code = result.train$stock_names[[idx]]
    if(!is.element(code, filter)){
      next
    }
    model = result.train$models[[idx]]
    
    if (is.null(pcs)){
      feature_matrix = X
    } else {
      feature_matrix =  X %*% pcs
    }
    feature_matrix = feature_matrix[,model$rfe]
    prediction = predict(model, feature_matrix)
    projections = cbind(projections, as.numeric(prediction))
    
    Y = Ys[[idx]]
    Y_vectors = cbind(Y_vectors, Y - mean(Y))
  }
  # Compute covarience matrix of the Y according to the strategy. (use create_Y)
  Sigma = t(Y_vectors) %*% Y_vectors / length(all_indices)
  portfolio = data.frame(projections %*% solve(Sigma))
  portfolio = portfolio / rowSums(abs(portfolio))
  projections = data.frame(projections)
  portfolio = (1 - risk) * portfolio + risk * projections / rowSums(abs(projections))
  names(portfolio) = names(projections) = filter
  return(list("portfolio" = portfolio / rowSums(abs(portfolio)),
              "projections" = projections,
              "signals" = projections / sqrt(diag(Sigma))
              ))
}

### TEST ###

validation = function(risk = 0){
  if(test_size == 0){
    print("No validation dataset.")
    return(NULL)
  }
  Ys = create_Y(test_days, result.train$strategies, result.train$stock_names)
  performances = c()
  for (strategy in 1:n_strategies){
    portfolios = get_portfolio(test_days, strategy, risk)$portfolio
    stock_names = result.train$stock_names
    strategies = result.train$strategies
    
    inds = which(strategies == strategy & is.element(stock_names, filter))
    Y_vectors = c()
    for (i in inds){
      Y_vectors = cbind(Y_vectors, Ys[[i]])
    }
    performance = rowMeans(as.matrix(Y_vectors * portfolios))
    performances = cbind(performances, performance)
  }
  performances = data.frame(performances)
  names(performances) = as.factor(1:n_strategies)
  mu = colMeans(performances)
  sigma2 = sapply(performances, var)
  n = 5 * 52 / (1:n_strategies)
  annual = exp(n * (mu + sigma2/2)) # lognormal mean
  annual_sd = sqrt((exp(n* sigma2) - 1) * exp(n * (2*mu + sigma2))) # lognormal variance
  return(list("mu" = mu,
              "mu_sd" = sapply(performances, sd)/sqrt(nrow(performances)),
              "annual" = annual,
              "annual_sd" = annual_sd,
              "n" = nrow(performances)
              ))
}

if(!exists("result.train")){
  print("Training.")
  result.train = train(p, ntree)
} else {
  print("Using existing result.train.")
}

print("Validation:")
print(validation(risk_param))
portfolio = c()
signals = c()
projections = c()
for(strategy in 1:n_strategies){
  day = 1 # today = 1. Yesterday = 2
  portfolio = rbind(portfolio, get_portfolio(day, strategy, risk_param)$portfolio)
  signals = rbind(signals, get_portfolio(day, strategy)$signals)
  projections = rbind(projections, get_portfolio(day, strategy)$projections)
}

visualize_strategy = function(strategy){
  stopifnot(all(names(portfolio) == filter))
  par(mfrow = c(1,2))
  barplot(as.numeric(portfolio[strategy,]),
          names.arg = sort(filter),
          main = "Portfolio",
          las = 2)
  barplot(as.numeric(signals[strategy,]),
          names.arg = sort(filter),
          main = paste("Signals: T =", strategy),
          las = 2)
}

print("Sizing")
print(portfolio)
print("Projections")
print(projections)
visualize_strategy(visualize)


# US markets: 9:30 - 16:00
# EUR markets: 9:00 - 17:30
