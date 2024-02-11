# Analyze stock proces using most recent information


# ----------------- PARAMETERS ----------------------
{
  codes = c("AMZN", "IBM", "MSFT",
            "INTC", "VUSA.AS", "GOOG",
            "NVDA", "META", "AAPL",
            "SPY", "TSLA", "AMD",
            "SHOP")
  n = 1400 # total number of days
  filter = c("AAPL", "GOOG", "VUSA.AS", "MSFT")
  test_size = 0.3
  n_strategies = 4
}


# ----------------- RAW DATASETS --------------------
{
  print("Downloading data.")
  codes = sort(codes)
  df_from_code = function(code){
    df = read.csv(paste("https://query1.finance.yahoo.com/v7/finance/download/",
                          code,
                          "?period1=1514764800&period2=1798675200&interval=1d&events=history&includeAdjustedClose=true",
                          sep = ""))
    return(df[nrow(df):1,])
  }
  raw_dfs = lapply(codes, df_from_code)
  
  new_price = function(code, price){
    i = which(codes == code)
    raw_dfs[[i]]$Adj.Close[1] = price
    raw_dfs <<- raw_dfs
  }
  # new_price("AAPL", 190.00)
  # new_price("IBM", 183.00)
}

# ----------------- PREPROCESSING --------------------
{
  filter = sort(filter)
  stopifnot(all(is.element(filter,codes)))
  get_dates = function(df){
    return(df$Date)
  }
  common_dates = sort(Reduce(intersect, lapply(raw_dfs,get_dates)), decreasing = TRUE)
  if (length(common_dates) > n){
    common_dates = common_dates[1:n]
  }
  print(paste("Using", length(common_dates), "days."))
}

# ----------------- TRAIN / TEST INDICIES --------------------
{
  all_indices = (n_strategies+1):(length(common_dates)-10)
  if(test_size > 0){
    test_days = sample(all_indices[-c(1,2)], floor(length(all_indices)*test_size/3), replace = FALSE)
    test_days = unique(c(test_days, test_days - 1, test_days - 2))
    train_days = all_indices[which(!is.element(all_indices,test_days))]
  } else {
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
  X = cbind(X, 1)
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
#' @return: a list, of lists, each having the same length.
train = function(p = 0){
  print(paste("p =", p))
  X = create_X(train_days) # common to all ensembles
  
  # CREATE THE Y VECTORS
  strategies = rep(1:n_strategies, length(filter))
  stock_names = rep(filter, each = n_strategies)
  Ys = create_Y(train_days, strategies, stock_names)
  stopifnot(length(Ys) == length(strategies))
  
  # CREATE THE FEATURES
  create_orthogonal_matrix = function(basis){
    eye = diag(rep(1,nrow(basis)))
    for (i in 1:ncol(basis)){
      basis[,i] = basis[,i] / norm(basis[,i], type="2")
    }
    while(TRUE){
      j = ncol(basis) + 1
      if (j > nrow(basis)){
        return(basis)
      }
      v = rnorm(ncol(X))
      v = v / norm(v,type="2")
      for (i in 1:(j-1)){
        u = basis[,i]
        v = v - as.numeric(t(u) %*% v)*u
        v = v/norm(v,type="2")
      }
      basis = cbind(basis, v)
    }
  }
  
  get_dim_reduction = function(X, Y, p){
    stopifnot(length(Y) == nrow(X))
    stopifnot(p <= ncol(X))
    betas = c()
    U = diag(rep(1,ncol(X)))
    for (i in 1:p){
      trunc = (X %*% U)[,i:ncol(X)]
      Hat = solve(t(trunc) %*% trunc)  %*% t(trunc) # TODO: can't do this! Singularity.
      beta = rep(0, ncol(X))
      beta[i:ncol(X)] = Hat %*% Y
      betas = cbind(betas, U %*% beta)
      for (j in 1:50){
        U = create_orthogonal_matrix(betas)
        max_err = max(abs(U %*% t(U) - diag(rep(1, ncol(X)))))
        if (max_err < 1e-7){
          break
        }
        print(paste("U not orthonormal. Try:", j))
        print(paste("Maximum error:", max_err))
        if (j == 50){
          stop()
        }
      }
      X = X - X %*% beta %*% t(beta) / norm(beta, "2")^2
    }
    return(betas)
  }
  wrapper = function(Y){
    get_dim_reduction(X, Y, p)
  }
  if (p > 0 && p < ncol(X)){
    print("Reducing dimension.")
    dim_reductions = lapply(Ys, wrapper)
  }
  
  # CREATE RANDOM FOREST FOR EACH FEATURE
  print("Growing the random forest.")
  library(randomForest)
  get_forest_from_index = function(idx){
    if (p > 0 && p < ncol(X)){
      features = X %*% dim_reductions[[idx]]
    } else {
      features = X
    }
    model = randomForest(x = features, y = Ys[[idx]],
                         ntree = 1200,
                         mtry = 3)
    return(model)
  }
  models = lapply(1:length(Ys), get_forest_from_index)
  return(list("models" = models,
              "dim_reductions" = if (p > 0 && p < ncol(X)) dim_reductions else NULL,
              "strategies" = strategies,
              "stock_names" = stock_names))
}

### PORTFOLIO CONSTRUCTION ###

get_portfolio = function(days, strategy){
  projections = c()
  X = create_X(days)
  Ys = create_Y(all_indices, result.train$strategies, result.train$stock_names)
  Y_vectors = c()
  for (idx in 1:length(result.train$models)){
    if(result.train$strategies[[idx]] != strategy){
      next
    }
    code = result.train$stock_names[[idx]]
    betas = result.train$dim_reductions[[idx]]
    model = result.train$models[[idx]]
    
    # use result.train
    # predict the 3 stock prices using the top row in X
    if (is.null(betas)){
      prediction = predict(model, X)
    } else {
      prediction = predict(model, X %*% betas)
    }
    projections = cbind(projections, as.numeric(prediction))
    
    Y = Ys[[idx]]
    Y_vectors = cbind(Y_vectors, Y)
  }
  # Compute covarience matrix of the Y according to the strategy. (use create_Y)
  Sigma = t(Y_vectors) %*% Y_vectors
  portfolio = data.frame(projections %*% solve(Sigma))
  projections = data.frame(projections)
  names(portfolio) = names(projections) = filter
  return(list("portfolio" = portfolio / rowSums(abs(portfolio)),
              "projections" = projections
              ))
}


### TEST ###

validation = function(){
  Ys = create_Y(test_days, result.train$strategies, result.train$stock_names)
  performances = c()
  for (strategy in 1:n_strategies){
    portfolios = get_portfolio(test_days, strategy)$portfolio
    stock_names = result.train$stock_names
    strategies = result.train$strategies
    
    inds = which(strategies == strategy)
    Y_vectors = c()
    for (i in inds){
      Y_vectors = cbind(Y_vectors, Ys[[i]])
    }
    performance = rowMeans(as.matrix(Y_vectors * portfolios))
    performances = cbind(performances, performance)
  }
  performances = data.frame(performances)
  names(performances) = as.factor(1:n_strategies)
  return(list("means" = colMeans(performances),
              "sds" = sapply(performances, sd)/sqrt(nrow(performances)),
              "n" = nrow(performances)
              ))
}

print("Training.")
result.train = train()

print("Validation:")
print(validation())
portfolio = c()
projections = c()
for(strategy in 1:n_strategies){
  portfolio = rbind(portfolio, get_portfolio(1, strategy)$portfolio)
  projections = rbind(projections, get_portfolio(1, strategy)$projections)
}
print("Sizing")
print(portfolio)
print("Projections")
print(projections)
strategy = 4
barplot(as.numeric(portfolio[strategy,]),
        names.arg = sort(filter),
        main = paste("Strategy:", strategy),
        las = 2)

# 
# X1 = c()
# X2 = c()
# X3 = c()
# X4 = c()
# for (i in 1:length(dfs.train)){
#   df = dfs.train[[i]]
#   code = codes[i]
#   if(!is.element(code,filter)){
#     next
#   }
#   close = log(df$Adj.Close)
#   n = length(close)
#   
#   close1 = close[-n] - close[-1]
#   close2 = close[-(n:(n-1))] - close[-(1:2)]
#   close3 = close[-(n:(n-2))] - close[-(1:3)]
#   close4 = close[-(n:(n-3))] - close[-(1:4)]
#   
#   X1 = cbind(X1, close1 - mean(close1))
#   X2 = cbind(X2, close2 - mean(close2))
#   X3 = cbind(X3, close3 - mean(close3))
#   X4 = cbind(X4, close4 - mean(close4))
# }
# 
# Sigma1 = matrix(as.numeric(t(X1) %*% X1), ncol = length(filter))
# Sigma2 = matrix(as.numeric(t(X2) %*% X2), ncol = length(filter))
# Sigma3 = matrix(as.numeric(t(X3) %*% X3), ncol = length(filter))
# Sigma4 = matrix(as.numeric(t(X4) %*% X4), ncol = length(filter))
# 
# Y1 = Y_hat[,(1:length(filter))*4-3]
# Y2 = Y_hat[,(1:length(filter))*4-2]
# Y3 = Y_hat[,(1:length(filter))*4-1]
# Y4 = Y_hat[,(1:length(filter))*4]
# 
# P1 = Y1 %*% solve(Sigma1)
# P2 = Y2 %*% solve(Sigma2)
# P3 = Y3 %*% solve(Sigma3)
# P4 = Y4 %*% solve(Sigma4)
# 
# if(test_size == 0){
#   df = data.frame(rbind(P1[1,], P2[1,], P3[1,], P4[1,]))
#   names(df) = filter
#   df = cbind(df, "Return (%)" = 100*c(sum(P1[1,]*Y1[1,])/sum(abs(P1[1,])),
#                                       sum(P2[1,]*Y2[1,])/sum(abs(P2[1,])),
#                                       sum(P3[1,]*Y3[1,])/sum(abs(P3[1,])),
#                                       sum(P4[1,]*Y4[1,])/sum(abs(P4[1,]))
#                                       ))
#   print("Forecast (%):")
#   print(paste("1 day: ", 100*mean(Y1[1,]),
#               ", 2 day: ", 100*mean(Y2[1,]),
#               ", 3 day: ", 100*mean(Y3[1,]),
#               ", 4 day: ", 100*mean(Y4[1,]),
#               sep = ""))
#   return(df)
# }
# 
# # Using: Sigma1, Sigma2, Sigma3, beta_hat
# 
# Y_hat = X %*% beta_hat
# Y1 = Y_hat[,(1:length(filter))*4-3]
# Y2 = Y_hat[,(1:length(filter))*4-2]
# Y3 = Y_hat[,(1:length(filter))*4-1]
# Y4 = Y_hat[,(1:length(filter))*4]
# 
# P1 = Y1 %*% solve(Sigma1)
# P2 = Y2 %*% solve(Sigma2)
# P3 = Y3 %*% solve(Sigma3)
# P4 = Y4 %*% solve(Sigma4)
# 
# # Performance:
# a1 = (mean(rowSums(P1 * Y[,(1:length(filter))*4-3])/rowSums(abs(P1)))+1)^365
# a2 = (mean(rowSums(P2 * Y[,(1:length(filter))*4-2])/rowSums(abs(P2)))+1)^(365/2)
# a3 = (mean(rowSums(P3 * Y[,(1:length(filter))*4-1])/rowSums(abs(P3)))+1)^(365/3)
# a4 = (mean(rowSums(P4 * Y[,(1:length(filter))*4])/rowSums(abs(P4)))+1)^(365/4)
# print(c(a1, a2, a3, a4))
# 
# 
# test_performace = FALSE
# if (test_performace){
#   performance = c()
#   for (i in 1:50){
#     performance = rbind(performance, execute(n))
#   }
#   means = colMeans(performance)
#   sds = sqrt(colMeans((performance - means)^2))
#   print(rbind(means, sds))
# }
# 
# portfolio = execute(n,0,filter = filter)
# print("Portfolio sizing:")
# print(portfolio)
# 
# strategy = 4
# barplot(as.numeric(portfolio[strategy,-ncol(portfolio)]),
#         names.arg = sort(filter),
#         main = paste("Strategy:", strategy),
#         las = 2)


# US markets: 9:30 - 16:00
# EUR markets: 9:00 - 17:30
