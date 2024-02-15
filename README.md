## Stock Market Analysis with Random Forests

This repository contains R code for conducting stock market analysis using random forest models. The code downloads historical stock price data from Yahoo Finance, preprocesses it, and trains random forest models to predict future stock prices. 

**Features:**

1. **Data Collection:** The code downloads historical stock price data from Yahoo Finance for specified stocks.

2. **Preprocessing:** It preprocesses the data, ensuring only dates common to all stocks are used for analysis.

3. **Model Training:** Random forest models are trained for each stock and forecasting horizon combination. Models can be saved and loaded into R to increase speed.

4. **Portfolio Construction:** Portfolios are constructed based on the trained models, computing projections, the strength of the signals, and portfolio weights for several forecast horizons.

5. **Validation:** If a validation set is specified, the code evaluates the performance of the portfolios on the test set.

6. **Visualization:** Visualization functions are provided to visualize the constructed portfolios and signals for a given forecast horizon.

**Usage:**

1. Set parameters such as stock symbols, number of days for analysis, and test dataset size in the code.

2. Run the code to perform data collection, preprocessing, model training, and portfolio construction.

3. Visualize the constructed portfolios and signals using the provided visualization function.

**Dependencies:**

- [randomForest](https://cran.r-project.org/web/packages/randomForest/) R package

**Authors:**

Ryan Cotsakis

**Acknowledgments:**

- Thanks to Yahoo Finance for providing historical stock price data.

**Disclaimer:**

This code is for educational and informational purposes only. It does not constitute financial advice. Use at your own risk.
