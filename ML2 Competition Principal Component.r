#Import libraries
library(ISLR2)
library(keras)
library(pracma)

#Read data
xraw = read.csv("inflation_data.csv", header = TRUE) %>% as.matrix()

#Seperate in date column and infaltion series. 
#Note, date singular column has been created in excel spreadsheet using similir decimal to quarter methodology 
#to previous data of .25 = quarter 2, .5 = quarter 3, .75 = quarter 4
xdate = xraw[,1]
xdata = xraw[,4:20]

#Extract the first principal component
X = scale(xdata) # standardize the data
XX = t(X) %*% X
ev = eigen(XX) 
M = 1                      # set number of factors
phi = ev$vectors[,c(1:M)]  # extract the corresponding eigenvectors
Z = X %*% phi              # compute the scores

#Sign-normalized the first principal component so that it comoves with inflation
Z[,1] = Z[,1]*sign(phi[1]) 

#Create lagged values 
lagm = function(x, k) {
  n = nrow(x)
  pad = matrix(NA, k, ncol(x))
  rbind(pad, x[1:(n - k), ])
}

#Initialize list to store inflation forecasts 
forecast_list = list()

#For loop to run through all series 
for (x in c(1:ncol(xdata))) {

#Forecast inflation using an AR(4) + first component
newdata = cbind(xdata[, x], Z[, 1])
xframe = data.frame(y = xdata[, x],
                    L1 = lagm(newdata, 1), L2 = lagm(newdata, 2), L3 = lagm(newdata, 3), L4 = lagm(newdata, 4), 
                    L5 = lagm(newdata, 5), L2 = lagm(newdata, 6), L3 = lagm(newdata, 7), L4 = lagm(newdata, 8) 
                    
)

#Remove NA rows from dataset
xframe = xframe[-(1:8), ]

#Run Regressesion
ar2 = lm(y ~., data = xframe)

#Test variables
testx = c(1, newdata[nrow(newdata), ], as.matrix(xframe[nrow(xframe), 2:15]))

#Forecast for series
forecast = testx %*% ar2$coefficient
forecast_list = append(forecast_list,values = forecast)
}

#change list to dataframe
prediction = do.call(rbind.data.frame, forecast_list)

#Output
write.csv(prediction,"Forecast_PC.csv", row.names = TRUE)

"Procedure - Only change to data is a column was added in excel in column A combining the year and quarter like so
1960, 1960.25, 1960.5,1960.75. Outside of the that the code will run as executed assuming datasets are downloaded
necessary libraries are installed. The headers of the final output are changed in the csv file to reflect the example

I used principal compenet analysis along with 8 lagged variables to forecast the inflation. I wanted to something 
other than neural networks and irrated upon the work form the final assignment. Both results seemed similar in the 
aggregate, but definetly different across each individual time series.
