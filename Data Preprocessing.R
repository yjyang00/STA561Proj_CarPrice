library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(glmnet)

# Data Preprocessing ---------------------------------------------------------------------

## read data
data = read.csv("vehicles.csv")

## drop unnecessary columns
car_nc = data %>% filter(state=='nc')
car_nc_clean = car_nc %>% 
  select(-c(id, url, region_url, image_url, description, county, 
            posting_date, model, state, region, VIN, long, lat)) %>%
  mutate(price=as.numeric(price), odometer=as.numeric(odometer))

## check categories in each feature
sapply(car_nc_clean[,c(2:6, 8:13)], unique) 

## replace NA with ""
car_nc_clean[car_nc_clean == ''] = NA

## check proportion of NA in each column
colMeans(is.na(car_nc_clean))

## drop NA if proportion of that column is below 4%: year, manufacturer, fuel, odometer, title_status, transmission
car_nc_clean = car_nc_clean %>%
  drop_na(c(year, manufacturer, fuel, odometer, title_status, transmission))

colMeans(is.na(car_nc_clean))

## Deal with outliers in price
car_nc_clean = car_nc_clean %>%
  filter(price<1e6) # remove extreme prices: 135008900, 113456789
summary(car_nc_clean$price)

median =  median(car_nc_clean[car_nc_clean$price>0,]$price) 
MAD = mad(car_nc_clean[car_nc_clean$price>0,]$price) 
lb = median-MAD 
ub = median+MAD 

car_nc_clean = car_nc_clean %>%
  filter(price>lb & price<ub)

summary(car_nc_clean$price)
hist(car_nc_clean$price) # distribution of price

## group paint_color to be white, black, silver, blue, red, grey, other
table(car_nc_clean$paint_color) 
car_nc_clean_trans = car_nc_clean %>%
  mutate(year = ifelse(year<2013, 1, 0)) %>%
  mutate(paint_color = fct_collapse(paint_color, 
                                    "other" = c("brown", "custom", "green", "orange","purple", "yellow")))
table(car_nc_clean_trans$paint_color) # all categories are larger than 500

## transform year into binary variable: >2013, <2013
car_nc_clean_trans=car_nc_clean_trans %>%
  mutate(year=as.factor(year), manufacturer=as.factor(manufacturer), condition=as.factor(condition),
         cylinders=as.factor(cylinders), fuel=as.factor(fuel), title_status=as.factor(title_status), 
         transmission=as.factor(transmission), drive=as.factor(drive), size=as.factor(size), 
         type=as.factor(type), paint_color=as.factor(paint_color))
str(car_nc_clean_trans)
colMeans(is.na(car_nc_clean_trans))

# Creating Dummy Variables ---------------------------------------------------------------------

X = car_nc_clean_trans[,2:ncol(car_nc_clean_trans)]
dummy_df = model.matrix(price~., model.frame(~ ., car_nc_clean_trans, na.action=na.pass))
colnames(dummy_df)[1]="Intercept"
dummy_df = cbind(car_nc_clean_trans$price, dummy_df) # combine with price
colnames(dummy_df)[1]="price"
colnames(dummy_df)
colMeans(is.na(dummy_df)) # check the missing rate in each column

## store data
write.csv(dummy_df, "dummy_df.csv")
write.csv(car_nc_clean, "nc_clean.csv")


# Hot Deck Imputation -------------------------------------------------------------------------
df_hd = car_nc_clean_trans
df_hd <- apply(df_hd, 2, function(x) {
  ifelse(is.na(x), sample(x[!is.na(x)], 1), x)
})
colMeans(is.na(df_hd))
df_hd = as.data.frame(df_hd)
head(df_hd)

num.col = c("price", "year","odometer")
for (col in num.col) {
  df_hd[,col] = as.numeric(df_hd[,col])
}

## fit model
## creat dummy
## Assuming the character columns in your dataframe are named 'char_col1' and 'char_col2'
set.seed(1)
dummy_df = model.matrix(price~., model.frame(~ ., df_hd, na.action=na.pass))
colnames(dummy_df)[1]="Intercept"
dummy_df = cbind(df_hd$price, dummy_df) # combine with price
colnames(dummy_df)[1]="price"
colMeans(is.na(dummy_df))


# fit lasso regression with cross-validation
fit <- cv.glmnet(as.matrix(dummy_df[, -1]), dummy_df[,1], alpha = 1, standardize = TRUE)
lambda_opt <- fit$lambda.min
lasso_fit <- glmnet(as.matrix(dummy_df[, -1]), dummy_df[,1], alpha = 1, lambda = lambda_opt)
predict_lasso = predict(lasso_fit, scale(as.matrix(dummy_df[, -1])))

# R2
cvfit = cv.glmnet(as.matrix(dummy_df[, -1]), dummy_df[,1])
rsq = 1 - cvfit$cvm/var(dummy_df[,1])
R2 = rsq[which.min(abs(cvfit$lambda-lambda_opt))]
R2
