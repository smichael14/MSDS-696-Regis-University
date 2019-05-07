# MSDS-696-Regis-University
Shawn Michael
Regis University


The purpose of the Black Friday data collection is to gain a deeper understanding of demographics pertaining to the customers they service versus the products offered by the business.  The data analysis was completed with R, using several packages such as: tidyverse, broom and ranger.  
In addition to investigating the relationships between consumers and products, I wanted to create models for predicting the product sales.  It is important to note, that the models I am creating do not follow the typical creation approach.  Instead of creating a model for the whole dataset with a single prediction model, I created thousands of models, one for each product offering.  Then I took the mean calculation of MAE from each model. 
This is powerful for several reasons:
     1.	Forecasting individual projects
     2.	Forecasting groups of products (top 10 or categorical)
     3.	Statistic and Coefficients are available for each model for analysis.
Since the dataset was used as a competition in the past, I wanted to see if I could improve on the results I was seeing from other competitors.  Based on the results that were still available, my model output values are among the top finishers. 


### Loading Libraries
```{r message=FALSE, warning=FALSE}
# libraries required for exploring data
library(tidyverse)
library(broom)
library(readr)
library(dplyr)
library(tidyr)
library(lattice)
library(gmodels)
library(ggplot2)
library(skimr)
library(rsample) 
library(Metrics)
library(ranger)
```



# Reading Data  

I have been working on both Mac and Windows through this project, therefore I would adjust my path accordingly.

```{r}
# mac book path
# read file and assign NA to missing values
#sales <- read.csv("~/OneDrive/Regis/MSDS 696/Shawn Michael Final Project 2/BlackFridayTest.csv", na = c("", "NA"))

#pc path
# read file and assign NA to missing values
sales <- read.csv("C:/Users/shawn/OneDrive/Regis/MSDS 696/Shawn Michael Final Project 2/BlackFriday.csv" ,na = c("", "NA"))
```


# Data Discovery  

```{r}
# glimpse allows to view the data quickly and in an organized manner
glimpse(sales)
```
![alt text](https://github.com/smichael14/MSDS-696-Regis-University/blob/master/Images/1%20dd%20glimpse.GIF)


Investigate the data to get an understanding what is in the dataset.

```{r}
# skim is similar to summary, however it provides much more information
skim(sales)
```
![alt text](https://github.com/smichael14/MSDS-696-Regis-University/blob/master/Images/2%20skim.GIF)

After looking at the dataset, you can see from the output that I have two data fields that contain NA's.  In addition, Occupation and Marital_Status are listed as integers, but probably better served as factors since they both contain finite sets.

```{r}
# converting missing data NAs to 0 and converting Occupation & Marital to factors
cleaned_sales <- sales %>%
  mutate(Product_Category_2 = as.integer(replace_na(Product_Category_2, 0)),
         Product_Category_3 = as.integer(replace_na(Product_Category_3, 0)),
         Occupation = as.factor(Occupation),
         Marital_Status = as.factor(Marital_Status))

# skim cleaned dataset
skim(cleaned_sales)
```
![alt text](https://github.com/smichael14/MSDS-696-Regis-University/blob/master/Images/3%20cleaned.GIF)

### Understanding the data  


#### Consumers Gender  

Create a quick view to understand the relationship between gender and sales.

```{r}
# create a dataset filtered by User_ID
Consumer_Genders <- cleaned_sales %>%
  select(User_ID, Gender) %>%
  group_by(User_ID) %>%
  distinct()

summary(Consumer_Genders$Gender)
```
![alt text]( https://github.com/smichael14/MSDS-696-Regis-University/blob/master/Images/4%20m%20f%20plot.GIF)

As you can see, males represent 72% of the total sales.

```{r}
# barplot of consumers vs gender
 ggplot(data = Consumer_Genders, aes(x = Gender, y = ..count.., fill= Gender)) +
                geom_bar() +
                labs(subtitle = "Unique Consumers by Gender",
                caption="Black Friady Dataset",
                y = "Consumer Count",
                x = "Gender",
                title = "Bar Chart")
```


Creating a view to see with products have the most sales

```{r}
# create dataset that groups by Product_ID and then counts unique instances
order_products <- cleaned_sales %>% 
  group_by(Product_ID) %>% 
  summarise(count=n()) %>% 
  arrange(desc(count))

top_10_Products <- as.data.frame(head(order_products, 10))
top_10_Products
```

#### Top 10 products sold

```{r}
# plot showing the top 10 items sold 
ggplot(data = top_10_Products) +
  aes(x = reorder(Product_ID, count), fill = Product_ID, weight = count) +
  geom_bar() +
  scale_fill_viridis_d(option  = "viridis") +
  labs(title = 'Top 10 Products',
    y = 'Sales Count',
    x = 'Product ID',
    caption = 'Black Friday Dataset',
    subtitle = 'Most Popular Products') +
  coord_flip()
```

![alt text](https://github.com/smichael14/MSDS-696-Regis-University/blob/master/Images/5%20top%20plot.GIF)

#### Purchases by Age

```{r}
# create dataset that groups consumers by age
consumer_age <- cleaned_sales %>% 
  group_by(Age) %>%
  summarise(count = n()) %>% 
  arrange(desc(count))

# create a plot to illustrates Age vs Purchases
 ggplot(consumer_age, aes(x = reorder(Age, count), y= count, fill=Age)) +
                geom_bar(stat = "identity") +
                labs(subtitle = "Unique Consumers by Age",
                caption="Black Friady Dataset",
                y = "Consumer Count",
                x = "Age Groups",
                title = "Bar Chart")
```
![alt text]( https://github.com/smichael14/MSDS-696-Regis-University/blob/master/Images/6%20age%20purchase.GIF)

#### Puchases by City Category

```{r}
# create dataset that groups by City_Category
consumer_city <- cleaned_sales %>% 
  group_by(City_Category) %>%
  summarise(count = n()) %>% 
  arrange(desc(count))

# create a plot to illustrates Consumers City vs Purchases
 ggplot(consumer_city, aes(x = reorder(City_Category, count), y= count, fill=City_Category)) +
                geom_bar(stat = "identity") +
                labs(subtitle = "Unique Consumers by City Category",
                caption="Black Friady Dataset",
                y = "Consumer Count",
                x = "City Category",
                title = "Bar Chart")
```
![alt text]( https://github.com/smichael14/MSDS-696-Regis-University/blob/master/Images/7%20purchase%20cit.GIF)

#### Purchases per Occupation  

```{r}
# dataset that groups consumers by Occupation
consumer_city <- cleaned_sales %>% 
  group_by(Occupation) %>%
  summarise(count = n()) %>% 
  arrange(desc(count))

# create a plot to illustrates Consumers City vs Purchases
 ggplot(consumer_city, aes(x = reorder(Occupation, count), y= count, fill=Occupation)) +
                geom_bar(stat = "identity") +
                scale_fill_viridis_d(option  = "plasma") +
                labs(subtitle = "Unique Consumers by Occupation",
                caption="Black Friady Dataset",
                y = "Consumer Count",
                x = "Occupation Category",
                title = "Bar Chart")
```
![alt text]( https://github.com/smichael14/MSDS-696-Regis-University/blob/master/Images/8%20purchase%20occupation.GIF)

#### Stay in Current City vs Purchase  

```{r}
# create dataset of consumers vs time in current city
consumer_Stay_In_Current_City_Years <- cleaned_sales %>% 
  group_by(Stay_In_Current_City_Years) %>%
  summarise(count = n()) %>% 
  arrange(desc(count))

# create a plot to illustrates Consumers City vs Purchases
 ggplot(consumer_Stay_In_Current_City_Years, aes(x = reorder(Stay_In_Current_City_Years, count),
                                                 y= count, fill=Stay_In_Current_City_Years)) +
                geom_bar(stat = "identity") +
                labs(subtitle = "Unique Consumers by Stay In Current City Years",
                caption="Black Friady Dataset",
                y = "Consumer Count",
                x = "Stay In Current City Years",
                title = "Bar Chart")
```
![alt text]( https://github.com/smichael14/MSDS-696-Regis-University/blob/master/Images/9%20purchase%20city%20stay.GIF)

#### Multi-Dimensional Plotting

```{r}
# 3D plot illustrating Gender, Age vs purchases 
ggplot(data = cleaned_sales) +
  aes(x = Age, y = Purchase, fill = Gender) +
  geom_boxplot() +
  labs(title = "Box Plot",
    caption="Black Friady Dataset",
    subtitle = "Age, Purchase, Gender") +
  theme_minimal()
```
![alt text]( https://github.com/smichael14/MSDS-696-Regis-University/blob/master/Images/10%20box%20plot%20age.GIF)

```{r}
# create a plot to illustrates Consumers City vs Age vs Gender vs Purchases
ggplot(data = cleaned_sales) +
  aes(x = Gender, y = Purchase, fill = City_Category) +
  geom_boxplot() +
  labs(title = "Facet Wrap - Purchases",
    caption = "Black Friday Dataset",
    subtitle = "Purchase Vs Age Vs Gender Vs City Category") +
  theme_minimal() +
  facet_wrap(vars(Age))
```
![alt text]( https://github.com/smichael14/MSDS-696-Regis-University/blob/master/Images/11%20facet%20wrap%20gender.GIF)


```{r}
# bar plot that illustrates 4 data points
ggplot(data = cleaned_sales) +
  aes(x = Age, fill = City_Category, weight = Purchase) +
  geom_bar() +
  scale_fill_viridis_d(option  = "viridis") +
  labs(title = "Bar Plot",
    caption="Black Friady Dataset",
    y = "Purchase",
    subtitle = "Age, Purchase, Gender and City Category")+
  facet_wrap(vars(Gender))
```
![alt text]( https://github.com/smichael14/MSDS-696-Regis-University/blob/master/Images/12%20age%20purchase%20gender%20city.GIF)

```{r}
ggplot(data = cleaned_sales) +
  aes(x = Age, y = Purchase, fill = Age) +
  geom_violin(scale = "area", adjust = 1) +
  scale_fill_brewer(palette = "RdBu") +
  labs(title = "Facet Plot by Gender",
    x = "Age",
    y = "Purchases",
    caption = "Black Friday Dataset",
    subtitle = "Age vs Purchases ") +
    facet_wrap(vars(Gender))
```
![alt text]( https://github.com/smichael14/MSDS-696-Regis-University/blob/master/Images/13%20facet%20wrap%20gender.GIF)

```{r}
ggplot(data = cleaned_sales) +
  aes(x = Occupation, fill = Age, size = Purchase) +
  geom_bar(position = "fill") +
  scale_fill_viridis_d(option  = "plasma") +
  labs(title = "Normalized Occupation vs Sales",
    caption = "Black Friday Dataset") +
  theme(legend.position = 'left')
```
![alt text]( https://github.com/smichael14/MSDS-696-Regis-University/blob/master/Images/14%20Normalized.GIF)

# Data Modeling

```{r}
# Prepare the nested dataframe gap_nested
sales_nested <- cleaned_sales %>% 
  group_by(Product_ID) %>% 
  select(-User_ID, -Product_Category_2, - Product_Category_3) %>%
  nest()

# view the nested dataset
head(sales_nested)
```

### Top 5 products

```{r}
top_nested <- sales_nested %>%
  filter(Product_ID %in% c("P00265242",
                           "P00110742",
                           "P00025442",
                           "P00112142",
                           "P00057642"))
```

## Linear Regression Model on the top 5 products

```{r}
# Build a linear model for each product
sales_models <- top_nested %>%
    mutate(model = map(data, ~lm(Purchase~., data = .x)))
    
# Extract the model for the first product   
P002652422_model <- sales_models$model[[1]]

# View the summary for the first product
summary(P002652422_model)
```

Viewing the statistical analysis of the model, through tidy.  These values are the coefficients that are extracted from the model summary. 


```{r}
# view the coefficients of the model
tidy(P002652422_model)
```
 
Glance extracts the statistics of a model and places them in a useable data frame.

```{r}
# view the statistical values
glance(P002652422_model)
```

Augment from the Broom Toolkit sums prediction columns to the dataset that is being modeled. I am building an observational data frame, which contains data to build the model and the predicted model for each observation in the column .fitted.   

```{r}
#
head(augment(P002652422_model))
```


## Build Model for the complete dataset
![alt text]( https://github.com/smichael14/MSDS-696-Regis-University/blob/master/Images/18.GIF)

![alt text]( https://github.com/smichael14/MSDS-696-Regis-University/blob/master/Images/19.GIF)

```{r}
# removing unique values for model creation
cleaned_model <- cleaned_sales %>%
  select(-User_ID, -Product_ID,-Product_Category_2, -Product_Category_3)

# setting seed so I can reproduce outcome
set.seed(55)

# created model split of 75%
sales_split <- initial_split(cleaned_model, prop = 0.75)

# prepare train and test data
train_sales <- training(sales_split)
test_sales <- testing(sales_split)

# view data split
nrow(train_sales)
nrow(test_sales)
```
The output above is to demonstrate the split of data.  

Below 

```{r}
# train and vaidate performance with the train data
# cross validation, v= number of splits
cross_validation_split <- vfold_cv(train_sales, v = 3)
cross_validation_split
```
```{r}
# mapping train & validate
cross_validation_data <- cross_validation_split %>%
  mutate(train = map(splits, ~training(.x)),
         validate = map(splits, ~testing(.x)))
```

## Creating Multi-featured model

Using the train cross validation data to build the multi feature linear model. 


```{r}
cross_validation_models_lm <- cross_validation_data %>%
  mutate(model = map(train, ~lm(formula = Purchase~., data = .x)))

# displaying the lm model
cross_validation_models_lm
```

3 split lists have been created.

### Validation and performance of models

I need to add the actual and predicted columns to the model.

```{r}
# prepared  actual and predicted values for comparison  
cross_validation_prep_lm <- cross_validation_models_lm %>%
  mutate(validate_actual = map(validate, ~.x$Purchase),
         validate_predicted = map2(model, validate, ~predict(.x, .y)))
cross_validation_prep_lm
```

```{r}
# MAE calculation
# map2 will produce a double vector, instead of a list
cross_validation_eval_lm <- cross_validation_prep_lm %>%
  mutate(validate_mae = map2_dbl(validate_actual, validate_predicted,
                                 ~mae(actual = .x, predicted = .y)))

# calculate the mean for all observations
mean(cross_validation_eval_lm$validate_mae)
```

## Decision Trees  

In the code below, I created models for each of the observations in the dataset.

```{r}
# create model using the ranger library
cv_models_rf <- cross_validation_data %>%
  mutate(model = map(train, ~ranger(formula = Purchase~.,
                                   data = .x, seed = 55)))

# Generate predictions using the random forest model
cv_prep_rf <- cv_models_rf %>% 
  mutate(validate_actual = map(validate, ~.x$Purchase),
         validate_predicted = map2(.x = model, .y = validate, ~predict(.x, .y)$predictions))
```

Map2 iterates over all of the folds and mtry parameters, to build a new ranger models for each fold and mtry combinations.

```{r}
# Calculate validate MAE for each fold and mtry combination
cv_eval_rf <- cv_prep_rf %>% 
  mutate(validate_mae = map2_dbl(.x = validate_actual, .y = validate_predicted, ~mae(actual = .x, predicted = .y)))
```




```{r}
# Generate validate predictions for each model
cv_prep_rf <- cv_models_rf %>%
  mutate(validate_predicted = map2(model, validate,
                                   ~predict(.x, .y)$predictions))

# Calculate the mean of validate_mae column
mean(cv_eval_rf$validate_mae)
```

### Tuning the tree to optimize the results


```{r}
# Prepare for tuning your cross-validation folds by varying mtry
cross_validation_tune <- cross_validation_data %>%
  crossing(mtry = 2:5)
```

```{r}
# add num.trees = ### after data
cross_validation_tune_rf <- cross_validation_tune %>%
  mutate(model = map2(train, mtry, ~ranger(formula = Purchase~.,
                                           data = .x, mtry = .y,
                                           num.trees = 100,
                                           seed = 55)))
```

```{r}
# Generate validate predictions for each model
cv_prep_tunerf <- cross_validation_tune_rf %>% 
  mutate(validate_actual = map(validate, ~.x$Purchase),
         validate_predicted = map2(.x = model, .y = validate, ~predict(.x, .y)$predictions))

```

```{r}
# Calculate validate MAE for each fold and mtry combination
cross_validation_eval_tune_rf <- cv_prep_tunerf %>% 
  mutate(validate_mae = map2_dbl(.x = validate_actual, .y = validate_predicted, ~mae(actual = .x, predicted = .y)))
cross_validation_tune_rf
```

```{r}
# Calculate the mean validate_mae for each mtry used  
cross_validation_eval_tune_rf %>% 
  group_by(mtry) %>% 
  summarise(mean_mae = mean(validate_mae))
```


# Measuring the Test Performance
Step one in the process was splitting my data into two main sections, train and test.  In this case, I intentionally left out the test portion in order to evaluate the final model with an independent set of data. 

I used the train data and split it into iterative sections of Train and Validate so I was able to cross validate the data for a more accurate means of model selection.

Each train interaction was leveraged to build a model, and I held out the validate portion so I could validate my iterative model.

The results created a means of evaluating validation performance for every cross-validation fold on each model.  I could then aggregate validation performance for each model to determine which model was truly performing the best overall. 


```{r}
# building the best model
best_model <- ranger(formula = Purchase~.,
                     data =  train_sales,
                     mtry = 4,
                     num.trees = 100,
                     seed = 55)
best_model
```
### Using the Test data on our Model

Every train portion was used to build a model, while the validation portion of the train dataset was used in validation. This affords the ability to measure the validation performance on each of the cross-validation folds, for each model, and associated hyperparameter. Combining the validation performance for each model, allowed for me to compare multiple models, as well as their individual hyperparameters. This provided insight to identify the model-hyperparameter combination, with the best overall performance.

```{r}
# prepare the actual and predicted values to compare
test_actual <- test_sales$Purchase
test_predict <- predict(best_model, test_sales)$predictions
```

### Model Performance on New Data

```{r}
# compare mean absolute error
mae(test_actual, test_predict)
```
![alt text]( https://github.com/smichael14/MSDS-696-Regis-University/blob/master/Images/20.GIF)

