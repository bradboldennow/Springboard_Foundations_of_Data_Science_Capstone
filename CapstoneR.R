raw.data <- read.csv("rawTelco.csv")
View(raw.data)
str(raw.data)
summary(raw.data)

library(ggplot2)
library(dplyr)
library(tidyr)
library(caTools)

# Check class of senior citizen variable
class(raw.data$SeniorCitizen)
# Because it is an integer, we will transform this variable to factor for better analysis
raw.data$SeniorCitizen = as.factor(raw.data$SeniorCitizen)
class(raw.data$SeniorCitizen)
levels(raw.data$SeniorCitizen)

# Add binary churn column
raw.data = raw.data %>% mutate(churn_yes = ifelse(raw.data$Churn == "Yes", 1, 0))
# Add binary columns for services for calculations
raw.data = raw.data %>% mutate(phoneservice_yes = ifelse(raw.data$PhoneService == "Yes", 1, 0))
raw.data = raw.data %>% mutate(multiplelines_yes = ifelse(raw.data$MultipleLines == "Yes", 1, 0))
raw.data = raw.data %>% mutate(internetservice_yes = ifelse(raw.data$InternetService == "No", 0, 1)) # Different ifelse statement because values are not yes/no
raw.data = raw.data %>% mutate(onlinesecurity_yes = ifelse(raw.data$OnlineSecurity == "Yes", 1, 0))
raw.data = raw.data %>% mutate(onlinebackup_yes = ifelse(raw.data$OnlineBackup == "Yes", 1, 0))
raw.data = raw.data %>% mutate(deviceprotection_yes = ifelse(raw.data$DeviceProtection == "Yes", 1, 0))
raw.data = raw.data %>% mutate(techsupport_yes = ifelse(raw.data$TechSupport == "Yes", 1, 0))
raw.data = raw.data %>% mutate(streamingtv_yes = ifelse(raw.data$StreamingTV == "Yes", 1, 0))
raw.data = raw.data %>% mutate(streamingmovies_yes = ifelse(raw.data$StreamingMovies == "Yes", 1, 0))
raw.data = raw.data %>% mutate(contract_bin = ifelse(raw.data$Contract == "Month-to-month", 1, 
                                                     ifelse(raw.data$Contract == "One year", 2, 3)))


# Add a column with each customers total number of services
raw.data = raw.data %>% mutate(sum_services = phoneservice_yes + multiplelines_yes + internetservice_yes + onlinesecurity_yes + onlinebackup_yes +deviceprotection_yes + techsupport_yes + streamingtv_yes + streamingmovies_yes)

# Add a column with each customers total number of core services (phone and internet)
raw.data = raw.data %>% mutate(sum_core = phoneservice_yes + internetservice_yes)

# There are 11 observations that are missing the total charges. This could be a human error or perhaps the 
# customer did not pay their bill. 
raw.data[is.na(raw.data$TotalCharges), ]
# After examining the observations with missing charges, it is not clear as to why these values are missing.
# Because of this, we will omit these records from our analysis.
raw.data = raw.data[complete.cases(raw.data),]

# Bin the monthly charges for analysis
raw.data = raw.data %>% mutate(month_bin = ifelse(MonthlyCharges <= 33.25, 1,
                                       ifelse(MonthlyCharges <= 48.25, 2,
                                              ifelse(MonthlyCharges <= 63.25, 3,
                                                     ifelse(MonthlyCharges <= 78.25, 4,
                                                            ifelse(MonthlyCharges <= 93.25, 5,
                                                                  ifelse(MonthlyCharges <= 108.25, 6,
                                                                         ifelse(MonthlyCharges > 108.25, 7, 0))))))))
# Bin the tenure for analysis                                                                                                                                                   ifelse(MonthlyCharges < 120, 7))))))))
raw.data = raw.data %>% mutate(tenure_bin = ifelse(tenure <= 9, 1,
                                                   ifelse(tenure <= 18, 2,
                                                          ifelse(tenure <= 27, 3,
                                                                 ifelse(tenure <= 36, 4,
                                                                        ifelse(tenure <= 45, 5,
                                                                               ifelse(tenure <= 54, 6,
                                                                                      ifelse(tenure <= 63, 7,
                                                                                          ifelse(tenure <= 72, 8, 0)))))))))

# Create data frames separating the churners from non-churners for analysis.
churn_no = raw.data[raw.data$Churn == "No", ]
churn_yes = raw.data[raw.data$Churn == "Yes", ]
summary(churn_no)
summary(churn_yes)

# Box plot depticing tenure for each level of churn
ggplot(raw.data, aes(x = Churn, y = tenure)) +
  geom_boxplot()

# Proportion tables with the prop of contract types for churners and non-churners
prop.table(table(churn_no$Contract))
prop.table(table(churn_yes$Contract))
prop.table(table(raw.data$Churn))
# Bar chart depicitng proportion tables above. 
ggplot(raw.data, aes(x = Churn, fill = Contract)) + 
  geom_bar(position = "fill")

# Jitter plot depecting tenrue and monthly charges with level of churn
ggplot(raw.data, aes(x = tenure, y = MonthlyCharges, col = Contract)) +
  geom_jitter(alpha = .4) +
  facet_wrap(~Churn)

# Correlation table for variables
correlation_table = cor(raw.data[,unlist(lapply(raw.data, is.numeric))])

# Plot depicting the tenure of internet serive holders, split up by churn level and colored by contract type.
ggplot(raw.data, aes(x = as.factor(internetservice_yes), y = tenure, col = Contract)) +
  geom_jitter() +
  facet_wrap(~Churn)

#Split data into training and test subsets
set.seed(999) 
split = sample.split(raw.data$Churn, SplitRatio = .65)
train = subset(raw.data, split ==TRUE)
test = subset(raw.data, split == FALSE)

nrow(train)
nrow(test)


#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# First logistic regression model with all original variables included.
glm.1 <- glm(Churn~ gender + SeniorCitizen + Partner + Dependents + tenure + PhoneService + MultipleLines + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod + MonthlyCharges + TotalCharges, 
               data=train,family=binomial)
summary(glm.1)
glm.probs <- predict(glm.1,train,type="response")

glm.pred <- rep("No", nrow(train))
glm.pred[glm.probs > 0.50] = "Yes"   # Defines the treshold probability

table(train$Churn,glm.pred) # Confusion Matrix

# Same model above but using the test data
glm.1test = glm(Churn~ gender + SeniorCitizen + Partner + Dependents + tenure + PhoneService + MultipleLines + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod + MonthlyCharges + TotalCharges, 
                data=test,family=binomial)

summary(glm.1test)
glm.probstest <- predict(glm.1test,test,type="response")

glm.predtest <- rep("No", nrow(test))
glm.predtest[glm.probstest > 0.50] = "Yes"   # Defines the treshold probability

table(test$Churn,glm.predtest) # Confusion Matrix

#------------------------------------------------------------------------------------------------------------
# Second model, removing the variables that do nothave at least one star from above
glm.2 <- glm(Churn~ tenure + MultipleLines + InternetService + Contract + PaperlessBilling
             + PaymentMethod + TotalCharges, 
             data=train,family=binomial)
summary(glm.2)
glm.probs2 <- predict(glm.2,train,type="response")

glm.pred2 <- rep("No", nrow(train))
glm.pred2[glm.probs2 > 0.5] = "Yes"   # Defines the treshold probability

table(train$Churn,glm.pred2) # Confusion Matrix

# Same second model but running with the test data
glm.2test <- glm(Churn~ tenure + MultipleLines + InternetService + Contract + PaperlessBilling
             + PaymentMethod + TotalCharges, 
             data=test,family=binomial)
summary(glm.2test)
glm.probs2test <- predict(glm.2test,test,type="response")

glm.pred2test <- rep("No", nrow(test))
glm.pred2test[glm.probs2test > 0.5] = "Yes"   # Defines the treshold probability

table(test$Churn,glm.pred2test) # Confusion Matrix

#------------------------------------------------------------------------------------------------------------
# Third model, removed variables with lowest significance from the second model. 
glm.3 <- glm(Churn~ tenure + MultipleLines + InternetService + Contract + PaperlessBilling, 
             data=train,family=binomial)
summary(glm.3)
glm.probs3 <- predict(glm.3,train,type="response")

glm.pred3 <- rep("No", nrow(train))
glm.pred3[glm.probs3 > 0.5] = "Yes"   # Defines the treshold probability

table(train$Churn,glm.pred3) # Confusion Matrix

#Same third model, but running with the test data.
glm.3test <- glm(Churn~ tenure + MultipleLines + InternetService + Contract + PaperlessBilling, 
             data=test,family=binomial)
summary(glm.3test)
glm.probs3test <- predict(glm.3test,test,type="response")

glm.pred3test <- rep("No", nrow(test))
glm.pred3test[glm.probs3test > 0.5] = "Yes"   # Defines the treshold probability

table(test$Churn,glm.pred3test) # Confusion Matrix

#-------------------------------------------------------------------------------------------------------------------
