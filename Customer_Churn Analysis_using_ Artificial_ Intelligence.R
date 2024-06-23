#checking missing data
sum(is.na(Churn_Dataset))

library(dplyr)


#removing irrelevant variables
churn <- Churn_Dataset %>%
  select(-c(RowNumber, CustomerId, Surname)) %>%
  mutate(
    HasCrCard = as.factor(HasCrCard),
    IsActiveMember = as.factor(IsActiveMember)
  )

#summary statistics
summary(churn)

#Numeric Data:Age,Tenure and Credit Score
#boxplot of Age of Customers
boxplot(churn$Age, main = 'Boxplot of Age of Customers', ylab = 'Age')

#boxplot of Credit Score
boxplot(churn$CreditScore, main = 'Boxplot of Credit Score of Customers', ylab = 'CreditScore')


# Boxplot of Tenure of Customers
boxplot(churn$Tenure, main = 'Boxplot of Tenure of Customers', ylab = 'Tenure')


#Visualizing Categorical Data

library(lattice)
histogram(~ Age | Geography, data = churn)

# Reset graphical parameters to default
par(mfrow = c(1, 1))  # Reset layout to single plot

#Visualizing Categorical Data
#Visualizing Gender and Geography Exited
# Calculate counts for each combination of Gender and Geography
counts <- table(churn$Gender, churn$Geography)
# Define colors for each Gender category
colors <- c("blue", "pink")  # Blue for Male, Pink for Female
# Create bar plot
barplot(counts, 
        beside = TRUE,          # Stacks bars beside each other
        col = colors,           # Assign colors
        legend.text = TRUE,     # Show legend
        main = "Bar Chart of Gender vs Geography",  # Main title
        xlab = "Gender",        # X-axis label
        ylab = "Count")         # Y-axis label
# Add legend with empty fill for Geography
legend("topright", 
       legend = unique(churn$Geography),  # Legend labels
       fill = c("white", "white", "white"))  # Legend colors (white for no fill)


#Visualizing Gender versus Exited
exit_counts <- table(churn$Gender[churn$Exited == 1])
# Define colors for the bars
bar_colors <- c("pink", "blue")  # Assuming two genders: Male and Female
# Create bar plot with customized width for each group
barplot(exit_counts, 
        col = bar_colors,             # Assign colors to bars
        legend.text = TRUE,           # Show legend
        main = "Customers Exited vs Gender",  # Main title
        xlab = "Gender",              # X-axis label
        ylab = "Number of Customers", # Y-axis label
        ylim = c(0, max(exit_counts) * 1.4),  # Set y-axis limits
        width = c(0.02, 0.02))          # Width for each bar (adjust as needed)
# Add legend
legend("topright", 
       legend = c("Female", "Male"),  # Legend labels based on Gender categories
       fill = bar_colors)             # Legend colors

#Visualizing Exited Versus IsActiveMember
# Calculate counts of exited customers by IsActiveMember status
exit_counts <- table(churn$Exited, churn$IsActiveMember)

# Define colors for the bars
bar_colors <- c("yellow", "green")  # Pink for IsActiveMember = 0 (No), Blue for IsActiveMember = 1 (Yes)
# Create bar plot
barplot(exit_counts, 
        col = bar_colors,             # Assign colors to bars
        legend.text = TRUE,           # Show legend
        main = "Customers Exited vs IsActiveMember",  # Main title
        xlab = "Exited",              # X-axis label (Exited)
        ylab = "Number of Customers", # Y-axis label
        ylim = c(0, max(exit_counts) * 1.2),  # Set y-axis limits
        names.arg = c("No", "Yes"),   # Labels for Exited categories
        beside = TRUE)                # Stacks bars beside each other
# Add legend
legend("topright", 
       legend = c("Not Active", "Active"),  # Legend labels based on IsActiveMember categories
       fill = bar_colors)                  # Legend colors


#Visualizing HasCrCard Versus Exited
# Calculate counts of exited customers by HasCrCard status
exit_counts <- table(churn$Exited, churn$HasCrCard)
# Define colors for the bars
bar_colors <- c("light blue", "brown")  # Pink for HasCrCard = 0 (No), Blue for HasCrCard = 1 (Yes)
# Create bar plot
barplot(exit_counts, 
        col = bar_colors,             # Assign colors to bars
        legend.text = TRUE,           # Show legend
        main = "Customers Exited vs HasCrCard",  # Main title
        xlab = "Exited",              # X-axis label (Exited)
        ylab = "Number of Customers", # Y-axis label
        ylim = c(0, max(exit_counts) * 1.2),  # Set y-axis limits
        names.arg = c("No", "Yes"),   # Labels for HasCrCard categories
        beside = TRUE)                # Stacks bars beside each other
# Add legend
legend("topright", 
       legend = c("No Credit Card", "Has Credit Card"),  # Legend labels based on HasCrCard categories
       fill = bar_colors)             # Legend colors

#Visualizing Customers Exited using pie chart
# Calculate counts of customers exited and retained
ch <- table(churn$Exited)
# Calculate percentages
pct <- round(ch / sum(ch) * 100)
# Labels for pie chart
lbls <- paste(pct, "%")
# Create pie chart
pie(ch,
    labels = lbls,
    main = "Percentage of Customers Exited and Retained\n[0: Retained and 1: Customers Exited]")

#Visualizing IsActiveMember in a pie chart
# Calculate counts of active and inactive customers
am_counts <- table(churn$IsActiveMember)
# Calculate percentages
pct <- round(am_counts / sum(am_counts) * 100)
# Labels for pie chart
lbls <- paste(pct, "%")
# Create pie chart
pie(am_counts,
    labels = lbls,
    main = "Percentage of Active and Inactive Members\n[0: Inactive and 1: Active]")

#Visualizing HasCrCard in a pie chart
# Calculate counts of customers with and without a credit card
cc_counts <- table(churn$HasCrCard)
# Calculate percentages
pct <- round(cc_counts / sum(cc_counts) * 100)
# Labels for pie chart
lbls <- paste(pct, "%")
# Create pie chart
pie(cc_counts,
    labels = lbls,
    main = "Percentage of Customers with and without Credit Card\n[0: No, 1: Yes]")

#Visualizing Continous Variables;Balance and EstimatedSalary
#Histogram of Balance Distribution
hist(churn$Balance,
     main = "Histogram of Balance Distribution",
     xlab = "Balance")

#histogram of Estimated Salary distribution
hist(churn$EstimatedSalary,
     main = "Histogram of Estimated Salary Distribution",
     xlab = "Estimated Salary")

#Visualizing Numeric Variables versus Categorical Variables

#Visualizing NumofProducts versus Exited
# Subset data for customers who exited and those who did not
exited <- churn$NumOfProducts[churn$Exited == 1]
not_exited <- churn$NumOfProducts[churn$Exited == 0]

# Combine data into a list for boxplot
data <- list("Exited" = exited, "Not Exited" = not_exited)

# Create boxplot
boxplot(data,
        col = c("yellow", "green"),  # Colors for the boxes
        names = c("Exited", "Not Exited"),  # Group labels
        main = "NumOfProducts vs Exited",  # Main title
        xlab = "Exited",  # X-axis label
        ylab = "NumOfProducts")  # Y-axis label

#Visualizing Age versus Exited
# Subset data for customers who exited and those who did not
exited <- churn$Age[churn$Exited == 1]
not_exited <- churn$Age[churn$Exited == 0]

# Combine data into a list for boxplot
data <- list("Exited" = exited, "Not Exited" = not_exited)

# Create side-by-side boxplot
par(mar = c(5, 5, 4, 2) + 0.1)  # Adjust margins if needed
boxplot(data,
        col = c("yellow", "green"),  # Colors for the boxes
        names = c("Exited", "Not Exited"),  # Group labels
        main = "Age vs Exited",  # Main title
        xlab = "Exited",  # X-axis label
        ylab = "Age",  # Y-axis label
        border = "black")  # Border color

#Visualizing Continous  Variables versus Categorical Variables

#Visualizing EstimatedSalary versus Exited
# Subset data for customers who exited and those who did not
exited <- churn$EstimatedSalary[churn$Exited == 1]
not_exited <- churn$EstimatedSalary[churn$Exited == 0]
# Combine data into a list for boxplot
data <- list("Exited" = exited, "Not Exited" = not_exited)
# Create side-by-side boxplot
par(mar = c(5, 5, 4, 2) + 0.1)  # Adjust margins if needed
boxplot(data,
        col = c("yellow", "green"),  # Colors for the boxes
        names = c("Exited", "Not Exited"),  # Group labels
        main = "Estimated Salary vs Exited",  # Main title
        xlab = "Exited",  # X-axis label
        ylab = "Estimated Salary",  # Y-axis label
        border = "black")  # Border color

#Visualizing Balance versus Exited
# Subset data for customers who exited and those who did not
exited <- churn$Balance[churn$Exited == 1]
not_exited <- churn$Balance[churn$Exited == 0]
# Combine data into a list for boxplot
data <- list("Exited" = exited, "Not Exited" = not_exited)
# Create side-by-side boxplot
par(mar = c(5, 5, 4, 2) + 0.1)  # Adjust margins if needed
boxplot(data,
        col = c("yellow", "green"),  # Colors for the boxes
        names = c("Exited", "Not Exited"),  # Group labels
        main = "Balance vs Exited",  # Main title
        xlab = "Exited",  # X-axis label
        ylab = "Balance",  # Y-axis label
        border = "black")  # Border color


#Building Machine Language Model,Decision Tree

set.seed(456)
train_sample <- sample(10000, 9000)
churn_train <- churn[train_sample, ]#training data
churn_test <- churn[-train_sample, ] #test data


#check the proportion of class variable
prop.table(summary(churn_train$Exited))
prop.table(summary(churn_test$Exited))

# Convert 'Exited' to a factor
churn_train$Exited <- factor(churn_train$Exited)

#Training a model on the data

# Load the C50 package (if not already loaded)
library(C50)

#C50(train_predicators(x),train_target(y))trains the C5.0 decision tree
#churn_train is your training dataset
# Train the C5.0 decision tree model
churn_model <- C5.0(churn_train[-11], churn_train$Exited,
                    control = C5.0Control(minCases = 400))

#display simple facts about the tree
churn_model

#plot the tree
plot(churn_model)

#Evaluatingmodel performance with the test
#create a factor vector of predictions on test data
churn_pred<-predict(churn_model,churn_test)


#cross tabulation of predicted versus actual classes
install.packages("gmodels")
library(gmodels)


library(gmodels)  # Load the gmodels package if not already loaded

# Example CrossTable function call with correct parameter placement
CrossTable(churn_test$Exited, churn_pred,
           prop.chisq = FALSE, prop.c = FALSE, PROP.R = FALSE,
           dnn = c('actual Exited', 'predicted Exited'))

