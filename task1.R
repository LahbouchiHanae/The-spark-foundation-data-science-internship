#create the dataframe
Hours <- c(2.5, 5.1, 3.2, 8.5, 3.5, 1.5, 9.2, 5.5, 8.3, 2.7, 7.7, 5.9, 4.5, 3.3, 1.1, 8.9, 2.5, 1.9, 6.1, 7.4, 2.7, 4.8, 3.8, 6.9, 7.8)
Scores <- c(21, 47, 27, 75, 30, 20, 88, 60, 81, 25, 85, 62, 41, 42, 17, 95, 30, 24, 67, 69, 30, 54, 35, 76, 86)

student_scores <- data.frame(Hours, Scores)

#print student_scores
student_scores

#loading required libraries
library("ggplot2")
library("dplyr")
library("tibble")

#plotting Scores vs. Hours with linear trend line
ggplot(student_scores, aes(Hours, Scores))+
  geom_point()+
  geom_smooth(
    method = "lm",
    se = FALSE
  )

#fit linear regression to student_scores data frame
mdl_scores_predict <- lm(Scores ~ Hours, data = student_scores)

#Show details of the model
summary(mdl_scores_predict)

# Create a tibble with number of hours = 9.25
explanatory_data <- tibble(
  Hours = 9.25
)
#store predictions in prediction_data variable
prediction_data = explanatory_data %>%
  mutate(
    Scores = predict(mdl_scores_predict, explanatory_data)
  )
#show results
prediction_data

sprintf("A student who studies 9.25 hours/day is estimated to score %f",prediction_data[2])

#Model performance
sprintf("the modele performance is : %.2f%%", summary(mdl_scores_predict)$adj.r.squared*100)
