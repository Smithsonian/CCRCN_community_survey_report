
# This document was started on 3-13-2018 to gain practice with ggplot2 and R Markdown to help 
# James Holmquist at SERC with the Coastal Carbon RCN December 2017 Community Survey.


# Turn on necessary libraries for this project.
library(dbplyr)
library(ggplot2)
library(rmarkdown)
library(tidyverse)

# Set the working directory to a common file path.
setwd("F:/SERC Files/Jim Project/Coastal Carbon Survey")
getwd() # Call the working directory just to double check!

# Upload the dataset into a tibble usig read_csv
library(readr)
Survey_Responses <- read_csv("F:/SERC Files/Jim Project/CCRCN_Dec2017_Survey_Reponses.csv")
View(Survey_Responses)

# Use the ggplot() function and bind the plot to a specific data frame using the
# data argument
ggplot(data = Survey_Responses)

# Change the column names to something easier to call in R; spaces are tricky with names.
names(Survey_Responses) <- c("Timestamp", "Email", "Name", "Institution", "Title_Role",
                            "Spatial_Scale", "Career_milestone", "Work_location", "Data_synthesis",
                            "Data_access", "Make_public?", "Network_support", "Data_kind",
                            "Motivation", "Training_type", "Workshop_interest", "Workshop_idea",
                            "Workshop_worked", "Workshop_avoid", "CWCRP_5", "Feedback_concerns")

# Make a simple bar chart with counts of each Title_Role.
ggplot(data = Survey_Responses, aes(Title_Role)) +
  geom_bar()

# It appears that there are quite a few unique values, though maybe some of them could be condensed.
# Let's write a function to condense the similar responses into a single identifier, we'll start with students.
student = grepl("Student", Survey_Responses$Title_Role)
students = as.character(student)
Survey_Responses$Student = students
Survey_Responses$Title_Role2 = if_else(Survey_Responses$Student=="TRUE", "Student", Survey_Responses$Title_Role)
# No we'll cluster Land and Program Manager 
manager = grepl("Manager", Survey_Responses$Title_Role)
managers = as.character(manager)
Survey_Responses$Manager = managers
Survey_Responses$Title_Role3 = if_else(Survey_Responses$Manager=="TRUE", "Land/Program Manager", Survey_Responses$Title_Role2)
# Now we'll cluster Policy Experts
policy = grepl("Policy", Survey_Responses$Title_Role)
policys = as.character(policy)
Survey_Responses$Policy = policys
Survey_Responses$Title_Role4 = if_else(Survey_Responses$Policy=="TRUE", "Policy Expert", Survey_Responses$Title_Role3)

# Let's check to see how we're doing!
ggplot(data = Survey_Responses, aes(Title_Role4)) +
  geom_bar()
# Not too bad, the graph already looks cleaner! But we can do better still. 
# Now lets see if we can make it into only 4 categories by putting everything else into an other category.
# We can get rid of student as this question is asked later on in the survey.

final = grepl("Land|Poli|Scie", Survey_Responses$Title_Role4)
Final = as.character(final)
Survey_Responses$Final = Final
Survey_Responses$Title_Role5 = if_else(Survey_Responses$Final=="TRUE", Survey_Responses$Title_Role4, "Other")

# Let's see how it looks now!
ggplot(data = Survey_Responses, aes(Title_Role5)) +
  geom_bar()
# This is great, it shows us that the majority of participants are scientists, with a few other
# important titles, and an other category for the odd-balls.

# Let's look at some of the other variables from this survey and determine what might need some work.
ggplot(data = Survey_Responses, aes(Spatial_Scale)) + # What size of experimental units are we looking at?
  geom_bar()
ggplot(data = Survey_Responses, aes(Career_milestone)) + # Where are you at in your career?
  geom_bar()
ggplot(data = Survey_Responses, aes(Work_location)) + # US work or no? 
  geom_bar()
