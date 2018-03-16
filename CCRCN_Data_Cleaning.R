
# The purpose of this script is to assist James Holmquist of the Smithsonian Environmental
# Research Center in manipulating, cleaning, analyzing and ultimately displaying data 
# received from the Coastal Carbon RCN December 2017 Community Survey. 

    # Started on March 15, 2018 by Christopher Adkison, SERC Biogeochemistry Intern


# First turn on the necessary libraries to use this script.
library(dbplyr)
library(ggplot2)
library(rmarkdown)
library(tidyverse)
library(readr)

# Then, set the working directory to a common file path.
setwd("F:/SERC Files/Jim Project/Coastal Carbon Survey")
getwd() # Call the working directory just to double check!

# Upload the dataset into a tibble usig read_csv
Survey_Responses <- read_csv("F:/SERC Files/Jim Project/CCRCN_Dec2017_Survey_Reponses.csv")
View(Survey_Responses)

# Use the ggplot() function and bind the plot to a specific data frame using the
# data argument
ggplot(data = Survey_Responses)

# Change the column names to something easier to call in R; spaces are tricky with names.
# This step in particular is important because the google doc csv output uses the questions as 
# column names. These are far too long and need to be changed into something easier to use.
names(Survey_Responses) <- c("Timestamp", "Email", "Name", "Institution", "Title_Role",
                             "Spatial_Scale", "Career_milestone", "Work_location", "Data_synthesis",
                             "Data_access", "Make_public?", "Network_support", "Data_kind",
                             "Motivation", "Training_type", "Workshop_interest", "Workshop_idea",
                             "Workshop_worked", "Workshop_avoid", "CWCRP_5", "Feedback_concerns")


*************************************** Section 1 Questions ***********************************

1. # Which best describes your role in coastal wetland carbon science?
      # This question has 4 answers, however, the last answer choice as "other" is
      # a text field where respondents can use their own title. This is useful information
      # to have, but can become overwhelming for analysis. Using the grepl function we will
      # rename all data entries back into one of the original 4 categories and input
      # these new reformated names into a new column for simplicity's sake.

# Let's write a function to condense the similar responses into a single identifier, we'll start with scientists.
scientist = grepl("Scientist", Survey_Responses$Title_Role)
scientists = as.character(scientist)
Survey_Responses$Student = scientists
Survey_Responses$Title_Role2 = if_else(Survey_Responses$Student=="TRUE", "Scientist", Survey_Responses$Title_Role)
# Now we'll cluster Land and Program Manager 
manager = grepl("Manager", Survey_Responses$Title_Role)
managers = as.character(manager)
Survey_Responses$Manager = managers
Survey_Responses$Title_Role3 = if_else(Survey_Responses$Manager=="TRUE", "Land/Program Manager", Survey_Responses$Title_Role2)
# Now we'll cluster Policy Experts
policy = grepl("Policy", Survey_Responses$Title_Role)
policys = as.character(policy)
Survey_Responses$Policy = policys
Survey_Responses$Title_Role4 = if_else(Survey_Responses$Policy=="TRUE", "Policy Expert", Survey_Responses$Title_Role3)

# Now lets see if we can make it into only 4 categories by putting everything else into an other category.
final = grepl("Land|Poli|Scie", Survey_Responses$Title_Role4)
Final = as.character(final)
Survey_Responses$Final = Final
Survey_Responses$Title_Role5 = if_else(Survey_Responses$Final=="TRUE", Survey_Responses$Title_Role4, "Other")

# Let's see how it looks now!
ggplot(data = Survey_Responses, aes(Title_Role5)) +
  geom_bar()
table(Survey_Responses$Title_Role5)


2. # Which best describes the spatial scale of your work?
      # This question does not have anything too tricky to deal with, so it will 
      # simply be plotted.
ggplot(data = Survey_Responses, aes(Spatial_Scale)) + 
  geom_bar()
table(Survey_Responses$Spatial_Scale)


3. # Where are you in your career?
ggplot(data = Survey_Responses, aes(Career_milestone)) + 
  geom_bar()
table(Survey_Responses$Career_milestone)


4. # I primaril work...
ggplot(data = Survey_Responses, aes(Work_location)) +  
  geom_bar()
table(Survey_Responses$Work_location)


*************************************** Section 2 Questions **********************************

1. # What type of coastal wetland carbon data would you like to see synthesized 
   # and publically available?
      # 
