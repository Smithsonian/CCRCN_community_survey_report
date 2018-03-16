
# The purpose of this script is to assist James Holmquist of the Smithsonian Environmental
# Research Center in manipulating, cleaning, analyzing and ultimately displaying data 
# received from the Coastal Carbon RCN December 2017 Community Survey. 

    # Started on March 15, 2018 by Christopher Adkison, SERC Biogeochemistry Intern


# First turn on the necessary libraries to use this script.
library(rmarkdown)
library(tidyverse)

# Upload the dataset into a tibble usig read_csv
Survey_Responses <- read_csv("data/CCRCN_survey_results_PII_scrubbed_180316.csv")
View(Survey_Responses)

# Change the column names to something easier to call in R; spaces are tricky with names.
# This step in particular is important because the google doc csv output uses the questions as 
# column names. These are far too long and need to be changed into something easier to use.
names(Survey_Responses) <- c("Timestamp", "Institution", "Title_Role",
                             "Spatial_Scale", "Career_milestone", "Work_location", "Data_synthesis",
                             "Data_access", "Make_public?", "Network_support", "Data_kind",
                             "Motivation", "Training_type", "Workshop_interest", "Workshop_idea",
                             "Workshop_worked", "Workshop_avoid", "CWCRP_5", "Feedback_concerns")


#*************************************** Section 1 Questions ***********************************

#1. # Which best describes your role in coastal wetland carbon science?
      # This question has 4 answers, however, the last answer choice as "other" is
      # a text field where respondents can use their own title. This is useful information
      # to have, but can become overwhelming for analysis. Using the grepl function we will
      # rename all data entries back into one of the original 4 categories and input
      # these new reformated names into a new column for simplicity's sake.

# Let's write a function to condense the similar responses into a single identifier, we'll start with scientists.
question_1_response <- Survey_Responses %>% 
  mutate(scientist = grepl("Scientist", Title_Role),
         landProgramManager = grepl("Manager", Title_Role),
         policyExpert = grepl("Policy", Title_Role), 
         other = if_else(scientist == F & landProgramManager == F & policyExpert == F , T, F)) %>%
  group_by(scientist, landProgramManager, policyExpert, other) %>% 
  tally()

scientist_count <- sum(question_1_response$n[question_1_response$scientist == T])
manager_count <- sum(question_1_response$n[question_1_response$landProgramManager == T])
policy_count <-  sum(question_1_response$n[question_1_response$policyExpert == T])
other_count <- sum(question_1_response$n[question_1_response$other == T])

question_1_response_b <- data.frame(Title_Role = c("Scientist", "Manager", "Policy", "other"),
           count = c(scientist_count, manager_count, policy_count, other_count),
           ranks = 1:4)

question_1_lollipop <- ggplot(question_1_response_b, aes(x=ranks, y = count)) +
  geom_point(size=3) +
  geom_segment(aes(x=ranks,
                   xend=ranks,
                   y=0,
                   yend=count)) +
  xlab("Title") +
  scale_x_discrete(name = NULL, limits=as.character(question_1_response_b$Title_Role)) +
  theme_gray()



# Let's see how it looks now!
ggplot(data = question_1_response_b, aes(Title_Role)) +
  geom()
table(Survey_Responses$Title_Role) +



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
