# example for comma separated strings

library(tidyverse)

input_answers <- c("rat", "rat,dog", "cat", "cat,dog", "dog", "dog", "cat") 
output_answers <- unlist(strsplit(input_answers, ",")) # transform answers with mulpiple options to single coutns

ggplot(data = as.data.frame(output_answers), aes(x=output_answers)) +
  geom_bar()
 

input_answers2 <- c("rat", "rat,dog", "cat", "cat,dog", "dog", "dog", "cat", "moose,cow") 

input_answers2B <- str_replace(input_answers2, "moose,cow", "moosecow")
output_answers2b <- unlist(strsplit(input_answers2B, ",")) # transform answers with mulpiple options to single coutns

ggplot(data = as.data.frame(output_answers2b), aes(x=output_answers2b)) +
  geom_bar()
