# example for comma separated strings

library(tidyverse)

input_answers <- c("rat", "rat,dog", "cat", "cat,dog", "dog", "dog", "cat") 
output_answers <- unlist(strsplit(input_answers, ",")) # transform answers with mulpiple options to single coutns

ggplot(data = as.data.frame(output_answers), aes(x=output_answers)) +
  geom_bar()
 