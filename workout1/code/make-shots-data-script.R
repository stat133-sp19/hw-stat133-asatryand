#Title: Data Preparation

#Description: In the following script, data of each individual GSW player is read. I also mutate the data to include their name and during which minute each action took place. Following that, I create summaries and export them.

#Inputs: The inputs are the .csv files including individual data of each player

#Outputs: Outputs created are .txt and .csv files that I have created for summaries and such data

library(dplyr)

setwd("~/Desktop/workout1/code")
curry <- read.csv(file = file.path("../data/stephen-curry.csv"), stringsAsFactors = FALSE)
iguodala <- read.csv(file = file.path("../data/andre-iguodala.csv"), stringsAsFactors = FALSE)
green <- read.csv(file = file.path("../data/draymond-green.csv"), stringsAsFactors = FALSE)
durant <- read.csv(file = file.path("../data/kevin-durant.csv"), stringsAsFactors = FALSE)
thompson <- read.csv(file = file.path("../data/klay-thompson.csv"), stringsAsFactors = FALSE)

curry <- mutate(curry, name = 'Stephen Curry')
iguodala <- mutate(iguodala, name = 'Andre Iguodala')
green <- mutate(green, name = 'Draymond Green')
durant <- mutate(durant, name = 'Kevin Durant')
thompson <- mutate(thompson, name = 'Klay Thompson')

curry$shot_made_flag[curry$shot_made_flag == 'n'] <- "shot_no"
curry$shot_made_flag[curry$shot_made_flag == 'y'] <- "shot_yes"
iguodala$shot_made_flag[iguodala$shot_made_flag == 'n'] <- "shot_no"
iguodala$shot_made_flag[iguodala$shot_made_flag == 'y'] <- "shot_yes"
green$shot_made_flag[green$shot_made_flag == 'n'] <- "shot_no"
green$shot_made_flag[green$shot_made_flag == 'y'] <- "shot_yes"
durant$shot_made_flag[durant$shot_made_flag == 'n'] <- "shot_no"
durant$shot_made_flag[durant$shot_made_flag == 'y'] <- "shot_yes"
thompson$shot_made_flag[thompson$shot_made_flag == 'n'] <- "shot_no"
thompson$shot_made_flag[thompson$shot_made_flag == 'y'] <- "shot_yes"


curry <- mutate(curry, minute = (curry$period * 12) - curry$minutes_remaining)
iguodala <- mutate(iguodala, minute = (iguodala$period * 12) - iguodala$minutes_remaining)
green <- mutate(green, minute = (green$period * 12) - green$minutes_remaining)
durant <- mutate(durant, minute = (durant$period * 12) - durant$minutes_remaining)
thompson <- mutate(thompson, minute = (thompson$period * 12) - thompson$minutes_remaining)

sink(file = '../output/andre-iguodala-summary.txt')
summary(iguodala)
sink()

sink(file = '../output/stephen-curry-summary.txt')
summary(curry)
sink()

sink(file = '../output/draymond-green-summary.txt')
summary(green)
sink()

sink(file = '../output/kevin-durant-summary.txt')
summary(durant)
sink()

sink(file = '../output/klay-thompson-summary.txt')
summary(thompson)
sink()

combined <- rbind(curry, durant, green, iguodala, thompson)

write.csv(combined, file = "../data/shots-data.csv", row.names = FALSE)

sink(file = '../output/shots-data-summary.txt')
summary(combined)
sink()

