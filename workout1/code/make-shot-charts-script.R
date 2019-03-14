#Title

#Description

#Inputs

#Outputs

library(dplyr)
library(ggplot2)
library(jpeg)
library(grid)

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

curry_scatterplot <- ggplot(data = curry) + geom_point(aes(x = x, y = y, color = shot_made_flag))

court_file <- "../images/nba-court.jpg"
court_image <- rasterGrob(
  readJPEG(court_file),
  width = unit(1, "npc"),
  height = unit(1, "npc")
)

curry_shot_chart <- ggplot(data = curry) + annotation_custom(court_image, -250, 250, -50, 420) + 
  geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) + ggtitle('Shot Chart: Steph Curry (2016 Season)') +
  theme_minimal()

iguodala_shot_chart <- ggplot(data = iguodala) + annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) + ggtitle('Shot Chart: Andre Iguodala (2016 Season') +
  theme_minimal()

green_shot_chart <- ggplot(data = green) + annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) + ggtitle('Shot Chart: Draymond Green (2016 Season') +
  theme_minimal()

durant_shot_chart <- ggplot(data = durant) + annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) + ggtitle('Shot Chart: Kevin Durant (2016 Season)') +
  theme_minimal()

thompson_shot_chart <- ggplot(data = thompson) + annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) + ggtitle('Shot Chart: Klay Thompson (2016 Season)') +
  theme_minimal()

setwd("~/Desktop/workout1/images")

pdf(file = 'andre-iguodala-shot-chart.pdf', width = 6.5, height = 5)
iguodala_shot_chart
dev.off()

pdf(file = 'draymond-green-shot-chart.pdf', width = 6.5, height = 5)
green_shot_chart
dev.off()

pdf(file = 'kevin-durant-shot-chart.pdf', width = 6.5, height = 5)
durant_shot_chart
dev.off()

pdf(file = 'klay-thompson-shot-chart.pdf', width = 6.5, height = 5)
thompson_shot_chart
dev.off()

pdf(file = 'stephen-curry-shot-chart.pdf', width = 6.5, height = 5)
curry_shot_chart
dev.off()

all_together <- rbind(curry, thompson, durant, iguodala, green)

all_together_chart <- ggplot(data = all_together) + annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) + ggtitle('Shot Charts: GSW (2016 Season)')

all_together_chart <- all_together_chart + facet_wrap( ~ name, ncol = 3)
all_together_chart

pdf(file = 'gsw-shot-charts.pdf', width = 8, height = 7)
all_together_chart
dev.off()

png(filename = 'gsw-shot-charts.png', width = 8, height = 7, units = 'in', res = 300)
all_together_chart
dev.off()
