# Scratch Project ALY6070 ################################################
#
#
# Author: Daniel Brown
# Date 10/28/2017

# Data Preprocessing #####################################################
## Importing Libraries ###################################################
library(tidyverse)
library(RColorBrewer)

## Importing dataset into $R$ ############################################
df <- read.csv("250750views.csv")

## Selecting relevent variables and renaming them
dataset <- dplyr::select(df, "is_remixed", "sprites_website", 
                         "scripts_website", "viewers_website", 
                         "lovers_website")
names(dataset) <- c("remix.status", "sprites", "scripts", 
                    "views", "loves")

# Conducting analysis ####################################################
## Performing linear regression to correlation 
## between remix status and other variables. #############################

regressor <- lm(formula = remix.status ~., data = dataset)
summary(regressor)
regressor2 <- lm(formula = remix.status ~ views + sprites + scripts,
                 data = dataset)
summary(regressor2)
regressor3 <- lm(formula = remix.status ~ views + scripts, data = dataset)
summary(regressor3)
regressor4 <- lm(formula = remix.status ~ views, data = dataset)
summary(regressor4)

### Testing for exponential correlation ##################################

data.log <- aggregate(dataset$remix.status, by=list(dataset$views), FUN=sum)
names(data.log) <- c("views", "remix.totals")
log.reg <- lm(formula = log(remix.totals) ~ views, data = data.log)
summary(log.reg)

## Determining correlation between Loves and other variables #############
love.reg <- lm(formula = loves ~., data = dataset)
love.reg2 <- lm(formula = loves ~ views + sprites + scripts, 
                data = dataset)
love.reg3 <- lm(formula = loves ~ views + sprites, 
                data = dataset)
summary(love.reg3)

# Visualizing Data #######################################################
ggplot(data = dataset, aes(x = views, fill = remix.status)) + 
  geom_histogram(color = "black", alpha = 0.4) + ggtitle("Views") + xlab("Views") +
  ylab("Counts") + scale_fill_brewer(palette = "Paired") + theme(line = element_line(size = 1))
ggplot(data = data.log, aes(x = views, y = log(remix.totals), 
                            color = I("Blue"))) +
  geom_point() + geom_smooth(color = I("Green")) + 
  ggtitle("Log Remix Totals vs. Views") + xlab("Views") +
  ylab("Log Remix") 
