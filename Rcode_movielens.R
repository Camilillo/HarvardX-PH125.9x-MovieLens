##########################################################
# EDX MovieLens Project HarvardX PH125.9x
# R script
# Author: Camilo Lillo
# May 16, 2021
##########################################################

##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse))  install.packages("tidyverse",  repos = "http://cran.us.r-project.org")
if(!require(caret))      install.packages("caret",      repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2))    install.packages("ggplot2",    repos = "http://cran.us.r-project.org")
if(!require(stringi))    install.packages("stringi",    repos = "http://cran.us.r-project.org")
if(!require(scales))     install.packages("scales",     repos = "http://cran.us.r-project.org")
if(!require(dismo))      install.packages("dismo",      repos = "http://cran.us.r-project.org")
if(!require(dplyr))      install.packages("dplyr",      repos = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(ggplot2)
library(stringi)
library(scales)
library(dismo)
library(dplyr)
library(recosystem)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# I use R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx  <- movielens[-test_index,]
temp <- movielens[ test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>%
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx     <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, movielens, removed)

edx0 = edx

##########################################################
# Exploratory data analysis
##########################################################

# Response variable distribution

dim(edx)

fig1 <- edx %>% group_by(rating) %>% summarize(total = length(rating)) %>%
  ggplot(aes(x = rating, y = 100*total/(sum(total)))) +
  geom_bar(stat="identity", position=position_dodge(), width = 0.4, color = "black", fill = "orange") +
  
  geom_text(aes(label=percent(total/(sum(total)) %>% round(1))), vjust=1.6, color="black",
            position = position_dodge(0.9), size = 3) +
  
  labs(title="Rating Distribution",
       x="Rating", y = "Relative Frequency (%)")
fig1

## user analysis

tab_userId = edx %>% group_by(userId) %>%
  summarize(avg = mean(rating),
            sds = sd(rating),
            cv  = 100*sd(rating)/mean(rating),
            len = length(rating))  

tab_userId = tab_userId[order(-tab_userId$avg),]
tab_userId = tab_userId %>% mutate(sds = ifelse(is.na(sds), 0, sds),
                                   cv  = ifelse(is.na(cv), 0, cv))
tab_userId$rank = c(1:NROW(tab_userId))
tab_userId = tab_userId %>% mutate(icsup = avg + 1.96*(sds/sqrt(len)))
tab_userId = tab_userId %>% mutate(icinf = avg - 1.96*(sds/sqrt(len)))

fig2 = tab_userId %>% filter(len > 100) %>% ggplot(aes(rank)) +
  geom_line(aes(y = avg, colour = "rating average")) +
  geom_line(aes(y = icsup, colour = "upper IC(95%)")) +
  geom_line(aes(y = icinf, colour = "lower IC(95%)")) +
  labs(title="", x = "users ranking (users who have watched more than 100 movies)", y = "ratings")
fig2

## movie analysis

tab_movieId = edx %>% group_by(title) %>%
  summarize(avg = mean(rating),
            sds = sd(rating),
            cv  = 100*sd(rating)/mean(rating),
            len = length(rating))  

tab_movieId = tab_movieId[order(-tab_movieId$avg),]
tab_movieId = tab_movieId %>% mutate(sds = ifelse(is.na(sds), 0, sds),
                                     cv  = ifelse(is.na(cv), 0, cv))
tab_movieId$rank = c(1:NROW(tab_movieId))
tab_movieId = tab_movieId %>% mutate(icsup = avg + 1.96*(sds/sqrt(len)))
tab_movieId = tab_movieId %>% mutate(icinf = avg - 1.96*(sds/sqrt(len)))

fig3 = tab_movieId %>% filter(len > 100) %>% ggplot(aes(rank)) +
  geom_line(aes(y = avg, colour = "rating average")) +
  geom_line(aes(y = icsup, colour = "upper IC(95%)")) +
  geom_line(aes(y = icinf, colour = "lower IC(95%)")) +
  labs(title="", x = "movie ranking (viewed more than 100 times)", y = "ratings")
fig3

## both effects

tab_userId  =  tab_userId[order(tab_userId$avg),]
tab_movieId = tab_movieId[order(tab_movieId$avg),]

tab_userId$k  = (c(1:NROW(tab_userId) ) - 0.5)/NROW(tab_userId )
tab_movieId$k = (c(1:NROW(tab_movieId)) - 0.5)/NROW(tab_movieId)

fig4 =
  ggplot() +
  geom_line(data=tab_userId,  aes(x = avg, y = k, colour = "rating (users) dist."))  +
  geom_line(data=tab_movieId, aes(x = avg, y = k, colour = "rating (movies) dist.")) +
  labs(title="", x = "ratings", y = "ECDF")
fig4

## year analysis

# edx$year = as.numeric(stri_reverse(substr(stri_reverse(edx$title), 2, 5)))
# tab_years = edx %>%
#   group_by(year) %>%
#   summarize(avg = mean(rating),
#             sds = sd(rating),
#             cv  = 100*sd(rating)/mean(rating),
#             len = length(rating))  
# 
# fig5.1 = tab_years %>% ggplot(aes(x=year, y=len)) +
#   geom_line(aes(y=len, group = 1), col="black") +
#   geom_point(aes(y=len, group = 1), col="black")  +
#   labs(title="", x = "year", y = "number of movies by year")
# fig5.1

edx$year = as.numeric(stri_reverse(substr(stri_reverse(edx$title), 2, 5)))
edx = edx %>% mutate(year = ifelse(year < 1980, "< 1980", year))
tab_years2 = edx %>%
  group_by(year) %>%
  summarize(avg = mean(rating),
            sds = sd(rating),
            cv  = 100*sd(rating)/mean(rating),
            len = length(rating))  

fig5.2 = tab_years2 %>% ggplot(aes(x=year, y=avg)) +
  geom_line(aes(y=avg, group = 1), col="black") +
  geom_point(aes(y=avg, group = 1), col="black") +
  geom_errorbar(aes(ymin=avg-(1.96*sds/sqrt(len)),
                    ymax=avg+(1.96*sds/sqrt(len))), width=.2,
                position=position_dodge(0.05)) +
  labs(title="", x = "year", y = "rating average") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
fig5.2

# analysis by genres

tab_genres = edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(avg = mean(rating),
            sds = sd(rating),
            cv  = 100*sd(rating)/mean(rating),
            len = length(rating))  

b <- max(tab_genres$len)/(max(tab_genres$avg) - 3)
a <- b*(0 - 3)

fig6 = tab_genres %>% ggplot(aes(x=reorder(genres, -avg), y=len)) +
  geom_bar(col="black", stat="identity", fill = "orange") +
  geom_line(aes(y=avg * b + a, group = 1), col = "darkgreen", size = 1.2) +
  scale_y_continuous(name="n", sec.axis=sec_axis(~(. - a)/b, name = "Rating average")) +
  labs(title="", x = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
fig6

## mu IC 95% grouped by genres

fig7 = tab_genres %>% filter((genres != "IMAX")&(genres != "(no genres listed)")) %>% ggplot(aes(x=reorder(genres, -avg), y=avg)) +
  geom_line(aes(y=avg, group = 1), col="darkgreen") +
  geom_point(aes(y=avg, group = 1), col="black") +
  geom_errorbar(aes(ymin=avg-(1.96*sds/sqrt(len)), ymax=avg+(1.96*sds/sqrt(len))), width=.2,
                position=position_dodge(0.05)) +
  labs(title="", x = "", y = "ratings") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
fig7

##########################################################
# Model
##########################################################

## m1: global mean model
## select 10-folds from train set to develop cross-validation of diferents models

KF = 10
set.seed(2020-12-31, sample.kind = "Rounding")
edx$id_cv = kfold(1:NROW(edx), k = KF)
edx_val   = numeric()
for(u in 1:KF){
  m1_folds = edx %>% filter(id_cv != u) %>%
    summarise(avg = mean(rating))
  
  edx_test = edx %>% filter(id_cv == u)
  edx_test = edx_test[,c("userId", "movieId", "rating", "id_cv")]
  edx_test$pred_m1 = m1_folds
  
  edx_val = rbind(edx_val, edx_test, fill = TRUE)
  print(u)
}

RMSE(pred = edx_val$pred_m1, obs = edx_val$rating)

# ratings model for movie effects

edx$movieId = as.factor(edx$movieId)
edx_val_m2  = numeric()
for(u in 1:KF){
  xb <- mean(edx$rating)
  m2_folds = edx %>% filter(id_cv != u) %>%
    group_by(movieId) %>%
    summarise(b_i = mean(rating - xb))
  edx_test = edx %>% filter(id_cv == u)
  edx_test$pred_m2 <- xb + edx_test %>%
    left_join(m2_folds, by = "movieId") %>%
    pull(b_i)
  edx_val_m2 = rbind(edx_val_m2, edx_test, fill = TRUE)
}

edx_val_m2 = edx_val_m2[,c("userId", "movieId", "pred_m2")]
edx_val_m2 = na.omit(edx_val_m2)

edx_val  = left_join(edx_val, edx_val_m2, by = c("userId", "movieId"))

RMSE(pred = edx_val$pred_m2, obs = edx_val$rating, na.rm = TRUE)


# add user effect

edx$movieId = as.factor(edx$movieId)
edx_val_m3  = numeric()
for(u in 1:KF){
  
  xb <- mean(edx$rating)
  m3_folds = edx %>% filter(id_cv != u) %>%
    group_by(movieId) %>%
    summarise(b_i = mean(rating - xb))
  
  m3_add_user_in_folds = edx %>% filter(id_cv != u) %>% left_join(m3_folds, "movieId") %>%
    group_by(userId) %>% summarise(b_u = mean(rating - xb - b_i))
  
  edx_test = edx %>% filter(id_cv == u)
  
  edx_test$pred_m3 <- edx_test %>%
    left_join(m3_folds, by = "movieId") %>%
    left_join(m3_add_user_in_folds, by = "userId") %>%
    mutate(rating_pred = xb + b_i + b_u) %>%
    pull(rating_pred)
  
  edx_val_m3 = rbind(edx_val_m3, edx_test, fill = TRUE)
  print(u)
}

edx_val_m3 = edx_val_m3[,c("userId", "movieId", "pred_m3")]
edx_val_m3 = na.omit(edx_val_m3)
edx_val    = edx_val[,-1]

edx_val$userId    = as.character(edx_val$userId)
edx_val_m3$userId = as.character(edx_val_m3$userId)

edx_val$movieId    = as.character(edx_val$movieId)
edx_val_m3$movieId = as.character(edx_val_m3$movieId)

edx_val   = left_join(edx_val, edx_val_m3, by = c("userId", "movieId"))
RMSE(pred = edx_val$pred_m3, obs = edx_val$rating, na.rm = TRUE)

# these model can be better by the next model





set.seed(2021-05-28, sample.kind = "Rounding")

# Convert 'edx' and 'validation' sets to recosystem input format
edx_reco <-  with(edx0, data_memory(user_index = userId,
                                    item_index = movieId,
                                    rating = rating))
validation_reco  <-  with(validation, data_memory(user_index = userId,
                                                  item_index = movieId,
                                                  rating = rating))

# Create the model object
r <-  recosystem::Reco()

# Tune the parameters
opts <-  r$tune(edx_reco, opts = list(dim = 30, #c(20, 30, 40)
                                      lrate = 0.05, # seq(0.025, 0.1, 0.025)
                                      costp_l2 = c(0.05, 0.075, 0.1),
                                      costq_l2 = c(0.001, 0.005, 0.01, 0.015),
                                      nthread  = 8, niter = 10))
opts$min
# Train the model
r$train(edx_reco, opts = c(opts$min, nthread = 8, niter = 20))

# Calculate the prediction
y_hat_final_reco <-  r$predict(validation_reco, out_memory())

RMSE(y_hat_final_reco, validation$rating)

y_hat_final_reco2 = y_hat_final_reco
y_hat_final_reco2[y_hat_final_reco2 > 5.0] = 5.0
y_hat_final_reco2[y_hat_final_reco2 < 0.5] = 0.5

RMSE(y_hat_final_reco2, validation$rating)
