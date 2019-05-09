
# libraries ---------------------------------------------------------------

# First we load the libraries
if (require(pacman) == FALSE) {
  install.packages("pacman")
}
pacman::p_load(ggplot2, dplyr, caret, magrittr)

# load data ---------------------------------------------------------------

# Let's create 200k points on a 16x10 grid
n <- 50000
x <- runif(n, min = 0, max = 21)
y <- runif(n, min = 0, max = 16)

# We create the dataframe
flag <- data.frame(
  x = x,
  y = y
  )

# Now we add the colour, however this flags contain two crosses
flag <- mutate(flag, 
               flag_colour = ifelse(
                 ((x > 6) & (x<=10)) | ((y > 6) & (y<=10)), "cross", "bckgd"
                 )
               )

# defining the crossed part
crossed_flag <- flag[which(flag$flag_colour == "cross"),]

# define the inner cross and the cross
flag[which(flag$flag_colour == "cross"),] <- mutate(
  crossed_flag, 
  flag_colour = ifelse(
    ((x > 7) & (x<=9)) | ((y > 7) & (y<=9)), "inner_cross", "cross"
    )
  )

# visualizing the flag
NorwayPalette <- c("red", "white", "blue")
ggplot(flag) + 
  geom_point(aes(x = x, y = y, color = flag_colour), size = 0.1) + 
  coord_fixed(ratio = 1) + 
  scale_colour_manual(values = NorwayPalette)


# train and test ----------------------------------------------------------

# apply a linear function to define the train and the test
flag %<>% 
  mutate(
  flag_partition = ifelse(
    y > x * (11/16) - (55/16), "train", "test"
  )) 

# create test and train
trainSet <- flag %>%
  filter(flag_partition == "train")

testSet <- flag %>% 
  filter(flag_partition == "test")

# visualize the train 
ggplot(data = trainSet) +
  geom_point(aes(x = x, y = y, colour = flag_colour), size = 0.1) +
  coord_fixed(ratio = 1) + 
  scale_colour_manual(values = NorwayPalette)


# knn ---------------------------------------------------------------------

# mod_knn_split_nrwy <- train(
#   flag_colour ~., 
#   data = trainSet %>% select(-flag_partition),
#   method = "knn"
# )
mod_knn_split_nrwy <- readRDS(file = "models/mod_knn_split_nrwy.rds")

testSet$pred_flag_colour <- predict(
  object = mod_knn_split_nrwy, 
  newdata = testSet
)

ggplot(data = testSet) +
  geom_point(aes(x = x, y = y, colour = pred_flag_colour), size = 0.1) +
  geom_point(data = trainSet, aes(x = x, y = y, colour = flag_colour), 
             size = 0.1) +
  scale_colour_manual(values = NorwayPalette) +
  xlim(0,21) + ylim(0,16)


# decision tree -----------------------------------------------------------

# mod_dt_split_nrwy <- train(
#   flag_colour ~., 
#   data = trainSet %>% select(-flag_partition),
#   method = "rpart"
# )
mod_dt_split_nrwy <- readRDS(file = "models/mod_dt_split_nrwy.rds")

testSet$pred_flag_colour <- predict(
  object = mod_dt_split_nrwy, 
  newdata = testSet
)

ggplot(data = testSet) +
  geom_point(aes(x = x, y = y, colour = pred_flag_colour), size = 0.1) +
  geom_point(data = trainSet, aes(x = x, y = y, colour = flag_colour), 
             size = 0.1) +
  scale_colour_manual(values = NorwayPalette) +
  xlim(0,21) + ylim(0,16)


# rf ----------------------------------------------------------------------

# mod_rf_split_nrwy <- train(
#   flag_colour ~., 
#   data = trainSet %>% select(-flag_partition),
#   method = "rf"
# )
mod_rf_split_nrwy <- readRDS(file = "models/mod_rf_split_nrwy.rds")

testSet$pred_flag_colour <- predict(
  object = mod_rf_split_nrwy, 
  newdata = testSet
)

ggplot(data = testSet) +
  geom_point(aes(x = x, y = y, colour = pred_flag_colour), size = 0.1) +
  geom_point(data = trainSet, aes(x = x, y = y, colour = flag_colour), 
             size = 0.1) +
  scale_colour_manual(values = NorwayPalette) +
  xlim(0,21) + ylim(0,16)

# svm ----------------------------------------------------------------------

# mod_svm_split_nrwy <- train(
#   flag_colour ~., 
#   data = trainSet %>% select(-flag_partition),
#   method = "svmRadial"
# )
mod_svm_split_nrwy <- readRDS(file = "data/mod_svm_split_nrwy.rds")

testSet$pred_flag_colour <- predict(
  object = mod_svm_split_nrwy, 
  newdata = testSet
)

ggplot(data = testSet) +
  geom_point(aes(x = x, y = y, colour = pred_flag_colour), size = 0.1) +
  geom_point(data = trainSet, aes(x = x, y = y, colour = flag_colour), 
             size = 0.1) +
  scale_colour_manual(values = NorwayPalette) +
  xlim(0,21) + ylim(0,16)
