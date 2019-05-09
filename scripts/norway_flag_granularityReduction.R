
# libraries ---------------------------------------------------------------

# First we load the libraries
if (require(pacman) == FALSE) {
  install.packages("pacman")
}
pacman::p_load(ggplot2, dplyr, caret)

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

# visualizing the errors
NorwayPalette <- c("red", "white", "blue")
ggplot(flag) + 
  geom_point(aes(x = x, y = y, color = flag_colour), size = 0.1) + 
  coord_fixed(ratio = 1) + 
  scale_colour_manual(values = NorwayPalette)


# train and test ----------------------------------------------------------

train_id <- createDataPartition(
  y = flag$flag_colour,
  p = 0.2,
  list = F
)
trainSet <- flag[train_id,]
testSet <- flag[-train_id,]

# visualize train
ggplot(data = trainSet) +
  geom_point(aes(x = x, y = y, color = flag_colour), size = 0.1) + 
  coord_fixed(ratio = 1) + 
  scale_colour_manual(values = NorwayPalette)


# knn model ---------------------------------------------------------------

mod_knn_smll_nrwy <- train(
  flag_colour ~ .,
  data = trainSet,
  method = "knn"
)

testSet$pred_flag_color <- predict(
  object = mod_knn_smll_nrwy, 
  newdata = testSet
  )

ggplot(data = testSet) +
  geom_point(aes(x = x, y = y, colour = pred_flag_color), size = 0.1) +
  scale_colour_manual(values = NorwayPalette)


# rpart -------------------------------------------------------------------

mod_rpart_smll_nrwy <- train(
  flag_colour ~ .,
  data = trainSet,
  method = "rpart"
)

testSet$pred_flag_color <- predict(
  object = mod_rpart_smll_nrwy, 
  newdata = testSet
)

ggplot(data = testSet) +
  geom_point(aes(x = x, y = y, colour = pred_flag_color), size = 0.1) +
  scale_colour_manual(values = NorwayPalette)


# svm ---------------------------------------------------------------------
# 
# mod_svm_smll_nrwy <- train(
#   flag_colour ~ .,
#   data = trainSet,
#   method = "svmRadial"
# )
mod_svm_smll_nrwy <- readRDS(file = "models/model_svm_smll_nrw.rds")

testSet$pred_flag_color <- predict(
  object = mod_svm_smll_nrwy, 
  newdata = testSet
)

ggplot(data = testSet) +
  geom_point(aes(x = x, y = y, colour = pred_flag_color), size = 0.1) +
  scale_colour_manual(values = NorwayPalette)


# rf ----------------------------------------------------------------------

mod_rf_smll_nrwy <- train(
  flag_colour ~ .,
  data = trainSet,
  method = "rf"
)

testSet$pred_flag_color <- predict(
  object = mod_rf_smll_nrwy, 
  newdata = testSet
)

ggplot(data = testSet) +
  geom_point(aes(x = x, y = y, colour = pred_flag_color), size = 0.1) +
  scale_colour_manual(values = NorwayPalette)

# gbm ---------------------------------------------------------------------
# 
# mod_gbm_smll_nrwy <- train(
#   flag_colour ~ .,
#   data = trainSet,
#   method = "gbm"
# )
mod_gbm_smll_nrwy <- readRDS(file = "models/mod_gbm_smll_nrwy.rds")

testSet$pred_flag_color <- predict(
  object = mod_gbm_smll_nrwy, 
  newdata = testSet
)

ggplot(data = testSet) +
  geom_point(aes(x = x, y = y, colour = pred_flag_color), size = 0.1) +
  scale_colour_manual(values = NorwayPalette)
