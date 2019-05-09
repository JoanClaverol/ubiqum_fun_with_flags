
# libraries ---------------------------------------------------------------

if (require(pacman) == FALSE) {
  install.packages("pacman")
}
pacman::p_load(ggplot2, dplyr, caret)

# data creation -----------------------------------------------------------

# Let's create 50k points on a 3x2 grid
x <- runif(10000, min = 0, max = 3)
y <- runif(10000, min = 0, max = 2)

# Flag colour palette
japanPalette <- c("red", "white")

# Flag dataframe
japan_flag <- as.data.frame(x = x)
japan_flag$y <- y


# Now we add the colour
japan_flag <- mutate(
  japan_flag,
  flag_colour = ifelse(
    (x - 1.5)^2 + (y-1)^2 > 3/10, "white", "red")
  )

# data visualization ------------------------------------------------------

ggplot(data = japan_flag) +
  geom_point(aes(x = x, y = y, color = flag_colour), size = 0.1) + 
  coord_fixed(ratio = 1) +
  scale_color_manual(values = japanPalette)

# train and test ----------------------------------------------------------

# We are going to choose the left half of the flag to predict the other half
trainSet <- japan_flag[which(x < 1.8), ]
testSet <- japan_flag[-which(x < 1.8), ]

# TrainSet plot. We see that it contains the left part of the flag
ggplot(trainSet) + 
  geom_point(aes(x = x, y = y, color = flag_colour), size  = 0.1) + 
  coord_fixed(ratio = 1) + 
  xlim(0,3) +
  scale_colour_manual(values = japanPalette)


# can we predict the rest of the flag? ------------------------------------

# trying with glm ---------------------------------------------------------

# glm model creation
# mod_glm <- train(
#   flag_colour ~ .,
#   data = trainSet,
#   method = "glm"
# )
mod_glm <- readRDS(file = "models/model_glm.rds")

# getting the results
testSet$predColour_glm <- predict(object = mod_glm, newdata = testSet)

# visualize the errors 
ggplot() +
  geom_point(data = trainSet, aes(x = x, y = y, colour = flag_colour), 
             size  = 0.1) + 
  geom_point(data = testSet, aes(x = x, y = y, colour = predColour_glm),
             size  = 0.1) +
  coord_fixed(ratio = 1) + 
  xlim(0,3) +
  scale_colour_manual(values = japanPalette)


# trying with knn --------------------------------------------------------

# knn model creation
# mod_knn <- train(
#   flag_colour ~ .,
#   data = trainSet,
#   method = "knn"
# )
mod_knn <- readRDS(file = "models/model_knn.rds")

# getting the results
testSet$predColour_knn <- predict(object = mod_knn, newdata = testSet)

# visualize the errors 
ggplot() +
  geom_point(data = trainSet, aes(x = x, y = y, colour = flag_colour), 
             size  = 0.1) + 
  geom_point(data = testSet, aes(x = x, y = y, colour = predColour_knn),
             size  = 0.1) +
  coord_fixed(ratio = 1) + 
  xlim(0,3) +
  scale_colour_manual(values = japanPalette)


# trying with rf ----------------------------------------------------------

# rf model creation
# mod_rf <- train(
#   flag_colour ~ .,
#   data = trainSet,
#   method = "rf"
# )
mod_rf <- readRDS(file = "models/model_rf.rds")

# getting the results
testSet$predColour_rf <- predict(object = mod_rf, newdata = testSet)

# visualize the errors 
ggplot() +
  geom_point(data = trainSet, aes(x = x, y = y, colour = flag_colour), 
             size  = 0.1) + 
  geom_point(data = testSet, aes(x = x, y = y, colour = predColour_rf),
             size  = 0.1) +
  coord_fixed(ratio = 1) + 
  xlim(0,3) +
  scale_colour_manual(values = japanPalette)

# trying with svm ---------------------------------------------------------

# svm model creation
# mod_svm <- train(
#   flag_colour ~ .,
#   data = trainSet,
#   method = "svmRadial"
# )
mod_svm <- readRDS(file = "models/model_svm.rds")

# getting the results
testSet$predColour_svm <- predict(object = mod_svm, newdata = testSet)

# visualize the errors 
ggplot() +
  geom_point(data = trainSet, aes(x = x, y = y, colour = flag_colour), 
             size  = 0.1) + 
  geom_point(data = testSet, aes(x = x, y = y, colour = predColour_svm),
             size  = 0.1) +
  coord_fixed(ratio = 1) + 
  xlim(0,3) +
  scale_colour_manual(values = japanPalette)



