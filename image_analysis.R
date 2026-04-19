# Load required libraries
library(magick)    # for image processing
library(class)     # for KNN classification
library(tidyverse)  

# -------------------------
# Read Training Images
# -------------------------

# Filenames for training images (p = plane, c = cat, d = dog)
pic1 <- c("p1.jpg", "c1.jpeg", "d1.jpg",
          "p2.jpg", "c2.jpg", "d2.jpg",
          "p3.jpg", "c3.jpg", "d3.jpg",
          "p4.jpg", "c4.jpg", "d4.jpg",
          "p5.jpg", "c5.jpg", "d5.jpg",
          "p6.jpg", "c6.jpg", "d6.jpg",
          "p7.jpg", "c7.jpg", "d7.jpg",
          "p8.jpg", "c8.jpg", "d8.jpg")

# Reorder images so all planes, then cats, then dogs
pic1 <- pic1[c(seq(1,22,3), seq(2,23,3), seq(3,24,3))]

# Add folder path
pic1 <- paste0("pics/", pic1)

# Read images into a list
train <- list()
for (i in 1:24) {
  train[[i]] <- image_read(pic1[i])
}

# -------------------------
# Read Test Images
# -------------------------

pic2 <- c("p9.jpg", "c9.jpg", "d9.jpg",
          "p10.png", "c10.jpg", "d10.jpg")

pic2 <- paste0("pics/", pic2)

test <- list()
for (i in 1:6) {
  test[[i]] <- image_read(pic2[i])
}

# -------------------------
# Explore Images
# -------------------------

train[[1]]  # display first training image
test[[1]]   # display first test image

# Plot one example of each class (plane, cat, dog)
par(mfrow = c(1, 3))
for (i in c(1, 9, 17)) plot(train[[i]])
par(mfrow = c(1,1))

# -------------------------
# Resize Images
# -------------------------

# Resize all images to 100x100 pixels (force exact size with "!")
for (i in 1:24) {
  train[[i]] <- image_scale(train[[i]], "100x100!")
}
for (i in 1:6) {
  test[[i]] <- image_scale(test[[i]], "100x100!")
}

# Check resized images
train[[1]]
test[[1]]

# -------------------------
# Convert Image to Vector
# -------------------------

img_to_vector <- function(img) {
  # Extract RGB pixel data as a 3D array: [channel, row, col]
  arr <- image_data(img, channels = "rgb")
  
  # Convert each channel from raw/hex to integer values (0–255)
  r <- as.integer(arr[1,,])  # Red channel
  g <- as.integer(arr[2,,])  # Green channel
  b <- as.integer(arr[3,,])  # Blue channel
  
  # Flatten all channels into one long numeric vector
  c(r, g, b)
}

# Apply conversion to all images → matrix where each row = one image
train_matrix <- t(sapply(train, img_to_vector))
test_matrix  <- t(sapply(test, img_to_vector))

# -------------------------
# Create Labels
# -------------------------

# Training labels: 8 images per class
train_label <- rep(c("plane","cat","german_shepherd"), each = 8)

# Test labels (true values for evaluation)
test_label <- rep(c("plane","cat","german_shepherd",
                    "plane","cat","german_shepherd"))

# Combine features + labels into data frames
images_train <- as_tibble(train_matrix) %>% mutate(label = train_label)
images_test  <- as_tibble(test_matrix) %>% mutate(label = test_label)

# -------------------------
# k-NN Classification
# -------------------------

pred_label <- knn(
  train = images_train %>% select(-label),  # feature matrix (training)
  test  = images_test %>% select(-label),   # feature matrix (testing)
  cl    = images_train %>% pull(label),     # training labels
  k     = 6                                 # number of neighbors
)

# -------------------------
# Results & Evaluation
# -------------------------

# Combine true and predicted labels
image_res <- images_test %>%
  select(label) %>% 
  mutate(pred_label = pred_label)

# Number of test samples
N <- nrow(image_res)

# Confusion matrix (actual vs predicted)
confusion_mat <- table(image_res %>% select(label, pred_label))
confusion_mat

# Accuracy = correct predictions / total
accuracy <- (confusion_mat %>% diag %>% sum) / N
accuracy