process_image <- function(path, label) {
  img <- image_read(path) %>%
    image_scale("25x25!")
  
  arr <- image_data(img, channels = "rgb")
  
  r <- as.integer(arr[1,,])
  g <- as.integer(arr[2,,])
  b <- as.integer(arr[3,,])
  
  tibble(
    label = label,
    pixels = list(c(r, g, b))
  )
}


train_paths <- list.files("flower_image_train", full.names = TRUE)

images_train <- map_dfr(train_paths, function(p) {
  label <- str_extract(p, "rose|sunflower|tulip")
  process_image(p, label)
}) %>%
  unnest_wider(pixels, names_sep = "_")

test_paths <- list.files("flower_image_test", full.names = TRUE)

images_test <- map_dfr(test_paths, function(p) {
  label <- str_extract(p, "rose|sunflower|tulip")
  process_image(p, label)
}) %>%
  unnest_wider(pixels, names_sep = "_")
