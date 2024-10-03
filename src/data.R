white_wines$colour <- "white"
red_wines$colour <- "red"
wines <- rbind(white_wines, red_wines)
wines <- na.omit(wines)
wines$rating <- ""

wines_sample <- wines[sample(nrow(wines), size = 5000, replace = FALSE), ]
