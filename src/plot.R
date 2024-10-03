ggplot(wines, aes(x = pH)) +
  geom_histogram(aes(y = after_stat(density)), colour = 1, fill = "#FFFFFF", bins = 200) +
  geom_density(lwd = 1.2, linetype = 2, colour = "#FF69B4", fill = "#FF69B4", alpha = 0.25)

ggplot(wines, aes(x = pH, y = citric.acid)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
