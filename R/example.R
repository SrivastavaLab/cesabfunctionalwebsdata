
dd <- data_frame(x = runif(15, 2, 20),
                 y = x * 3 + 15 + rnorm(n = 15, sd = 5))

p <- dd %>%
  ggplot(aes(x = x, y = y)) + geom_point()

mm <- lm(y ~ x, data = dd)

predict_function <- function(mod) {
  force(mod)
  function(xs) {
    predict(mod, se.fit = TRUE, list(x = newx))
  }
}
newx <- runif(20, 3, 50)
new <- predict_function(mm)(newx)


p + geom_pointrange(aes(ymin = ymin,
                        ymax = ymax),
                    data = data_frame(x = newx,
                                      y = new$fit,
                                      ymin = new$fit - new$se.fit,
                                      ymax = new$fit + new$se.fit), colour = "red")
