# data
# https://www.worldatlas.com/articles/the-top-sunflower-seed-producing-countries-in-the-world.html

df <- data.frame(country = c("Ukraine", "Russia", "Argentina", "China", "Romania", "Other"),
             prod = c(11.0, 10.6, 3.1, 2.4, 2.1, 15.3))
write.csv(df, "2022/data/sunflower.csv")

# link to tableau
