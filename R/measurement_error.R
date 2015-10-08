error <- read.csv('field_data/demographic_data/2015_measurement_error.csv')

library(ggplot2) 
library(lme4)
head(error)
error$tag1 <- as.factor(error$tag1)

plot( ch ~ tag1, data = error)
plot( c1 ~ tag1, data = error)
plot( c2 ~ tag1, data = error)
plot( log(c2*c1) ~ tag1, data = error)

plot(canopy ~ tag1, data = error)

lmer( canopy ~ (1|tag1) , data = error)

df <- aggregate( canopy ~ tag1, data = error, 'mean')
df$sd <- aggregate( canopy ~ tag1, data = error, 'sd')[, 2]

plot(sd ~ canopy, data = df ) 
