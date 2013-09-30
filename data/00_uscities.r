# Load the US cities data
library(maps)
data(us.cities)

# Extract and normalize
us.cities <- us.cities
loc <- cbind(us.cities$pop, us.cities$long, us.cities$lat)
colnames(loc) <- c("pop", "long", "lat")
pop <- cbind(us.cities$pop)
name <- us.cities$name
rownames(pop) <- us.cities$name
colnames(pop) <- c("pop")