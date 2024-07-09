f <- system.file("ex/elev.tif", package="terra")
r <- rast(f) %>% 
  terra::project("EPSG:32631")
plot(r)

# focal
f.mean <- terra::focal(r, w = 7, fun = "mean")
f.median <- terra::focal(r, w = 7, fun = "median")
f.min <- terra::focal(r, w = 7, fun = "min")
f.max <- terra::focal(r, w = 7, fun = "max")

plot(c(f.mean, f.median, f.min, f.max))

summary(r[45:50, 45:50])

plot(r)
r[40:60, 50:70] <- NA
plot(r)
summary(r)

f1 <- focal(r, 7, "mean", na.policy="omit", na.rm=TRUE)   


r
plot(r)

f.mean <- terra::focal(r, w = 7, fun = "mean")
f.median <- terra::focal(r, w = 7, fun = "median")
f.min <- terra::focal(r, w = 7, fun = "min")
f.max <- terra::focal(r, w = 7, fun = "max")

