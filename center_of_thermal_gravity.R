load("~/COG/Data/thmini.RData")

data = thmini

d2017 = data[ which(data$year == 2017),]
d2018 = data[ which(data$year == 2018),]
d2019 = data[ which(data$year == 2019),]

x = list(d2017=d2017, 
         d2018=d2018, 
         d2019=d2019)

gravity <- matrix(0, length(x), ncol = 3)

for (i in 1:length(x)){
  
  # i = 1
  
  x[[i]] = na.exclude(x[[i]][, c("lat", "lon", "sst", "thor")])
  temp_box = data.frame(x[[i]])
  
  qplot(temp_box$lon, temp_box$lat, col = temp_box$sst)
  
  temp_box$thor = ifelse(temp_box$sst < 22 & temp_box$sst > 14, 1, 0)
  
  X=sum(temp_box$thor*temp_box$lon)/sum(temp_box$thor) #mean lon weighted by catch
  Y=sum(temp_box$thor*temp_box$lat)/sum(temp_box$thor) #mean lat weighted by catch

  gravity[i, 1] = X
  gravity[i, 2] = Y

}

gravity[,3] = c(2017:2019)

gravity = as.data.frame(gravity)

colnames(gravity) = c("Lon", "Lat", "Year")

xlim = range(data$lon)
ylim = range(data$lat)

par(mfrow = c(1,2))

plot(NULL,
     cex.main = 2,
     xlim = xlim,
     ylim = ylim,
     xlab = "", ylab = "",
     axes = F, 
     col = "gray", xaxt = "n", yaxt = "n", lwd = 0.5, cex.lab = 1)
maps::map(add = T, fill = T)
sp::degAxis(1, cex.axis = 1)
sp::degAxis(2, las = 2, cex.axis = 1)
raster::scalebar(d = 200, xy = NULL, type = 'bar', divs = 4, below = 'km', cex = 1, adj = c(0,-1))
points(gravity[, 1], gravity[, 2], pch = 16, col = 4, cex = 1)

plot(gravity[, 1], gravity[, 2], pch = 16, col = 4, cex = 1, axes = F, xlab = "", ylab = "" )
text(gravity[,1], gravity[,2], gravity[,3], cex = 1, pos = 2, offset = 0.5)

s <- seq(length(x)-1)  # one shorter than data
arrows(gravity[s,1], gravity[s,2], gravity[s+1,1], gravity[s+1,2], 
       length = 0.2, angle=20, lwd = 2, col = alpha("blue", 1))

maps::map(add = T, fill = T)
sp::degAxis(1, cex.axis = 1)
sp::degAxis(2, las = 2, cex.axis = 1)

