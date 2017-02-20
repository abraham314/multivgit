jaccard <- function(raster1, raster2, threshhold, warn.uneven=TRUE) {
  
  # Get the values above the threshhold
  raster1.bin <- raster1 >= threshhold
  raster2.bin <- raster2 >= threshhold
  
  if (warn.uneven) {
    raster1.size <- count(raster1.bin, 1)
    raster2.size <- count(raster2.bin, 1)
    # Sort from smaller to larger
    sizes <- sort(c(raster1.size, raster2.size))
    if (sizes[2] / sizes[1] > 20) {
      warning("The extents of raster values above the threshhold differ more than 20-fold: Jaccard coefficient may not be informative.")
    }
  }
  
  # Calculate the intersection of the two rasters, this is given by adding 
  # the binary rasters together -> 2 indicates intersection
  combination <- raster1.bin + raster2.bin
  intersection <- combination == 2
  
  # Union is all the area covered by the both rasters
  union <- combination >= 1
  
  return(count(intersection, 1) / count(union, 1))
}