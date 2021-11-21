
library(rgl)

# A trefoil knot

#open3d()
c <- seq(1,10,by=0.1)
knot <- cylinder3d( center = cbind(7.201260+c-0.699997,-1.724621+c*0.320273,
                 -2.628671+c*0.638302), radius = 2, closed = FALSE)

shade3d(addNormals(subdivision3d(knot, depth = 0.1)), col = "green")  
axes3d(edges = "bbox", labels = TRUE, tick = TRUE, nticks = 5, 
       box = FALSE, expand = 1.03)
