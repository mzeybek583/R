# Load necessary library
if (!require("rgl")) install.packages("rgl", dependencies = TRUE)
library(rgl)

# Define parameters for the trefoil knot
c <- seq(1, 10, by = 0.1)

# Create the cylinder3d object for the trefoil knot
knot <- cylinder3d(
  center = cbind(7.201260 + c - 0.699997, 
                 -1.724621 + c * 0.320273, 
                 -2.628671 + c * 0.638302), 
  radius = 2, 
  closed = FALSE
)

# Improve visualization with more subdivisions
knot_subdiv <- subdivision3d(knot, depth = 3)

# Add shading and axes
shade3d(addNormals(knot_subdiv), col = "green")
axes3d(edges = "bbox", labels = TRUE, tick = TRUE, nticks = 5, 
       box = TRUE, expand = 1.03)

# Set aspect ratio and view parameters for better visualization
aspect3d(1, 1, 1)
rglwidget()
