if (!require("svDialogs")) install.packages("svDialogs")
library(svDialogs)

radius <- as.numeric(dlgInput("Enter silo radius (m):", "10")$res)
cyl_height <- as.numeric(dlgInput("Enter cylindrical part height (m):", "50")$res)
cone_height <- as.numeric(dlgInput("Enter conical bottom height (m):", "10")$res)
fill_height <- as.numeric(dlgInput("Enter material (fill) height (m):", "22")$res)

if (fill_height <= cone_height) {
  volume <- (1/3) * pi * radius^2 * fill_height
} else {
  cone_vol <- (1/3) * pi * radius^2 * cone_height
  cyl_vol <- pi * radius^2 * (fill_height - cone_height)
  volume <- cone_vol + cyl_vol
}

cat("\nFilled volume:", round(volume, 2), "m3\n")
