# Image processing 101
# install.packages("magick")
library(magick)

tiger <- image_read('http://jeroen.github.io/images/tiger.svg')
image_info(tiger)

tiger.blur <- image_blur(tiger)
#tiger.egde <- tiger + tiger.blur
#class(tiger)

frink <- image_read("https://jeroen.github.io/images/frink.png")
print(frink)

frink.blur <- image_blur(frink,radius = 5)
print(frink.blur)

frink.enhance <- image_append(frink.blur)
print(frink.enhance)

## not conclusive
