# Load library
library(VennDiagram)
library(RColorBrewer)

rea <-c(3,4,5,6,7,8)
ret <- c(1,3,4,5,9,10,11,12,13,14,15,16,17,18,19,20,21,22)
comp <- c(1,2)

# Chart
d<-venn.diagram(
  x = list(rea, ret, comp),
  fill = c("red", "#EFC000FF", "#0073C2FF"),
  category.names = c("Reasoning" , "Retrieval" , "Comprehension"),
  filename = 'ChatGPT4failures.png',
  output=TRUE,
  
  
  # Output features
  imagetype="png" ,
  height = 2000 , 
  width = 2000 , 
  resolution = 500,
  compression = "lzw",
  
  # Circles
  lwd = 2,
  lty = 'blank',
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  
  # Set names
  cat.cex = 0.6,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.pos = c(-29, 27, 135),
  cat.dist = c(0.055, 0.055, 0.085),
  cat.fontfamily = "sans",
  rotation = 1
)


