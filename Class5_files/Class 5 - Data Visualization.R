library(ggplot2)
ggplot(cars) +
  aes(x=speed, y=dist) +
  geom_point()

url <- "https://bioboot.github.io/bimm143_S20/class-material/up_down_expression.txt"
genes <- read.delim(url)
head(genes)

ggplot(genes) +
  aes(x = Condition1, y = Condition2) +
  geom_point()

p <- ggplot(genes) +
  aes(x=Condition1, y=Condition2, col=State) +
  geom_point()

p + scale_colour_manual( values=c("blue","gray","red") ) +
  labs(title = "Gene Expression Changes Upon Drug Treatment",
       x = "Control, no drug", y = "Drug Treatment")
