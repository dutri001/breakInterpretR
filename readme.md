# breakInterpretR
*Loïc Dutrieux*

### *Visually interpret temporal segments in time-series*

## Prepare input data
You can do so by using `bfastSpatial::zooExtract()`

## Run the shiny application
You should have the following packages installed
- shiny
- zoo
- bfast
- strucchange
- ggplot2
- lubridate


```r
# Install dependencies
install.packages('shiny', 'zoo', 'bfast', 'strucchange', 'ggplot2', 'lubridate')
```

```r
# Run the app directly from github
library(shiny)
runGitHub('dutri001/breakInterpretR', launch.browser = TRUE)
```