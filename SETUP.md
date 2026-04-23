# Setup environment (R)

Install R (>= 4.2), then install dependencies:

```r
install.packages(c(
  "readxl",
  "readr",
  "ggplot2",
  "pROC",
  "rsample",
  "fastDummies",
  "car"
))
```

Run pipeline:

```r
source("main.R"); main()
```
