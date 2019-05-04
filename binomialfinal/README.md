---
output:
  github_document:
    html_preview: false
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Binomial Package

The package "binomialfinal" is an effort to simply the means of finding probabilistic outcomes using number of trials and probability.

Create your probability and number of trials, and begin finding out the probabilities of your outcomes!

```{r}
trials <- 5
prob <- 0.3
success <- 3
bin_probability(3, 5, 0.3)
```

