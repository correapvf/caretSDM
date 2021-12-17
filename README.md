# caretSDM

This package add more tools and features to caret to create and evaluate models,
specially aiming for species destribution models (SDMs). This package contais functions
to evaluate create ROC curves, create response plots, calculate the importance of
of variables (also including a jackknife method), choose and set a thresholder in a model,
import spatial blocks for cross-validation from ENMeval or blockCV into caret control,
create and evaluate ensemble models (the method here is different from caretEnsemble),
create confidence maps based on bootstrap, and create MaxEnt models using caret 'train'
function (it uses the jar version of MaxEnt to create and predict models).


## Installation
```r
library(devtools)
install_github("correapvf/caretSDM")
```