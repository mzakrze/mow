# mow

mow.R script automates experiments with random forest algorithm. You can play with different tuning parameters for random forest algorithm, perform more precise analysis for specific parameters set and find optimal tuning parameters(for your specific problems) via genetic algorithm.

Script can be run in 3 modes:
* [single_run]Runs more precise analysis for one parameters set
* [search_params]Uses genetic algorithm to find optimal parameters set
* [auc_plot]Runs multiple simulations with different set of parameters and draws plot of result of classification model achieved by these parameters

Files config_auc_plot.R, config_search_params.R, config_single.R contain run configuration for aforementioned modes.

Results are dumped in the 'build' directory with timestamps.

In project there is contained sample test data - 'Student alcohol consumption'. Source: https://www.kaggle.com/uciml/student-alcohol-consumption. 

Needed packages:
```R
install.packages("ggplot2")
install.packages("randomForest")
install.packages("pROC")
install.packages("GA")
install.packages("ROCR")
```

Run:
Makefile contains intructions to run script in different modes. Attached sample configuration files are templates. You can change them freely.
```sh
$ make single_run
```
