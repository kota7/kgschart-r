
yaxis_classifier <- readRDS('train/outcome/yaxis-classifier.rds')


devtools::use_data(yaxis_classifier, internal=TRUE, overwrite=TRUE)
