
yaxis_classifier      <- readRDS('train/outcome/yaxis-classifier.rds')
caption_ja_classifier <- readRDS('train/outcome/caption-ja-classifier.rds')
caption_en_classifier <- readRDS('train/outcome/caption-en-classifier.rds')


devtools::use_data(yaxis_classifier,
                   caption_ja_classifier,
                   caption_en_classifier,
                   internal=TRUE, overwrite=TRUE)
