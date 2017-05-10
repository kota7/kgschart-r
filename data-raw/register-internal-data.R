library(yaml)

rds_files <- c(
  yasis_predictor = 'train/outcome/yaxis-predictor.rds'
)

yml_files <- c(
  yaxis_config = 'train/outcome/yaxis-config.yml'
)

x <- lapply(rds_files, readRDS)
y <- lapply(yml_files, yaml.load_file)

#devtools::use_data()
