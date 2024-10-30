library(dplyr)

setwd("data/lc")

files.in.dir <- list.files()
files.to.keep <- (read.csv("../lc_filenames.csv") %>%
  dplyr::select(FILENAME))
files.to.remove <- list(files.in.dir[!files.in.dir %in% files.to.keep$FILENAME])
str(files.to.remove)
do.call(unlink, files.to.remove)

setwd("../ndvi")

files.in.dir <- list.files()
files.to.keep <- (read.csv("../ndvi_filenames.csv") %>%
                    dplyr::select(FILENAME))
files.to.remove <- list(files.in.dir[!files.in.dir %in% files.to.keep$FILENAME])
str(files.to.remove)
do.call(unlink, files.to.remove)
