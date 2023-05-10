# setting path to drop box folder
# to be read in by other scripts
# so path is only change in one place

# note--putting a couple of commonly used locations of dropbox
# (substitute your path there as needed). Alternatively it
# tries to automatically figure out where dropbox is located

path1 <- "C:/Users/ohler/Dropbox" # path to the dropbox folder (tim's computer)

path2 <- "~/Dropbox" # Martin's path 

path <- NULL # (substitute your path here as needed)
if (dir.exists(path1)) {
  path <- path1
  
} else if (dir.exists(path2)){
  path <- path2
  
} else {
  if(!dir.exists(path)) {
    stop('please set path to dropbox in dropbox_path.R script')
  }
}

