library(fs)

file_copy("image/AR.jpg", "image/test/AR_cpied.jpg")


files <- list.files("image/", "(jpg)|(JPG)", full.names = T)

i = 1
for (f in files) {
  f_new <- paste0("image/test/", i, ".jpg")
  
  file_copy(f, f_new)
  
  i <- i + 1
}