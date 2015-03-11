# Use RCurl to upload an RData file containing specific R objects
#
# Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
# Source: https://github.com/shaptonstahl/R
#
# Call with:
#   source("http://www.haptonstahl.org/R/postObjects/postObjects.R")

postObjects <- function(filename, save.to.URL, post.params=list(), ...) {
  # Post an RData file and some parameters to URL containing the objects listed in ...
  # Returns TRUE if successful, FALSE otherwise.
  # post.params should be a named list of key-value pairs to be POSTed.
  #
  # Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
  # Load using: source("http://www.haptonstahl.org/R/postObjects/postObjects.R")
  library(RCurl)
  temp.file.name <- tempfile()
  save(..., file=temp.file.name)
  post.successful <- FALSE
  for(i.try in 1:10) {
    post.response <- postForm(uri=save.to.URL, 
      uploadedfile=fileUpload(filename=temp.file.name), saveto=filename, .params=post.params)
    if( "success" == post.response ) {
      post.successful <- TRUE
      break
    }
  }
  file.remove(temp.file.name)
  return(post.successful)
}

# Example:
# x <- rnorm(1000)
# y <- list(x, banana=rexp(100))
# postObjects(filename="test3.RData", save.to.URL="http://mysite.edu/folder/save.php", post.params=list(pw="thepassword"), x, y)
# 
# Where save.php looks like:
# 
# <?php
# if('thepassword' == $_POST['pw']) {
#   $save_to_folder = 'results/';
#   $file_to_save = $save_to_folder . basename($_POST['saveto']);
#   if(move_uploaded_file($_FILES['uploadedfile']['tmp_name'], $file_to_save)) {
#     echo "success";
#   } else{
#     echo "Hello, world!";
#   }
# } else {
#   echo "Hello, world!";
# }
# ?>
