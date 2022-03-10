# This file contains an example function structure with descriptions followed by a clean template that is more useful for copying/pasting

# You can have one or more functions in a single .R file but the following conventions are typically followed:
  # If you include multiple functions in a file they should have related functionality
  # File with single functions typically share the same name as the function, this also makes it easier to find functions in R packages outside of R/RStudio.

##### R function documentation description #####
#' @title This is the help documentation title and should be short
#' @description Provide a longer description detailing what your function does and any general comments on use.
#' 
#' @param PARAMETER_NAME Then provide a description of the parameter including the default, default = NULL.
#' @param ANOTHER_PARAMETER A different (boolean) parameter, default = TRUE.
#' 
#' @return A short description of the object(s) returned by a function, if you have a large number of parameters or returns you can add sections or lists as below:
#' @section SECTION TITLE HERE
#' Stuff you want in the section. This could be a list like so:
#' \itemize{
#'   \item{First thing in list - description of first thing in list, structure this info however you want}
#' }
#' 
#' @examples 
#' exampleFunction(PARAMETER_NAME = "This is not a drill", ANOTHER_PARAMETER = TRUE)
#' 
#' @export

exampleFunction <- function(PARAMETER_NAME = NULL,
                            ANOTHER_PARAMETER = TRUE){
  
  # PUT CODE HERE!!!!
  
  # Don't forget to provide the return object(s) documented above
  return()
}

# Other useful tidbits
  # @export means this documentation will show up in your R package and won't be "behind the scenes"

  # You don't have to provide default parameters for parameters but setting them to NULL means that 
  # if you don't intentionally provide information your function won't work which can sometimes be 
  # good to avoid accidentally using data by the same name in your environment.

  # If you don't include the parameters somewhere in this code you will get a warning that an 
  # argument was provided but not used



##### Clean template for copying #####
#' @title 
#' @description 
#' 
#' @param 
#' 
#' @return 
#' 
#' @examples 
#' 
#' @export

FUNCTION_NAME <- function(){

  return()
}

