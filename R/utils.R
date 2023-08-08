#' Get OS name
#' 
#' @return  A string of OS name
#' @examples
#' get_os()
#' 
#' @export
get_os <- function(){
  switch(Sys.info()["sysname"],
    "Windows" = "win",
     "Linux"  = "linux",
                "mac"
  )
}
