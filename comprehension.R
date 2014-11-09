Compr <- function(f, ...){
    for (arg in ...)        
        inspect(arg)
    array(NA, sapply(..., length))    
}


