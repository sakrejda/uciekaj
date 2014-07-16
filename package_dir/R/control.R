read.cntrl <- function(file) {
    cntrl <- yaml.load_file(file)
    class(cntrl) <- c(class(cntrl),'jags_cntrl')
    attr(x=cntrl, which='data') <- FALSE
    return(cntrl)
}

init.cntrl <- function(cntrl, reinit=FALSE) {
    all_in <- all(
        c(  'inits.files','data.files',
            'data.dir','adapter.file' ) %in% names(cntrl)
    )
    if (!all_in) {
        stop("Control list (cntrl) does not contain all required elements.")
    }
    data <- NULL
    if (reinit) {
        attr(x=cntrl, which='data') <- FALSE
        cntrl[['data']] <- NULL
        cntrl[['inits']] <- NULL
    }
    if ( !attr(x=cntrl, which='data') ) {
        cntrl <- within( data=cntrl, expr = {
	    data <- list()
            for ( f in data.files ) {
                data[[f]] <- read.table(
                    file = paste(data.dir,f,sep='/')
                )
            }
	    inits <- list()
            for ( f in inits.files ) {
                inits[[f]] <- read.table(
                    file = paste(data.dir,f,sep='/')
                )
            }
        })
        adapt_env <- new.env()
        source(file=cntrl[['adapter.file']], local=adapt_env)
        cntrl[['final.data']]  <- with(data=adapt_env, data_adapter(cntrl))
        cntrl[['final.inits']] <- with(data=adapt_env, inits_adapter(cntrl))

        attr(x=cntrl, which='data') <- TRUE
    }
    return(cntrl)
}
