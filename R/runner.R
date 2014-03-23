jags.run <- function(
    file = '',
    data = NULL,
    inits = NULL,
    variable.names = NULL,
    n.chains = 1,
    n.adapt = 100,
    n.iter = 100,
    n.samples = 100
) {
    if ( file == '' ) {
        stop("No model file specified.")
    }
    if ( is.null(data)           ) {
        warning("No data specified.")
    }
    if ( is.null(inits)          ) {
        warning("No initial values specified.")
    }
    if ( is.null(variable.names)    ) {
        warning("No variables are being monitored.")
    }
    if ( n.chains < 1 ) {
        stop("At least one chain must be run.")
    }
    if ( n.adapt < 100 ) {
        warning("Fewer than 100 burn-in iterations.")
    }
    if ( n.iter < 100 ) {
        warning("Fewer than 100 sampling iterations.")
    }
    thin <- round(n.iter / n.samples)
    if ( thin < 1 ) {
        stop("Not enough iterations for the requested number of samples.")
    }
    adaptTime <- system.time(
        expr = {
            if (is.null(inits)) {
                mod <- jags.model(
                    file = file,
                    data = data,
                    n.chains = n.chains,
                    n.adapt = n.adapt
                )
            } else {
                mod <- jags.model(
                    file = file,
                    data = data,
                    inits = inits,
                    n.chains = n.chains,
                    n.adapt = n.adapt
                )
            }
        }
    )
    sampleTime <- system.time(
        expr = {
            samples <- jags.samples(
                model = mod,
                variable.names = variable.names,
                n.iter = n.iter,
                thin = thin,
                progress.bar = 'text'
            )
        }
    )
    timing <- list( adapt = adaptTime, sample = sampleTime )
    out <- list(
        model = mod,
        sample = samples,
        timing = timing
    )
}

jags.env <- function(env) {
    out <- with(
        data = env,
        expr = {
            out <- jags.run(
                file = model.file,
                data = final.data,
                inits = final.inits,
                variable.names = monitor,
                n.chains = n.chains,
                n.adapt = n.adapt,
                n.iter = n.iter,
                n.samples = n.samples
            )
            return(out)
        }
    )
    return(out)
}

jags.cntrl <- function(cntrl) {
    if (!('jags_cntrl' %in% class(cntrl))) {
        stop("Argument 'cntrl' must be of class 'jags_cntrl'.")
    }
    if (!attr(x=cntrl, which='data')) {
        warning("Argument 'cntrl' not initialized, initializing.")
        cntrl <- init.cntrl(cntrl)
    }
    out <- jags.run(
        file = cntrl[['model.file']],
        data = cntrl[['final.data']],
        inits = cntrl[['final.inits']],
        variable.names = cntrl[['monitor']],
        n.chains = cntrl[['n.chains']],
        n.adapt = cntrl[['n.adapt']],
        n.iter = cntrl[['n.iter']],
        n.samples = cntrl[['n.samples']]
    )
    out[['cntrl']] <- cntrl
    return(list(out = out))
}

jags.file <- function(file) {
    if ( !is.character(file) ) {
        stop("Argument 'file' must be a character vector.")
    }
    cntrl <- read.cntrl(file)
    cntrl <- init.cntrl(cntrl)
    out <- jags.cntrl(cntrl)
    return(out)
}

jags.prep <- function(file, target) {
    if ( !file.exists(target) ) {
	dir.create(target, recursive=TRUE)
    }
    if ( !is.character(file) ) {
        stop("Argument 'file' must be a character vector.")
    }
    cntrl <- read.cntrl(file)
    cntrl <- init.cntrl(cntrl)

    file.copy(from=cntrl[['model.file']], to=file.path(target,basename(cntrl[['model.file']])))
    data_names <- names(cntrl[['final.data']])
    dump(
        list = data_names,
        file = file.path(target,'data.dump'),
        envir = as.environment(cntrl[['final.data']])
    )

    if (!is.null(cntrl[['final.inits']])) {
        init_names <- names(cntrl[['final.inits']])
        dump(
            list = init_names,
            file = file.path(target, 'init.dump'),
            envir = as.environment(cntrl[['final.inits']])
        )
    }

    thin <- round(cntrl[['n.iter']] / cntrl[['n.samples']])
    if ( thin < 1 ) {
        stop("Not enough iterations for the requested number of samples.")
    }
    monitor <- paste(
        "monitor ", cntrl[['monitor']],
        ", thin(", thin, ")\n", sep='')
    run_file <- c(
        paste(
            "model in ", "\"", basename(cntrl[['model.file']]), "\"", "\n",
            "data in ", "\"", "data.dump", "\"", "\n",
            "compile, nchains(", cntrl[['n.chains']], ")", "\n",
        sep=''),
        ifelse(is.null(cntrl[['final.inits']]),'',paste(
            "parameters in ", "\"", "init.dump", "\"", "\n",
        sep='')),
        paste(
            "initialize", "\n",
            "adapt ", cntrl[['n.adapt']], "\n",
	    "update 1", "\n",
        sep=''),
        monitor,
        paste(
            "update ", cntrl[['n.iter']], "\n",
            "coda *", "\n",
        sep='')
    )
    cat( file = file.path(target, "run.jags"), run_file)
}
