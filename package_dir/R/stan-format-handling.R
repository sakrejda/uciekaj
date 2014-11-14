process_stan_names <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  sx <- strsplit(x=x, split='\\.')
  d <- sapply(sx,length) - 1
  max_d <- max(d)
  par_names <- lapply(sx, `[`, 1)

  indexing <- mapply(
    FUN=function(sx, d, max_d) {
      indexing <- rep(NA,max_d)
      if ( d != 0 ) {
        indexing[1:d] <- sx[2:(d+1)]
      }
      return(indexing)
    },
    sx = sx,
    d = d,
    MoreArgs = list(max_d = max_d),
    SIMPLIFY=FALSE
  )

  par_names <- mapply(c, par_names, indexing, SIMPLIFY=FALSE)
  par_names <- data.frame(do.call(what=rbind, args=par_names))
  names(par_names) <- c('parameter', letters[1:max_d+8])
  par_names <- lapply(par_names, as.character)

  return(par_names)
}

read_stan_file <- function(file) {
  samples_df <- read.table(file=file, header=TRUE, sep=',', stringsAsFactors=FALSE, comment.char='#')
  samples_mat <- as.matrix(samples_df)
  rownames(samples_mat) <- 1:nrow(samples_mat)
  return(samples_mat)
}

get_optim_output <- function(file='file-data.txt', pipeline=NULL) {
  paths <- get_paths(file=file)
  optim_paths <- paths[
    grepl(pattern='optim', x=names(paths), fixed=TRUE) &
    grepl(pattern='output', x=names(paths), fixed=TRUE) ]
  optim_estimates <- lapply(optim_paths, read_stan_file)
  optim_estimates <- do.call(what=rbind, args=optim_estimates)
  rownames(optim_estimates) <- 1:nrow(optim_estimates)
  optim_estimates <- optim_estimates %>% as.data.frame %>%
    pipeline_data_transformation(
    pipeline=pipeline, final_names='all', multipath=TRUE)
  optim_estimates[['try']] <- 1:5
  optim_estimates <- melt(optim_estimates, id.vars='try')
  colnames(optim_estimates) <- c('try','parameters','value')
  optim_estimates[['parameters']] <- as.character(optim_estimates[['parameters']])
  return(optim_estimates)
}

get_sampler_output <- function(file='file-data.txt', pipeline=NULL) {
  paths <- get_paths(file=file)
  sampler_paths <- paths[
    grepl(pattern='sampler', x=names(paths), fixed=TRUE) &
    grepl(pattern='output', x=names(paths), fixed=TRUE) ]
  sampler_estimates <- lapply(sampler_paths, read_stan_file)
  sampler_estimates <- sampler_estimates %>% lapply(as.data.frame) %>%
    mapply(FUN=function(i, df) {df[['chain']] <- i; return(df)},
      i = 1:length(sampler_estimates), df = ., SIMPLIFY=FALSE) %>%
    lapply(FUN=function(df) {df[['iteration']] <- 1:nrow(df); return(df)}) %>%
    lapply(FUN=function(df) {
      pipeline_data_transformation(data=df, pipeline=pipeline, final_names='all', multipath=TRUE)
    }) %>% lapply(melt, id.vars=c('chain','iteration'))
  sampler_estimates <- do.call(what=rbind, args=sampler_estimates)
  sampler_estimates[['parameters']] <- as.character(sampler_estimates[['variable']])
  sampler_estimates[['variable']] <- NULL
  return(sampler_estimates)
}

pairs <- function(x) {
  pairs <- expand.grid(x,x) %>% apply(1,sort) %>% t %>% unique
  pairs <- pairs[pairs[,1] != pairs[,2],]
  return(pairs)
}

pull_vectors <- function(s_matrix, s_masks) {
  o <- list()
  for ( i in 1:length(s_masks) ) {
    par <- names(s_masks)[i]
    o[[par]] <- s_matrix[,s_masks[[i]]]
  }
  return(o)
}

pull_indexing <- function(s_names, s_masks) {
  o <- list()
  for ( i in 1:length(s_masks) ) {
    par <- names(s_masks)[i]
    o[[par]] <- lapply(s_names,`[`, s_masks[[i]])
  }
  return(o)
}

process_stan_file <- function(file) {
  results <- read_stan_file(file)
  stan_names <- process_stan_names(colnames(results))
  return(list(results=results, names=stan_names))
}

summarize_stan_estimates <- function(results, FUN=quantile, ...) {
  summary <- data.frame(
    parameter = results[['names']][['parameter']],
    stringsAsFactors=FALSE
  )
  if (length(results[['names']]) > 1) {
    for ( i in 2:length(results[['names']]) ) {
      summary[[names(results[['names']])[i]]] <- results[['names']][[i]]
    }
  }

  if (!is.null(FUN)) {
    summary <- data.frame(summary, t(apply(results$results, 2, FUN, ...)), check.names=FALSE)
  } else {
    vv <- matrix(data=results$results, ncol=1)
    colnames(vv) <- 'optimum'
    summary <- data.frame(summary, vv)
  }
  return(summary)
}


