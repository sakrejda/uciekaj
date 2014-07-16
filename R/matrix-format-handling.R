read_samples <- function(path, pattern) {
	sample_files <- dir(path=path, pattern=pattern, full.names=TRUE)
	o <- lapply(sample_files,readRDS)
	return(o)
}

merge_samples <- function(sample_list, along='chain') {
	f <- function(...) abind(..., along=length(dim(list(...)[[1]]))-1)
	if (along == 'chain') {
		o <- do.call(what=mapply, args=c(list(abind), sample_list))
	} else if (along == 'iteration') {
		o <- do.call(what=mapply, args=c(list(f    ), sample_list))
	} else { stop("Along must be between 0 and D.") }
	o <- lapply(o, function(x) {class(x) <- 'mcarray'; return(x)})
	o <- lapply(o, function(x) {dimnames(x) <- NULL; return(x)})
}

as_jags_init <- function(estimates) {
  dimension_counts <- aggregate(formula=j ~ parameter, data=estimates, FUN=length)
  
  scalars <- dimension_counts[['parameter']][dimension_counts[['j']] == 1]
  scalar_estimates <- estimates[estimates[['parameter']] %in% scalars,]
  scalar_var_noms <- scalar_estimates[['parameter']]
  scalar_var_values <- list(scalar_estimates[['50%']])
  names(scalar_var_values) <- scalar_var_noms

  vectors <- unique(dimension_counts[['parameter']][dimension_counts[['j']] > 1])
  vector_var_values <- list()
  for ( i in length(vectors) ) {
    vector_name <- vectors[i]
    vector_var_values[[vector_name]] <- estimates[
      estimates[['parameter']] == vector_names,'50%']
  }

  init_values <- c(scalar_var_values, vector_var_values)

  return(init_values)
}
 
split_samples <- function(sample_matrix, sample_summary, mask_list) {
  o <- list()
  for ( i in seq_along(mask_list)) {
    nom <- names(mask_list)[i]
    o[[nom]] <- list(
      matrix = sample_matrix[mask_list[[i]],],
      summary = sample_summary[mask_list[[i]],]
    )
    attr(o[[nom]][['matrix']],'id') <- attr(sample_matrix,'id')[mask_list[[i]],]
  }
  return(o)
}

melt_sample_matrix <- function(sample_matrix) {
  parameter_info <- attr(sample_matrix, 'id')
	rownames(parameter_info) <- NULL
  wide_format <- cbind(parameter_info,as.data.frame(as.numeric(sample_matrix)))
  sample_melt <- melt(data=wide_format, id.vars=c('parameter','j','k','chain'))
  sample_melt[['variable']] <- as.numeric(as.character(sample_melt[['variable']]))
  names(sample_melt)[names(sample_melt) == 'variable'] <- 'iteration'
  return(sample_melt)
}




