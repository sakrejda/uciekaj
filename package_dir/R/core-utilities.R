parse_dim <- function(s) {
	d <- dim(s)
	nd <- length(d)
	n_chains <- d[nd]
	n_iter <- d[nd-1]
	n_indexes <- nd - 2
	index_range <- list(chain=1:n_chains)
	for ( i in 1:n_indexes ) {
		index_range[[letters[i+9]]] <- 1:d[i]
	}
	o <- list(
		n_chains = n_chains,
		n_iter = n_iter,
		n_indexes = n_indexes,
		index_range = expand.grid(index_range)
	)
	return(o)
}

