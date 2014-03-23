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

as.matrix.mcarray <- function(x, use.attr=FALSE, ...) {
	d <- parse_dim(x)
	nr <- nrow(d[['index_range']])
	nc <- d[['n_iter']]	
	
	if (!use.attr) { 
		nc <- nc + ncol(d[['index_range']])
		o <- matrix(data=NA, nrow=nr, ncol=nc)
		fsc <- ncol(d[['index_range']])+1
		o[,1:ncol(d[['index_range']])] <- as.matrix(d[['index_range']])
		o <- apply(X=o, MARGIN=1, FUN = function(r, fsc) {
			r[fsc:length(r)] <- do.call(
				what="[", 
				args= c(
					list(x=x), 
					as.list(r[2:(fsc-1)]), 
					list(1:d[['n_iter']]), 
					list(r[1]) 
				)
			)
			return(r)
		}, fsc = fsc)
		o <- t(o)
		colnames(o) <- c(
			colnames(d[['index_range']]),
			1:d[['n_iter']]
		)
	} else {
		o <- matrix(data=NA, nrow=nr, ncol=nc)
		o <- sapply(X=1:nrow(o), FUN=function(i) {
			o[i,] <- do.call(
				what='[', 
				args=c(
					list(x=x),
					d[['index_range']][i,2:ncol(d[['index_range']])],
					list(1:d[['n_iter']]),
					d[['index_range']][i,1]
				)
			)
		})
		o <- t(o)
		colnames(o) <- 1:d[['n_iter']]
		attr(x=o, which="id") <- d[['index_range']]
	}
	return(o)
}

as.data.frame.mcarray <- function(x, ...) {
	require(reshape)
	d <- parse_dim(x)
	smat <- as.matrix(x)
	sdf <- melt(data=as.data.frame(smat), id.vars=colnames(d[['index_range']]))
	sdf[['variable']] <- as.numeric(as.character(sdf[['variable']]))
	return(sdf)
}

pairs.mcarray <- function(x, ...) {
	x <- as.matrix(x)
	d <- parse_dim(s)
	xx <- x[,(ncol(d[['index_range']])+1):ncol(x)]
	rownames(xx) <- sapply(
		X=as.data.frame(t(x[,1:ncol(d[['index_range']])])), paste, collapse='-')
	o <- pairs(t(xx), ...)
	return(o)
}

cor.mcarray <- function(x, ...) {
	d <- parse_dim(x)
	x <- as.matrix(x)
	xx <-  x[,(ncol(d[['index_range']])+1):ncol(x)]
	rownames(xx) <- sapply(
    X=as.data.frame(t(x[,1:ncol(d[['index_range']])])), paste, collapse='-')
  o <- cor(t(xx), ...)
	return(o)
}



mcarray.list.2.matrix <- function(x) {
	test <- all(lapply(x, class) == 'mcarray')
	if (!test) stop("Function only applies to list of mcarrays.")
	noms <- names(x)
	matrices <- lapply(X=x, FUN=as.matrix, use.attr=TRUE)
	noms <- sapply(noms, function(nom) {
		nr <- nrow(matrices[[nom]])
		return(rep(nom,nr))
	})
	noms <- unlist(noms)
	attribs <- lapply(X=matrices, FUN=function(x) {
		at <- attr(x=x, which='id')
	})
	attribs <- do.call(what=rbind, args=attribs)
	attribs[['parameter']] <- noms
	rownames(attribs) <- 1:nrow(attribs)
	matrices <- do.call(what=rbind, args=matrices)
	attr(x=matrices, which='id') <- attribs
	return(matrices)
}

mcarray.list.2.estimates <- function(x) {
	mat <- mcarray.list.2.matrix(x)
	ids <- attr(x=mat, which='id')
	autoc <- sapply(apply(mat,1,acf, plot=FALSE, lag.max=1), function(x) { x$acf[2] })
	es <- apply(mat,1,function(x) {effectiveSize(as.mcmc(x))})
	est <- t(apply(mat, 1, quantile, probs=c(0.025, 0.5, 0.975)))
	estimates <- cbind(ids, data.frame(acf=autoc, sample_size=es), est)
	return(estimates)
}



mcarray.list.2.cor <- function(x, to.data.frame=FALSE) {
	x = mcarray.list.2.matrix(x)
	cmat <- cor(t(x))
	noms <- c(by(data=attr(x,'id'), INDICES=1:nrow(attr(x,'id')),
							 FUN=paste, sep='-', collapse='-', simplify=TRUE))
	colnames(cmat) <- noms
	rownames(cmat) <- noms
	if(to.data.frame) {
		cmat[upper.tri(cmat)] <- NA
		cmat <- melt(cmat)
		cmat <- cmat[!is.na(cmat[['value']]),]
		cmat <- cmat[order(abs(cmat[['value']]), decreasing=TRUE),]
		names(cmat) <- c('A','B','COR')
		cmat <- cmat[cmat[['A']] != cmat[['B']],]
	}
	return(cmat)
}

