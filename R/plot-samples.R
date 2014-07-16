plot_sample_trace <- function(sample_matrix, type=geom_line()) {
	sample_melt <- melt_sample_matrix(sample_matrix)
	cn <- colnames(sample_melt)
	pl <- ggplot(data=sample_melt, aes(x=iteration, y=value, colour=factor(chain)))
	pl <- pl + type
	cnr <- cn[!(cn %in% c('chain','iteration','value'))]
	n_levels <- sapply(cnr, function(x) length(unique(sample_melt[[x]])))
	max_levels <- names(n_levels)[n_levels == max(n_levels)][1]
	other_levels <- names(n_levels)[names(n_levels) != max_levels]
	pl <- pl + facet_grid(
		facets = paste0(max_levels, "~", paste(other_levels, collapse='+'),
		scales='free_y')
	)
	return(pl)
}

