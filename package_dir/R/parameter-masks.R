## Helps handling standardized form output.

generate_parameter_masks <- function(parameters, parameter_patterns, expr=expr) {
  parameter_masks <- lapply(
    X=parameter_patterns,
    FUN=function(x, parameters) grepl(pattern=x, x=parameters),
    parameters=parameters
  )
  names(parameter_masks) <- names(parameter_patterns)
  parameter_masks <- list2env(parameter_masks,parent=.GlobalEnv)
  new_masks <- eval(expr,parameter_masks)
  parameter_masks <- c(new_masks, as.list(parameter_masks))
  return(parameter_masks)
}





