#!/usr/bin/env Rscript
suppressPackageStartupMessages(library("optparse"))
suppressPackageStartupMessages(library("uciekaj"))

# Parse command line:
option_list <- list(
    make_option(c("-v", "--verbose"), action="store_true", default=TRUE,
        help="Print extra output [default]"),
    make_option(c("-q", "--quietly"), action="store_false", default=FALSE,
        dest="verbose", help="Print little output"),
    make_option(c("-c", "--control"), default='./run.yaml',
        dest="control", help="Control file (YAML)."),
    make_option(c("-o", "--output"), default=getwd(),
        dest="output", help="Directory for output files.")
)
opt <- parse_args(OptionParser(option_list=option_list))

# Run
output <- jags.file(opt[['control']])

# Save output
saveRDS(
    object = output[['cntrl']],
    file = file.path(output[['out']], 'cntrl-final.rds')
)
saveRDS(
    object = output[['sample']],
    file = file.path(output[['out']], 'sample.rds')
)

saveRDS(
    object = output[['timing']],
    file = file.path(output[['out']], 'timing.rds')
)

