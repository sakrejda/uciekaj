#!/usr/bin/env Rscript
suppressPackageStartupMessages(library("optparse"))
suppressPackageStartupMessages(library("yaml"))
suppressPackageStartupMessages(library("rjags"))
suppressPackageStartupMessages(library("ecoPiffle"))
suppressPackageStartupMessages(library("RSQLite"))


option_list <- list(
    make_option(c("-v", "--verbose"), action="store_true", default=TRUE,
        help="Print extra output [default]"),
    make_option(c("-q", "--quietly"), action="store_false",
        dest="verbose", help="Print little output"),
    make_option(c("-c", "--control"), default='./run.yaml',
        dest="control", help="Control file (YAML)."),
    make_option(c("-o", "--output"), default='/tmp',
        dest="output", help="Directory for output files.")
)

opt <- parse_args(OptionParser(option_list=option_list))

yaml_file <- yaml.load_file(input=opt[['control']])


#1) Copy data script to output dir
#2) Copy model to output dir
#3) Run data script
#4) Run model with qsub
#5) Save output to data directory with saveRDS
#6) Save data and initial values to data directory with saveRDS



