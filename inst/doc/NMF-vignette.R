## ----pkgmaker_preamble, echo=FALSE, results='asis'----------------------------
library(NMF)
latex_preamble()
if(!requireNamespace("Biobase")) BiocManager::install("Biobase")

## ----bibliofile, echo=FALSE, results='asis'-----------------------------------
latex_bibliography('NMF')

## ----options, echo=FALSE------------------------------------------------------
set.seed(123456)
library(knitr)

# Helper functions:
hook_try <- function(before, options, envir){
	
	.try_defined <- FALSE
	
	# remove hacked version of try
	if( !before ){
		if( .try_defined && exists('try', envir = envir, inherits = FALSE) ){
			remove(list = 'try', envir = envir)
		}
		.try_defined <<- FALSE
		return(invisible())
	}
	
	if( !is.null(options$try) ){
		
		# signal
		do.signal <- isFALSE(options$try)
		if( isManualVignette() && isTRUE(options$try) ){
			do.signal <- TRUE
		}
		# define hacked version of try()
		.try <- try_message(do.signal)
		assign('try', .try, envir)
		.try_defined <<- TRUE
	}
}

chunkOutputHook <- function(name, hook, type = c('output', 'source', 'chunk')){
	type <- match.arg(type)
	function(){
		
		if( !requireNamespace('knitr', quietly = TRUE) ) 
			stop("Package 'knitr' is required to setup knit hook '", name, "'")
		
		.hook_bkp <- NULL
		function(before, options, envir){
			# do nothing if the option is not ON
			if( is.null(options[[name]]) ) return()
			
			# set/unset hook
			if( before ){
				# store current hook function
				if( is.null(.hook_bkp) ) .hook_bkp <<- knitr::knit_hooks$get(type)
				
				# define hook wrapper
				hook_wrapper <- function(x, options){
					res <- .hook_bkp(x, options)
					hook(res, options)
				}
				
				args <- list()
				args[[type]] <- hook_wrapper
				do.call(knitr::knit_hooks$set, args)
			}else{
				args <- list()
				args[[type]] <- .hook_bkp
				do.call(knitr::knit_hooks$set, args)
				.hook_bkp <<- NULL
			}
		}
	}
}


hook_backspace <- chunkOutputHook('backspace', 
		function(x, options){
			if( !isTRUE(options$backspace) ) x
			str_bs(x)
		}
)

str_bs <- function(x){
	# remove leading backspaces
	x <- gsub("^\b+", "", x)
	# remove backspaces at beginning of line
	x <- gsub("\n\b+", '\n', x)
	while( length(grep('\b', x, fixed = TRUE)) ) 
		x <- gsub('[^\n\b][\b]', '', x)
	
	x
}

isManualVignette <- function(){
	isTRUE(getOption('R_RUNNING_MANUAL_VIGNETTE'))
}

try_message <- function(signal = FALSE){
	function(expr){
		tryCatch(expr, error = function(e){
					if( signal ) message(e)
					else message('Error: ', conditionMessage(e))
					invisible()
				})
	}
}

knit_hooks$set(try = hook_try, backspace = hook_backspace())

## ----load_library, echo=FALSE, include=FALSE----------------------------------
# Load
library(NMF)
# limit number of cores used
nmf.options(cores = 2)

## ----load_library_fake, eval=FALSE--------------------------------------------
#  # Install
#  install.packages('NMF')
#  # Load
#  library(NMF)

## ----updateObject, eval=FALSE-------------------------------------------------
#  # eg., load from some RData file
#  load('object.RData')
#  # update class definition
#  object <- nmfObject(object)

## ----features, echo=FALSE-----------------------------------------------------
nalgo <- length(nmfAlgorithm())
nseed <- length(nmfSeed())

## ----nmfAlgorithm-------------------------------------------------------------
# list all available algorithms
nmfAlgorithm()
# retrieve a specific algorithm: 'brunet' 
nmfAlgorithm('brunet')
# partial match is also fine
identical(nmfAlgorithm('br'), nmfAlgorithm('brunet')) 

## ----nmfSeed------------------------------------------------------------------
# list all available seeding methods
nmfSeed()
# retrieve a specific method: 'nndsvd' 
nmfSeed('nndsvd')
# partial match is also fine
identical(nmfSeed('nn'), nmfSeed('nndsvd'))

## ----show_Rversions-----------------------------------------------------------
nmfAlgorithm(all=TRUE)

# to get all the algorithms that have a secondary R version
nmfAlgorithm(version='R')

## ----perftable_setup, cache=TRUE----------------------------------------------
# retrieve all the methods that have a secondary R version
meth <- nmfAlgorithm(version='R')
meth <- c(names(meth), meth)
meth

if(requireNamespace("Biobase", quietly=TRUE)){
# load the Golub data
data(esGolub)

# compute NMF for each method
res <- nmf(esGolub, 3, meth, seed=123456)

# extract only the elapsed time
t <- sapply(res, runtime)[3,]
}

## ----perftable, echo=FALSE, results='asis'------------------------------------
# speed-up
m <- length(res)/2
su <- cbind( C=t[1:m], R=t[-(1:m)], Speed.up=t[-(1:m)]/t[1:m])

library(xtable)
xtable(su, caption='Performance speed up achieved by the optimized C++ implementation for some of the NMF algorithms.', label='tab:perf')

## ----citations, eval=FALSE----------------------------------------------------
#  # plain text
#  citation('NMF')
#  
#  # or to get the bibtex entries
#  #toBibtex(citation('NMF'))

## ----esGolub------------------------------------------------------------------
if(requireNamespace("Biobase", quietly=TRUE)){
data(esGolub)
esGolub
esGolub <- esGolub[1:200,]

# remove the uneeded variable 'Sample' from the phenotypic data
esGolub$Sample <- NULL
}

## ----algo_default, cache=TRUE-------------------------------------------------
if(requireNamespace("Biobase", quietly=TRUE)){
# default NMF algorithm
res <- nmf(esGolub, 3)
}

## ----single_show--------------------------------------------------------------
if(requireNamespace("Biobase", quietly=TRUE)){
res 
}

## ----single_show_model--------------------------------------------------------
if(requireNamespace("Biobase", quietly=TRUE)){
fit(res)
}

## ----single_show_estimate-----------------------------------------------------
if(requireNamespace("Biobase", quietly=TRUE)){
V.hat <- fitted(res)
dim(V.hat)
}

## ----singlerun_summary--------------------------------------------------------
if(requireNamespace("Biobase", quietly=TRUE)){
summary(res)

# More quality measures are computed, if the target matrix is provided: 
summary(res, target=esGolub)
}

## ----singlerun_summary_factor-------------------------------------------------
if(requireNamespace("Biobase", quietly=TRUE)){
summary(res, class=esGolub$Cell)
}

## ----get_matrices-------------------------------------------------------------
if(requireNamespace("Biobase", quietly=TRUE)){
# get matrix W
w <- basis(res)
dim(w)

# get matrix H
h <- coef(res)
dim(h)
}

## ----subset-------------------------------------------------------------------
if(requireNamespace("Biobase", quietly=TRUE)){
# keep only the first 10 features
res.subset <- res[1:10,] 
class(res.subset)
dim(res.subset)
# keep only the first 10 samples 
dim(res[,1:10])
# subset both features and samples:
dim(res[1:20,1:10])
}

## ----single_extract-----------------------------------------------------------
if(requireNamespace("Biobase", quietly=TRUE)){
# only compute the scores
s <- featureScore(res)
summary(s)

# compute the scores and characterize each metagene
s <- extractFeatures(res)
str(s)
}

## ----algo_list----------------------------------------------------------------
nmfAlgorithm()

## ----algo_lee, cache=TRUE-----------------------------------------------------
if(requireNamespace("Biobase", quietly=TRUE)){
# using Lee and Seung's algorithm
res <- nmf(esGolub, 3, 'lee')
algorithm(res)
}

## ----algo_ns, cache=TRUE------------------------------------------------------
if(requireNamespace("Biobase", quietly=TRUE)){
# using the Nonsmooth NMF algorithm with parameter theta=0.7
res <- nmf(esGolub, 3, 'ns', theta=0.7)
algorithm(res)
fit(res)
}

## ----algo_pe, cache=TRUE------------------------------------------------------
if(requireNamespace("Biobase", quietly=TRUE)){
# using the PE-NMF algorithm with parameters alpha=0.01, beta=1
res <- nmf(esGolub, 3, 'pe', alpha=0.01, beta=1)
res
}

## ----seed_list----------------------------------------------------------------
nmfSeed()

## ----seed, cache=TRUE---------------------------------------------------------
if(requireNamespace("Biobase", quietly=TRUE)){
res <- nmf(esGolub, 3, seed='nndsvd')
res
}

## ----seed_numeric, cache=TRUE-------------------------------------------------
if(requireNamespace("Biobase", quietly=TRUE)){
# single run and single numeric seed
res <- nmf(esGolub, 3, seed=123456)
showRNG(res)

# multiple runs and single numeric seed
res <- nmf(esGolub, 3, seed=123456, nrun=2)
showRNG(res)

# single run with a 6-length seed
res <- nmf(esGolub, 3, seed=rep(123456, 6))
showRNG(res)
}

## ----seed_WH------------------------------------------------------------------
if(requireNamespace("Biobase", quietly=TRUE)){
# initialize a "constant" factorization based on the target dimension
init <- nmfModel(3, esGolub, W=0.5, H=0.3)
head(basis(init))

# fit using this NMF model as a seed
res <- nmf(esGolub, 3, seed=init)
}

## ----algo_multirun, cache=TRUE------------------------------------------------
if(requireNamespace("Biobase", quietly=TRUE)){
res.multirun <- nmf(esGolub, 3, nrun=5)
res.multirun
}

## ----multirun_keep, cache=TRUE------------------------------------------------
if(requireNamespace("Biobase", quietly=TRUE)){
# explicitly setting the option keep.all to TRUE
res <- nmf(esGolub, 3, nrun=5, .options=list(keep.all=TRUE))
res
}

## ----multirun_keep_alt, eval=FALSE--------------------------------------------
#  if(requireNamespace("Biobase", quietly=TRUE)){
#  # or using letter code 'k' in argument .options
#  nmf(esGolub, 3, nrun=5, .options='k')
#  }

## ----parallel_multicore_alt, eval=FALSE---------------------------------------
#  if(requireNamespace("Biobase", quietly=TRUE)){
#  # the default call will try to run in parallel using all the cores
#  # => will be in parallel if all the requirements are satisfied
#  nmf(esGolub, 3, nrun=5, .opt='v')
#  
#  # request a certain number of cores to use => no error if not possible
#  nmf(esGolub, 3, nrun=5, .opt='vp8')
#  
#  # force parallel computation: use option 'P'
#  nmf(esGolub, 3, nrun=5, .opt='vP')
#  
#  # require an improbable number of cores => error
#  nmf(esGolub, 3, nrun=5, .opt='vP200')
#  }

## ----mpi, eval=FALSE----------------------------------------------------------
#  # file: mpi.R
#  
#  if(requireNamespace("Biobase", quietly=TRUE)){
#  ## 0. Create and register an MPI cluster
#  library(doMPI)
#  cl <- startMPIcluster()
#  registerDoMPI(cl)
#  library(NMF)
#  
#  # run on all workers using the current parallel backend
#  data(esGolub)
#  res <- nmf(esGolub, 3, 'brunet', nrun=n, .opt='p', .pbackend=NULL)
#  
#  # save result
#  save(res, file='result.RData')
#  
#  ## 4. Shutdown the cluster and quit MPI
#  closeCluster(cl)
#  mpi.quit()
#  }

## ----force_seq, cache=TRUE, backspace = TRUE----------------------------------
if(requireNamespace("Biobase", quietly=TRUE)){
# parallel execution on 2 cores (if possible)
res1 <- nmf(esGolub, 3, nrun=5, .opt='vp2', seed=123)

# or use the doParallel with single core
res2 <- nmf(esGolub, 3, nrun=5, .opt='vp1', seed=123)

# force sequential computation by sapply: use option '-p' or .pbackend=NA  
res3 <- nmf(esGolub, 3, nrun=5, .opt='v-p', seed=123)
res4 <- nmf(esGolub, 3, nrun=5, .opt='v', .pbackend=NA, seed=123)

# or use the SEQ backend of foreach: .pbackend='seq'
res5 <- nmf(esGolub, 3, nrun=5, .opt='v', .pbackend='seq', seed=123)

# all results are all identical
nmf.equal(list(res1, res2, res3, res4, res5))
}

## ----estimate_rank, cache=TRUE------------------------------------------------
if(requireNamespace("Biobase", quietly=TRUE)){
# perform 10 runs for each value of r in range 2:6
estim.r <- nmf(esGolub, 2:6, nrun=10, seed=123456)
}

## ----estimate_rank_plot, fig.width=10, fig.height=6---------------------------
if(requireNamespace("Biobase", quietly=TRUE)){
plot(estim.r)
}

## ----estimate_rank_hm_include, fig.width=14, fig.height=7, fig.keep='last'----
if(requireNamespace("Biobase", quietly=TRUE)){
consensusmap(estim.r, annCol=esGolub, labCol=NA, labRow=NA)
}

## ----estimate_rank_random, cache=TRUE, fig.width=10, fig.height=6, fig.keep='last'----
if(requireNamespace("Biobase", quietly=TRUE)){
# shuffle original data
V.random <- randomize(esGolub)
# estimate quality measures from the shuffled data (use default NMF algorithm)
estim.r.random <- nmf(V.random, 2:6, nrun=10, seed=123456)
# plot measures on same graph
plot(estim.r, estim.r.random)
}

## ----multimethod, cache=TRUE--------------------------------------------------
if(requireNamespace("Biobase", quietly=TRUE)){
# fit a model for several different methods 
res.multi.method <- nmf(esGolub, 3, list('brunet', 'lee', 'ns'), seed=123456, .options='t')
}

## ----compare------------------------------------------------------------------
if(requireNamespace("Biobase", quietly=TRUE)){
compare(res.multi.method)

# If prior knowledge of classes is available
compare(res.multi.method, class=esGolub$Cell)
}

## ----errorplot_compute, cache=TRUE--------------------------------------------
if(requireNamespace("Biobase", quietly=TRUE)){
# run nmf with .option='t'
res <- nmf(esGolub, 3, .options='t')
# or with .options=list(track=TRUE)
}

## ----errorplot, out.width="0.5\\textwidth", fig.show='hold'-------------------
if(requireNamespace("Biobase", quietly=TRUE)){
plot(res)
plot(res.multi.method)
}

## ----heatmap_coef_basis_inc, fig.width=14, fig.height=7, fig.keep='last'------
if(requireNamespace("Biobase", quietly=TRUE)){
layout(cbind(1,2))
# basis components
basismap(res, subsetRow=TRUE)
# mixture coefficients
coefmap(res)
}

## ----heatmap_consensus_inc, out.width="0.49\\textwidth", crop=TRUE, echo=1:2----
if(requireNamespace("Biobase", quietly=TRUE)){
# The cell type is used to label rows and columns 
consensusmap(res.multirun, annCol=esGolub, tracks=NA)
plot(1:10)
f2 <- fig_path("2.pdf")
}

## ----hack_consensus, include=FALSE--------------------------------------------
if(requireNamespace("Biobase", quietly=TRUE)){
file.copy('consensus.pdf', f2, overwrite=TRUE)
}

## ----custom_algo_sig----------------------------------------------------------
my.algorithm <- function(x, seed, param.1, param.2){
	# do something with starting point
	# ...
	
	# return updated starting point
	return(seed)
}

## ----custom_algo--------------------------------------------------------------
my.algorithm <- function(x, seed, scale.factor=1){
	# do something with starting point
	# ...
	# for example: 
	# 1. compute principal components	
	pca <- prcomp(t(x), retx=TRUE)
	
	# 2. use the absolute values of the first PCs for the metagenes
	# Note: the factorization rank is stored in object 'start'	
	factorization.rank <- nbasis(seed)
	basis(seed) <- abs(pca$rotation[,1:factorization.rank])
	# use the rotated matrix to get the mixture coefficient
	# use a scaling factor (just to illustrate the use of extra parameters)
	coef(seed) <- t(abs(pca$x[,1:factorization.rank])) / scale.factor
	
	# return updated data
	return(seed)
}

## ----define_V-----------------------------------------------------------------
n <- 50; r <- 3; p <- 20
V <-syntheticNMF(n, r, p)

## ----custom_algo_run----------------------------------------------------------
nmf(V, 3, my.algorithm, scale.factor=10)

## ----custom_algo_run_obj------------------------------------------------------
# based on Kullback-Leibler divergence
nmf(V, 3, my.algorithm, scale.factor=10, objective='KL')
# based on custom distance metric
nmf(V, 3, my.algorithm, scale.factor=10
	, objective=function(model, target, ...){
            ( sum( (target-fitted(model))^4 ) )^{1/4} 
		}
)

## ----custom_algo_run_mixed, error = TRUE, try = TRUE--------------------------
# put some negative input data 
V.neg <- V; V.neg[1,] <- -1;

# this generates an error
try( nmf(V.neg, 3, my.algorithm, scale.factor=10) )

# this runs my.algorithm without error
nmf(V.neg, 3, my.algorithm, mixed=TRUE, scale.factor=10)

## ----nmf_models---------------------------------------------------------------
nmfModel()

## ----custom_algo_NMFoffset----------------------------------------------------
my.algorithm.offset <- function(x, seed, scale.factor=1){
	# do something with starting point
	# ...
	# for example: 
	# 1. compute principal components	
	pca <- prcomp(t(x), retx=TRUE)
	
	# retrieve the model being estimated
	data.model <- fit(seed)
	
	# 2. use the absolute values of the first PCs for the metagenes
	# Note: the factorization rank is stored in object 'start'	
	factorization.rank <- nbasis(data.model)
	basis(data.model) <- abs(pca$rotation[,1:factorization.rank])	
	# use the rotated matrix to get the mixture coefficient
	# use a scaling factor (just to illustrate the use of extra parameters)
	coef(data.model) <- t(abs(pca$x[,1:factorization.rank])) / scale.factor
	
	# 3. Compute the offset as the mean expression
	data.model@offset <- rowMeans(x)	
	
	# return updated data
	fit(seed) <- data.model
	seed
}

## ----custom_algo_NMFOffset_run------------------------------------------------
# run custom algorithm with NMF model with offset
nmf(V, 3, my.algorithm.offset, model='NMFOffset', scale.factor=10)

## ----custom_seed--------------------------------------------------------------

# start: object of class NMF
# target: the target matrix
my.seeding.method <- function(model, target){
	
	# use only the largest columns for W
	w.cols <- apply(target, 2, function(x) sqrt(sum(x^2)))
	basis(model) <- target[,order(w.cols)[1:nbasis(model)]]
	
	# initialize H randomly
	coef(model) <- matrix(runif(nbasis(model)*ncol(target))
						, nbasis(model), ncol(target))

	# return updated object
	return(model)
}

## ----custom_seed_run----------------------------------------------------------
nmf(V, 3, 'snmf/r', seed=my.seeding.method)

## ----options_algo, eval=1:6---------------------------------------------------
#show default algorithm and seeding method
nmf.options('default.algorithm', 'default.seed')

# retrieve a single option
nmf.getOption('default.seed')

# All options
nmf.options()

## ----print_options------------------------------------------------------------
nmf.printOptions()

## ----sessionInfo, echo=FALSE, results='asis'----------------------------------
toLatex(sessionInfo())

