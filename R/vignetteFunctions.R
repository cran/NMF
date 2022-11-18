#### Code below is taken over from pkgmaker package

#' LaTeX Utilities for Vignettes
#' 
#' \code{latex_preamble} outputs/returns command definition LaTeX commands to 
#' be put in the preamble of vignettes.
#' 
#' Argument \code{PACKAGE} is not required for \code{latex_preamble}, but must 
#' be correctly specified to ensure \code{biblatex=TRUE} generates the correct
#' bibliography command.
#'  
#' @param R logical that indicate if general R commands should be added 
#' (e.g. package names, inline R code format commands) 
#' @param CRAN logical that indicate if general CRAN commands should be added
#' (e.g. CRAN package citations) 
#' @param Bioconductor logical that indicate if general Bioconductor commands 
#' should be added (e.g. Bioc package citations) 
#' @param GEO logical that indicate if general GEOmnibus commands should be added
#' (e.g. urls to GEO datasets) 
#' @param ArrayExpress logical that indicate if general ArrayExpress commands 
#' should be added (e.g. urls to ArrayExpress datasets)
#' @param biblatex logical that indicates if a \code{\\bibliography} command 
#' should be added to include references from the package's REFERENCES.bib file. 
#' 
#' @param only a logical that indicates if the only the commands whose 
#' dedicated argument is not missing should be considered.
#' @param file connection where to print. If \code{NULL} the result is returned
#' silently.
#' 
#' @import stringr
#' @export
#' @rdname latex
#' @examples
#' 
#' latex_preamble()
#' latex_preamble(R=TRUE, only=TRUE)
#' latex_preamble(R=FALSE, CRAN=FALSE, GEO=FALSE)
#' latex_preamble(GEO=TRUE, only=TRUE)
#' 
latex_preamble <- function(PACKAGE, R=TRUE, CRAN=TRUE, Bioconductor=TRUE
		, GEO=TRUE, ArrayExpress=TRUE, biblatex=FALSE, only=FALSE, file=''){
	
	cmd <- "%%%% PKGMAKER COMMANDS %%%%%%
			\\usepackage{xspace}
			"
	
	inc <- function(arg){
		e <- parent.frame()
		(!only || eval(substitute(hasArg(arg), list(arg=substitute(arg))), e)) && arg
	}
	
	if( inc(R) ){
		cmd <- c(cmd, 
				"% R
						\\let\\proglang=\\textit
						\\let\\code=\\texttt 
						\\providecommand{\\Rcode}{\\code}
						\\providecommand{\\pkgname}[1]{\\textit{#1}\\xspace}
						\\providecommand{\\Rpkg}[1]{\\pkgname{#1} package\\xspace}
						\\providecommand{\\citepkg}[1]{\\cite{#1}}
						")
	}
	
	if( inc(CRAN) ){
		cmd <- c(cmd,
				"% CRAN
						\\providecommand{\\CRANurl}[1]{\\url{https://cran.r-project.org/package=#1}}
						%% CRANpkg
						\\makeatletter
						\\def\\CRANpkg{\\@ifstar\\@CRANpkg\\@@CRANpkg}
						\\def\\@CRANpkg#1{\\href{https://cran.r-project.org/package=#1}{\\pkgname{#1}}\\footnote{\\CRANurl{#1}}}
						\\def\\@@CRANpkg#1{\\href{https://cran.r-project.org/package=#1}{\\pkgname{#1}} package\\footnote{\\CRANurl{#1}}}
						\\makeatother
						%% citeCRANpkg
						\\makeatletter
						\\def\\citeCRANpkg{\\@ifstar\\@citeCRANpkg\\@@citeCRANpkg}
						\\def\\@citeCRANpkg#1{\\CRANpkg{#1}\\cite*{Rpackage:#1}}
						\\def\\@@citeCRANpkg#1{\\CRANpkg{#1}~\\cite{Rpackage:#1}}
						\\makeatother
						\\providecommand{\\CRANnmf}{\\href{https://cran.r-project.org/package=NMF}{CRAN}}
						\\providecommand{\\CRANnmfURL}{\\url{https://cran.r-project.org/package=NMF}}
						")
	}
	
	if( inc(Bioconductor) ){
		cmd <- c(cmd,
				"% Bioconductor
						\\providecommand{\\BioCurl}[1]{\\url{http://www.bioconductor.org/packages/release/bioc/html/#1.html}}
						\\providecommand{\\BioCpkg}[1]{\\href{http://www.bioconductor.org/packages/release/bioc/html/#1.html}{\\pkgname{#1}} package\\footnote{\\BioCurl{#1}}}
						\\providecommand{\\citeBioCpkg}[1]{\\BioCpkg{#1}~\\cite{Rpackage:#1}}
						% Bioconductor annotation
						\\providecommand{\\BioCAnnurl}[1]{\\url{http://www.bioconductor.org/packages/release/data/annotation/html/#1.html}}
						\\providecommand{\\BioCAnnpkg}[1]{\\href{http://www.bioconductor.org/packages/release/data/annotation/html/#1.html}{\\Rcode{#1}} annotation package\\footnote{\\BioCAnnurl{#1}}}
						\\providecommand{\\citeBioCAnnpkg}[1]{\\BioCAnnpkg{#1}~\\cite{Rpackage:#1}}
						")
	}
	
	if( inc(GEO) ){
		cmd <- c(cmd, 
				"% GEO
						\\providecommand{\\GEOurl}[1]{\\href{http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=#1}{#1}\\xspace}
						\\providecommand{\\GEOhref}[1]{\\GEOurl{#1}\\footnote{\\url{http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=#1}}}
						")
	}
	
	if( inc(ArrayExpress) ) cmd <- c(cmd,
				"% ArrayExpress
						\\providecommand{\\ArrayExpressurl}[1]{\\href{http://www.ebi.ac.uk/arrayexpress/experiments/#1}{#1}\\xspace}
						\\providecommand{\\ArrayExpresshref}[1]{\\ArrayExpressurl{#1}\\footnote{\\url{http://www.ebi.ac.uk/arrayexpress/experiments/#1}}}
						")
	
	if( biblatex ){
		if( missing(PACKAGE) )
			stop("Argument `PACKAGE` is required when specifying `biblatex=TRUE`.")
		cmd <- c(cmd, latex_bibliography(PACKAGE, file=NULL))
	} 
	
	# output or return commands
	cmd <- c(cmd, "%%%% END: PKGMAKER COMMANDS %%%%%%\n")
	cmd <- str_c(cmd, collapse="\n")
	if( !is.null(file) ) cat(cmd, file = file, sep='')
	else cmd
	
}

#' @describeIn latex \code{latex_bibliography} prints or return a LaTeX command that includes a 
#' package bibliography file if it exists.
#' 
#' @param PACKAGE package name
#' 
#' @export
#' 
latex_bibliography <- function(PACKAGE, file=''){
	
	rpkg.bib <- "%\\bibliography{Rpackages}\n"
	cmd <- rpkg.bib
	# get REFERENCES.bib file
	reffile <- packageReferenceFile(PACKAGE=PACKAGE)
	if( is.file(reffile) ){
		cmd <- paste0(cmd, "\\bibliography{", gsub("\\.bib$", "", reffile), "}\n")
	}
	
	# add post-processing knit hook
	if( !requireNamespace('knitr', quietly = TRUE) ) 
		stop("Package 'knitr' is required to run latex_bibliography.")
	
	knitr::knit_hooks$set(document = function(x){
				# write bibfile if necessary
				if( length(pkgs <- parsePackageCitation(x)) ){
					# write bibfile
					write.pkgbib(gsub("^Rpackage:", '', pkgs), file='Rpackages.bib', prefix='Rpackage:')
					# uncomment inclusion line
					x <- gsub("%\\bibliography{Rpackages}", "\\bibliography{Rpackages}", x, fixed = TRUE)
				}
				x
			})
	
	if( !is.null(file) ) cat(cmd, file=file)
	else cmd
}


parsePackageCitation <- function(x){
	
	if( length(x) > 1L ) x <- paste(x, collapse = "\n")
	
	.parse <- function(x, pattern, idx){
		dr <- str_match_all(x, pattern)
		dr <- dr[sapply(dr, length)>0L]
		unlist(lapply(dr, '[', , idx))
	}
	
	# extract package citations: \citeCRANpkg - like
	x <- gsub(".*[^%]* *\\\\begin\\{document\\}(.*)", "\\1", x)
	cite <- .parse(x, "\\\\cite((CRAN)|(BioC)|(BioCAnn))?pkg[*]?\\{([^}]*)\\}", 6L)
	# \cite{Rpackage:pkgname, ...} - like
	cite2 <- .parse(x, "\\\\(no)?cite[^{ ]*\\{([^}]*)\\}", 3L)
	if( length(cite2) ){
		cite2 <- .parse(cite2, 'Rpackage:([^,} ]+)', 2L)
		cite <- c(cite, cite2)
	}
	# remove Rpackage prefix
	if( length(cite) ){
		cite <- str_trim(unlist(strsplit(cite, ",")))
		cite <- gsub('^Rpackage:', '', cite)
	}
	inc <- character()
	if( length(cite) > 0L ){
		inc <- unique(str_trim(unlist(strsplit(cite, ","))))
	}
	inc  
}


packageReferenceFile <- function(PACKAGE = NULL, check = FALSE){
	f <- packagePath('REFERENCES.bib', package = PACKAGE, check = FALSE)
	if( check && length(f) && nzchar(f) && !file.exists(f) ) return('')
	f
}


write.pkgbib <- function(entry=NULL, file="Rpackages.bib", prefix='', append = FALSE, verbose = TRUE)
{
	# special handling of file=NULL: use stdout()
	if( is.null(file) ){
		file <- stdout()
		verbose <- FALSE
	}	
	## use all installed packages if nothing is specified
	if( is.null(entry) ){ 
		if( verbose ) message("Generating Bibtex entries for all installed packages ", appendLF=FALSE)
		entry <- unique(installed.packages()[,1])
		if( verbose ) message("[", length(entry), "]")
	}
	
	bibs <- 
			if( is(entry, 'bibentry') )	entry
			else if( is.character(entry) ){
				if( length(entry) == 0 ){
					if( verbose ) message("Empty package list: nothing to be done.")
					return(invisible())
				}
				
				pkgs <- entry
				bibs <- sapply(pkgs, function(x) try(citation(x)), simplify=FALSE)
				#bibs <- lapply(pkgs, function(x) try(toBibtex(citation(x))))
				n.installed <- length(bibs)
				
				## omit failed citation calls
				ok <- sapply(bibs, is, 'bibentry')
				pkgs <- pkgs[ok]
				bibs <- bibs[ok]
				n.converted <- sum(ok)
				
				## add bibtex keys to each entry
				pkgs <- lapply(seq_along(pkgs), function(i){
							if(length(bibs[[i]]) > 1)
								paste(prefix, pkgs[i], c('', 2:length(bibs[[i]])), sep = "") 
							else paste(prefix, pkgs[i], sep='')
						})
				pkgs <- do.call("c", pkgs)
				bibs <- do.call("c", bibs)		
				# formatting function for bibtex keys:
				# names with special characters must be enclosed in {}, others not.
				as.bibkey <- function(x){
					i <- grep("[.]", x)
					if( length(i) > 0 )
						x[i] <- paste("{", x[i], "}", sep='')
					x
				}		
				#bibs <- mapply(function(b,k){ if( is.null(b$key) ) b$key <- as.bibkey(k); b}, bibs, pkgs, SIMPLIFY=FALSE)
				bibs <- mapply(function(b,k){ if( is.null(b$key) ) b$key <- k; b}, bibs, pkgs, SIMPLIFY=FALSE)
				bibs <- do.call("c", bibs)
				
				if(verbose) message("Converted ", n.converted, " of ", n.installed, " package citations to BibTeX")					
				bibs
			} else
				stop("Invalid argument `entry`: expected a bibentry object or a character vector of package names.")
	
	if( length(bibs) == 0 ){
		if( verbose ) message("Empty bibentry list: nothing to be done.")
		return(invisible())
	}
	
	## write everything to the .bib file
	not_anonymous <- !identical(file,'')
	fh <- if( is.character(file) ){
				if( not_anonymous && !grepl("\\.bib$", file) ) # add .bib extension if necessary 
					file <- paste(file, '.bib', sep='')
				fh <- file(file, open = if(append && not_anonymous) "a+" else "w+" )
				if( not_anonymous )
					on.exit( if( isOpen(fh) ) close(fh) )
				fh
			} else if( is(file, 'connection') )
				file
			else
				stop("Invalid argument `file`: expected a filename, NULL, or a connection [", class(file), "]")
	
	if( !is(fh, 'connection') )
		stop("Invalid connection: ", fh)		
	file.desc <- summary(fh)['description']
	
	if( verbose ) message(if( append ) "Adding " else "Writing ", length(bibs) , " Bibtex entries ... ", appendLF=FALSE)
	bibs_str <- toBibtex(bibs)
	# bibs_str <- bibs_str[!grepl("citekey", bibs_str)]
	writeLines(bibs_str, fh)
	if(verbose) message("OK\nResults written to file '", file.desc, "'")
	
	## return Bibtex items invisibly
	if( !not_anonymous ) attr(bibs, 'connection') <- fh 
	invisible(bibs)
}


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


isManualVignette <- function(){
	isTRUE(getOption('R_RUNNING_MANUAL_VIGNETTE'))
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