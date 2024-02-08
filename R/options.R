###% Options management
###% 
###% 
###% @author Renaud Gaujoux \email{renaud@@cbio.uct.ac.za}
###% 

setupPackageOptions <- function(..., NAME=NULL, ENVIR=topenv(parent.frame()), RESET = isLoadingNamespace()){
	
	defaults <- .list_or_named_dots(...)
	
	# do not write into the Global environment
	e <- parent.frame()
	if( missing(ENVIR) && identical(e, .GlobalEnv) ){
		ENVIR <- NULL
	}
	
	# get calling package
	pkg <- packageName(.Global=TRUE)
	
	# prefix for the wrapper functions
	fprefix <- if( is.null(NAME) ) tolower(pkg) else NAME
	
	# define name for the option set
	optname <- pkg
	if( !is.null(NAME) )
		optname <- paste(optname, NAME, sep=':')
	
	# create package_options object
	optobj <- as.package_options(optname, defaults=defaults)
	
	# check if options with the same key are not already registered
	OLD <- getOption(optobj$name)
	if( !is.null(OLD) && !RESET )
		stop("Package specific options '", OLD$name, "' already exist: " 
				, " (", length(OLD$options())," default option(s))")
	
	# register the package_options object in global options
	message(if( is.null(OLD) ) "Setting" else "Resetting"
			, " package specific options: ", optobj$name
			, " (", length(optobj$options())," default option(s))")
	options(setNames(list(optobj), optobj$name))
	
	# (re)load registered package_options object from global options
	optobj <- getOption(optobj$name)
	stopifnot( !is.null(optobj) )
	
	# define wrapper functions in the supplied environment
	if( !is.null(ENVIR) ){
		isfun <- unlist(eapply(optobj, is.function))
		isfun <- isfun[names(isfun) != 'newOptions']
		ifun <- which(isfun)
		lapply(names(isfun)[ifun], function(x){
					f <- get(x, envir=optobj)
					assign(paste(fprefix, x, sep='.'), f, envir=ENVIR)
				})
	}
	
	# return package_options object
	optobj
}

.options <- function(..., .DATA){
	
	opts <- if( is.package_options(.DATA) || is.environment(.DATA) ) .DATA$.options else .DATA
	
	params <- .list_or_named_dots(...)
	# return complete option list if no other argument was passed
	if( is.null(params) ) return(opts)
	
	# initialise opts to an empty list if necessary 
	if( is.null(opts) ) opts <- list()
	stopifnot( is.list(opts) )
	
	# READ ACCESS
	if ( is.null(names(params)) ){
		if( !is.character(c(...)) )
			stop('character strings expected for option names')
		
		cparams <- c(...)
		# retrieve options as a list (use sapply to also get non-existing options)
		res <- sapply(cparams, function(n){
					# follow link if necessary
					opts[[option_symlink_target(n, opts)]]
				}, simplify=FALSE)
		return(res)
	}
	
	# WRITE ACCESS
	old <- sapply(names(params), 
			function(name){
				# assign the new value into the options environment
				val <- params[[name]]
				old <- opts[[name]]
				# change value of target if symlink and the new value is not a symlink
				if( is_option_symlink(old) && !is_option_symlink(val) )
					opts[[option_symlink_target(name, opts)]] <<- val
				else
					opts[[name]] <<- val
				# return the option's old value
				old
			}
			, simplify = FALSE
	)	
	#old <- old[!sapply(old, is.null)]
	
	# update package_options object in place if necessary (NB: it is an environment)
	if( is.package_options(.DATA) || is.environment(.DATA) ) .DATA$.options <- opts
	
	# return old values of the modified options
	return( invisible(old) )
}


.list_or_named_dots <- function(..., named.only=FALSE){
	
	dots <- list(...)
	if( length(dots) == 0L ) return()
	
	params <- dots
	if( is.null(names(dots)) && length(dots)==1L ){
		if ( is.list(dots[[1L]]) ){ 
			params <- dots[[1L]]
			if( is.null(names(params)) || any(names(params)=='') )
				stop("single list argument must only have named elements")
		}
	}
	if( named.only ){
		if( is.null(names(params)) || any(names(params)=='') )
			stop("all arguments be named")
	}
	
	params
}



as.package_options <- function(..., defaults=NULL){
	
	args <- .list_or_named_dots(...)
	
	x <- if( is.null(names(args)) ) args[[1]] 
	if( !is.null(names(args)) ) defaults <- args
	if( is.null(x) ) x <- basename(tempfile(''))
	
	# early exit if already a package_options object
	if( is.package_options(x) ){
		
		# new defaults?: clone into a new package_options object
		if( !missing(defaults) && is.list(defaults) ){
			optname <- basename(tempfile(str_c(x$name, '_')))
			x <- as.package_options(x$.options, defaults)
			x$name <- optname
		}
		
		return(x)
	}
	
	# create a package_options object
	.OPTOBJ <- structure(list2env(list(name=NULL, .options=NULL, .defaults=defaults))
			, class='package_options')
	
	if( is.character(x) ){
		
		# build name as 'package:*'
		x <- sub("^package:", '', x)
		.OPTOBJ$name <- paste('package:', x[1L], sep='')
		
	}else if( is.list(x) ){
		.OPTOBJ$name <- tempfile('package:')
		.OPTOBJ$.options <- x
	}else
		stop("Invalid argument `x`: must be a character string or a list.")
	
	# define options() 
	.OPTOBJ$options <- function(...){
		# call .options on package_options object
		.options(..., .DATA=.OPTOBJ)
	}
	# define getOption
	.OPTOBJ$getOption <- function (x, default = NULL) 
	{
		# use local specific function options()
		options <- .OPTOBJ$options
		
		if (missing(default)) 
			return(options(x)[[1L]])
		if (x %in% names(options())) 
			options(x)[[1L]]
		else default
	}
	# define newOption
	.OPTOBJ$newOptions <- function(...){
		defs <- .list_or_named_dots(..., named.only=TRUE)
		
		lapply(seq_along(defs),
				function(i){
					name <- names(defs)[i]
					value <- defs[[i]]
					# check defaults
					in_opts <- name %in% names(.OPTOBJ$.defaults) && !identical(.OPTOBJ$.defaults[[name]], value)
					if( in_opts && !isLoadingNamespace() ){
						message("Skipping option ", .OPTOBJ$name, "::`", name, "`: already defined with another default value")
					}else{
						if( in_opts )
							message("Overwriting option ", .OPTOBJ$name, "::`", name, "` : already defined with another default value")
						.OPTOBJ$.defaults[[name]] <- value
						.OPTOBJ$.options[[name]] <- value
					}
				})
		invisible()
	}
	# define resetOptions
	.OPTOBJ$resetOptions <- function(..., ALL=FALSE){
		
		defaults <- .OPTOBJ$.defaults
		if( ALL ){
			.OPTOBJ$.options <- NULL
		}
		if( length(list(...)) > 0L ){
			onames <- c(...)
			if( !is.character(onames) )
				stop('character strings expected for resetting option names')
			defaults <- defaults[names(defaults) %in% onames]
			if( length(not_default <- onames[!onames %in% names(defaults)]) ){
				.OPTOBJ$.options[not_default] <- NULL
			}
		}
		if( length(defaults) ){
			.OPTOBJ$options(defaults)
		}
	}
	# define showOptions
	.OPTOBJ$printOptions <- function() print(.OPTOBJ)
	
	# initialise with default options 
	.OPTOBJ$resetOptions()
	
	# return pacakge_options object
	.OPTOBJ
}

is.package_options <- function(x){
	is(x, 'package_options')
}

option_symlink <- function(x){
	if( !is.character(x) )
		stop("Symbolic link options must be character strings")
	structure(x, class='option_symlink')
}

is_option_symlink <- function(x, opts){
	if( missing(opts) ) is(x, 'option_symlink')
	else is(opts[[x]], 'option_symlink')
}

option_symlink_target <- function(x, opts){
	
	if( !is.list(opts) )
		stop("invalid argument `opts`: must be a list object")
	
	n <- 0
	track <- NULL
	while( is_option_symlink(x, opts) ){
		if( x %in% track )
			stop("cycling symbolic link options: ", str_out(c(track, x), Inf, sep=' -> '))
		track <- c(track, x)
		x <- opts[[x]]
		n <- n + 1
		
	}
	x
	
}

packageName <- function(envir=packageEnv(), .Global=FALSE, rm.prefix=TRUE){
	
	if( is.null(envir) ) envir <- packageEnv() 
	if( is.character(envir) ){
		return( sub("^package:", "", envir) )
	}
	
	# retrieve package environment
	e <- envir
	
	# try with name from environment
	nm <- environmentName(e)
	if( identical(e, .GlobalEnv) && .Global ) return(nm)
	else if( isNamespace(e) || identical(e, baseenv()) ) return(nm)
	else if( grepl("^package:", nm) ){# should work for devtools packages
		if( rm.prefix ) 
			nm <- sub("^package:", "", nm)
		return(nm)
	}
	
	# try to find the name from the package's environment (namespace) 
	if( exists('.packageName', e) && .packageName != 'datasets' ){
		if( .packageName != '' )
			return(.packageName)
	}
	# get the info from the loadingNamespace
	info <- getLoadingNamespace(info=TRUE)
	if( !is.null(info) ) # check whether we are loading the namespace 
		info$pkgname
	else{# error
		stop("Could not reliably determine package name [", nm, "]")
	}
}


packagePath <- function(..., package=NULL, lib.loc=NULL, check = TRUE){
	
	# try to find the path from the package's environment (namespace)
	pname <- packageName(package)
	
	# check if one is currently loading the namespace
	path <- NULL
	if( !is.null(info <- getLoadingNamespace(info=TRUE)) && info$pkgname == pname ){
		path <- info$path
	}else {
		# try loaded/installed package
		path <- find.package(package=pname, lib.loc=lib.loc, quiet=TRUE)		
	}
	# somehow this fails when loading an installed package but is works 
	# when loading a package during the post-install check
	if( !length(path) || path == '' ){
		# get the info from the loadingNamespace
		if( !is.null(info <- getLoadingNamespace(info=TRUE)) ){
			path <- info$path
		}
	}
	
	# check if the path was found
	if( !length(path) || !nzchar(path) ){
		if( check ) stop("Could not find path to package ", package)
		return(NULL)
	}
	
	# for development packages: add inst prefix if necessary
	if( isDevNamespace(pname) ){
		# handle special sub-directories of the package's root directory
		dots <- list(...)
		Rdirs <- c('data', 'R', 'src', 'exec', 'tests', 'demo'
				, 'exec', 'libs', 'man', 'help', 'html'
				, 'Meta')
		if( length(dots) && !sub("^/?([^/]+).*", "\\1", ..1) %in%  Rdirs)
			path <- file.path(path,'inst')
	}
	
	# add other part of the path
	file.path(path, ...)	
}

packageEnv <- function(pkg, skip=FALSE, verbose=FALSE){
	
	# return package namespace
	if( !missing(pkg) && !is.null(pkg) ){
		# - if the package is loaded: use asNamespace because as.environment does not
		# return a correct environment (don't know why)
		# - as.environment('package:*') will return the correct environment
		# in dev mode.
		env <- if( is.environment(pkg) ) topenv(pkg)
				else if( isLoadingNamespace(pkg) ) getLoadingNamespace(env=TRUE)
				else if( !is.null(path.package(pkg, quiet=TRUE)) ) asNamespace(pkg)
				else if( isNamespaceLoaded(pkg) ) asNamespace(pkg)
				else if( pkg %in% search() ) as.environment(pkg)
				else as.environment(str_c('package:', pkg)) # dev mode
		return(env)
	}
	
	envir = parent.frame()
#	message("parent.frame: ", str_ns(envir))
	pkgmakerEnv <- topenv()
#	message("pkgmaker ns: ", str_ns(pkgmakerEnv))
	
	n <- 1
	skipEnv <- pkgmakerEnv
	while( identical(e <- topenv(envir), skipEnv) 
			&& !identical(e, emptyenv()) 
			&& !identical(e, .GlobalEnv) ){
		if( verbose > 1 ) print(e)
		n <- n + 1
		envir <- parent.frame(n)
	}
	
	if( !skip ){
		if( identical(e, .BaseNamespaceEnv) ){
			if( verbose ) message("packageEnv - Inferred ", str_ns(skipEnv))
			return( skipEnv )
		}
		if( verbose ) message("packageEnv - Detected ", str_ns(e))
		return(e)
	}
	if( verbose > 1 ) message("Skipping ", str_ns(skipEnv))
	# go up one extra namespace
	skipEnv <- e
	while( identical(e <- topenv(envir), skipEnv) 
			&& !identical(e, emptyenv()) 
			&& !identical(e, .GlobalEnv) ){
		if( verbose > 1 ) print(e)
		n <- n + 1
		envir <- parent.frame(n)
	}
	if( identical(e, .BaseNamespaceEnv) ){
		if( verbose ) message("packageEnv - Inferred ", str_ns(skipEnv))
		return( skipEnv )
	}
	if( verbose ) message("packageEnv - Detected ", str_ns(e))
	return(e)
}

as_package <- function(x, ..., quiet=FALSE, extract=FALSE){
	
	if( !requireNamespace('devtools', quietly = TRUE) ) 
		stop("Package 'devtools' is required to load development packages")
	
	if( is.null(x) ) return( devtools::as.package() )
	if( devtools::is.package(x) ) return(x)
	
	if( extract && grepl("\\.tar\\.gz$", x) ){ # source file
		# extract in tempdir
		tmp <- tempfile(x)
		on.exit( unlink(tmp, recursive=TRUE) )
		pkg <- basename(sub("_[0-9.]+\\.tar\\.gz$", '', x))
		desc <- file.path(pkg, 'DESCRIPTION')
		untar(x, desc, exdir=tmp)
		return(devtools::as.package(file.path(tmp, pkg)))
	} else { # check for 'package:*'
		if( grepl('^package:', x) ){
			libs <- .libPaths()
			pkg <- sub('^package:', '', x)
			p <- lapply(libs, find.package, package=pkg, quiet=TRUE, verbose=FALSE)
			p <- unlist(p[sapply(p, length)>0])
			if( !length(p) ){
				if( !quiet )
					stop("Could not find installed package ", pkg)
				return()
			}
			x <- p[1L]
		}
	}
	# try development packages
	res <- try(devtools::as.package(x), silent=TRUE)
	if( !is(res, 'try-error') )
		return(res)
	# try loaded or installed packages
	if( length(res <- find.package(package=x, quiet=TRUE)) )
		return(devtools::as.package(res))
	if( quiet )
		stop("Could not find package ", x)
	NULL
}

# internal overload of as.package
as.package <- as_package



isLoadingNamespace <- function(ns, nodev=FALSE){
	
	if( missing(ns) ) !is.null(getLoadingNamespace(nodev=nodev))
	else{
		nspkg <- getLoadingNamespace(nodev=nodev, env=is.environment(ns))
		if( is.null(nspkg) ) FALSE
		else identical(nspkg, ns)
	}
}


getLoadingNamespace <- function(env=FALSE, info=FALSE, nodev=FALSE){
	is.loading <- try(nsInfo <- loadingNamespaceInfo(), silent=TRUE)
	if( !is(is.loading, 'try-error') ){
		if( env ) asNamespace(as.name(nsInfo$pkgname))
		else if( info ){
			nsInfo$path <- file.path(nsInfo$libname, nsInfo$pkgname)
			nsInfo 
		}else nsInfo$pkgname
		
	}else if( !nodev ){ # devtools namespaces are allowed
		if( (is_pkgcall('devtools') && (i <- is_funcall(ns_get('devtools::load_all')))) ||
				(is_pkgcall('pkgload') && (i <- is_funcall(ns_get('pkgload::load_all')))) || # for devtools > 1.12
				(is_pkgcall('roxygen24') && (i <- is_funcall(ns_get('roxygen24::source_package')))) ){
			# find out the package that is currently being loaded by load_all
			e <- sys.frame(i)
			pkg <- e[["pkg"]]
			
			if( is.null(pkg) ){ # try to load from path
				if( is.null(path <- e[["path"]]) ) stop("Could not infer loading namespace")
				pkg <- as_package(path)
				
			}
			
			# extract namespace
			if( env ){
				if( isDevNamespace(pkg$package) ) asNamespace(pkg$package)
				else pkg$ns
			}else if( info ){
				list(
						pkgname = pkg$package
						, path = pkg$path
						, libname = dirname(pkg$path)
				)
			}else pkg$package
		}
	}
	else NULL
}


isDevNamespace <- function(ns){
	if( missing(ns) ){
		e <- parent.frame()
		ns <- methods::getPackageName(topenv(e))
	}
	
	# cannot be true if the namespace is not loaded
	if( !isNamespaceLoaded(ns) ) return( FALSE )
	# get the namespace environment
	if( isString(ns) ) ns <- asNamespace(ns)
	# check for the presence of a .__DEVTOOLS__ object
	exists('.__DEVTOOLS__', where=ns)
	
}


ns_get <- function(x, ns = NULL, ...){
	if( is.null(ns) ){
		ns <- gsub("^([^:]+)::.*", "\\1", x)
		x <- gsub(".*::([^:]+)$", "\\1", x)
	}
	if( !isNamespace(ns) ){
		ns <- tryCatch(asNamespace(ns), error = function(e) NULL)
		if( is.null(ns) ) return()
	}
	get0(x, envir = ns, ...)
}


str_ns <- function(envir=packageEnv()){
	if( !is.environment(envir) )
		stop("Invalid argument: must be an environment [", class(envir), ']')
	stringr::str_c(if( isNamespace(envir) ) 'namespace' else 'environment',
			" '", packageName(envir, rm.prefix=FALSE), "'")
}


require.quiet <- function(...){utils::capture.output(suppressPackageStartupMessages(suppressMessages(suppressWarnings( res <- withVisible(require(...)))))); if( res$visible ) res$value else invisible(res$value)}

pkg_calls <- function(){
	n <- sys.nframe() - 1
	i <- 1
	res <- character()
	while( i <= n ){
		f <- sys.function(i)
		e <- environment(f)
		if( !is.null(e) ){
			pkg <- methods::getPackageName(e, create=FALSE)
			if( pkg != '' ) res <- c(res, pkg)
		}
		i <- i + 1
	}
	res
}

is_pkgcall <- function(pkg){
	
	pkg %in% pkg_calls()
	
}


is_funcall <- function(fun){
	
	n <- sys.nframe()
	i <- 1
	dg <- digest::digest(fun)
	while( i <= n ){
		f <- sys.function(i)
		ca <- sys.call(i)
#		cat(digest(f), dg, getPackageName(environment(f), FALSE), "\n")
		if( digest::digest(f) == dg ) return(i)
		i <- i + 1
	}
	FALSE
}

mkoptions <- function(...){
	
	.DATA <- new.env(parent=emptyenv())
	.defaults <- list(...)
	.DATA$.options <- list(...)
	function(...){		
		.options(..., .DATA=.DATA)
	}
}

unit.test <- function(x, expr, framework=NULL, envir=parent.frame()){
	
	sid <- as.character(deparse(substitute(x)))	
	hash <- suppressWarnings(digest(x))
	# get test environment
	eTest <- packageTestEnv()
	# wrap test into a function
	f <- function(){}
	environment(f) <- eTest
	body(f) <- substitute({expr})
	
	if( !grepl('"', sid) )
	{
		lmessage('Creating unit test for object: `', sid, '`')
		eval(substitute(attr(x, 'test') <- f, list(x=substitute(x), f=f)), envir)
	}else
		lmessage('Creating unit test: ', sid)
	
	# add the test to the package test environment
	eTest[[str_c(sid, ':', hash)]] <- list(test=f, name=sid, object=is.name(x))
	# return the test function
	f
}


packageTestEnv <- function(pkg){
	
	if( !missing(pkg) && !is.null(pkg) ){
		e <- packageEnv(pkg)
		return( e$.packageTest )
	}
	
	e <- packageEnv()
	# create test environment if necessary
	if( is.null(e$.packageTest) )
		e$.packageTest <- new.env(parent=e)
	e$.packageTest
}


lmessage <- function(level, ..., appendLF=TRUE, sep='', force=FALSE){
	getLogger(force=force)$lmessage(level, ..., appendLF=appendLF, sep=sep, force=force)
}

sVariable <- function(default=NULL){
	.val <- default
	function(value){
		if( missing(value) ) .val
		else{
			old <- .val
			.val <<- value
			old
		}
	}
}

log_lastLF <- sVariable(list(Inf, TRUE))

.LOG_OPTIONS <- setupPackageOptions(NAME='logger', RESET=TRUE,
		# should a the next logger be indented
		autoindent = TRUE
)
logger.options <- .LOG_OPTIONS$options
logger.getOption <- .LOG_OPTIONS$getOption

getLogger <- local({
			
			# store info for top call
			.topCall <- NULL
			# list of logger objects
			.lastLogger <- list()
			
			function(..., type='STDOUT', new=TRUE, force=FALSE){
				
				# return NULL logger if not in verbose mode
				if( !force && !lverbose() ) return( new_logger('NULL') )
				
				# top caller
				call1 <- sys.call(1)
				topCall <- callInfo(1L)
				caller <- callInfo(-1L)
				# reset all static variables
				if( !identical(.topCall, topCall) ){
					.topCall <<- topCall
					.lastLogger <<- list()
				}
				
				## build logger object
				if( !length(.lastLogger) ){ # new stored for future calls
					logger <- new_logger(type, ...)
					logger$caller(caller)
					.lastLogger[[as.character(caller$nframe)]] <<- logger
				}else{ # increment indentation
					
					autonew <- missing(new)
					if( new ){ # get new logger object
						last <- getLogger(new=FALSE)
						if( !autonew || (!is.null(last$caller()) && last$caller()$nframe < caller$nframe) ){
							# instanciate a new logger object only if the current caller is lower in the stack
							logger <- new_logger(type, ...)
							logger$caller(caller)
							# auto indent if last logger is higher in the stack
							if( !logger.getOption('autoindent') ){
								# reset autoindent option on.exit
								on.exit( logger.options(autoindent=TRUE), add=TRUE)
								logger$nindent(last$nindent(), add=FALSE)
							} else if( last$caller()$nframe < caller$nframe ){
								logger$nindent(last$nindent() + 1L, add=FALSE)
							}
							.lastLogger[[as.character(caller$nframe)]] <<- logger
						}else logger <- last
						# add initial new line if the last log message was from a higher logger
						logger$breaknext(log_lastLF()[[1L]] < caller$nframe)
					}else{
						# get logger for exact frame number 
						if( is.null(logger <- .lastLogger[[as.character(caller$nframe)]]) ){
							# get first higher logger
							i <- which(as.numeric(names(.lastLogger)) <= caller$nframe)
#					str(.lastLogger)
							if( length(i) )	logger <- .lastLogger[[tail(i, 1L)]]
							else logger <- .lastLogger[[1L]]
							
						}
					}
				}
				
				# return logger
				logger
			}
		})


new_logger <- function(type, ..., nindent=0L, indent=' '){
	
	.data <- list(
			lastLF = TRUE
			, breaknext = FALSE
			, cindent = indent
			, nindent = nindent
			, indent = rep(indent, nindent)
			, caller = NULL
	)
	
	# init logger object
	.logger <- list()
	
	.logger$data <- function(value){
		if( missing(value) ) .data
		else .data <<- value
	}
	
	# init type-specific slots
	f <- match.fun(paste('new_logger', type, sep=''))
	.logger <- f(.logger, ...)
	
	# get last used appendLF value
	.logger$lastLF <- function(){ .data$lastLF }
	
	# get initial new line flag
	.logger$breaknext <- function(val){
		if( missing(val) ) .data$breaknext
		else .data$breaknext <<- val
	}
	
	# get caller data
	.logger$caller <- function(val){
		if( missing(val) ) .data$caller
		else .data$caller <<- val
	}
	
	# get/set indentation
	.logger$indent <- function(val){
		if( missing(val) ) .data$indent
		else{
			if( is.numeric(val) ) return( .logger$nindent(val) )
			old <- .data$indent
			.data$indent <<- val
			old
		}
	}
	# get/set number of indentation
	.logger$nindent <- function(val, add=TRUE){
		if( missing(val) ) .data$nindent
		else{
			old <- .data$nindent
			.data$nindent <<- val + if( add ) .data$nindent else 0L
			if( .data$nindent >= 0L )
				.data$indent <<- paste(rep(.data$cindent, .data$nindent), collapse='')
			old
		}
	}
	
	# new message with auto-indentation and breakline
	.logger$message <- function(..., appendLF=TRUE, sep=''){
		
		if( is.null(.logger$write) ) return()
		
		msg <- if( .data$breaknext || isTRUE(logger.getOption('breaknext')) ){
			logger.options(breaknext=NULL)
			.data$breaknext <<- FALSE
			"\n"
		}
		# add indentation if on a new line
		lastLFframe <- log_lastLF()
		callerFrame <- .logger$caller()$nframe
		if( .data$lastLF || lastLFframe[[2L]] || (is.finite(lastLFframe[[1L]]) && lastLFframe[[1L]] >= callerFrame) )
			msg <- paste(msg, .data$indent, sep='')
		msg <- paste(msg, paste(..., sep='', collapse=sep), if( appendLF ) "\n", sep='')
		
		# store in global variable the frame number if it the line is left open
		log_lastLF(list(if( !appendLF ) callerFrame else Inf, appendLF))
		.data$lastLF <<- appendLF
		
		# call logger write function
		.logger$write(msg)
	}
	
	# log message with specific level
	.logger$lmessage <- function(level, ..., force=FALSE){
		if( force || lverbose() >= level ) .logger$message(...)
	}
	
	# show info about the logger
	.logger$show <- function(){
		cat("<Type: ", class(.logger)[1L], ">\n", sep='')
		.logger$info()
	}
	
	# wrap into a logger object
	structure(.logger, class=c(paste('logger', type, sep=''), 'logger'))
}


# NULL logger
new_loggerNULL <- function(.logger, ...){
	
	# write to log
	.logger$write <- NULL
	# special info
	.logger$info <- function(){
		cat("Output: NULL\n")
	}
	# return logger object
	.logger
}

# Logger that writes on the standard output
new_loggerSTDOUT <- function(.logger, ...){
	
	# append to log
	.logger$write <- function(...){
		cat(..., sep='')
	}
	
	.logger$info <- function(){
		cat("Output: stdout\n")
	}
	
	# return logger object
	.logger
	
}

# Logger that writes on the standard error
new_loggerSTDERR <- function(.logger, ...){
	
	# append to log
	.logger$write <- function(...){
		message(..., appendLF=FALSE)
	}
	
	.logger$info <- function(){
		cat("Output: stderr\n")
	}
	
	# return logger object
	.logger
	
}


isNamespaceLoaded2 <- isNamespaceLoaded <- function(ns){
	if( is.environment(ns) ){
		if( !isNamespace(ns) ) return(FALSE)
		else ns <- getPackageName(ns)
	}
	if( isString(ns) ) base::isNamespaceLoaded(ns)
	else stop("Invalid argument `ns`: only support strings and environments.")
}


isString <- function(x, y, ignore.case=FALSE){
	if( res <- is.character(x) && length(x) == 1L ){
		if( !missing(y) ){
			if( !isString(y) ) stop("Invalid argument 'y': must be a string itself.")
			if( ignore.case ) {
				x <- toupper(x)
				y <- toupper(y)
			}
			res <-  x == y
		}
	}
	res
}

isNumber <- function(x){ 
	is.numeric(x) && length(x) == 1
}

isReal <- function(x){ 
	isNumber(x) && !is.integer(x)
}

is.dir <- function(x) file_test('-d', x)

is.file <- function(x) file_test('-f', x)

orderVersion <- function(x, ..., decreasing=FALSE){
	
	NAs <- which(is.na(x))
	tx <- gsub("[^0-9]+",".", paste('_', x, sep=''))
	stx <- strsplit(tx, ".", fixed=TRUE)
	mtx <- max(sapply(stx, length))
	tx <- sapply(stx, 
			function(v) paste(sprintf("%06i", c(as.integer(v[-1]),rep(0, mtx-length(v)+1))), collapse='.')
	)	
	res <- order(tx, ..., decreasing = decreasing)
	# put NAs at the end
	if( length(NAs) ){
		res <- c(setdiff(res, NAs), NAs)
	}
	res
}


ExposeAttribute <- function(object, ..., .MODE='rw', .VALUE=FALSE){
	
	# setup exposed arguments
	args <- list(...)
	if( length(args) ){
		# use the same mode for all attributes
		if( isString(.MODE) == 1L )
			.MODE <- rep(.MODE, length(args))
		else if( length(.MODE) != length(args) ){
			stop("Argument .MODE must provide an access mode for each argument in `...`.")
		}
		
		if( is.null(names(args)) ) # add names if necessary
			args <- setNames(args, rep('', length(args)))
		un <- names(args)==''
		if( any(!sapply(args[un], isString)) )
			stop("All unnamed argument must be the name of an attribute, i.e. a character string.")
		
		# set attributes that have values if requested
		if( .VALUE ){
			sapply(names(args)[!un], function(x){
						attr(object, x) <<- args[[x]]
					})
		}else{ # or use the values as access permission
			.MODE[!un] <- args[!un]
		}
		#
		
		# store exposed attributes with names as regular expressions
		eargs <- ifelse(un, args, names(args))
		eargs <- as.list(setNames(.MODE, eargs))
		# add ereg start-end
		names(eargs) <- paste('^', names(eargs), '$', sep='')
	}else{
		eargs <- .MODE
	}
	
	# store access rights
	attr(object, '.ExposeAttribute') <- eargs
	class(object) <- c(class(object), 'ExposeAttribute')
	object
}


alphacol <- function(col, alpha = FALSE){
	col <- as.matrix(as.character(as.hexmode(col2rgb(col, alpha))))
	if( !is.logical(alpha) ){
		if( alpha < 1 ) alpha <- alpha * 255
		alpha <- round(alpha)
		col[4] <- as.character(as.hexmode(alpha))
	}
	apply(col, 2, function(x) paste("#", paste(x, collapse=''), sep=''))
}

callInfo <- function(n=-1L){
	if( n<0L ) n <- n-1 
	f <- sys.function(n)
	e <- sys.frame(n)
	sysn <- if( n<0L ) sys.nframe()-n else n
	list(fd=digest(f), env=capture.output(e), nframe=sysn)
}


compile_src <- function(pkg=NULL, load=TRUE){
	
	if( !is.null(pkg) ){
		p <- as.package(pkg)
		path <- p$path
	}else{
		pkg <- packageName()
		path <- packagePath(lib=NA) # do not look installed packages
	}
	
	owd <- getwd()
	on.exit(setwd(owd))
	
	# Compile code in /src
	srcdir <- file.path(path, 'src')
	message("# Checking '", srcdir, "' ... ", appendLF=FALSE)
	if( !file.exists(srcdir) ){
		message("NO")
	} else {
		message("YES")
		message("## Compiling '",srcdir,"' ##")
		setwd(srcdir)
		Sys.setenv(R_PACKAGE_DIR=path)
		R.SHLIB(pkg, " *.cpp ")
		message("## DONE")
		if( load ){
			if( existsFunction('load_dll', where='package:devtools') ){ # post 0.8
				f <- getFunction('load_dll', where='package:devtools')
				f(pkg)
			}else{ # prior 0.8
				f <- getFunction('load_c', where='package:devtools')
				f(pkg)
			}
		}
	}
}

exitCheck <- function(){
	
	.success <- FALSE
	function(x){
		if( nargs() == 0L ) .success
		else{
			.success <<- TRUE
			x
		}
	}
}

expand_dots <- function(..., .exclude=NULL){
	
	dotsCall <- as.list(eval(quote(substitute(list(...))), sys.parent()))
	if( length(dotsCall) >= 1L ) dotsCall <- dotsCall[-1L]
	
	# extract defaults from ... arguments
	defaults <- list(...)
	if( length(defaults) == 1L && is.null(names(defaults)) ){
		defaults <- defaults[[1L]]
	}
	if( length(defaults) ){
		excl <- names(allFormals(sys.function(sys.parent())))
		if( !is.null(.exclude) ) excl <- c(excl, .exclude)
		defaults <- defaults[!names(defaults) %in% excl]
		dotsCall <- expand_list(dotsCall, defaults, .exact=FALSE)
	}
	#
	
	# return expanded dot args
	dotsCall
}

hasNames <- function(x, all=FALSE){
	nm <- names(x)
	if( length(x) == 0L ) TRUE
	else !is.null(nm) && (!all || !is.element('', nm) )
}


str_desc <- function(object, exdent=0L){
	p <- sapply(object, function(x){
				if( is.atomic(x) && length(x) == 1L ) x
				else paste("<", class(x), ">", sep='')
			})
	str_wrap(str_out(p, NA, use.names=TRUE, quote=FALSE), exdent=exdent)
}

str_fun <- function(object){
	s <- capture.output(args(object))
	paste(s[-length(s)], collapse="\n")
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

R.SHLIB <- function(libname, ...){
	R.CMD('SHLIB', '-o ', libname, .Platform$dynlib.ext, ...)
}

R.CMD <- function(cmd, ...){
	R.exec('CMD ', cmd, ' ', ...)
}

R.exec <- function(..., lib.loc=NULL){
	cmd <- paste(file.path(R.home('bin'), 'R'), ' ', ..., sep='', collapse='')
	# add lib path
	ol <- set_libPaths(lib.loc)
	on.exit(set_libPaths(ol))
	
	message(cmd)
	system(cmd, intern=interactive())
}

set_libPaths <- function(lib.loc=NULL){
	ol <- Sys.getenv('R_LIBS')
	olib <- .libPaths()
	res <- list(R_LIBS=ol, .libPaths=olib)
	if( is_NA(lib.loc) ) return(res)
	
	# add lib path
	if( is.null(lib.loc) ) lib.loc <- .libPaths()
	if( is.character(lib.loc) ){
		# current .libPaths
		.libPaths(lib.loc)
		# R_LIBS env variable
		rlibs <- paste(lib.loc, collapse=.Platform$path.sep)
		Sys.setenv(R_LIBS=rlibs)
	}else if( is.list(lib.loc) ){
		Sys.setenv(R_LIBS=lib.loc$R_LIBS)
		.libPaths(lib.loc$.libPaths)
	}
	
	res
}







#.nmf.Options.Runtime <- character()

# define functions nmf.options and nmf.getOptions
#' NMF Package Specific Options
#'
#' @section Available options:
#' \describe{
#' 
#' \item{cores}{Default number of cores to use to perform parallel NMF computations.
#' Note that this option is effectively used only if the global option \code{'cores'} is 
#' not set.
#' Moreover, the number of cores can also be set at runtime, in the call to \code{\link{nmf}}, 
#' via arguments \code{.pbackend} or \code{.options} (see \code{\link{nmf}} for more details).}
#' 
#' \item{default.algorithm}{Default NMF algorithm used by the \code{nmf} function when argument 
#' \code{method} is missing. 
#' The value should the key of one of the registered NMF algorithms or a valid specification of an NMF algorithm.
#' See \code{?nmfAlgorithm}.}
#' 
#' \item{default.seed}{Default seeding method used by the \code{nmf} function when argument \code{seed} is missing.
#' The value should the key of one of the registered seeding methods or a vallid specification of a seeding method. 
#' See \code{?nmfSeed}.}
#' 
#' \item{track}{Toggle default residual tracking. 
#' When \code{TRUE}, the \code{nmf} function compute and store the residual track in the result -- if not otherwise specified in argument \code{.options}.
#' Note that tracking may significantly slow down the computations.}
#' 
#' \item{track.interval}{Number of iterations between two points in the residual track. 
#' This option is relevant only when residual tracking is enabled. 
#' See \code{?nmf}.}
#' 
#' \item{error.track}{this is a symbolic link to option \code{track} for backward compatibility.}
#' 
#' \item{pbackend}{Default loop/parallel foreach backend used by the \code{nmf} function when 
#' argument \code{.pbackend} is missing.
#' Currently the following values are supported: \code{'par'} for multicore, 
#' \code{'seq'} for sequential, \code{NA} for standard \code{sapply} (i.e. do not use a foreach loop), 
#' \code{NULL} for using the currently registered foreach backend.}
#' 
#' \item{parallel.backend}{this is a symbolic link to option \code{pbackend} for backward compatibility.}
#' 
#' \item{gc}{Interval/frequency (in number of runs) at which garbage collection is performed.}
#' 
#' \item{verbose}{Default level of verbosity.}
#' 
#' \item{debug}{Toogles debug mode.
#' In this mode the console output may be very -- very -- messy, and is aimed at debugging only.}
#' 
#' \item{maxIter}{ Default maximum number of iteration to use (default NULL).
#' This option is for internal/technical usage only, to globally speed up examples or tests
#' of NMF algorithms. To be used with care at one's own risk...
#' It is documented here so that advanced users are aware of its existence, and can avoid possible 
#' conflict with their own custom options.
#' }
#' } % end description
#' 
#' 
#' @rdname options
#' @name options-NMF
NULL
.OPTIONS <- setupPackageOptions(
	# default algorithm
	default.algorithm='brunet'
	# default seeding method
	, default.seed='random'
	# track error during NMF updates
	, error.track = option_symlink('track') # for backward compatibility
	, track=FALSE
	# define the tracking interval
	, track.interval=30
	# define garbage collection interval
	, gc=50
	# define default parallel backend 
	, parallel.backend= option_symlink('pbackend') # for backward compatibility
	, pbackend= if( parallel::detectCores() > 1 ) 'par' else 'seq'
	# toogle verbosity
	, verbose=FALSE
	# toogle debug mode
	, debug=FALSE
, RESET=TRUE)

#' \code{nmf.options} sets/get single or multiple options, that are specific
#' to the NMF package. 
#' It behaves in the same way as \code{\link[base]{options}}.
#' 
#' @inheritParams base::options
#' @param ... option specifications. For \code{nmf.options} this can be named arguments or 
#' a single unnamed argument that is a named list (see \code{\link{options}}.
#' 
#' For \code{nmf.resetOptions}, this must be the names of the options to reset.
#' 
#' @export
#' @rdname options
#' @examples
#' 
#' # show all NMF specific options
#' nmf.printOptions()
#' 
#' # get some options
#' nmf.getOption('verbose')
#' nmf.getOption('pbackend')
#' # set new values
#' nmf.options(verbose=TRUE)
#' nmf.options(pbackend='mc', default.algorithm='lee')
#' nmf.printOptions()
#' 
#' # reset to default
#' nmf.resetOptions()
#' nmf.printOptions()
#' 
nmf.options <- .OPTIONS$options

#' \code{nmf.getOption} returns the value of a single option, that is specific 
#' to the NMF package.
#' It behaves in the same way as \code{\link[base]{getOption}}.
#' 
#' @inheritParams base::getOption
#' 
#' @export
#' @rdname options
nmf.getOption <- .OPTIONS$getOption

#' \code{nmf.resetOptions} reset all NMF specific options to their default values.
#' 
#' @param ALL logical that indicates if options that are not part of the default set 
#' of options should be removed.
#' 
#' @export
#' @rdname options
nmf.resetOptions <- .OPTIONS$resetOptions

#' \code{nmf.printOptions} prints all NMF specific options along with their default values, 
#' in a relatively compact way.
#' @export
#' @rdname options
nmf.printOptions <- .OPTIONS$printOptions

#nmf.options.runtime <- function(){
#	nmf.options(.nmf.Options.Runtime)	
#}


# debugging utility
nmf.debug <- function(fun, ...){
	if( nmf.getOption('debug') ){
		call.stack <- sys.calls()
		n <- length(call.stack)
		if( is.null(fun) ) fun <- as.character(call.stack[[n-1]]) 
		message('DEBUG::', fun, ' -> ', ...)
	}
	return(invisible())
}
