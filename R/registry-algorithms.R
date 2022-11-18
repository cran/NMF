
##### All these functions were taken over from pkgmaker package
#' @export 
isCHECK <- function(){
	isCRANcheck() ||  # known CRAN check flags
			!isFALSE(utestCheckMode()) ||  # unit test-specific flag
			isTRUE(getOption('R_CHECK_RUNNING_EXAMPLES_')) # roxygen generated example flag
}

#' @export 
isCRANcheck <- function(...){
	
	tests <- list(...)
	if( !length(tests) ){ #default tests
		tests <- list('timing', 'cran')
	}
	test_sets <- c(timing="_R_CHECK_TIMINGS_", cran='_R_CHECK_CRAN_INCOMING_')
	tests <- sapply(tests, function(x){
				# convert named tests
				if( length(i <- which(x %in% names(test_sets))) ){
					y <- test_sets[x[i]]
					x <- x[-i]
					x <- c(x, y)
				}
				# get environment variables
				evar <- unlist(sapply(x, Sys.getenv))
				all(nchar(as.character(evar)) > 0)
			})
	
	any(tests)
}


checkMode_function <- function(varname){
	
	.varname <- varname
	function(value, raw = FALSE){
		if( missing(value) ) Sys.getenv_value(.varname, raw = raw)
		else{
			old <- Sys.getenv_value(.varname, raw = TRUE)
			if( is_NA(value) ) Sys.unsetenv(.varname) # unset
			else do.call(Sys.setenv, setNames(list(value), .varname)) # set value
			# return old value
			old	
		}	
	}
}

utestCheckMode <- checkMode_function('_R_CHECK_RUNNING_UTESTS_')


packageRegistry <- function(regname=NULL, quiet=FALSE, entry=FALSE, update=!entry, package=topenv(parent.frame())){
	
#	library(registry)
	metaregname <- '.packageRegistry'
	name <- regname
	
	# get package environment
	e <- packageEnv(package)	
	# get namespace name
	nm <- packageName(e)
	
	# get package meta-registry
	pkgreg <- .packageMetaRegistry(package, quiet)
	
	# return meta registry if no name is specified
	if( is.null(name) )	return(pkgreg)
	else{
		if( is.null(pkgreg) ){
			if( quiet ) return(NULL)
			# throw error
			stop("Could not find registry '", name, "' in package `", nm, "`: meta registry does not exist.")	
		} 
		# retrieve sub-registry entry
		nm <- packageSlot(pkgreg) 
		reg <- regfetch(pkgreg, key=name, exact=TRUE, error=FALSE)
		
		if( is.null(reg) ){# not found
			if( quiet ) return(NULL)
			# throw error
			stop("Could not find registry `", name, "` in package `", nm, "`.")
		}else{
			# synchronise and substitute by primary sub-registry (if necessary)
			if( update ) reg <- .update_pkgreg(reg)
			# return plain registry entry if requested
			if( entry )	return(reg)
			# return sub-registry object
			reg$regobj
		}
	}
}


packageRegistries <- function(regname=NULL, package=NULL, primary=FALSE){
	lns <- loadedNamespaces()
	if( !is.null(package) ) lns <- lns[lns %in% package]
	
	# early exit if no namespace
	if( !length(lns) ) return( character() )
	
	res <- lapply(lns, function(ns){
				reg <- packageRegistry(package=ns, quiet=TRUE)
				if( is.null(reg) ) return( character() )
				regnames <- reg$get_entry_names()
				res <- setNames(regnames, rep(ns, length(regnames)))
				if( primary ){
					pr <- sapply(res, function(n) reg$get_entry(n)$parent)
					res <- res[ nchar(pr) == 0L ]
				}
				res
			})
	
	res <- unlist(res)			
	if( !is.null(regname) ){
		res <- res[res == regname]
		if( primary && length(res) > 1L ){
			warning("Package registry - Found multiple primary registries '", regname, "' in packages "
					, str_out(res, Inf), " [using first one only]")
			res <- res[1L]
		}
	}
	res
}


regfetch <- function(regobj, ..., all=FALSE, error=TRUE, exact=FALSE
		, KEYS = NULL
		, verbose=FALSE, entry=FALSE, msg=NULL){
	
	# load the registry package
#	library(registry)
	# list -- all -- keys if no key is specified
	keylist <- allkeys <- regobj$get_entry_names()
	if( !all ) keylist <- grep("^[^.]", keylist, value=TRUE)
	
	index_fields <- if( !is.null(KEYS) ){
				if( !is.list(KEYS) ) stop("Invalid argument <KEYS>: must be a list of field values.")
				KEYS
			}else list(...)
	# extract primary key
	key <- if( length(index_fields) ){
		# remove fields that are not keys if named list
		if( !is.null(names(index_fields)) )
			index_fields <- regkeys(regobj, index_fields)
		if( length(index_fields) ){
			paste(unlist(index_fields), collapse='_')
			str_out(index_fields, Inf, use.names=TRUE)
		}
	}
	if( is.null(key) ){
		return(keylist)
	}
	
	# set verbosity level
	if( !missing(verbose) ){
		ol <- lverbose(verbose)
		on.exit( lverbose(ol) )
	}
	
	if( !is.null(msg) ) msg <- str_c(msg, ' - ')
	
	if( regobj$n_of_entries() == 0L ){
		if( error )	stop(msg, "Registry is empty: no matching entry for key ", dQuote(key), ".")
		else return(NULL)
	}
	
	# get entry
	d <- do.call(regobj$get_entries, index_fields)
	# no entry found
	if( is.null(d) ){
		if( error ){
			stop(msg, "No matching entry for key ", dQuote(key), " in the registry."
					, "\n  Use one of: ", str_wrap(str_out(sort(allkeys), Inf), exdent=2), '.')
		}else return(NULL)
	}
	
	# look for exact matches
	if( is.list(index_fields) ){
		ex <- sapply(d, function(x) all(mapply(identical, index_fields, x[names(index_fields)])))
	}else{
		ex <- names(d) == index_fields
	} 
	
	# limit to exact mathes
	if( length(i <- which(ex)) ){
		d <- d[i]
	}else if( exact ){
		if( error ){
			stop(msg, "No exact match for key '", key, "' in the registry."
					, "\n  Use one of: ", str_wrap(str_out(allkeys, Inf), exdent=2), '.')
		}else return(NULL) 
	}
	
	if( all ) return(d)
	
	# multiple match
#	str(d)
	if( length(d) > 1L ){
		if( error ){
			stop(msg, "Multiple entries found for key ", dQuote(key), ": ", str_out(sort(names(d)), Inf), '.')
		}else return(NA)
	}
	
	# check single match
	if( length(d) != 1L )
		stop("Unexpected error: more than one entry was selected.")
	
	# return single match
	d <- d[[1L]]
	
	# return registry object if the entry is an automatic sub-registry
	if( !entry && is(regobj, 'object_subregistry') ) d$object
	else d
}

clone_regobj <- function(regobj, empty=FALSE){
	tmp <- tempfile('registry')
	on.exit(unlink(tmp))
	saveRDS(regobj, file=tmp)
	newreg <- readRDS(tmp)
	# empty entries if necessary
	if( empty ){
		sapply(newreg$get_entry_names(), newreg$delete_entry)
	}
	newreg
}


.packageMetaRegistry <- function(package, quiet=FALSE, create=FALSE){
	
#	library(registry)
	metaregname <- '.packageRegistry'
	# get package environment
	e <- packageEnv(package)	
	# get namespace name
	nm <- packageName(e)
	
	# create registry environment if necessary
	if( !exists(metaregname, e, inherits=FALSE) ){
		if( !create ){
			if( quiet ) return(NULL)
			# throw error
			stop("Meta registry in package `", nm, "` does not exist.")
		}
#		if( !isLoadingNamespace(e) ){
#			stop("Can only create a package meta-registry when loading it"
#				," [loading namespace: ", if(!is.null(ns <- getLoadingNamespace()) ) ns else 'none', "].")
#		}
		message("Creating meta registry in package '", nm, "' ... ", appendLF=FALSE)
		# create registry object with special classes for the registry and entries
		meta <- registry(c(paste('package', nm, 'subregistry', sep='_'), 'package_subregistry')
				, c(paste('package', nm, 'metaregistry', sep='_'), 'package_metaregistry'))
		## set fields
		# access key
		meta$set_field("key", type="character", is_key = TRUE, index_FUN = match_exact)
		# sub-registry object
		meta$set_field("regobj", type="registry", is_mandatory = TRUE)
		# human readable description
		meta$set_field("description", type="character", is_mandatory = TRUE)
		# short object description
		meta$set_field("entrydesc", type="character", is_mandatory = TRUE)
		# parent package = owner of the primary registry
		meta$set_field("parent", type="character", default = '')
		# owner package (its value is forced)
		meta$set_field("package", type="character", default = nm, alternatives=nm)
		#
		
		# fix registry
		meta <- fix_registry(meta)
		# add package attribute
		attr(meta, 'package') <- nm
		# store within the calling package environment
		assign(metaregname,  meta, envir = e)
		message('OK')
	}
	# get package meta registry
	get(metaregname, envir=e, inherits = FALSE)
}


.update_pkgreg <- function(regentry){
	
	verbose <- getOption('verbose')
	
	# directly return entry if:
	# - one is loading the namespace of the package (primary or not)
	if( isLoadingNamespace(regentry$package) ) return(regentry) 		
	# - not a primary registry
	if( nchar(regentry$parent) > 0L ) return(regentry)
	
	primary <- regentry$package
	primaryreg <- regentry$regobj
	key <- regentry$key
	fullkey <- str_c(primary, '::', key)
	# sync if loaded packages changed
	hash <- digest(c(.cacheNS[[fullkey]], ns <- loadedNamespaces()))
#		print(ns)
#		print(.cacheNS)
	if( !identical(hash, .cacheMD5[[fullkey]]) ){
		if( verbose ) message('Updating registry ', fullkey, " ... ", appendLF=FALSE)
		# remove entries from unloaded packages
		if( length(.cacheNS[[fullkey]]) && length(notloaded <- setdiff(.cacheNS[[fullkey]], ns)) ){
			ndel <- sapply(notloaded, function(p){
						if( verbose > 1L ) message("\n Removing entries from package ", p, " ... ", appendLF=FALSE)
						e <- primaryreg$get_entry_names()
						n <- sapply(e, function(x){
									rec <- primaryreg$get_entry(x)
									if( rec$REGISTERINGpackage == p ){
										primaryreg$delete_entry(x)
										1L
									}else 0L
								})
						if( verbose > 1L ) message('OK [', sum(n), ']')
						sum(n)
					})
		}
		
		# list packages that have local versions of this registry
		reglist <- packageRegistries(fullkey)
#			print(reglist)
		pkgs <- names(reglist)
		# add entries from new packages into the primary registry
		if( length(miss <- setdiff(pkgs, .cacheNS[[fullkey]])) ){
			nadd <- sapply(miss, function(p){
						if( verbose > 1L ) message("\n Adding entries from package ", p, " ... ", appendLF=FALSE)
						reg <- packageRegistry(fullkey, package=p)
						e <- reg$get_entries()
						n <- sapply(e, function(x){
									# add entry if it does not exists already
									oldentry <- regfetch(primaryreg, KEYS=x, exact=TRUE, error=FALSE)
									if( is.null(oldentry) ){
										do.call(primaryreg$set_entry, x)
										1L
									}else 0L				
								})
						if( verbose > 1L ) message('OK [', sum(n), ']')
						sum(n)
					})
		}
		# store contributing packages and MD5 hash
		.cacheNS[[fullkey]] <<- pkgs
		.cacheMD5[[fullkey]] <<- digest(c(.cacheNS[[fullkey]], ns)) 
		if( verbose ) message('OK')
	}
	
	regentry
}


.update_pkgreg <- local({
			.cacheNS <- list()
			.cacheMD5 <- list()
			function(regentry){
				
				verbose <- getOption('verbose')
				
				# directly return entry if:
				# - one is loading the namespace of the package (primary or not)
				if( isLoadingNamespace(regentry$package) ) return(regentry) 		
				# - not a primary registry
				if( nchar(regentry$parent) > 0L ) return(regentry)
				
				primary <- regentry$package
				primaryreg <- regentry$regobj
				key <- regentry$key
				fullkey <- str_c(primary, '::', key)
				# sync if loaded packages changed
				hash <- digest(c(.cacheNS[[fullkey]], ns <- loadedNamespaces()))
#		print(ns)
#		print(.cacheNS)
				if( !identical(hash, .cacheMD5[[fullkey]]) ){
					if( verbose ) message('Updating registry ', fullkey, " ... ", appendLF=FALSE)
					# remove entries from unloaded packages
					if( length(.cacheNS[[fullkey]]) && length(notloaded <- setdiff(.cacheNS[[fullkey]], ns)) ){
						ndel <- sapply(notloaded, function(p){
									if( verbose > 1L ) message("\n Removing entries from package ", p, " ... ", appendLF=FALSE)
									e <- primaryreg$get_entry_names()
									n <- sapply(e, function(x){
												rec <- primaryreg$get_entry(x)
												if( rec$REGISTERINGpackage == p ){
													primaryreg$delete_entry(x)
													1L
												}else 0L
											})
									if( verbose > 1L ) message('OK [', sum(n), ']')
									sum(n)
								})
					}
					
					# list packages that have local versions of this registry
					reglist <- packageRegistries(fullkey)
#			print(reglist)
					pkgs <- names(reglist)
					# add entries from new packages into the primary registry
					if( length(miss <- setdiff(pkgs, .cacheNS[[fullkey]])) ){
						nadd <- sapply(miss, function(p){
									if( verbose > 1L ) message("\n Adding entries from package ", p, " ... ", appendLF=FALSE)
									reg <- packageRegistry(fullkey, package=p)
									e <- reg$get_entries()
									n <- sapply(e, function(x){
												# add entry if it does not exists already
												oldentry <- regfetch(primaryreg, KEYS=x, exact=TRUE, error=FALSE)
												if( is.null(oldentry) ){
													do.call(primaryreg$set_entry, x)
													1L
												}else 0L				
											})
									if( verbose > 1L ) message('OK [', sum(n), ']')
									sum(n)
								})
					}
					# store contributing packages and MD5 hash
					.cacheNS[[fullkey]] <<- pkgs
					.cacheMD5[[fullkey]] <<- digest(c(.cacheNS[[fullkey]], ns)) 
					if( verbose ) message('OK')
				}
				
				regentry
			}
		})


fix_registry <- function(regobj){
	
	# get private environment
	.REGENV <- environment(environment(regobj$n_of_entries)$f)
	
	# do not fix twice
	if( isFixed <- exists('.isFixed', .REGENV, inherits=FALSE) ){
		return(regobj)
	}
#	message("REGENV:\n", capture.output(print(ls(.REGENV, all=TRUE))))
#	message("env(delete_entry)\n", capture.output(print(ls(environment(environment(regobj$delete_entry)$f), all=TRUE))))
	# dummy variables for R CMD check
	PERMISSIONS <- 
			.get_entry_indices <-  
			.get_entry_names <- 
			SEALED_ENTRIES <-
			DATA <- 
			.delete_entry <- 
			NULL
	
	# .get_entries
	.get_entries <- get('.get_entries', .REGENV, inherits=FALSE)
	.get_fields <- get('.get_fields', .REGENV, inherits=FALSE)
	hook <- function(...){
		# remove fields that are not keys
		fld <- .get_fields()
		if( length(fld) ){
			keyfield <- names(fld[sapply(fld, function(x) isTRUE(x$is_key) )])
			index_fields <- list(...)
			if( !is.null(names(index_fields)) ){
				i <- match(keyfield, names(index_fields))
				index_fields <- index_fields[i[!is.na(i)]]
			}
		}
		do.call(.get_entries, index_fields)
	}
	assign('.get_entries', hook, .REGENV)
	#
	
	# fix bug in delete_entry
	hook <- function(...){
		key <- list(...)
		isString <- function(x) is.character(x) && length(x) == 1L
		if( length(key) == 1L && isString(key[[1L]]) ){
			
			errmsg <- paste0("Could not delete entry '", key[[1L]],"': ")
			if (!PERMISSIONS["delete_entries"]) 
				stop(errmsg, "deletion of entries denied due to restricted permissions.", call. = FALSE)
			entry_index <- .get_entry_indices(key)
			
			# fix: check for exact match (on full key)
			if( key[[1L]] %in% .get_entry_names() ){
				entry_index <- match(key[[1L]], .get_entry_names())
			}
			#end_fix
			if( !length(entry_index) ){
#				if( !quiet ) warning(errmsg, "not in registry.", immediate.=TRUE, call. = FALSE)
				return()
			}
			if (length(entry_index) != 1)
				stop(errmsg, "multiple matches.", call. = FALSE)
			if (entry_index %in% SEALED_ENTRIES) 
				stop(errmsg, "deletion of entry not allowed in sealed registry.", call. = FALSE)
			DATA[entry_index] <<- NULL
		} else .delete_entry(...)
	}
	environment(hook) <- .REGENV
	regobj$delete_entry <- hook
	#
	
	# fix bug in get_entry
	hook <- function(...){
		key <- list(...)
		isString <- function(x) is.character(x) && length(x) == 1L
		if( length(key) == 1L && isString(key[[1L]]) ){
			res <- .get_entries(...)
			if( key[[1L]] %in% names(res) )
				res[[key[[1L]]]]
			else res[[1L]] 
		}else .get_entries(...)[[1]]
	}
	environment(hook) <- .REGENV
	regobj$get_entry <- hook
	#
	
	# flag the registry as fixed
	assign('.isFixed', TRUE, .REGENV)
	# return fixed registry
	regobj
}



regkeys <- function(regobj, ...){
	
	# get keys
	fld <- regobj$get_fields()
	keyfield <- names(fld[sapply(fld, function(x) isTRUE(x$is_key) )])
	if( nargs() == 1L ) return(keyfield)
	
	index_fields <- list(...)
	if( is.null(names(index_fields)) && length(index_fields)==1L )
		index_fields <- index_fields[[1L]]
	index_fields <- index_fields[!sapply(index_fields, is.null)]
	if( !length(index_fields) ) return(list())
	
	# remove fields that are not keys
	i <- match(keyfield, names(index_fields))
	index_fields[i[!is.na(i)]]
}


setPackageRegistry <- function(regname, regobj
		, description='', entrydesc=NA
		, ...
		, package=topenv(parent.frame())
		, overwrite=FALSE){
	
#	library(registry)
	
	# force overwrite in dev mode
	if( missing(overwrite) && isDevNamespace(package) ){
		overwrite <- TRUE
	}
	# check if sub-registry already exists
	oldreg <- packageRegistry(regname, quiet=TRUE, package=package)
	if( !is.null(oldreg) && !overwrite ){
		return( oldreg )
	}
	
	# get meta-registry (force creation)
	regenv <- .packageMetaRegistry(package, create=TRUE)
	nm <- packageSlot(regenv)
	ns_str <- str_c("package '", nm, "'")
	
	if( !is.null(oldreg) ){
		if( !overwrite ){
			if( isLoadingNamespace() ){ # exit if loading a namespace
				message("NOTE: Did not create registry '", regname,"' in ", ns_str, ": registry already exists.")
				return(oldreg)
			}
			stop("Could not create registry '", regname,"' in ", ns_str, ": registry already exists")
		}else{
			message("Removing registry '", regname,"' from ", ns_str)
			regenv$delete_entry(regname)
		}
	}
	message("Creating registry '", regname,"' in ", ns_str, ' ... ', appendLF=FALSE)
	
	.add_regclass <- function(x, newcl, before){
		cl <- class(x)
		ir <- which(cl == before)
		class(x) <- c(if( ir > 1 ) cl[1:(ir-1)] 
				, newcl, cl[ir:length(cl)])
		x
	}
	
	pkgregclass <- c(paste(regname, 'package_registry', sep='_'), 'package_registry')
	if( is.character(regobj) ){# regobj specifies the S4 class of the registry entries
		objtype <- regobj[1]
		regobj <- registry(entry_class = paste(regname, 'entry', sep='_')
				, registry_class = c(pkgregclass, 'object_subregistry'))
		# access key
		regobj$set_field("key", type="character", is_key = TRUE
				, index_FUN = match_partial_ignorecase)
		# object
		regobj$set_field("object", type=objtype, is_mandatory=TRUE, validity_FUN = validObject)
	}else if( is(regobj, 'registry') ){
		if( !is(regobj, 'package_registry') ){
			regobj <- .add_regclass(regobj, pkgregclass, 'registry')
		}
	}else{
		message('ERROR')
		stop("Invalid argument 'regobj': must be a class name or a registry object.")
	}
	# add field for REGISTERING package
	if( !"REGISTERINGpackage" %in% regobj$get_field_names() )
		regobj$set_field("REGISTERINGpackage", type='character', is_mandatory=TRUE, index_FUN=match_exact)
	# fix registry object
	regobj <- fix_registry(regobj)
	# add package
	attr(regobj, 'package') <- nm
	
	# create new meta entry
	regenv$set_entry(key=regname, regobj=regobj
			, description=description, entrydesc=entrydesc
			, ...)
	message('OK')
	# return newly created registry
	regenv$get_entry(regname)$regobj
}


setPackageRegistryEntry <- function(regname, key, ..., overwrite=FALSE, verbose=FALSE
		, where=topenv(parent.frame()), msg=NULL){
	
	if( isLoadingNamespace() ){
		verbose <- TRUE
		if( missing(overwrite) ) overwrite <- TRUE
	}
	registry <- regname
	package <- where
	
	# check if the name provided is not empty
	if( nchar(key) == 0 ) stop('Invalid argument <key>: cannot be an empty string.')
	
	# build full key, that includes the name of the top calling namespace
	fullkey <- key
	top_ns <- topns(strict=FALSE)
	#
	
	# retrieve package registry (it must exist or this will throw an error)
	package <- packageEnv(package)
	subregentry <- packageRegistry(registry, package=package, entry=TRUE, update=TRUE)
	# get regobj (do that to ensure it is updated with entries from other packages)
	regobj <- subregentry$regobj
	
	# setup complete list of fields
	fields <- list(...)
	objdesc <- if( !is_NA(subregentry$entrydesc) ) subregentry$entrydesc else paste(registry, 'object')
	objdesc <- paste(objdesc, " '", key, "'", sep='')
	if( length(fields)==1L ){
		objdesc <- paste(objdesc, ' [', class(fields[[1L]]), ']', sep='')
		if( is.null(names(fields)) && is(regobj, 'object_subregistry') )
			names(fields) <- 'object'
	}
	fields$key <- key
	regpkg <- packageName(top_ns, .Global=TRUE)
	fields$REGISTERINGpackage <- regpkg
#	str(fields)
	#
	
	# check if the object is already registered
	oldentry <- regfetch(regobj, KEYS=fields, exact=TRUE, error=FALSE, all=TRUE)
	# error if already exists and not overwriting		
	if( !is.null(oldentry) && !overwrite ){ 
		if( verbose ) message("ERROR")
		stop("Cannot register ", objdesc, ": key already exists.")	
	}
	
	# add entry
	if( verbose ){
		action <- if( is.null(oldentry) ) 'Registering' else 'Replacing'
		message(action, " ", objdesc, msg, " ... ", appendLF=FALSE)
	}
	# delete old entry
	if( !is.null(oldentry) ){
		regobj$delete_entry(names(oldentry)[1L])
	}
	# do add entry
	do.call(regobj$set_entry, fields)
	if( verbose ) message("OK")
	
	# if the registration happens during loading another package: 
	# create local registry and add entry to it.
	# It will be merged to the main registry on the next call to 
	# packageRegistry after the package is loaded.
	lns <- getLoadingNamespace(env=TRUE)
	if( !is.null(lns <- getLoadingNamespace(env=TRUE)) && !identical(lns, package) ){
		# clone registry
		if( nchar(subregentry$parent) ){
			warning("Deriving package registry '", registry, "' in package ", lns
					, " from ", subregentry$parent, " instead of ", subregentry$package, immediate.=TRUE)
			parent <- subregentry$parent
		}else parent <- subregentry$package
		fullregistry <- str_c(parent, '::', registry)
		
		if( is.null(locregobj <- packageRegistry(fullregistry, package=lns, quiet=TRUE)) ){
			# clone registry
			locregobj <- clone_regobj(regobj, empty=TRUE)
			# attach to loading namespace
			locregobj <- setPackageRegistry(fullregistry, locregobj
					, description = subregentry$description
					, entrydesc = subregentry$entrydesc
					, parent = parent
					, package = lns)
		}
		
		action <- 'Adding'
		if( !is.null(locentry <- regfetch(locregobj, KEYS=fields, exact=TRUE, error=FALSE, all=TRUE)) ){
			action <- 'Overwriting'
			locregobj$delete_entry(names(locentry)[1L])
		}
		# add entry into local registry
		if( verbose ) message(action, " entry '", key, "' in registry '", packageName(lns), "::", fullregistry, "' ... ", appendLF=FALSE)
		do.call(locregobj$set_entry, fields)
		if( verbose ) message("OK")
	}
	#
	
	# return registered object
	regfetch(regobj, KEYS=fields, exact=TRUE)
	
}


regfetch <- function(regobj, ..., all=FALSE, error=TRUE, exact=FALSE
		, KEYS = NULL
		, verbose=FALSE, entry=FALSE, msg=NULL){
	
	# load the registry package
#	library(registry)
	# list -- all -- keys if no key is specified
	keylist <- allkeys <- regobj$get_entry_names()
	if( !all ) keylist <- grep("^[^.]", keylist, value=TRUE)
	
	index_fields <- if( !is.null(KEYS) ){
				if( !is.list(KEYS) ) stop("Invalid argument <KEYS>: must be a list of field values.")
				KEYS
			}else list(...)
	# extract primary key
	key <- if( length(index_fields) ){
		# remove fields that are not keys if named list
		if( !is.null(names(index_fields)) )
			index_fields <- regkeys(regobj, index_fields)
		if( length(index_fields) ){
			paste(unlist(index_fields), collapse='_')
			str_out(index_fields, Inf, use.names=TRUE)
		}
	}
	if( is.null(key) ){
		return(keylist)
	}
	
	# set verbosity level
	if( !missing(verbose) ){
		ol <- lverbose(verbose)
		on.exit( lverbose(ol) )
	}
	
	if( !is.null(msg) ) msg <- str_c(msg, ' - ')
	
	if( regobj$n_of_entries() == 0L ){
		if( error )	stop(msg, "Registry is empty: no matching entry for key ", dQuote(key), ".")
		else return(NULL)
	}
	
	# get entry
	d <- do.call(regobj$get_entries, index_fields)
	# no entry found
	if( is.null(d) ){
		if( error ){
			stop(msg, "No matching entry for key ", dQuote(key), " in the registry."
					, "\n  Use one of: ", str_wrap(str_out(sort(allkeys), Inf), exdent=2), '.')
		}else return(NULL)
	}
	
	# look for exact matches
	if( is.list(index_fields) ){
		ex <- sapply(d, function(x) all(mapply(identical, index_fields, x[names(index_fields)])))
	}else{
		ex <- names(d) == index_fields
	} 
	
	# limit to exact mathes
	if( length(i <- which(ex)) ){
		d <- d[i]
	}else if( exact ){
		if( error ){
			stop(msg, "No exact match for key '", key, "' in the registry."
					, "\n  Use one of: ", str_wrap(str_out(allkeys, Inf), exdent=2), '.')
		}else return(NULL) 
	}
	
	if( all ) return(d)
	
	# multiple match
#	str(d)
	if( length(d) > 1L ){
		if( error ){
			stop(msg, "Multiple entries found for key ", dQuote(key), ": ", str_out(sort(names(d)), Inf), '.')
		}else return(NA)
	}
	
	# check single match
	if( length(d) != 1L )
		stop("Unexpected error: more than one entry was selected.")
	
	# return single match
	d <- d[[1L]]
	
	# return registry object if the entry is an automatic sub-registry
	if( !entry && is(regobj, 'object_subregistry') ) d$object
	else d
}


pkgreg_fetch <- function(regname, ..., msg=NULL, where=topenv(parent.frame())){
	# get package registry
	regentry <- packageRegistry(regname, package=where, entry=TRUE, update=TRUE)
	# define addon error message
	if( missing(msg) && !is_NA(regentry$entrydesc) ) msg <- regentry$entrydesc
	# fetch from registry
	regfetch(regentry$regobj, ..., msg=msg)
}

pkgreg_remove <- function(regname, ..., msg=NULL, where=topenv(parent.frame()), quiet=FALSE){
	# get package registry
	regentry <- packageRegistry(regname, package=where, entry=TRUE, update=TRUE)
	# define addon error message
	if( missing(msg) && !is_NA(regentry$entrydesc) ) msg <- regentry$entrydesc
	# fetch from registry
	entry <- regfetch(regentry$regobj, ..., exact=TRUE, error=FALSE, all=TRUE, msg=msg)
	
	res <- if( !is.null(entry) ){
				# get the method registry and the method's fullname
				name <- names(entry)
				
				if( !quiet ){
					msg <- paste0("Removing ", msg, " '", name, "' from registry '", regname, "'")
					message(msg, ' ... ', appendLF=FALSE)
				}
				# delete from registry
				regentry$regobj$delete_entry(name)
				if( !quiet ) message('OK')
				TRUE
			}else{
				if( !quiet ){
					name <- str_out(list(...), Inf, use.names=TRUE)
					warning("Could not remove ", msg, " '", name, "': no matching registry entry.", call.=FALSE)
				}
				FALSE
			}
	
	if( quiet ) invisible(res)
	else res
}

str_out <- function(x, max=3L, quote=is.character(x), use.names=FALSE, sep=", ", total = FALSE){
	if( is_NA(max) ) max <- Inf
	suffix <- NULL
	nTotal <- length(x)
	if( max > 2 && length(x) > max ){
		suffix <- "..."
		x <- c(head(x, max-1), tail(x, 1))
	}
	x <- head(x, max)
	
	# add quotes if necessary
	quote <- 
			if( isTRUE(quote) ) "'"
			else if( is.character(quote) ) quote
	if( !is.null(quote) ) x <- unlist(lapply(x, function(v) paste(quote,v,quote, sep='')))
	else if( all(sapply(x, isInteger)) ) x <- unlist(lapply(x, function(v) str_c(v,'L')))
	# add names if necessary
	if( use.names && !is.null(names(x)) ){
		nm <- str_c(names(x),'=')
		x <- paste(ifelse(nm=='=','',nm), x, sep='')
	}
	# insert suffix
	if( !is.null(suffix) ){
		x <- c(head(x, length(x)-1L), suffix, tail(x, 1L))
	}
	s <- paste(paste(x, collapse=sep), sep='')
	
	if( total ) s <- paste0(s, ' (', format(nTotal, big.mark=",", scientific=F), ' total)')
	
	# return formatted string 
	s
}


is_NA <- function(x){ 
	is.atomic(x) && length(x) == 1L && is.na(x)
#   x <- unname(x)
#	identical(x, NA) || identical(x, as.character(NA)) || identical(x, as.numeric(NA)) || identical(x, as.integer(NA))
}

isInteger <- function(x){ 
	is.integer(x) && length(x) == 1
}


topns <- function(strict=TRUE){
	ns <- topns_name(n=1L, strict=strict)
	if( ns == '.GlobalEnv' ) return( .GlobalEnv )
	else if( nchar(ns) ) asNamespace(ns)
	#packageEnv(skip=TRUE, verbose=verbose)
}


topns_name <- function(n=1L, strict=TRUE, unique=TRUE){
	
	if( n==1L && !is.null(ns <- getLoadingNamespace()) ){
		return(ns)
	}
	nf <- sys.nframe()
	i <- 0
	res <- character()
	while( i <= nf && length(res) < n ){
		e <- sys.frame(i)
		if( !strict || !identical(e, .GlobalEnv) ){
			pkg <- methods::getPackageName(e, create=FALSE)
			if( pkg != '' ){
				res <- c(res, pkg)
			}
		}
		i <- i + 1
	}
	
	if( !length(res) ){# try with packageEnv
		e <- packageEnv(skip=TRUE)
		if( isNamespace(e) ){
			res <- methods::getPackageName(e)
#			print(res)
		}else{
			#warning('Could not find top namespace.', immediate.=TRUE)
			return('')
		}
	}
	
	if( unique || n==1L ) res <- match.fun('unique')(res)
	if( length(res) || n>1L ) res else ''
}

hasArg2 <- function (name) 
{
	name <- as.name(name)
	## apply methods::hasArg
	aname <- as.character(substitute(name))
	fnames <- names(formals(sys.function(sys.parent())))
	if (is.na(match(aname, fnames))) {
		if (is.na(match("...", fnames))) 
			FALSE
		else {
			dotsCall <- eval(quote(substitute(list(...))), sys.parent())
			!is.na(match(aname, names(dotsCall)))
		}
	}
	else eval(substitute(!missing(name)), sys.frame(sys.parent()))
	##
}


allFormals <- function(f){
	
	# look inside method for S4 methods
	if( is(f, 'MethodDefinition') ){
		
		# check if the method is defined as a wrapper function
		f <- f@.Data
		lf <- try(codetools::getAssignedVar(body(f)), silent=TRUE)
		if( !identical(lf, '.local') ) return( formals(f) )
		# extract arguments from local function
		lfun <- extractLocalFun(f)
		res <- formals(lfun)
		# set default values from the generic, only for arguments that have no 
		# default values in the method
		generic_args <- formals(f)
		meth_no_default <- sapply(res, is.symbol) 
		gen_no_default <- sapply(generic_args, is.symbol)
		generic_args <- generic_args[ !gen_no_default ]
		generic_args <- generic_args[ names(generic_args) %in% names(res[meth_no_default]) ]
		if( length(generic_args) ){
			res[names(generic_args)] <- generic_args
		}
		# return complete list of arguments
		res
		
	}else if( is.function(f) ) formals(f)
	
}

extractLocalFun <- function(f){
	bf <- body(f)
	
	txt <- as.character(bf)[2]
	# in R-2.14.2 -- at least, as.character does not return the complete body
	# so some text manipulation is necessary 
	if( !grepl("\\{", txt) ){
		sf <- capture.output(print(bf))
		w <- tail(grep("^\\s*\\.local\\(", sf), 1L)
		txt <- paste(sf[-w], collapse="\n")
	}
	expr <- parse(text=txt)
	e <- new.env()
	eval(expr, e)
} 

expand_list <- function(x, ..., .exact=TRUE, .names=!.exact){
	
	# extract defaults from ... arguments
	defaults <- list(...)
	if( length(defaults) == 1L && is.null(names(defaults)) ){
		defaults <- defaults[[1L]]
	}
	# early exit if no defaults
	if( !length(defaults) ) return(x)
	
	# match names from x in defaults
	x_ex <- x
	if( !.exact ){
		i <- pmatch(names(x), names(defaults))
		# first expand names if necessary
		if( length(w <- which(!is.na(i))) ){
			names(x_ex)[w] <- names(defaults)[i[w]]
			# apply to as well if necessary
			if( .names ) names(x)[w] <- names(defaults)[i[w]]
		}
	}
	
	# expand list
	i <- match(names(defaults), names(x_ex))
	if( length(w <- which(is.na(i))) ){
		n <- names(defaults)[w]
		lapply(n, function(m){
					if( is.null(defaults[[m]]) ) x[m] <<- list(NULL) 
					else x[[m]] <<- defaults[[m]]
				})
	}
	
	x
}



.getExtraEnv <- function(package){
	if( missing(package) || is.null(package) ) where <- topns(FALSE)
	else if( isString(package) ) {
		package <- sub("^package:", "", package)
		if( package == 'R_GlobalEnv') where <- .GlobalEnv
		else where <- asNamespace(package)
	}
	else stop("Invalid argument `package`: must be missing or a package name.")
	where
}

# extra handler registry
extra_handlers <- setPackageRegistry('extra_handler', 'function' 
		, description = 'Handler functions for package-specific extra tasks'
		, entrydesc = 'extra handler')

# extra action registry
extra_actions <- registry()
extra_actions$set_field("key", type="character", is_key = TRUE, index_FUN = match_exact)
extra_actions$set_field("package", type="character", is_key = TRUE, index_FUN = match_exact)
extra_actions$set_field("handler", type='character', is_mandatory=TRUE, is_key=TRUE)
extra_actions$set_field("args", type='list', default=list())
extra_actions <- setPackageRegistry('extra_action', extra_actions
		, description = 'Handler functions for package-specific extra actions'
		, entrydesc = 'extra action')

#' Install/Run Extra Things After Standard Package Installation
#' 
#' @description
#' These functions define a framework to register actions for which default sets of arguments
#' can be defined when (lazy-)loading a package, and run later on, e.g., after the package 
#' is installed using dedicated commands.
#' 
#' \code{setPackageExtraHandler} defines main action handler functions, for which 
#' actions are defined as a set of arguments and registered using \code{setPackageExtra}. 
#'  
#' @param handler name of a handler, e.g, \code{'install'}.
#' It must be unique across all handlers registered by any other packages.  
#' @param fun handler function that will be called with the arguments registered
#' with \code{packageExtra(name, ...)}
#' @param package package name where to store/look for the internal registries.
#' End users should not need to use this argument.
#' 
#' @return the runner function associated with the newly registered handler,
#' as built by \code{packageExtraRunner}.  
#'  
#' @rdname packageExtra
setPackageExtraHandler <- function(handler, fun, ...){
	
	# add entry to the registry
	setPackageRegistryEntry('extra_handler', handler, fun, ...)
	# build associated runner
	runner <- packageExtraRunner(handler)
}

#' @describeIn packageExtra retrieves a given handler from the registry. 
#' 
#' @param ... extra arguments passed to internal function calls.
#' In \code{packageExtraHandler}, these are passed to \code{\link{pkgreg_fetch}}.
#' 
#' In \code{setPackageExtra}, these define default arguments for the handler function. 
#' These are overwritten by arguments in the call to runner function if any.
#'  
packageExtraHandler <- function(handler=NULL, ...){
	# load handler from registry
	pkgreg_fetch('extra_handler', key=handler, ...)
}
#' @describeIn packageExtra registers extra actions for a given handler.
#' 
#' For example, calling \code{setPackageExtra('install', pkgs='non_CRAN_pkg', repos='http://non-standard-repo')}
#' in a source file of package 'myPkg' registers the call 
#' \code{install.packages('non_CRAN_pkg', repos='http://non-standard-repo', ...)}
#' in a registry internal to the package. 
#' All calls to \code{setPackageExtra('install', ...)} can then be run by the user, as
#' a post installation step via \code{install.extrapackages('myPkg', ..)}.
#' 
#' @param extra name of the extra action.
#' @param .wrap logical that indicates if a function that runs the extra action should
#' be returned or only the default arguments
#' 
setPackageExtra <- function(handler, extra, ...){
	
	# check that a handler is defined in the registry
	fhandler <- packageExtraHandler(handler, exact=TRUE, error=FALSE)
	if( is.null(fhandler) ){
		handlers <- packageExtraHandler()
		stop("Could not register action '", extra, "': handler '", handler, "' is not defined"
				, if( length(handlers) ){
							str_c(".\n  Available handlers are: ", str_out(handlers, Inf))
						} else " [handler registry is empty]." )
	}
	args <- list(...)
	pkg <- packageName(topenv(parent.frame()), .Global=TRUE)
	setPackageRegistryEntry('extra_action', key=extra, handler=handler, args=args
			, package = pkg, where = topenv()
			, msg=str_c(" for handler '", handler, "'"))
}


.wrapExtra <- function(fhandler, args=list()){
	
	# define wrapper function
	f <- function(...){
		cl <- match.call()
		cl[[1L]] <- as.name('fhandler')
		# add default arguments
		lapply(names(args), function(a){
					if( !a %in% names(cl) )
						cl[[a]] <<- as.name(substitute(a, list(a=a)))
				})
		eval(cl)
	}
	# set registered arguments as default arguments
	formals(f) <- c(args, formals(f))
	f
}
#' @describeIn packageExtra retrieve a given extra action, either as its registry entry,
#' or as a function that would perform the given action.
#' 
packageExtra <- function(handler=NULL, extra=NULL, package=NULL, .wrap=FALSE){
	
	# load extra registry
	extras <- pkgreg_fetch('extra_action', key=extra, handler=handler, package=package
			, exact=TRUE, all=!.wrap)
	
	# return whole registry if no other argument is provided
	if( missing(handler) || is.null(extra) || !.wrap ) return( extras )
	
	args <- extras$args
	fhandler <- packageExtraHandler(handler, package='pkgmaker')
	if( is.null(fhandler) ){
		handlers <- packageExtraHandler(package='pkgmaker')
		stop("Could not find action handler '", handler, "' in pkgmaker global handler registry.\n"
				, "  Available handlers are: ", str_out(handlers, Inf))
	}
	# define wrapper function
	.wrapExtra(fhandler, args)		
}
#' @describeIn packageExtra defines a function to run all or some of the actions registered 
#' for a given handler in a given package.
#' For example, the function \code{install.extrapackages} is the runner defined for the extra handler \code{'install'} 
#' via \code{packageExtraRunner('install')}.
#' 
#' @param .verbose logical that indicates if verbose messages about the extra actions being
#' run should be displayed.
#' 
packageExtraRunner <- function(handler){
	
	.handler <- handler
	function(package, extra=NULL, handler=NULL, ..., .verbose=getOption('verbose')){
		
		if( missing(handler) ) handler <- .handler
		.local <- function(p, ...){
			# load list of extras
			extras <- packageExtra(handler=handler, extra=extra, package=p)
			# execute extras
			sapply(extras, 
					function(def, ...){
						e <- def$key
						h <- def$handler
						f <- packageExtra(handler=h, extra=e, package=p, .wrap=TRUE)
						if( .verbose ){
							message("# Running extra action '", h, ':', e, "' ...")
							message("# Action: ", str_fun(f))
							on.exit( message("# ERROR [", e, "]\n") )
						}
						res <- f(...)
						if( .verbose ){
							on.exit()
							message("# OK [", e, "]\n")
						}
						res
					}
					, ...)
		}
		invisible(sapply(package, .local, ...))
	}
}

#' @describeIn packageExtra runs all extra actions registered for a given package.
#' 
install.extras <- packageExtraRunner(NULL)
#' @describeIn packageExtra install sets of packages that can enhance a 
#' package, but may not be available from CRAN.
#' 
#' It is defined as the extra handler for 
#' the extra action handler \code{'install.packages'}.
#' All arguments in \code{...} are passed to \code{\link{install.packages}}.
#' By default, packages that are already installed are not re-installed.
#' An extra argument \code{force} allows to force their installation.
#' The packages are loaded if their installation is successful. 
#' 
install.extrapackages <- setPackageExtraHandler('install.packages', 
		function(pkgs, ..., force=FALSE){
			res <- sapply(pkgs, function(pkg, ...){
						if( force || !require.quiet(pkg, character.only=TRUE) ){
							install.packages(pkg, ...)
							require(pkg, character.only=TRUE)
						}else message("Loaded extra package: ", pkg)
					}, ...)
		}
)



oneoffVariable <- function(default=NULL){
	.var <- default
	function(value){
		if( missing(value) ){
			res <- .var
			.var <<- default
			res
		}else
			.var <<- value
	}
}


Sys.getenv_value <- function(name, raw = FALSE){
	val <- Sys.getenv(name, unset = NA, names = FALSE)
	if( raw ) return(val)
	# convert false values to FALSE if required
	if( is.na(val) || !nchar(val) || identical(tolower(val), 'false') || val == '0' ){
		val <- FALSE
	}
	val
}








# NMF algorithm registry access methods
# 
# Author: Renaud Gaujoux
###############################################################################

#' @include registry.R
#' @include NMFStrategy-class.R
#' @include NMFStrategyFunction-class.R
#' @include NMFStrategyIterative-class.R
NULL

# create sub-registry for NMF algorithm
.registryAlgorithm <- setPackageRegistry('algorithm', "NMFStrategy"
		, description = "Algorithms to solve MF optimisation problems"
		, entrydesc = "NMF algorithm") 

nmfAlgorithmInfo <- function(show=TRUE){
    obj <- .registryAlgorithm
    if( show ) print(obj)
    invisible(obj)
}

# specific register method for registering NMFStrategy objects
setMethod('nmfRegister', signature(key='NMFStrategy', method='missing'), 
		function(key, method, ...){
			nmfRegister(name(key), key, ..., regname='algorithm')
		}
)

#' Registering NMF Algorithms
#' 
#' Adds a new algorithm to the registry of algorithms that perform 
#' Nonnegative Matrix Factorization.
#'  
#' @inheritParams NMFStrategy
#' @param ... arguments passed to the factory function \code{\link{NMFStrategy}},
#' which instantiate the \code{\linkS4class{NMFStrategy}} object that is stored
#' in registry. 
#' @param overwrite logical that indicates if any existing NMF method with the 
#' same name should be overwritten (\code{TRUE}) or not (\code{FALSE}), 
#' in which case an error is thrown.
#' @param verbose a logical that indicates if information about the registration 
#' should be printed (\code{TRUE}) or not (\code{FALSE}).
#' 
#' @export
#' @examples 
#' 
#' # define/regsiter a new -- dummy -- NMF algorithm with the minimum arguments
#' # y: target matrix
#' # x: initial NMF model (i.e. the seed)
#' # NB: this algorithm simply return the seed unchanged 
#' setNMFMethod('mynmf', function(y, x, ...){ x })
#' 
#' # check algorithm on toy data
#' res <- nmfCheck('mynmf')
#' # the NMF seed is not changed
#' stopifnot( nmf.equal(res, nmfCheck('mynmf', seed=res)) ) 
#' 
setNMFMethod <- function(name, method, ..., overwrite=isLoadingNamespace(), verbose=TRUE){
		
	# build call to NMFStrategy constructor
	call_const <- match.call(NMFStrategy)
	call_const[[1]] <- as.name('NMFStrategy')
	call_const$verbose <- NULL
	call_const$overwrite <- NULL
	# swap name and method if method is missing and name is a registered method
	if( missing(method) && !missing(name) && is.character(name) && existsNMFMethod(name) ){
		call_const$method <- name
		call_const$name <- NULL
	}
	# build the NMFStrategy object (in the parent frame to get the package slot right)
	e <- parent.frame()
	method <- eval(call_const, envir=e)
	# add to the algorithm registry
	res <- nmfRegister(method, overwrite=overwrite, verbose=verbose)
	# return wrapper function invisibly
	wrap <- nmfWrapper(method)
}

#' \code{nmfRegisterAlgorithm} is an alias to \code{setNMFMethod} for backward
#' compatibility.
#' 
#' @export 
#' @rdname setNMFMethod
nmfRegisterAlgorithm <- setNMFMethod


#' Registry for NMF Algorithms 
#' 
#' @name methods-NMF
#' @rdname registry-algorithm
#' @family regalgo Registry for NMF algorithms
NULL

#' Testing Compatibility of Algorithm and Models
#' 
#' \code{canFit} is an S4 generic that tests if an algorithm can 
#' fit a particular model.
#' 
#' @param x an object that describes an algorithm
#' @param y an object that describes a model
#' @param ... extra arguments to allow extension
#' 
#' @export
#' @inline
#' @family regalgo
setGeneric('canFit', function(x, y, ...) standardGeneric('canFit') )
#' Tells if an NMF algorithm can fit a given class of NMF models
#' 
#' @param exact for logical that indicates if an algorithm is considered able to fit 
#' only the models that it explicitly declares (\code{TRUE}), or if it should be
#' considered able to also fit models that extend models that it explicitly fits. 
#'    
setMethod('canFit', signature(x='NMFStrategy', y='character'),
		function(x, y, exact=FALSE){
			
			if( !exact ){
				
				# check for one model amongst all the models fittable by the strategy
				can <- if( length(mo <- modelname(x)) > 1 )
							sapply(mo, function(m) extends(y, m))
						else extends(y, mo)
				any(can)
				
			}else
				is.element(y, modelname(x))
		}
)
#' Tells if an NMF algorithm can fit the same class of models as \code{y}
setMethod('canFit', signature(x='NMFStrategy', y='NMF'),
		function(x, y, ...){
			canFit(x, modelname(y), ...)
		}
)
#' Tells if a registered NMF algorithm can fit a given NMF model
setMethod('canFit', signature(x='character', y='ANY'),
		function(x, y, ...){
			canFit(nmfAlgorithm(x), y, ...)
		}
)

#' \code{selectNMFMethod} tries to select an appropriate NMF algorithm that is 
#' able to fit a given the NMF model.
#' 
#' @param name name of a registered NMF algorithm
#' @param model class name of an NMF model, i.e. a class that inherits from class 
#' \code{\linkS4class{NMF}}.
#' @param load a logical that indicates if the selected algorithms should be loaded
#' into \code{NMFStrategy} objects
#' @param all a logical that indicates if all algorithms that can fit \code{model}
#' should be returned or only the default or first found.
#' @param quiet a logical that indicates if the operation should be performed quietly, 
#' without throwing errors or warnings.
#' 
#' @return \code{selectNMFMethod} returns a character vector or \code{NMFStrategy} objects, 
#' or NULL if no suitable algorithm was found.
#' 
#' @rdname registry-algorithm
#' 
selectNMFMethod <- function(name, model, load=FALSE, exact=FALSE, all=FALSE, quiet=FALSE){
	
	# lookup for an algorithm suitable for the given NMF model
	if( !isNMFclass(model) )
		stop("argument 'model' must be the name of a class that extends class 'NMF'")
	
	
	algo_list <- if( !missing(name) ){
				algo <- nmfAlgorithm(name)
				name(algo) 
			}else nmfAlgorithm()
	
	# lookup for all the algorithms that can fit the given model
	#NB: if only one model needs to be selected then first look for an exact fit as 
	# this would need to be done with exact=FALSE and TRUE anyways
	w <- sapply(algo_list, canFit, model, exact= if(all) exact else TRUE)	
	algo <- algo_list[w]
	
	# if no suitable algorithm was found, and an exact match is not required 
	# then look for other potential non-exact algorithms
	if( !all && !exact && length(algo) == 0 ){
		w <- sapply(algo_list, canFit, model, exact=FALSE)
		algo <- algo_list[w]
	}
	
	# return NULL if no algorithm was found
	if( length(algo) == 0L ){
		if( !quiet ) 
			stop("Could not find an NMF algorithm to fit model '", model, "'"
					, if( !missing(name) ) paste(" amongst ", str_out(algo_list, Inf)))
		return(NULL)
	}
	
	# if all=FALSE then try to choose the default algorithm if present in the list, or the first one
	res <- if( !all && length(algo) > 1L ){
				
				idx <- which( algo == nmf.getOption('default.algorithm') ) 
				if( !length(idx) ) idx <- 1L
				
				res <- algo[idx]
				if( !quiet ) 
					warning("Selected NMF algorithm '", res, "' amongst other possible algorithm(s): "
							, paste(paste("'", algo[-idx], "'", sep=''), collapse=", "))
				res
			}else # otherwise return all the algorithms
				algo
	
	# load the methods if required
	if( load ){
		if( length(res) > 1 ) sapply(res, nmfAlgorithm) else nmfAlgorithm(res)
	}
	else
		res	
}


#' \code{getNMFMethod} retrieves NMF algorithm objects from the registry.
#' 
#' @param ... extra arguments passed to \code{pkgreg_fetch}
#' or \code{pkgreg_remove}.
#' 
#' @export
#' @rdname registry-algorithm
getNMFMethod <- function(...) nmfGet('algorithm', ...)

#' Listing and Retrieving NMF Algorithms
#' 
#' \code{nmfAlgorithm} lists access keys or retrieves NMF algorithms that are 
#' stored in registry.
#' It allows to list 
#'  
#' @param name Access key. 
#' If not missing, it must be a single character string that is partially matched 
#' against the available algorithms in the registry.
#' In this case, if \code{all=FALSE} (default), then the algorithm is returned 
#' as an \code{NMFStrategy} object that can be directly passed to \code{\link{nmf}}.
#' An error is thrown if no matching algorithm is found.
#' 
#' If missing or \code{NULL}, then access keys of algorithms -- that 
#' match the criteria \code{version}, are returned.
#' This argument is assumed to be regular expression if \code{all=TRUE} or 
#' \code{version} is not \code{NULL}.
#' @param version version of the algorithm(s) to retrieve. 
#' Currently only value \code{'R'} is supported, which searched for plain R 
#' implementations. 
#' @param all a logical that indicates if all algorithm keys should be returned, 
#' including the ones from alternative algorithm versions (e.g. plain R 
#' implementations of algorithms, for which a version based on optimised 
#' C updates is used by default). 
#' @param ... extra arguments passed to \code{\link{getNMFMethod}} when \code{name} 
#' is not \code{NULL} and \code{all=FALSE}. It is not used otherwise.
#' 
#' @return an \code{\linkS4class{NMFStrategy}} object if \code{name} is not 
#' \code{NULL} and \code{all=FALSE}, or a named character vector that contains 
#' the access keys of the matching algorithms.
#' The names correspond to the access key of the primary algorithm: e.g. 
#' algorithm \sQuote{lee} has two registered versions, one plain R (\sQuote{.R#lee}) 
#' and the other uses optimised C updates (\sQuote{lee}), which will all get 
#' named \sQuote{lee}.
#' 
#' @export
#' @family regalgo
#' 
#' @examples 
#' 
#' # list all main algorithms 
#' nmfAlgorithm()
#' # list all versions of algorithms 
#' nmfAlgorithm(all=TRUE)
#' # list all plain R versions 
#' nmfAlgorithm(version='R')
#'  
nmfAlgorithm <- function(name=NULL, version=NULL, all=FALSE, ...){	
	
	# if one passes an NMFStrategy just returns it
	if( is(name, 'NMFStrategy') ) return(name)
	
	# force all=TRUE if type is provided
	if( !is.null(version) ) all <- TRUE
	
	# directly return the algorithm object if a key is supplied and all=FALSE
	if( !is.null(name) && !all ) return( getNMFMethod(name, ...) )
	
	# get all algorithms
	algo <- getNMFMethod(all=TRUE)
	# set names to match the primary key
	algo <- setNames(algo, sub("^\\.(.+#)?", '', algo))	
	# filter out hidden methods
	if( !all ) algo <- algo[!grepl("^\\.", algo)]
	# filter out methods not from the requested algorithm
	if( !is.null(name) ) algo <- algo[grepl(str_c("^", name), names(algo))]
	# filter out types
	if( !is.null(version)  ){
		type <- match.arg(version, c('R'))
		algo <- Filter( function(x) grepl(str_c("^\\.", version, '#'), x), algo)
	}
	
	# remove names if no arguments
	if( is.null(version) ) algo <- setNames(algo, NULL)
	# return the selected algorithm(s)
	algo
}

#' \code{existsNMFMethod} tells if an NMF algorithm is registered under the
#' 
#' @param exact a logical that indicates if the access key should be matched 
#' exactly (\code{TRUE}) or partially (\code{FALSE}).
#' 
#' @export
#' @rdname registry-algorithm  
existsNMFMethod <- function(name, exact=TRUE){	
	
	!is.null( getNMFMethod(name, error=FALSE, exact=exact) )
	
}

#' \code{removeNMFMethod} removes an NMF algorithm from the registry.
#' 
#' @export
#' @rdname registry-algorithm
removeNMFMethod <- function(name, ...){
	pkgreg_remove('algorithm', key=name, ...)
}


#' Wrapping NMF Algorithms
#' 
#' This function creates a wrapper function for calling the function \code{\link{nmf}} 
#' with a given NMF algorithm.
#' 
#' @param method Name of the NMF algorithm to be wrapped. 
#' It should be the name of a registered algorithm as returned by \code{\link{nmfAlgorithm}}, 
#' or an NMF algorithm object (i.e. an instance of \code{\linkS4class{NMFStrategy}}). 
#' @param ... extra named arguments that define default values for any arguments 
#' of \code{\link{nmf}} or the algorithm itself. 
#' @param .FIXED a logical that indicates if the default arguments defined in \code{...}
#' must be considered as fixed, i.e. that they are forced to have the defined values and cannot
#' be used in a call to the wrapper function, in which case, a warning about discarding them 
#' is thrown if they are used.
#' Non fixed arguments may have their value changed at call time, in which case it is honoured and 
#' passed to the \code{nmf} call.
#' 
#' \code{.FIXED} may also be a character vector that specifies which argument amongst \code{...}
#' should be considered as fixed.
#' @return a function with argument \code{...} and a set of default arguments defined 
#' in \code{...} in the call to \code{nmfWrapper}.
#' 
#' @seealso \code{\link{nmfAlgorithm}}, \code{\link{nmf}}
#' @keywords internal
#' @export
#' 
#' @examples 
#' 
#' # wrap Lee & Seung algorithm into a function
#' lee <- nmfWrapper('lee', seed=12345)
#' args(lee)
#' 
#' # test on random data
#' x <- rmatrix(100,20)
#' res <- nmf(x, 3, 'lee', seed=12345)
#' res2 <- lee(x, 3)
#' nmf.equal(res, res2)
#' res3 <- lee(x, 3, seed=123)
#' nmf.equal(res, res3)
#' 
#' \dontshow{ 
#' stopifnot(nmf.equal(res, res2))
#' stopifnot( !nmf.equal(res, res3)) 
#' }
#' 
#' # argument 'method' has no effect
#' res4 <- lee(x, 3, method='brunet')
#' nmf.equal(res, res4)
#' 
#' \dontshow{ 
#' stopifnot(nmf.equal(res, res4))
#' }
#' 
#' 
nmfWrapper <- function(method, ..., .FIXED=FALSE){
	
	# store original call
	.call <- match.call()
	
	# check that all arguments are named
	if( nargs() > 1L && any(names(.call)[-(1:2)]=='') )
		stop("Invalid call: all arguments must be named.")
	
	# store fixed arguments from default arguments
	.fixedargs <- 'method'
	.defaults <- names(.call)[-1L]
	.defaults <- .defaults[!.defaults %in% 'method']
	if( length(.defaults) ){
#		e <- parent.frame()
#		for(n in .defaults){
#			.call[[n]] <- eval(.call[[n]], envir=e)
#		}
		if( isTRUE(.FIXED) ) .fixedargs <- c(.fixedargs, .defaults)
		else if( is.character(.FIXED) ){
			.FIXED <- .FIXED[.FIXED %in% .defaults]
			.fixedargs <- c(.fixedargs, .FIXED)	
		}
	}
	# store in local environment
	.method <- method
	
	.checkArgs <- function(ca, args){
		# check for fixed arguments passed in the call that need
		# to be discarded
		nm <- names(ca)[-1L]
		if( any(fnm <- !is.na(pmatch(nm, .fixedargs))) ){
			warning("Discarding fixed arguments from wrapped call to ", .call[1L]
					, " [", str_out(nm[fnm], Inf), '].', immediate.=TRUE)
			ca <- ca[!c(FALSE, fnm)]
		}
		#
		
		# start with complete call
		.call <- ca
		# set values of wrapper default arguments if any
		if( length(.defaults) ){
			defaults <- args[.defaults]
			.call <- expand_list(ca, defaults, .exact=FALSE)
		}
		# change into a call to nmf
		.call[[1L]] <- as.name('nmf')
		.call[['method']] <- force(.method)
		as.call(.call)
	}
	
	# define wrapper function
	fwrap <- function(...){
		ca <- match.call()
		args <- formals()
		.call <- .checkArgs(ca, args)
		# eval in parent environment
		e <- parent.frame()
		eval(.call, envir=e)
	}
	
	# add default arguments to signature
	if( length(.defaults) ){
		formals(fwrap) <- expand_list(formals(fwrap), as.list(.call[.defaults]))
	}
	# add arguments from the NMF algorithm
	if( length(meth <- nmfFormals(.method)) ){
		formals(fwrap) <- expand_list(formals(fwrap), meth)
	}
	
	return( fwrap )
	
}

