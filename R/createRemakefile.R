createRemakefile <- function(viz.items=getContentInfos()) {
  
  #### munge the viz.yaml info ####
  
  # we'll be calling source() within this function (see createTimestampRecipe),
  # and viz scripts might depend on vizlab without saying so, so library(vizlab)
  # is needed if vizlab::vizmake() was called without loading vizlab first
  library(vizlab)
  
  # retrieve the content info list from the viz.yaml and fortify with expanded
  # script list, prepped source file recipe, main target
  viz.items <- lapply(viz.items, function(viz.item) {
    viz.item$script_files <- unpackScripts(viz.item)
    viz.item$psource <- createPreppedSourceRecipe(viz.item)
    viz.item$target <- selectTarget(viz.item)
    viz.item$timestamp <- createTimestampRecipe(viz.item)
    viz.item$arguments <- createArgumentRecipe(viz.item)
    viz.item
  })
  # loop a second time to look up the main-recipe target names for dependencies
  # that are referenced by viz id in the viz.yaml
  item.ids <- unlist(lapply(viz.items, `[[`, 'id'))
  viz.items <- lapply(viz.items, function(viz.item) {
    viz.item$item_deps <- unlist(lapply(unname(viz.item$depends), function(dep) {
      if(is.null(dep) || !(dep %in% item.ids)) {
        warning("cannot find dependency '", dep, "' of viz item '", viz.item$id, "' so cannot include the dependency in remake.yaml")
        return(NULL)
      } else { # the usual case
        dep.item <- viz.items[[names(item.ids[item.ids == dep])]]
        return(dep.item$target)
      }
    }))
    viz.item
  })
  
  #### prep the remake.yaml header ####
  
  # define file.extensions and packages section of remake
  info.yml <- getBlocks('info', FALSE)[[1]]
  file.extensions <- info.yml$'file-extensions' # will often be NULL, which is fine
  packages <- names(info.yml$'required-packages')
  
  # define sources and scripts sections of remake
  sources <- createSourcesVector(viz.items)
  
  #### prep the remake.yaml recipes ####
  
  # extract vectors of item IDs and block names corresponding to the elements of viz.items
  block.names <- unlist(lapply(viz.items, `[[`, 'block'))
  
  # create a nested list of item recipe sets within each block, where each
  # recipe set corresponds to 1 viz item and has 1 or more recipes. the unname()
  # calls are important for whisker::whisker.render
  blocks <- lapply(setNames(nm=unique(block.names)), function(block.name) {
    items <- viz.items[names(block.names[block.names == block.name])]
    recipe_sets <- lapply(unname(items), function(item) {
      # define an ~empty recipe list with placeholders to control the ordering
      recipes <- list() 
      if(item$block != 'resource') {
        # make a symbolic target so you can run vizmake('item-id'). unlike make,
        # remake doesn't remake symbolic targets unless their dependencies change
        if(item$id != item$target) {
          recipes$id <- createSymbolicRecipe(item)
        }
        # all non-resource items have a main_recipe
        recipes$main <- createMainRecipe(item)
        # possibly make a timestamp fetching recipe for this item
        if(!is.null(item$timestamp)) {
          recipes$timestamp <- item$timestamp
        }
      } else { # item$block == 'resource'
        recipes$main <- createResourceRecipe(item)
      }
      
      return(list(main_target=recipes$main$target, recipes=unname(recipes)))
    })
    block <- list(block=block.name, items=recipe_sets)
    return(block)
  })
  
  # amend the resource block to only include resources that get used
  resource.deps <- listActiveResources(viz.items)
  blocks[['resources']] <- createResourceBlock(blocks[['resources']], resource.deps)
  
  # append the content information targets (1 shared among all for speed, 1
  # specific to each viz item)
  blocks[['arguments']] <- createArgumentsBlock(viz.items)
  
  # append the prepped source targets to the blocks
  blocks[['scripts']] <- createScriptsBlock(viz.items)
  
  # add logicals for whisker::render to all recipes in blocks
  blocks <- lapply(blocks, function(block) {
    block$items <- lapply(block$items, function(item) {
      item$recipes <- lapply(item$recipes, function(recipe) {
        recipe$has_command <- length(recipe$command) > 0
        recipe$has_depends <- length(recipe$depends) > 0
        recipe$depends_one <- length(recipe$depends) == 1
        recipe$depends_more <- length(recipe$depends) > 1
        recipe
      })
      item
    })
    block
  })
  
  # create the list of grouped targets (one per block, one for the whole viz)
  job_groups <- createJobGroups(blocks)
  
  #### create remake.yaml from template ####

  # combine and prepare the information for templating. add some booleans
  # (has_xxx) to turn entire remake.yml sections on or off
  viz <- list(
    file_extensions = as.list(file.extensions),
    has_file_extensions = length(file.extensions) > 0,
    packages = as.list(packages),
    sources = as.list(sources),
    has_sources = length(sources) > 0,
    blocks = unname(blocks),
    job_groups = job_groups
  )
  
  # apply the template
  template <- readLines(system.file('remake/remake.mustache', package='vizlab'))
  remake.yml <- whisker::whisker.render(template=template, data=viz)
  writeLines(remake.yml, 'remake.yaml') # use .yaml to stay consistent with vizlab
  
  
  # return
  invisible()
}


# Expand a viz item's `scripts` argument into a vector of scripts (never just
# script folders) whose existence has been confirmed
unpackScripts <- function(viz.item) {
  item.scripts <- viz.item$scripts
  if(length(item.scripts) == 0) return(c())
  # pull sources (scripts) from all vizyaml items
  script.dirs <- item.scripts[dir.exists(item.scripts)]
  script.files <- item.scripts[!dir.exists(item.scripts) && file.exists(item.scripts)]
  script.missing <- setdiff(item.scripts, c(script.dirs, script.files))
  if(length(script.missing) > 0) {
    stop('missing scripts: ', paste0(script.missing, collapse=', '))
  }
  all.scripts <- c(dir(script.dirs, recursive=TRUE, full.names=TRUE), script.files)
  return(all.scripts)
}

# Create special targets for combined and subsetted script files so there can be
# 0 or 1 source dependency per viz item, never more, and optionally with only
# those functions called out in the viz.yaml.
createPreppedSourceRecipe <- function(viz.item) {
  needs.psource <- length(viz.item$script_files) > 0
  if(needs.psource) {
    list(
      target_name = sprintf("vizlab/remake/scripts/%s.R", viz.item$id),
      command = sprintf(
        "prepSources(outfile=target_name, %s%s)", 
        paste(paste0("'", viz.item$script_files, "'"), collapse=", "),
        if(length(viz.item$functions) > 0) {
          sprintf(", functions=I('%s')", paste(viz.item$functions, collapse=","))
        } else {
          ""
        })
    )
  } else {
    NULL
  }
}

# Create special targets for subsets of the viz.yaml arguments list, one target
# for each viz item. Allows remake to recognize items as out of date when any of
# their arguments in the viz.yaml change.
createArgumentRecipe <- function(item) {
  if(item$block == 'resource') {
    return(NULL)
  } else {
    list(
      target_name = sprintf('arguments_%s', item$id),
      command = sprintf("getElement(arguments_all, I('%s'))", item$id)
    )
  }
}

# Identify the main target for a viz.item, which may be a file, an R object, or
# a resource (a special kind of file).
selectTarget <- function(viz.item) {
  if(viz.item$block == 'resource') {
    # look up the resource target
    loc <- viz.item$location
    if(!file.exists(loc)) {
      loc <- system.file(loc, package='vizlab')
      if(!file.exists(loc)) {
        warning("couldn't find resource file for viz item '", viz.item$id, "'")
      }
    }
    loc
  } else if(exists('location', viz.item)) {
    # a common case: the item has a file target
    viz.item$location
  } else {
    # if the target is an R object, use the id as the variable (target) name
    viz.item$id
  }
}

# Create a recipe for a timestamp for a viz fetch item, or return NULL if a
# fetchTimestamp recipe is not needed
createTimestampRecipe <- function(viz.item) {
  # determine whether a timestamp check recipe is needed
  if(viz.item$block == 'fetch') {
    ts.fetcher <- tryCatch({
      # search the future environment for the fetchTimestamp method. The source
      # calls won't be needed if the viz functions are already loaded into the R
      # session, but they will be when calling vizmake() from a fresh session
      for(i in seq_along(viz.item$script_files)) {
        # source the scripts that should define the fetchTimestamp method
        source(viz.item$script_files[i], local=TRUE, verbose=FALSE)
      }
      get(paste0('fetchTimestamp.',viz.item$fetcher))
    }, error=function(e) {
      stop(paste0('could not find fetchTimestamp.', viz.item$fetcher,' method'))
    })
    is.always.current <- isTRUE(all.equal(ts.fetcher, alwaysCurrent))
    needs.timestamp <- !is.always.current
  } else {
    needs.timestamp <- FALSE
  }
  
  # create the recipe or return NULL
  if(needs.timestamp) {
    list(
      target_name = locateTimestampFile(viz.item$id),
      command = sprintf("fetchTimestamp(I('%s'))", viz.item$id)
    )
  } else {
    NULL
  }
  
}

# Create a recipe whose target is a viz id and whose dependency is the
# corresponding file
createSymbolicRecipe <- function(item) {
  list(
    target_name = item$id,
    depends = item$target,
    command = sprintf("readData(I('%s'))", item$id) # readData returns the filepath if it can't read the file
  )
}

# Create the main recipe for a viz item
createMainRecipe <- function(item) {
  list(
    target_name = item$target,
    
    # 4 kinds of dependencies: viz.yaml arguments, scripts, timestamps, and
    # other viz items
    depends = as.list(c(
      item$arguments$target_name,
      if(!is.null(item$psource)) item$psource$target_name else item$scripts,
      item$timestamp$target_name,
      item$item_deps)),
    
    # command is always verb(viz) except for resources, which have no command
    # (use c() as placeholder)
    command = sprintf("%s(I('%s'))", if(item$block == 'resource') 'c' else item$block, item$id)
  )
}

# Create a resource item recipe, which is a hybrid of the symbolic and main
# recipes for other item types
createResourceRecipe <- function(item) {
  list(
    # use a symbolic/variable target because (1) we're not creating the file and
    # (2) the full file paths are awkwardly long to include in the remake::make
    # output
    target_name = item$id,
    
    # depends: combine both the target and the item dependencies because they're
    # all just files that we can't make
    depends = c(
      item$item_deps),
    
    # command: readData is spotty for resources, so just return the filepath
    # consistently
    command = sprintf("c('%s')", item$target)
  )
}

# Create a vector of all sources to formally depend on in the remake.yaml
createSourcesVector <- function(viz.items) {
  script.files <- unlist(lapply(viz.items, `[[`, 'script_files'))
  sort(unique(c(
    dir('scripts/read', full.names=TRUE),
    script.files
  )))
}

# Create a recipe block describing how to prepare each combined/filtered R
# script on which items will depend
createScriptsBlock <- function(viz.items) {
  source.preps <- lapply(viz.items, `[[`, 'psource')
  source.preps <- unname(source.preps[!sapply(source.preps, is.null)])
  sp_list <- lapply(source.preps, function(prep) {
    list(main_target=prep$target_name, recipes=list(prep))
  })
  list(block='scripts', items=sp_list)
}

# Create a recipe block describing how to pull out arguments for each viz item
createArgumentsBlock <- function(viz.items) {
  arg.preps <- lapply(viz.items, `[[`, 'arguments')
  arg.preps <- unname(arg.preps[!sapply(arg.preps, is.null)])
  arg_list <- c(
    # shared target
    list(list(
      main_target = 'arguments_all',
      recipes=list(list(
        target_name = 'arguments_all',
        command = "collectItemArguments('viz.yaml')"
      ))
    )),
    lapply(arg.preps, function(prep) {
      list(main_target=prep$target_name, recipes=list(prep))
    })
  )
  list(block='arguments', items=arg_list)
}

# Determine which resource targets actually get used
listActiveResources <- function(viz.items) {
  all.deps <- unique(unlist(lapply(viz.items, `[[`, 'item_deps')))
  all.resources <- viz.items[sapply(viz.items, function(item) item$block == 'resource')]
  resource.targets <- sapply(unname(all.resources), `[[`, 'target')
  intersect(all.deps, resource.targets)
}

# Create an amended resource block that only includes those resources that
# actually get used
createResourceBlock <- function(resource.block, resource.deps) {
  resources.used <- sapply(resource.block$recipes, function(item) item$target_name %in% resource.deps)
  resource.block$recipes <- resource.block$recipes[resources.used]
  return(resource.block)
}

# Create group targets (one for each block, one for the entire viz). Can't just
# make the entire-viz group depend on the block groups because
# https://github.com/richfitz/remake/issues/129
createJobGroups <- function(blocks) {
  # create the list of grouped targets
  block.targets <- lapply(blocks, function(block) {
    as.list(unlist( # get rid of NULLs from items without main_target
      lapply(block$items, function(item) item$main_target)
    ))
  })
  target.groups <- c(block.targets, list('Viz'=as.list(unname(unlist(block.targets)))))
  
  # create the recipes. use uppercase so not to collide with function names
  uppercase <- function(name) {
    gsub("(^[[:alpha:]])", "\\U\\1", name, perl=TRUE)
  }
  job_groups <- lapply(names(target.groups), function(group.name) {
    list(
      target_name = uppercase(group.name),
      depends = target.groups[[group.name]]
    )
  })
  return(job_groups)
}

# Create directories that will be needed to build the vizzy
createDirectories <- function(viz.items=getContentInfos()) {
  # filter to items from those blocks where we could reasonably be creating
  # files in new directories
  viz.items <- viz.items[sapply(viz.items, function(item) {
    item$block %in% c("parameter", "fetch", "process", "visualize")
  })]
  
  # collect a vector of directories from the above viz.items and known
  # vizlab/remake needs
  dirs <- unique(c(
    dirname(unlist(lapply(viz.items, `[[`, 'location'))),
    'vizlab/remake/scripts', 'vizlab/remake/timestamps'
  ))
  
  # create the directories as needed
  for(d in dirs) {
    if(!dir.exists(d)) dir.create(d, recursive=TRUE)
  }
}
