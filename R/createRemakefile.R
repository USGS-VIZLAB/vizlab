createRemakefile <- function() {
  
  #### munge the viz.yaml info ####
  
  # retrieve the content info list from the viz.yaml and fortify with expanded
  # script list, prepped source file recipe, main target
  viz.items <- lapply(getContentInfos(), function(viz.item) {
    viz.item$script_files <- unpackScripts(viz.item)
    viz.item$psource <- createPreppedSourceRecipe(viz.item)
    viz.item$target <- selectTarget(viz.item)
    viz.item$fetch_timestamp <- needsTimestamp(viz.item)
    viz.item
  })
  # loop a second time to look up the main-recipe target names for dependencies
  # that are referenced by viz id in the viz.yaml
  item.ids <- unlist(lapply(viz.items, `[[`, 'id'))
  viz.items <- lapply(viz.items, function(viz.item) {
    viz.item$item_deps <- unlist(lapply(unname(viz.item$depends), function(dep) {
      dep.item <- viz.items[[names(item.ids[item.ids == dep])]]
      dep.item$target
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
      # make a symbolic target so you can run vizmake('item-id'). unlike make,
      # remake doesn't remake symbolic targets unless their dependencies change
      if(item$id != item$target) {
        recipes$id <- createSymbolicRecipe(item)
      }
      # possibly make a timestamp fetching recipe for this item
      if(item$fetch_timestamp) {
        recipes$main <- NA # placeholder so timestamp goes last, after main
        recipes$timestamp <- createTimestampRecipe(item)
      }
      # always make a main_recipe
      recipes$main <- createMainRecipe(item, recipes$timestamp, viz.items)
      
      # add logicals for whisker::render
      recipes <- lapply(recipes, function(recipe) {
        recipe$has_command <- length(recipe$command) > 0
        recipe$has_depends <- length(recipe$depends) > 0
        recipe$depends_one <- length(recipe$depends) == 1
        recipe$depends_more <- length(recipe$depends) > 1
        recipe
      })
      return(list(main_target=recipes$main$target, recipes=unname(recipes)))
    })
    block <- list(block=block.name, items=recipe_sets)
    return(block)
  })
  
  # amend the resource block to only include resources that get used
  resource.deps <- listActiveResources(viz.items)
  blocks[['resources']] <- createResourceBlock(blocks[['resources']], resource.deps)
  
  # append the prepped source targets to the blocks
  blocks[['scripts']] <- createScriptsBlock(viz.items)
  
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
  
  # create the remake.yml from the template
  template <- readLines(system.file('remake/remake.mustache', package='vizlab'))
  remake.yml <- whisker::whisker.render(template=template, data=viz)
  writeLines(remake.yml, 'remake.yaml') # use .yaml to stay consistent with vizlab
  
  #### create directories ####
  
  # create any directories that have been mentioned but don't yet exist
  dirs <- unique(c(
    dirname(unlist(lapply(unname(viz.items), `[[`, 'location'))),
    'vizlab/remake/scripts', 'vizlab/remake/timestamps'))
  for(d in dirs) {
    if(!dir.exists(d)) dir.create(d, recursive=TRUE)
  }
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
        }),
      has_command = TRUE,
      has_depends = FALSE,
      depends_one = FALSE,
      depends_more = FALSE
    )
  } else {
    NULL
  }
}

# Identify the main target for a viz.item, which may be a file, an R object, or
# a resource (a special kind of file).
selectTarget <- function(viz.item) {
  if(viz.item$block == 'resource') {
    # look up the resource target
    destloc <- file.path("target", viz.item$location)
    gsub('js/third-party', 'js', destloc)
  } else if(exists('location', viz.item)) {
    # a common case: the item has a file target
    viz.item$location
  } else {
    # if the target is an R object, use the id as the variable (target) name
    viz.item$id
  }
}

# Determine whether a fetchTimestamp recipe is needed; return T/F
needsTimestamp <- function(viz.item) {
  if(viz.item$block == 'fetch') {
    ts.fetcher <- tryCatch({
      get(paste0('fetchTimestamp.',viz.item$fetcher))
    }, error=function(e) NULL)
    is.always.current <- isTRUE(all.equal(ts.fetcher, alwaysCurrent))
    return(!is.always.current)
  } else {
    return(FALSE)
  }
}

# Create a recipe whose target is a viz id and whose dependency is the
# corresponding file
createSymbolicRecipe <- function(item) {
  list(
    target_name = item$id,
    depends = item$target
  )
}

# Create a recipe for a timestamp for a viz fetch item
createTimestampRecipe <- function(item) {
  list(
    target_name = locateTimestampFile(item$id),
    command = sprintf("fetchTimestamp(I('%s'))", item$id)
  )
}

# Create the main recipe for a viz item
createMainRecipe <- function(item, timestamp_recipe, viz.items) {
  list(
    target_name = item$target,
    
    # 3 kinds of dependencies: scripts, timestamps, and other viz items
    depends = c(
      as.list(if(length(item$psource) > 0) item$psource$target_name else item$scripts),
      if(item$fetch_timestamp) list(timestamp_recipe$target_name) else list(),
      as.list(item$item_deps)),
    
    # command is always verb(viz)
    command = paste0(
      if(item$block == 'resource') 'publish' else item$block, # except for resource, block == function name
      "(I('", item$id, "'))")
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
  sp_list <- lapply(source.preps, function(sprep) list(main_target=sprep$target_name, recipes=list(sprep)))
  list(block='scripts', items=sp_list)
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
    lapply(block$items, function(item) item$main_target)
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
