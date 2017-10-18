createRemakefile <- function() {
  
  # get header information
  info.yml <- getBlocks('info', FALSE)[[1]]
  file.extensions <- info.yml$'file-extensions' # will often be NULL, which is fine
  packages <- names(info.yml$'required-packages')
  
  # read all the viz item info, which we'll use a few times below
  viz.items <- getContentInfos()
  
  # pull sources (scripts) from all vizyaml items
  script.deps <- unique(unlist(lapply(unname(viz.items), `[[`, 'scripts')))
  script.dirs <- script.deps[sapply(script.deps, dir.exists)]
  script.files <- script.deps[sapply(script.deps, function(dep) { !dir.exists(dep) && file.exists(dep) })]
  script.missing <- setdiff(script.deps, c(script.dirs, script.files))
  if(length(script.missing) > 0) {
    stop('missing scripts: ', paste0(script.missing, collapse=', '))
  }
  sources <- sort(unique(c(
    dir(c('scripts/read', script.dirs), full.names=TRUE),
    script.files
  )))
  
  # create special targets for combined and subsetted script files so there can
  # be 0 or 1 source dependency per viz item
  viz.items <- lapply(viz.items, function(viz.item) {
    needs.psource <- length(viz.item$scripts) > 0
    if(needs.psource) {
      psource_name <- sprintf("vizlab/remake/scripts/%s.R", viz.item$id)
      scripts <- unlist(lapply(viz.item$scripts, function(script) if(dir.exists(script)) dir(script, full.names=TRUE) else script))
      viz.item$psource <- list(
        target_name = psource_name,
        command = sprintf(
          "prepSources(outfile=target_name, %s%s)", 
          paste(paste0("'", scripts, "'"), collapse=", "),
          if(length(viz.item$functions) > 0) sprintf(", functions=I('%s')", paste(viz.item$functions, collapse=",")) else ""),
        # depends = as.list(scripts), # not necessary because captured in command
        has_depends = FALSE
      )           
    }
    viz.item
  })
  # extract the special targets
  source.preps <- lapply(viz.items, `[[`, 'psource')
  source.preps <- unname(source.preps[!sapply(source.preps, is.null)])
  
  # create the main list of targets, one per viz item. I thought we'd need to
  # wrap filenames in quotes sometimes, but spaces in filenames seem to be no
  # problem, so leaving everything unwrapped unless/until we hit a snag
  viz.targets <- lapply(viz.items, function(viz.item) {
    # if there is no location, that means this is an R object target
    viz.item$target <- if(viz.item$block == 'resource') {
      destloc <- file.path("target", viz.item$location)
      gsub('js/third-party', 'js', destloc)
    } else if(exists('location', viz.item)) {
      viz.item$location
    } else {
      # if the target is an R object, use the id as the variable (target) name
      viz.item$id
    }
    return(viz.item)
  })
  # extract vectors of item IDs and block names corresponding to the elements of viz.targets
  item.ids <- unlist(lapply(viz.targets, `[[`, 'id'))
  block.names <- unlist(lapply(viz.targets, `[[`, 'block'))
  # create a nested list of items (targets) within each block. the unname()
  # calls are important for whisker::whisker.render
  blocks <- lapply(unique(block.names), function(block.name) {
    items <- viz.targets[names(block.names[block.names == block.name])]
    recipes <- lapply(unname(items), function(item) {
      recipe <- list(
        id = item$id, # just copy for fun, or maybe we'll use this as a comment in the makefile...
        target_name = item$target,
        depends = c(
          as.list(if(exists('psource', item)) item$psource$target_name else item$scripts),
          lapply(unname(item$depends), function(dep) {
            dep.item <- viz.targets[[names(item.ids[item.ids == dep])]]
            dep.item$target
          })),
        command = paste0(
          if(item$block == 'resource') 'publish' else item$block, # except for resource, the block name is the function name
          "(I('", item$id, "'))")
      )
      if(length(recipe$depends) == 0) recipe$depends <- NULL
      recipe$has_depends <- exists('depends', recipe)
      return(recipe)
    })
    block <- list(block=block.name, recipes=recipes)
    return(block)
  })
  
  # amend the resource block to have special commands and only include those
  # resources that actually get used. first determine which resource targets
  # actually get used
  all.deps <- unlist(lapply(blocks, function(block) {
    if(block$block == 'resource') {
      NULL
    } else {
      lapply(block$recipes, function(recipe) {
        recipe$depends
      })
    }
  }), recursive=TRUE)
  all.resources <- viz.targets[sapply(viz.targets, function(item) item$block == 'resource')]
  resource.targets <- sapply(unname(all.resources), `[[`, 'target')
  resource.deps <- intersect(all.deps, resource.targets)
  # extract, amend, and replace the resource block
  resource.block.index <- which(sapply(blocks, function(block) block$block == 'resource'))
  resource.recipes <- blocks[[resource.block.index]]$recipes
  # filter to just those that are used
  resource.recipes <- resource.recipes[sapply(resource.recipes, function(item) item$target_name %in% resource.deps)]
  # replace the list in blocks with this shortened list
  blocks[[resource.block.index]]$recipes <- resource.recipes
  
  # append the prepped source targets to the blocks
  blocks[[length(blocks) + 1]] <- list(block='scripts', recipes=source.preps)
  
  # create the list of grouped targets (one for each block, one for the entire
  # viz). can't just make the entire-viz group depend on the block groups
  # because https://github.com/richfitz/remake/issues/129
  uppercase <- function(name) {
    gsub("(^[[:alpha:]])", "\\U\\1", name, perl=TRUE)
  }
  final.targets <- viz.targets[sapply(viz.targets, function(item) {
    item$block != 'resource' || item$target %in% resource.deps
  })]
  final.target_names <- setNames(
    unlist(lapply(final.targets, `[[`, 'target')),
    unlist(lapply(final.targets, `[[`, 'block')))
  job_groups <- c(
    list(list(
      target_name='Viz',
      depends=as.list(unname(final.target_names)))),
    lapply(unique(block.names), function(block.name) {
      targets <- unname(final.target_names[names(final.target_names) == block.name])
      list(
        target_name=uppercase(block.name),
        depends=as.list(targets))
    })
  )
  
  # combine and prepare the information for templating. add some booleans
  # (has_xxx) to turn entire remake.yml sections on or off
  viz <- list(
    file_extensions = as.list(file.extensions),
    has_file_extensions = length(file.extensions) > 0,
    packages = as.list(packages),
    sources = as.list(sources),
    has_sources = length(sources) > 0,
    blocks = blocks,
    job_groups = job_groups
  )
  
  # create the remake.yml from the template
  template <- readLines(system.file('remake/remake.mustache', package='vizlab'))
  remake.yml <- whisker::whisker.render(template=template, data=viz)
  writeLines(remake.yml, 'remake.yaml') # use .yaml to stay consistent with vizlab
  
  invisible()
}
