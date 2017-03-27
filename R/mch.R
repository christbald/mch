#' multicore cluster hybrid
#'
#' ???
#' @export
mch <- function(cl, cores, func, params){

  if(length(cl) != length(cores))
    stop("cores vector has to be euqal to cluster size")

  if(typeof(params) != "list")
    stop("params has to be a list")


  clusterExport(cl = cl, "func")
  clusterExport(cl = cl, "params")

  norm_cores <- cores/sum(cores)
  params_count <- length(params)

  job_counts <- round(params_count * norm_cores, 0)

  ib <- 1
  cluster_params <- list()
  for(i in 1:(length(job_counts)-1)){
    cluster_params <- c(cluster_params, list(c(cores[i], ib, job_counts[i])))
    ib <- job_counts[i]+1
  }
  cluster_params <- c(cluster_params, list(c(cores[length(cores)], ib, params_count)))


  return(unlist(parLapplyLB(cl = cl, X = cluster_params, fun = function(e){
    require("doMC")
    registerDoMC(e[1])

    out <- foreach(i = e[2]:e[3]) %dopar% {
      func(params[[i]])
    }

    return(out)
  }), recursive = F))

}
