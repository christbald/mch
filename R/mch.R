#' multicore cluster hybrid
#'
#' Note: You need to install multicore in order to use mch:
#' install.packages('multicore',,'http://www.rforge.net/')
#' @export
mch <- function(cl, cores, func, mchParams){

  if(length(cl) != length(cores))
    stop("cores vector has to be euqal to cluster size")

  if(typeof(mchParams) != "list")
    stop("mchParams has to be a list")

  parallel::clusterExport(cl = cl, varlist = c("func", "mchParams"), envir = environment())

  norm_cores <- cores/sum(cores)
  params_count <- length(mchParams)

  job_counts <- round(params_count * norm_cores, 0)

  ib <- 1
  cluster_params <- list()
  if(length(job_counts) > 1){
    for(i in 1:(length(job_counts)-1)){
      cluster_params <- c(cluster_params, list(c(cores[i], ib, job_counts[i])))
      ib <- job_counts[i]+1
    }
  }
  cluster_params <- c(cluster_params, list(c(cores[length(cores)], ib, params_count)))

  print(cluster_params)

    return(unlist(parallel::parLapplyLB(cl = cl, X = cluster_params, fun = function(e){
      multicore::mclapply(e[2]:e[3], function(i) func(mchParams[[i]]), mc.cores = e[1])
    }), recursive = F))

}
