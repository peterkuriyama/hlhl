#If there is a check data file there, remove it before running this again
file.remove(paste0(results_dir, '//',  'twospp1_newcc_check_5.Rdata'))

#For testing the new comp coefficient curves
nreps <- 5 

#Adjust number of reps
to_loop$nreps <- nreps

#--------------------------------------------------------------------------------------------

#Create indices for each computer, plan is to do this on five computers
tot <- 1:nrow(to_loop)

#Specify one run for each core
tots <- split(tot, ceiling(seq_along(tot) / (nrow(to_loop) / ((nrow(to_loop) / nncores)))))

#Specify Index for each computer
#-----------------
run_this_ind <- 1

#-----------------

if(length(run_this_ind) == 1) to_run <- tots[[run_this_ind]]
if(length(run_this_ind) > 1){
  to_run <- unlist(tots[run_this_ind])
  names(to_run) <- NULL
} 

start_time <- Sys.time()

clusters <- parallel::makeCluster(nncores)
doParallel::registerDoParallel(clusters)

twospp <- foreach(ii = to_run,
  .packages = c('plyr', 'dplyr', 'reshape2', 'hlsimulator'), .export = c("shape_list1")) %dopar% {
    fixed_parallel(index = ii, ctl1 = ctl1, to_loop = to_loop, 
      change_these = c('nfish1', 'nfish2', 'comp_coeff'))  
}

#Close clusters
stopImplicitCluster()

#Record run time
run_time <- Sys.time() - start_time

#Format output
twospp <- ldply(twospp)  

if(length(run_this_ind) > 1) run_this_ind <- paste(run_this_ind, collapse = "")

assign(paste0("twospp", run_this_ind), twospp)
#From previous runs
# filename <- paste0("twospp", run_this_ind )

#Run now, run2
filename <- paste0("twospp", run_this_ind ) #for new competition coefficient

#Save output in U drive
save(list = filename, file = paste0(results_dir, "//" , paste0(filename, "_newcc_check_", nreps, '.Rdata')))


#Run just one fixed_parallel
# fixed_parallel(index = 14, ctl1 = ctl1, to_loop = to_loop, 
#   change_these = c('nfish1', 'nfish2', 'comp_coeff'))



