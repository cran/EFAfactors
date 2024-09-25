get.runs <- function(posi, trial.list){
  number.trial <- length(trial.list)
  trials.length <- length(trial.list[[number.trial]])
  for(i in (number.trial-1):1)
    trials.length <- c(trials.length[1] * length(trial.list[[i]]), trials.length)
  for(i in 1:number.trial)
    trials.length[i] <- trials.length[i] / length(trial.list[[i]])

  runs <- rep(1, number.trial)
  for(i in 1:number.trial){
    if(posi > trials.length[i]){
      runs[i] <- ceiling(posi / trials.length[i])
      posi <- posi - (runs[i] - 1) * trials.length[i]
      if(posi == 0 & i < number.trial)
        for(j in (i+1):number.trial)
          runs[j] <- length(trial.list[[j]])
    }
  }
  return(runs)
}
