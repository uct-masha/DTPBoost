source(here::here('models/R/scripts/getModelParameters.R'))
source(here::here('models/R/models/commonModelCode.R'))
source(here::here('models/R/models/diphtheriaModel.R'))
source(here::here('models/R/models/tetanusModel.R'))
source(here::here('models/R/models/pertussisModel.R'))
source(here::here('models/R/models/dtpModel.R'))

#### Define Scenarios ####
scenario <- NULL
yearValues <- 2^(0:5)  # c(5,10,15,20,25,30,35,40)
nTimes <- 10
est <- function(numYearsCurrent, timeStart) {
  timeNow <- Sys.time()
  if (numYearsCurrent==first(timeStart)) return("NA")
  dy <- nTimes*sum(yearValues[yearValues<numYearsCurrent])
  dt <- as.numeric(timeNow - timeStart, units='secs')
  dY <- nTimes*sum(yearValues)
  dT <- dt*dY/dy
  eta <- lubridate::duration(dT-dt)
  ETA <- lubridate::duration(dT)
  # browser()
  glue("Expect end by {timeStart+eta}")
  # paste0(eta,' remaining of total ', ETA, ' from ', timeStart)
}

timeStart <- Sys.time()
library(progressr)
handlers(list(
  handler_progress(
    format   = ":spin :current/:total (:message) [:bar] :percent in :elapsed ETA: :eta",
    width    = 60,
    complete = "+"
  )
))
with_progress({
  p <- progressor(steps=sum(yearValues))
  benchmarks <- lapply(yearValues,
                       function(numYears) {
                         nStars <- round(10*numYears/sum(yearValues))
                         pbar=c(rep('*',nStars),rep(' ', 10-nStars)) %>% paste0(collapse='')
                         print(glue::glue("[{pbar}] numYears={numYears} ({Sys.time()}): {est(numYears,timeStart)}"))
                         p(message = sprintf("P: Simulating for %g years", numYears))
                         result <- microbenchmark::microbenchmark(
                           run_model(parameters=parameters,
                                   scenario=NULL,
                                   timesteps=startyear+seq(0, numYears, dtout)),
                           times = nTimes
                         )
                         p(amount = numYears)
                         result
                       })
})
timeEnd <- Sys.time()
timeEnd

tbAct <- lapply(seq_along(benchmarks), function(i){
  b=benchmarks[[i]];
  tibble(numYears=yearValues[[i]],runTime=b$time*1e-9)
}) %>% bind_rows()

C<-lm(runTime~numYears, tbAct)$coefficients
tbPred <- tibble(numYears=c(0,unique(yearValues),100))%>%mutate(nYearPred=C[[2]]*numYears+C[[1]])
M=sprintf('%.02f',C[[2]])
B=sprintf('%.02f',C[[1]])
ggplot(tbAct,aes(x=numYears,y=runTime)) +
  geom_point() +
  geom_line(data = tbPred,aes(y=nYearPred)) +
  ggplot2::coord_cartesian(xlim=c(0,100), ylim=c(0,45)) +
  ggthemes::theme_solarized_2(light = T) +
  labs(title="DTP Model Runtime (Projected and Actual)", subtitle = glue::glue("Linear fit is {M}*numYears+{B} seconds")) +
  xlab("Simulation Time (Years)") +
  ylab("Model Run Time (Seconds)")

# mo <- run_model(parameters=parameters, scenario=NULL, timesteps=startyear+seq(0, 50, dtout))
# #### Plots ####
# pltModelOutput(mo, age_group="All", scenarios=c("Third Scenario"), sd=0.04)
