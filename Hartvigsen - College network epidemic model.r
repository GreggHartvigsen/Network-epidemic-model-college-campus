###########################################################################
# SEIRIQ network-based model using real college network for 
#   classes and local housing
# Gregg Hartvigsen
# SUNY Geneseo 
#
##############################################
if (!require(igraph)) {
  install.packages("igraph")
  library(igraph)
}
library(plotrix)
start.time = Sys.time()

#------------------------------
read.files = T # don't read files if already read in. Need to, though, for random nets
print.daily.progress = F
get.prop.R.by.major = F
multiple.I.time.steps = F # collect E+I over time for nreps times
#----------------------------------
# Global parameter values that you can change with little risk
R.not = 2.4 # R.not (Note this range: 0 <= R.not <= k, default = 2)
nreps = 1
network.type = c("College", "Random") # choose "College" or "Random" to test
do.contact.tracing = T # only T or F now. F creates an unbalanced design
num.days.ISOL.Q = 14
test.accuracy = 0.9
test.results.delay = 1 # in days
test.method = "random" # only supports random now
test.day.cycle = 0 #c(0, 1,2,3, 7,14, 28,7*15) # 0 is no testing. Otherwise, test every X days (1 would be every day, 7 is once a week - each person gets a random day)
prop.masked = 0. #c(0, 0.5,  0.75, 0.861, 1) #c(seq(0,1, by = 0.2)) # proportion of students that wear masks no matter 
mask.efficacy = 0 #c(0.5, 0.75, 0.95, 1) # as a proportion
num.days.E = 2 # number of days individuals are infectious (default = 3)
num.days.I = 10 # number of days individuals are infectious (default = 3)
num.E.T1 = 10
num.I.T1 = 10
num.wks = 30 # number of weeks to run model
num.days.in.sem = 15*7
DOW = rep(c("M","T","W","R","F","SAT","SUN"),num.wks) # Day of week
semester = c(1,2) # 1 = Fall 2019, 2 = Spring 2020
# Note weekends use mean(k), not median(k)
source("Hartvigsen - College network epidemic model functions.r")
G.data = read.csv("college19-20.csv") # Only read once
pidm = unique(G.data$pidm)
N = length(pidm) # number of students
if (multiple.I.time.steps) {
  multiple.I.time.step.data = matrix(0, nrow = num.wks*7, ncol = nreps)
}
if (read.files) {
  cat("Reading network files....\n")
  for (i in semester) {
    read.networks(i)
  }
}
k.19 = c(median(degree(g.M.19)),median(degree(g.T.19)),median(degree(g.W.19)),
         median(degree(g.R.19)),median(degree(g.F.19)))
k.20 = c(median(degree(g.M.20)),median(degree(g.T.20)),median(degree(g.W.20)),
         median(degree(g.R.20)),median(degree(g.F.20)))
Trans.prob.19 = 1 - (1 - R.not/k.19)^(1/num.days.I)
Trans.prob.20 = 1 - (1 - R.not/k.20)^(1/num.days.I)
Trans.prob.19[6] = mean(Trans.prob.19[1:5])
Trans.prob.20[6] = mean(Trans.prob.20[1:5])
# Make dataframe of all students and their states
major = character(N)
# local.address = character(N)
# M = as.character(G.data$majdesc)
for (i in 1:N){
  # this gets wrong data
  row.in.G.data = which(G.data$pidm == pidm[i])
  major[i] = as.character(G.data$majdesc[row.in.G.data])[1]
} # get majors
if ("Random" %in% network.type) {
  cat("making random networks....\n")
  make.random.networks()
}
#--------------------------------------------------------------------
# num.to.test.daily = round(prop.to.test.daily * N, 0)
n.sims = length(prop.masked) * nreps * length(mask.efficacy) * 
  length(test.day.cycle) * length(network.type)
sim.count = 0

for (net in network.type) {
  for (test.day in test.day.cycle) {
    for (me in 1:length(mask.efficacy)) {
      for (masking in 1:length(prop.masked)) {
        for (the.rep in 1:nreps){
          sim.count = sim.count + 1
          semester = 1 # fall 2019 = 1, spring 2020 = 2
          cat("Starting sim ",sim.count, "of", n.sims,"\n")
          #----------------------------------------------------
          # Build ind.data
          state = rep("S", N)
          dayE = dayI = rep(0,N)
          # day = rep("M",N)
          Testing = rep(F, N) # F (not testing) or T (testing)
          test.state = rep("NULL", N)
          # test.day = rep(1e5,N)
          test.num.of = rep(0,N)
          ISOL.day = rep(0,N)
          Q.state = rep("NULL",N)
          Q.day = rep(0,N)
          if (test.day > 0) {
            day.to.get.tested = sample(rep(1:test.day, length = N))
          } else {
            day.to.get.tested = rep(0,N)
          }
          n.masked = round(N * prop.masked[masking],0)
          masking.status = sample(c(rep(0, N - n.masked),rep(1,n.masked)))
          ind.data = data.frame(pidm,state,dayE,dayI,major,masking.status,
                                Testing,test.state,test.day,test.num.of,
                                ISOL.day, Q.state,Q.day,day.to.get.tested)
          ind.data$state = as.character(ind.data$state) # Needed for R < 4.0 apparently
          ind.data$major = as.character(ind.data$major)
          # ind.data$day = as.character(ind.data$day)
          
          ###########################################################
          # 1. Set up first day of simulation on a Monday
          
          Time.Step = 1 # set Time.Step variable
          # g = get.network.day(DOW[Time.Step]) # Get Monday network
          
          #--------------------------
          # Infect individuals with virus on Day 1
          ind.data = init.infect(ind.data, num.E.T1, num.I.T1, Time.Step)
          num.infections = num.E.T1 + num.I.T1
          #--------------------------
          # Collect data on Day 1
          states = count.ind.states(ind.data)
          if (print.daily.progress) {
            cat("T =",Time.Step, ". DOW = ", DOW[Time.Step], "nS = ", states[1], 
                "nE = ", states[2], "nI = ",states[3], "nISOL =", states[5],
                "nQ = ", states[6], "ntesting = ",length(which(ind.data$Testing == T)),"\n")
          }
          
          # Set up data collection matrix
          Time.Step.data = matrix(c(1,states),ncol = 7)
          ##########################################################
          # 2. Begin time step (day) loop
          
          Time.Step = 2 # already have data for Time.Step = 1, start on Time.Step = 2
          while(1) { # keep doing this until E + I = 0
            # Set up Time.step, get network, update students
            if (Time.Step == (num.days.in.sem +1)) {
              semester = 2 # switch to spring 2020 semester
              cat("Switching to Spring 2020\n")
            }
            if (Time.Step == (2*num.days.in.sem + 1)) {
              semester = 1
              cat("Switching back to Fall 2019\n")
            }
            
            Trans.prob = get.Trans.prob(DOW[Time.Step], semester)
            g = get.network.day(DOW[Time.Step], semester, net)
            # update the students: E -> I and I -> R to begin day
            ind.data = update.ind.data(ind.data, Time.Step)
            
            #--------------------------
            # 3 Break if no more E or I
            exposed.inds = which(ind.data$state == "E")
            # get inf inds that, are not masked or masked not not effective
            infectious.inds = which(ind.data$state == "I")
            if (length(exposed.inds) + length(infectious.inds) == 0) {
              Time.Step.data = rbind(Time.Step.data,c(Time.Step, count.ind.states(ind.data)))
              break
            }
            
            #-----------------------------------
            # DO TESTING BEFORE MASKING.
            # 4. Implement testing. If test.day = 0 then no testing
            if (test.day > 0) {
              ind.data = test(ind.data, Time.Step, test.method)
              ind.data = assess.test.results(g, ind.data, test.results.delay, 
                                             do.contact.tracing, Time.Step, test.day, test.accuracy)
              ind.data = rm.from.ISOL.Q(ind.data, Time.Step, num.days.ISOL.Q)
            }
            
            #--------------------------
            # CHECK FOR MASKING
            infectious.inds.not.masked = infectious.inds[which(ind.data$masking.status[infectious.inds] == 0)]
            infectious.inds.masked = infectious.inds[which(ind.data$masking.status[infectious.inds] == 1)]
            if (length(infectious.inds.masked) > 0) {
              infectious.inds.masked.ineffectively = infectious.inds.masked[which(runif(length(infectious.inds.masked)) < (1-mask.efficacy[me]))]
              infectious.inds = c(infectious.inds.not.masked, infectious.inds.masked.ineffectively)
            } else {
              infectious.inds = infectious.inds.not.masked
            }
            #--------------------------------------
            # SPREAD TO NEIGHBORS (inds means dataframe, verts means in network)
            if (length(infectious.inds) > 0) {
              # Get pidm for infectious inds
              infectious.pidm = ind.data$pidm[infectious.inds]
              # get which of the pidms are in today's network
              inf.pidm.index = which(infectious.pidm %in% as.numeric(V(g)$pidm))
              # get pidm vals for inds in network
              infectious.pidm = infectious.pidm[inf.pidm.index]
              # if there are infectious inds then spread disease to neighbors  
              if (length(infectious.pidm) > 0) { # if there are inf. inds in network
                # Get vertex ids of infected inds that are in this network
                infectious.verts = which(V(g)$pidm %in% infectious.pidm)
                if (length(infectious.verts) > 0) { 
                  # get neighbors of infectious inds. This gets self but they won't necessarily be S
                  neighbors.of.inf.verts = as.numeric(unlist(ego(g,nodes = infectious.verts)))
                  remove.inf.verts = which(neighbors.of.inf.verts %in% infectious.verts)
                  neighbors.of.inf.verts = neighbors.of.inf.verts[-remove.inf.verts] # remove cause already I
                  if (length(neighbors.of.inf.verts) > 0) {
                    # get pidm for each neighbor of inf ind
                    a.verts = sort(neighbors.of.inf.verts)
                    neighbors.of.inf.pidm = as.numeric(V(g)$pidm[a.verts])
                    neighbors.of.inf.pidm.unique = as.numeric(names(table(neighbors.of.inf.pidm)))
                    n.times.in.neighbor.array = as.numeric(table(neighbors.of.inf.pidm))
                    neighbors.of.inf.inds = which(ind.data$pidm %in% neighbors.of.inf.pidm)
                    neighbors.that.are.S.ind = neighbors.of.inf.inds[which(ind.data$state[neighbors.of.inf.inds] == "S")]
                    n.times.in.neighbor.array = n.times.in.neighbor.array[which(ind.data$state[neighbors.of.inf.inds] == "S")]
                    if (length(neighbors.that.are.S.ind) > 0) {
                      # If there are some then try to infect these neighbors
                      to.infect.inds = neighbors.that.are.S.ind[which(runif(length(neighbors.that.are.S.ind)) <= Trans.prob*n.times.in.neighbor.array)]
                      if (length(to.infect.inds) > 0) { # there are some to infect
                        ind.data$state[to.infect.inds] = "E"
                        ind.data$dayE[to.infect.inds] = Time.Step
                        num.infections = num.infections + length(unique(to.infect.inds)) 
                      }
                    }
                  }
                }
              }
            }
            #--------------------------
            # 6. Count number infectious and add to data matrix with Time.Step
            states = count.ind.states(ind.data)
            if (print.daily.progress) {
              cat("T =",Time.Step, ". DOW = ", DOW[Time.Step], "nS = ", states[1], 
                  "nE = ", states[2], "nI = ",states[3], "nISOL =", states[5],
                  "nQ = ", states[6], "ntesting = ",length(which(ind.data$Testing == T)),"\n")
            }
            Time.Step.data = rbind(Time.Step.data,c(Time.Step, states))
            if (Time.Step == length(DOW)) {
              cat("Semester ended\n")
              break
            }
            Time.Step = Time.Step + 1
          }
          #------------------------------------------------
          max.num.sick = max(rowSums(Time.Step.data[,c(3,4,6)]))
          max.sick.day = which(rowSums(Time.Step.data[,c(3,4,6)]) == max(rowSums(Time.Step.data[,c(3,4,6)])))[1]
          num.days = length(Time.Step.data[,1])
          ave.num.tests = round(mean(ind.data$test.num.of),3)
          # summary data in  model:
          if (sim.count == 1) {
            summary.data = data.frame(num.infections,max.num.sick, max.sick.day,num.days,
                                      do.contact.tracing,test.day, ave.num.tests, test.accuracy,
                                      prop.masked[masking], mask.efficacy[me],
                                      net)
            if (get.prop.R.by.major)  {
              prop.R.by.major = get.R.by.major.data(ind.data)
            }
          } else {
            summary.data = rbind(summary.data, c(num.infections,max.num.sick, max.sick.day,num.days,
                                                 do.contact.tracing,test.day,ave.num.tests, test.accuracy,
                                                 prop.masked[masking], mask.efficacy[me],
                                                 net))
            # if (get.prop.R.by.major)  {
            #   prop.R.by.major = cbind(prop.R.by.major, get.R.by.major.data(ind.data)[,2])
            # }
          }
          #---------------------------------------
          # What majors got sick? Sick means state = E, I, R, or ISOL
          got.sick = which(ind.data$state == "E" | ind.data$state == "I" | ind.data$state == "R" | ind.data$state == "ISOL")
          if (multiple.I.time.steps) {
            multiple.I.time.step.data[1:length(Time.Step.data[,1]),the.rep] = rowSums(Time.Step.data[,3:4])
          }
        } # rep
      } # proportion masking
    } # mask efficacy
  } # test.day.cycle
} # network type

Time.Step.data = as.data.frame(Time.Step.data)
names(Time.Step.data) = c("Time.Step", "S", "E", "I", "R", "ISOL", "Q")
names(summary.data) = c("num.infections","max.num.sick","max.sick.day","num.days",
                        "do.contact.tracing","test.cycle.days", "ave.num.of.tests",
                        "test.accuracy", "prop.masked", "mask.efficacy","Net.type")

##########################################################
cat("The experiment ran for:\n")
Sys.time() - start.time
