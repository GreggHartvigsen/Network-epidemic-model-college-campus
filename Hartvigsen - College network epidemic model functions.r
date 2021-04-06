library(igraph)
read.networks = function(semester) {
   if (1 %in% semester) {
      # Get 2019 data
      g.M.19 = read.graph("g.crn.housing.201909.M.gml", format = "gml")
      g.T.19 = read.graph("g.crn.housing.201909.T.gml", format = "gml")
      g.W.19 = read.graph("g.crn.housing.201909.W.gml", format = "gml")
      g.R.19 = read.graph("g.crn.housing.201909.R.gml", format = "gml")
      g.F.19 = read.graph("g.crn.housing.201909.F.gml", format = "gml")
      g.S.19 = read.graph("g.housing.201909.S19.gml", format = "gml")
      #-------------------------
      # Make networks undirected (weekend networks are already undirected)
      g.M.19 = as.undirected(g.M.19, mode = "each") # need to use "each" to keep edge attributes
      g.T.19 = as.undirected(g.T.19, mode = "each") # need to use "each" to keep edge attributes
      g.W.19 = as.undirected(g.W.19, mode = "each") # need to use "each" to keep edge attributes
      g.R.19 = as.undirected(g.R.19, mode = "each") # need to use "each" to keep edge attributes
      g.F.19 = as.undirected(g.F.19, mode = "each") # need to use "each" to keep edge attributes
      g.S.19 = as.undirected(g.S.19, mode = "each") # need to use "each" to keep edge attributes
      #-------------------------
      V(g.M.19)$pidm = as.numeric(V(g.M.19)$pidm)
      V(g.T.19)$pidm = as.numeric(V(g.T.19)$pidm)
      V(g.W.19)$pidm = as.numeric(V(g.W.19)$pidm)
      V(g.R.19)$pidm = as.numeric(V(g.R.19)$pidm)
      V(g.F.19)$pidm = as.numeric(V(g.F.19)$pidm)
      V(g.S.19)$pidm = as.numeric(V(g.S.19)$pidm)
      # Make networks global variables
      assign("g.M.19",g.M.19,envir = .GlobalEnv)
      assign("g.T.19",g.T.19,envir = .GlobalEnv)
      assign("g.W.19",g.W.19,envir = .GlobalEnv)
      assign("g.R.19",g.R.19,envir = .GlobalEnv)
      assign("g.F.19",g.F.19,envir = .GlobalEnv)
      assign("g.S.19",g.S.19,envir = .GlobalEnv)
   }
   if (2 %in% semester) {
      #---------------
      # Get 2020 data
      g.M.20 = read.graph("g.crn.housing.202001.M.gml", format = "gml")
      g.T.20 = read.graph("g.crn.housing.202001.T.gml", format = "gml")
      g.W.20 = read.graph("g.crn.housing.202001.W.gml", format = "gml")
      g.R.20 = read.graph("g.crn.housing.202001.R.gml", format = "gml")
      g.F.20 = read.graph("g.crn.housing.202001.F.gml", format = "gml")
      g.S.20 = read.graph("g.housing.202001.S20.gml", format = "gml")
      #-------------------------
      # Make networks undirected (weekend networks are already undirected)
      g.M.20 = as.undirected(g.M.20, mode = "each") # need to use "each" to keep edge attributes
      g.T.20 = as.undirected(g.T.20, mode = "each") # need to use "each" to keep edge attributes
      g.W.20 = as.undirected(g.W.20, mode = "each") # need to use "each" to keep edge attributes
      g.R.20 = as.undirected(g.R.20, mode = "each") # need to use "each" to keep edge attributes
      g.F.20 = as.undirected(g.F.20, mode = "each") # need to use "each" to keep edge attributes
      g.S.20 = as.undirected(g.S.20, mode = "each") # need to use "each" to keep edge attributes
      #-------------------------
      V(g.M.20)$pidm = as.numeric(V(g.M.20)$pidm)
      V(g.T.20)$pidm = as.numeric(V(g.T.20)$pidm)
      V(g.W.20)$pidm = as.numeric(V(g.W.20)$pidm)
      V(g.R.20)$pidm = as.numeric(V(g.R.20)$pidm)
      V(g.F.20)$pidm = as.numeric(V(g.F.20)$pidm)
      V(g.S.20)$pidm = as.numeric(V(g.S.20)$pidm)
      #-------------------------
      # Make networks global variables
      assign("g.M.20",g.M.20,envir = .GlobalEnv)
      assign("g.T.20",g.T.20,envir = .GlobalEnv)
      assign("g.W.20",g.W.20,envir = .GlobalEnv)
      assign("g.R.20",g.R.20,envir = .GlobalEnv)
      assign("g.F.20",g.F.20,envir = .GlobalEnv)
      assign("g.S.20",g.S.20,envir = .GlobalEnv)
   }
}

scramble.edges = function(g) {
   n.verts = length(V(g))
   n.edges = length(E(g))
   g = delete_edges(g, E(g))
   el = c(sample(1:n.verts, 2*n.edges, replace = T))
   g = add_edges(g, el)
   return(g)
}

make.random.networks = function() {
   # instead, rondomize the network
   g.M.19.rand = scramble.edges(g.M.19)
   g.T.19.rand = scramble.edges(g.T.19)
   g.W.19.rand = scramble.edges(g.W.19)
   g.R.19.rand = scramble.edges(g.R.19)
   g.F.19.rand = scramble.edges(g.F.19)
   g.S.19.rand = scramble.edges(g.S.19)
   g.M.20.rand = scramble.edges(g.M.20)
   g.T.20.rand = scramble.edges(g.T.20)
   g.W.20.rand = scramble.edges(g.W.20)
   g.R.20.rand = scramble.edges(g.R.20)
   g.F.20.rand = scramble.edges(g.F.20)
   g.S.20.rand = scramble.edges(g.S.20)
   #---------------------------------------------------   
   assign("g.M.19.rand",g.M.19.rand,envir = .GlobalEnv)
   assign("g.T.19.rand",g.T.19.rand,envir = .GlobalEnv)
   assign("g.W.19.rand",g.W.19.rand,envir = .GlobalEnv)
   assign("g.R.19.rand",g.R.19.rand,envir = .GlobalEnv)
   assign("g.F.19.rand",g.F.19.rand,envir = .GlobalEnv)
   assign("g.S.19.rand",g.S.19.rand,envir = .GlobalEnv)
   assign("g.M.20.rand",g.M.20.rand,envir = .GlobalEnv)
   assign("g.T.20.rand",g.T.20.rand,envir = .GlobalEnv)
   assign("g.W.20.rand",g.W.20.rand,envir = .GlobalEnv)
   assign("g.R.20.rand",g.R.20.rand,envir = .GlobalEnv)
   assign("g.F.20.rand",g.F.20.rand,envir = .GlobalEnv)
   assign("g.S.20.rand",g.S.20.rand,envir = .GlobalEnv)
}

#---------------------------------------------
# Function used to visualize the network
plot.network = function(g, Time.Step, ind.data) {
   par(mfrow = c(1,1))
   my.layout = layout_in_circle(g)
   my.cols = numeric(N)
   my.cols[which(ind.data$state=="S")] = "black"
   my.cols[which(ind.data$state=="E")] = "red"
   my.cols[which(ind.data$state=="I")] = "green"
   my.cols[which(ind.data$state=="R")] = "blue"
   plot(g, vertex.size = 10, vertex.color = my.cols,
        edge.color = "black", 
        main = paste("Time.Step = ", Time.Step),
        layout = my.layout, vertex.label = "")
   legend("topleft",legend = c("S","E","I","R"), 
          fill = c("black","red","green","blue"))
}

#--------------------------
# Get appropriate network for this Time.Step
get.network.day = function(day, semester, net) { # comes in as a letter for DOW
   if (semester == 1 & net == "College") {
      if (day == "M") assign("g", g.M.19, envir = .GlobalEnv)
      if (day == "T") assign("g", g.T.19, envir = .GlobalEnv)
      if (day == "W") assign("g", g.W.19, envir = .GlobalEnv)
      if (day == "R") assign("g", g.R.19, envir = .GlobalEnv)
      if (day == "F") assign("g", g.F.19, envir = .GlobalEnv)
      if (day == "SAT") assign("g", g.S.19, envir = .GlobalEnv)
      if (day == "SUN") assign("g", g.S.19, envir = .GlobalEnv)
   }
   if (semester == 2 & net == "College")  {
      if (day == "M") assign("g", g.M.20, envir = .GlobalEnv)
      if (day == "T") assign("g", g.T.20, envir = .GlobalEnv)
      if (day == "W") assign("g", g.W.20, envir = .GlobalEnv)
      if (day == "R") assign("g", g.R.20, envir = .GlobalEnv)
      if (day == "F") assign("g", g.F.20, envir = .GlobalEnv)
      if (day == "SAT") assign("g", g.S.20, envir = .GlobalEnv)
      if (day == "SUN") assign("g", g.S.20, envir = .GlobalEnv)
   }
   if (semester == 1 & net == "Random") {
      if (day == "M") assign("g", g.M.19.rand, envir = .GlobalEnv)
      if (day == "T") assign("g", g.T.19.rand, envir = .GlobalEnv)
      if (day == "W") assign("g", g.W.19.rand, envir = .GlobalEnv)
      if (day == "R") assign("g", g.R.19.rand, envir = .GlobalEnv)
      if (day == "F") assign("g", g.F.19.rand, envir = .GlobalEnv)
      if (day == "SAT") assign("g", g.S.19.rand, envir = .GlobalEnv)
      if (day == "SUN") assign("g", g.S.19.rand, envir = .GlobalEnv)
   }
   if (semester == 2 & net == "Random")  {
      if (day == "M") assign("g", g.M.20.rand, envir = .GlobalEnv)
      if (day == "T") assign("g", g.T.20.rand, envir = .GlobalEnv)
      if (day == "W") assign("g", g.W.20.rand, envir = .GlobalEnv)
      if (day == "R") assign("g", g.R.20.rand, envir = .GlobalEnv)
      if (day == "F") assign("g", g.F.20.rand, envir = .GlobalEnv)
      if (day == "SAT") assign("g", g.S.20.rand, envir = .GlobalEnv)
      if (day == "SUN") assign("g", g.S.20.rand, envir = .GlobalEnv)
   }
   return(g)
}

#----------------------------
# Get correct Trans.prob for each of 7 days
get.Trans.prob = function(day, semester) {
   if (semester == 1) {
      if (day == "M") assign("tp", Trans.prob.19[1], envir = .GlobalEnv)
      if (day == "T") assign("tp", Trans.prob.19[2], envir = .GlobalEnv)
      if (day == "W") assign("tp", Trans.prob.19[3], envir = .GlobalEnv)
      if (day == "R") assign("tp", Trans.prob.19[4], envir = .GlobalEnv)
      if (day == "F") assign("tp", Trans.prob.19[5], envir = .GlobalEnv)
      if (day == "SAT") assign("tp", Trans.prob.19[6], envir = .GlobalEnv)
      if (day == "SUN") assign("tp", Trans.prob.19[6], envir = .GlobalEnv)
   }
   if (semester == 2) {
      if (day == "M") assign("tp", Trans.prob.20[1], envir = .GlobalEnv)
      if (day == "T") assign("tp", Trans.prob.20[2], envir = .GlobalEnv)
      if (day == "W") assign("tp", Trans.prob.20[3], envir = .GlobalEnv)
      if (day == "R") assign("tp", Trans.prob.20[4], envir = .GlobalEnv)
      if (day == "F") assign("tp", Trans.prob.20[5], envir = .GlobalEnv)
      if (day == "SAT") assign("tp", Trans.prob.20[6], envir = .GlobalEnv)
      if (day == "SUN") assign("tp", Trans.prob.20[6], envir = .GlobalEnv)
   }
   return(tp)
}

#--------------------------------
# Update the ind dataframe (E->I, I->R)
update.ind.data = function(ind.data, Time.Step) {
   #--------------------------
   # Find all the infectious inds and try to recover
   infectious = which(ind.data$state == "I")
   if (length(infectious) > 0) {
      ready = infectious[which(Time.Step - ind.data$dayI[infectious] == num.days.I)] # those ready to recover
      if (length(ready) > 0) { # some are ready to go to R
         ind.data$state[ready] = "R"
      }
   }
   # Find all the exposed individuals and test to move to I
   exposed = which(ind.data$state == "E")
   if (length(exposed) > 0) {
      ready = exposed[which(Time.Step - ind.data$dayE[exposed] == num.days.E)] # number of days individuals are infectious (default = 3)
      if (length(ready) > 0) { # some are ready to go to I
         ind.data$state[ready] = "I"
         ind.data$dayI[ready] = Time.Step
      }
   }
   #--------------------------
   return(ind.data)
}

#------------------------
# Get individual states, return array
count.ind.states = function(ind.data) {
   NS = length(which(ind.data$state == "S"))
   NE = length(which(ind.data$state == "E"))
   NI = length(which(ind.data$state == "I"))
   NR = length(which(ind.data$state == "R"))
   NISOL = length(which(ind.data$state == "ISOL"))
   NQ = length(which(ind.data$state == "Q"))
   return(c(NS,NE,NI,NR,NISOL,NQ))
}

#------------------------
# Initial infection of individuals
init.infect = function(ind.data, num.E.T1, num.I.T1, Time.Step) {
   if (num.E.T1 > 0) {
      to.infect = sample(which(ind.data$state == "S"),num.E.T1) # get num.infected.T1 "S" inds to be "E"
      ind.data$state[to.infect] = "E" # make exposed on day 1
      ind.data$dayE[to.infect] = Time.Step # exposed on day 1
   }
   if (num.I.T1 > 0) {
      to.infect = sample(which(ind.data$state == "S"),num.I.T1) # get num.infected.T1 "S" inds to be "I"
      ind.data$state[to.infect] = "I" # make exposed on day 1
      ind.data$dayI[to.infect] = Time.Step # infected on day 1
   }
   return (ind.data)
}

###########################################################
# Testing functions
test = function(ind.data, Time.Step, test.method) {
   N = length(ind.data[,1])
   if (test.method == "random") {
      to.test = which(Time.Step %% ind.data$day.to.get.tested == 0 & 
                  (ind.data$state != "ISOL" & ind.data$state != "Q"))
      if (length(to.test) > 0) {
         ind.data$Testing[to.test] = TRUE # now getting tested
         ind.data$test.day[to.test] = Time.Step # day they're tested
         ind.data$test.state[to.test] = ind.data$state[to.test] # record state they were in
         ind.data$test.num.of[to.test] = ind.data$test.num.of[to.test] + 1
      }
   }
   return (ind.data)
}

#--------------------------------------------------------
assess.test.results = function(g, ind.data, test.results.delay, do.contact.tracing, Time.Step, test.day, test.accuracy) {
   if (test.day == 1) {
      inds.to.isolate = which(ind.data$Testing == T & ind.data$test.state == "I")
   } else {
      inds.to.isolate = which((ind.data$Testing == T &
                                  Time.Step - ind.data$test.day) >= test.results.delay & 
                                 ind.data$test.state == "I")
   }
   # enact the inaccuracy of the test now
   inds.to.isolate = sample(inds.to.isolate, round(length(inds.to.isolate)*test.accuracy,0))
   #------------------------------------
   if (length(inds.to.isolate) > 0) {
      ind.data$state[inds.to.isolate] = "ISOL"
      ind.data$ISOL.day[inds.to.isolate] = Time.Step
      if (do.contact.tracing) { # quarantine neighbors if doing contact tracing
         # inds may not be in today's network!
         inds.isolated = inds.to.isolate # these were isolated
         inds.isolated.pidm = ind.data$pidm[inds.isolated]
         # get verts in network that are infected to isolate
         vert.pidm.isolated = inds.isolated.pidm[which(inds.isolated.pidm %in% as.numeric(V(g)$pidm))]
         for (i in 1:length(vert.pidm.isolated)) {
            vert = which(as.numeric(V(g)$pidm) == vert.pidm.isolated[i])
            vert.nbrs = unique(as.numeric(unlist(ego(g, nodes = vert)))[-1]) # -1 removes target that is now ISOL
            nbrs.pidm = as.numeric(V(g)$pidm[vert.nbrs])
            if (length(nbrs.pidm) > 0) {
               ind.nbrs = which(ind.data$pidm %in% nbrs.pidm) # in ind.data
               ind.nbrs.not.ISOL.Q.I = ind.nbrs[which(ind.data$state[ind.nbrs] == "S" |
                                                         ind.data$state[ind.nbrs] == "E")]
               if (length(ind.nbrs.not.ISOL.Q.I) > 0) {
                  ind.data$Q.state[ind.nbrs.not.ISOL.Q.I] = ind.data$state[ind.nbrs.not.ISOL.Q.I]
                  ind.data$state[ind.nbrs.not.ISOL.Q.I] = "Q" # put neighbors into quarantine
                  ind.data$Q.day[ind.nbrs.not.ISOL.Q.I] = Time.Step # record Time.Step into quarantine
               }
            }
         }
      }
   }
   return (ind.data)
} # end assess.test.results()

#--------------------------------------------------------
rm.from.ISOL.Q = function(ind.data,Time.Step, num.days.ISOL.Q) {
   in.ISOL = which(ind.data$state == "ISOL")
   if (length(in.ISOL) > 0) {
      to.move = which(Time.Step - ind.data$ISOL.day[in.ISOL] >= num.days.ISOL.Q)
      if (length(to.move) > 0) {
         ind.data$state[in.ISOL[to.move]] = "R" # they recover 
      }
   }
   in.Q = which(ind.data$state == "Q")
   if (length(in.Q) > 0) {
      ind.to.remove.from.Q = in.Q[which(Time.Step - ind.data$Q.day[in.Q] >= num.days.ISOL.Q)]
      if (length(ind.to.remove.from.Q) > 0) {
         # Done with quarantine. Back to S or R
         move.to.R = which(ind.data$Q.state[ind.to.remove.from.Q] == "E" | 
                              ind.data$Q.state[ind.to.remove.from.Q] == "I")
         move.to.S = which(ind.data$Q.state[ind.to.remove.from.Q] == "S") # They were "S"
         if (length(move.to.R) > 0) {
            ind.data$state[move.to.R] = "R"
         }
         if (length(move.to.S) > 0) {
            ind.data$state[move.to.S] = "S"
         }
      }
   }
   return (ind.data)
}

remove.from.testing = function(ind.data, return.to.test, Time.Step) {
   # return individuals to Testing = F so can be tested again
   to.return = which(ind.data$Testing & (Time.Step - ind.data$test.day >= return.to.test))
   if (length(to.return) > 0) {
      ind.data$Testing[to.return] = F
   }
   return (ind.data)
}
# End testing functions

####################################################################
get.R.by.major.data = function(ind.data) {
   N.by.major = sort(table(ind.data$major),decreasing = T)
   N = as.numeric(as.character(N.by.major))
   major.names = names(N.by.major)
   num.R.inds.by.major = numeric(length(major.names))
   APL.by.major = numeric(length(major.names))
   for (i in 1:length(major.names)) {
      num.R.inds.by.major[i] = length(which(ind.data$state == "R" & ind.data$major == major.names[i]))/N[i]
      maj.vids = which(V(g.M.19)$major == major.names[i])
      temp.net = induced.subgraph(g.M.19, vids = maj.vids)
      APL.by.major[i] = average.path.length(temp.net)
   }
   dat = data.frame(major.names,num.R.inds.by.major,APL.by.major,N)
   return(dat)
}
