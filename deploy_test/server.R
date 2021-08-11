library(idefix)
library(shiny)




server <- function(input, output) {

  alt.cte = NULL
no.choice = NULL
c.lvls = NULL
prior.mean = NULL
prior.covar = NULL
cand.set = NULL
n.draws = NULL
lower = NULL
upper = NULL
parallel = TRUE
reduce = TRUE

levels <- c(3, 3, 3)
p.mean <- c(0.3, 0.7, 0.3, 0.7, 0.3, 0.7)
low = c(-Inf, -Inf, -Inf, 0, 0, -Inf)
up = rep(Inf, length(p.mean))
p.var <- diag(length(p.mean)) 
code <- c("D", "D", "D")
cand <- Profiles(lvls = levels, coding = code)


n.sets <- 12
alternatives <- c("A alternatíva", "B alternatíva")
attributes <- c("Ár", "Idő", "Zsúfoltság")

code <- c("D", "D", "D")

labels <- vector(mode="list", length(attributes))
labels[[1]] <- c("300 Ft", "600 Ft", "900 Ft")
labels[[2]] <- c("30 perc", "18 perc", "6 perc")
labels[[3]] <- c("Van ülőhely", "Nincs ülőhely, de állóhely van", "Kevés állóhely van")
i.text <- "Üdvözlöm! Kérem töltse ki a kérdőívet"
b.text <- "Kérem jelölje be, hogy melyik alternatívát preferálja"
e.text <- "Köszönjük, hogy kitöltötte a kérdőívet!"
dataDir <- getwd()
# Display the survey 
des = NULL
n.total = n.sets
alts = alternatives

atts = attributes
lvl.names = labels
coding = code
buttons.text = b.text
intro.text = i.text
end.text = e.text
data.dir = NULL
data.dir = './results'

prior.mean = p.mean
prior.covar = p.var
cand.set = cand 
lower = low
upper = up
n.draws = 50


#initialize


algorithm = "MOD"
sdata <- vector(mode = "list")
surveyData <- vector(mode = "list")
y.bin <- vector("numeric")
resp  <- vector("character")
n.atts <- length(atts)
n.alts <- length(alts)
n.levels <- as.vector(unlist(lapply(lvl.names,length)))
choice.sets <- matrix(data = NA, nrow = n.total * n.alts, ncol = n.atts)
buttons <- NULL
sn <- 0

# functions
Rcnames <- function(n.sets, n.alts, alt.cte, no.choice) {
  # rownames
  r.s <- rep(1:n.sets, each = n.alts)
  r.a <- rep(1:n.alts, n.sets)
  r.names <- paste(paste("set", r.s, sep = ""), paste("alt", r.a, sep = ""), sep = ".")
  if(no.choice){
    ncsek <- seq(n.alts, (n.sets * n.alts), n.alts)  
    r.names[ncsek] <- "no.choice"
  }
  # colnames alternative specific constants
  if(sum(alt.cte) > 0.2){
    cte.names <- paste(paste("alt", which(alt.cte == 1), sep = ""), ".cte", sep = "") 
  } else {
    cte.names <- NULL
  }
  # return
  return(list(r.names, cte.names))
}

Charbin <- function (resp, alts, n.alts, no.choice = FALSE) {
  # Error resp not in altsions
  if (!all(resp %in% alts)) {
    stop("1 or more responses do not match the possible response options.")
  }
  # Error altsions
  if (length(alts) != (n.alts + no.choice)) {
    stop("Number of response options is not correct")
  }
  map <- match(resp, alts)
  l <- list()
  for(i in 1:length(map)){
    l[[i]] <- rep(0, n.alts)
    if (no.choice) {
      l[[i]][map[i] - 1] <- 1
    } else {
      l[[i]][map[i]] <- 1
    }
  }
  v <- unlist(l)
  return(v)
}

saveData <- function(data, data.dir, n.atts) {
  # Data manipulation 
  d <- as.data.frame(cbind(data$desing, resp = data$bin.responses))
  unc_resp <- rep(data$responses, each = n.atts) 
  unc_setnr <- rep(1:length(data$responses), each = n.atts)
  unc_d <- cbind(set = unc_setnr, data$survey, resp = unc_resp) 
  # Create unique file names
  numname <- sprintf("%s_num_data.txt", as.integer(Sys.time()))
  charname <- sprintf("%s_char_data.txt", as.integer(Sys.time()))
  # Write files to data.dir
  utils::write.table(
    x = d,
    file = file.path(data.dir, numname), 
    row.names = TRUE, quote = FALSE, sep = "\t", col.names = NA
  )
  utils::write.table(
    x = unc_d,
    file = file.path(data.dir, charname), 
    row.names = TRUE, quote = FALSE, sep = "\t", col.names = NA
  )
}

  
  
  
if (is.null(des)) {
  n.init <- 0
} else {
  n.init <- nrow(des) / n.alts
  if (!isTRUE(all.equal(n.init, as.integer(n.init)))) {
    stop("the number of rows of 'des' are not a multiple of length(alts)")
  }
}
if (is.null(alt.cte) || all(alt.cte == 0)) {
  alt.cte <- rep(0, n.alts)
  n.cte <- 0
  cte.des <- NULL
} else {
  # Error 
  if (length(alt.cte) != n.alts) {
    stop("length(alts) does not match length(alt.cte)")
  }
  if (!all(alt.cte %in% c(0, 1))) {
    stop("'alt.cte' should only contain 0's or 1's.")
  }
  if (!any(alt.cte == 0)) {
    stop("'alt.cte' should at least contain 1 zero")
  }
  n.cte <- sum(alt.cte)
  if (!is.null(des)) {
    cte.des <- Altspec(alt.cte = alt.cte, n.sets = n.init)
    if (!isTRUE(all.equal(cte.des, matrix(des[ , 1:n.cte], ncol = n.cte)))) {
      stop("the first column(s) of 'des' are different from what is expected based on 'alt.cte'")
    }
  }
}
# Error handling
if (!is.null(no.choice)) {
  if (!is.numeric(no.choice)) {
    stop("'no.choice' should be an integer indicating the no choice alternative.")
  }
  if (!no.choice %% 1 == 0) {
    stop("'no.choice' should be an integer")
  }
  if (any(isTRUE(no.choice > (n.alts + 0.2)), isTRUE(no.choice < 0.2))) {
    stop("'no.choice' does not indicate one of the alternatives")
  }
  if (!isTRUE(all.equal(alt.cte[no.choice], 1))) {
    stop("the location of the 'no.choice' option in the 'alt.cte' vector should correspond with 1")
  }
}
if (!is.null(data.dir)) {
  if (!dir.exists(data.dir)) {
    stop("Directory 'data.dir' does not exist")
  }
}
if (n.total > n.init) {
  if (is.null(lower)) {
    lower <- rep(-Inf, length(prior.mean))
  }
  if (is.null(upper)) {
    upper <- rep(Inf, length(prior.mean))
  }
  if (!any(c(isTRUE(all.equal(length(prior.mean), length(lower))), isTRUE(all.equal(length(prior.mean), length(upper)))))) {
    stop("length 'prior.mean' should equal 'upper' and 'lower'")
  }
  if (algorithm == "MOD") {
    if (any(c(is.null(prior.mean), is.null(prior.covar), is.null(cand.set), 
              is.null(n.draws)))) {
      stop("When n.total is larger than the number of sets in argument des, arguments prior.mean, prior.covar, cand.set and n.draws should be specified.")
    }
    if (length(prior.mean) != ncol(cand.set) + sum(alt.cte)) {
      stop("Number of parameters in prior.mean does not match with cand.set + alt.cte")
    }
  } else if (algorithm == "CEA") {
    if (any(c(is.null(prior.mean), is.null(prior.covar), is.null(n.draws)))) {
      stop("When n.total is larger than the number of sets in argument des, arguments prior.mean, prior.covar and n.draws should be specified.")
    }
  }
  
  if (!isTRUE(all.equal(length(prior.mean), ncol(prior.covar)))) {
    stop("length of 'prior.mean' differs from number of columns 'prior.covar'")
  }
} else {
  if (!is.null(prior.mean)) {
    warning("'prior.mean' will be ignored, since there are no adaptive sets.")
  } 
  if (!is.null(prior.covar)) {
    warning("'prior.covar' will be ignored, since there are no adaptive sets.")
  }
  if (algorithm == "MOD" & !is.null(cand.set)) {
    warning("'cand.set' will be ignored, since there are no adaptive sets.")
  }
  if (!is.null(lower) || !is.null(upper)) {
    warning("'lower' and 'upper' bound will be ignored, since there are no adaptive sets.")
  }
  if (!is.null(n.draws)) {
    warning("'n.draws' will be ignored, since there are no adaptive sets.")
  }
}
if (is.null(des)) {
  if (algorithm == "MOD") {
    fulldes <- matrix(data = NA, nrow = (n.alts * n.total), ncol = ncol(cand.set))
  } else {
    fulldes <- matrix()
  }
} else {
  bs <- seq(1, (nrow(des) - n.alts + 1), n.alts)
  es <- c((bs - 1), nrow(des))[-1] 
  rowcol <- Rcnames(n.sets = n.init, n.alts = n.alts, alt.cte = alt.cte, 
                    no.choice = FALSE)
  rownames(des) <- rowcol[[1]]
  if (is.null(colnames(des))) {
    colnames(des) <- c(rowcol[[2]], paste("par", 1:(ncol(des) - n.cte), 
                                          sep = "."))
  }
  fulldes <- des
  # Error handling
  if (length(bs) != n.init) {
    stop("The number of rows in 'des' is not a multiple of length(atts)")
  }
  if ("no.choice.cte" %in% colnames(des)) {
    if (is.null(no.choice)) {
      warning("no.choice.cte column name detected in 'des' while 'no.choice = NULL'")
    }
  }
}
if (!(algorithm %in% c("MOD","CEA"))) {
  stop("algorithm should be 'MOD' or 'CEA'")
}

  # Count set number
  observeEvent(input$OK, {
    sn <<- sn + 1
  })
  # Set selection function
  Select <- function() {
    if (sn <= n.total) {
      # for initial sets 
      if (sn <= n.init) {
        set <- des[bs[sn]:es[sn], ]
      } else {
        ## sample drawing for adaptive sets
        # if First set
        if (sn == 1) {
          # sample draws from prior
          s <- tmvtnorm::rtmvnorm(n = n.draws, mean = prior.mean, 
                                  sigma = prior.covar, lower = lower, 
                                  upper = upper)
          w <- rep(1, nrow(s)) / nrow(s)
          if (sum(alt.cte) > 0.2) {
            s <- list(as.matrix(s[ , 1:sum(alt.cte)], ncol = sum(alt.cte)), 
                      s[ , -c(1:sum(alt.cte))])
          }
          # From second set
        } else {
          # Sample draws from updated posterior
          sam <- ImpsampMNL(n.draws = n.draws, prior.mean = prior.mean, 
                            prior.covar = prior.covar,
                            des = fulldes, n.alts = n.alts, y = y.bin, 
                            alt.cte = alt.cte, lower = lower, upper = upper)
          s <- sam$sample
          w <- sam$weights
        }
        ## Selecting set
        if (algorithm == "MOD") {
          # Select new set based on Modfed
          setobj <- SeqMOD(des = des, cand.set = cand.set, n.alts = n.alts, 
                           par.draws = s, prior.covar = prior.covar, 
                           alt.cte = alt.cte, weights = w, 
                           no.choice = no.choice, parallel = parallel, 
                           reduce = reduce)
        } else if (algorithm == "CEA") {
          setobj <- SeqCEA(des = des, lvls = n.levels, coding = coding,
                           n.alts = n.alts, par.draws = s, 
                           prior.covar = prior.covar, alt.cte = alt.cte,
                           weights = w, no.choice = no.choice, 
                           parallel = parallel, reduce = reduce)
        }
        set <- setobj$set
        db  <- setobj$db
        
        ## Design storage
        if (sn == 1) {
          rowcol <- Rcnames(n.sets = 1, n.alts = n.alts, alt.cte = alt.cte, no.choice = FALSE)
          rownames(set) <- rownames(set, do.NULL = FALSE, prefix = paste(paste("set", sn , sep = ""), "alt", sep = "."))
          colnames(set) <- c(rowcol[[2]], paste("par", 1:(ncol(set) - n.cte), sep = "."))
          fulldes <<- set
        } else {
          rowcol <- Rcnames(n.sets = 1, n.alts = n.alts, alt.cte = alt.cte, no.choice = FALSE)
          rownames(set) <- rownames(set, do.NULL = FALSE, prefix = paste(paste("set", sn , sep = ""), "alt", sep = "."))
          colnames(set) <- c(rowcol[[2]], paste("par", 1:(ncol(set) - n.cte), sep = "."))
          fulldes <<- rbind(fulldes, set)
        }
      }
      # Transform coded set to attribute level character set.
      choice.set <- Decode(des = set, n.alts = n.alts, lvl.names = lvl.names, coding = coding, 
                           alt.cte = alt.cte, c.lvls = c.lvls, no.choice = no.choice)[[1]]
      choice.set <- t(choice.set[ , 1:n.atts])
      # Fill in attribute names and alternatives names
      colnames(choice.set) <- alts
      rownames(choice.set) <- atts
      # Store uncoded choice set
      if (sn == 1) {
        choice.sets <<- choice.set
      } else {
        choice.sets <<- rbind(choice.sets, choice.set)
      }
      #return design 
      if (!is.null(no.choice)) {
        no.choice.set <- choice.set[ ,-no.choice]
        return(no.choice.set)
      } else {
        return(choice.set)
      }
    }
  }
  #When action button is clicked
  observeEvent(input$OK, {
    # survey phase 
    if (sn <= n.total ) {
      # Plot new choice set
      output$choice.set <-  renderTable(Select(), rownames = TRUE)
    }
   
    # Store responses and design
    if (sn > 1 && sn <= (n.total + 1)) {
      resp  <<- c(resp, input$survey)
      y.bin <<- Charbin(resp = resp, alts = alts, n.alts = n.alts)
      sdata[["bin.responses"]] <- y.bin
      sdata[["responses"]] <- resp
      sdata[["desing"]] <- fulldes
      sdata[["survey"]] <- choice.sets
      surveyData <<- sdata 
    } 
    # end phase 
    if (sn > n.total) {
      #Don't show choice set
      output$choice.set <-  renderTable(NULL)
    }
  })
  #Output response options after first action button click
  output$buttons <- renderUI({
    # radiobuttons
    if (input$OK > 0 && input$OK <= n.total) {
      return(list(radioButtons("survey", buttons.text,
                               alts , inline = TRUE, selected = "None")))
    }
  })
  # set nr
  observeEvent(input$ok,{
    if (sn == 1) {
      input$age <- renderText({ input$age })
    } else {input$age <- renderText(NULL)}
  })
  observeEvent(input$OK, {
    if (sn <= n.total) {
      output$set.nr <- renderText(paste(c("Döntési szituáció", sn, "/", n.total)))
    } else {output$set.nr <- renderText(NULL)}
  })
  # Introtext
  output$intro <- renderText(intro.text)
  observeEvent(input$OK, {
    output$intro <- renderText(NULL)
  })
  # End of survey
  observeEvent(input$OK, {
    # Display end text 
    if (input$OK > n.total) {
      # Display end text 
      output$end <- renderText(end.text)
    }
    # Quit application 
    if (input$OK > (n.total + 1)) {
      # Write data to file
      if (!is.null(data.dir)) {
        saveData(data = surveyData, data.dir = data.dir, n.atts = n.atts)
      }
      # Stop application 
      #stopApp()
    }
  })
}



