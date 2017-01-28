#'Find Cutoff Values.
#'
#'\code{findCutoffs} creates a data frame of cutoff values to be applied across
#'a jurisdiction's subunits (e.g. ZIP codes) for grading restaurants or other
#'inspected entities.
#'
#'In our documentation, we use the language ''ZIP code'' and ''restaurant'',
#'however, our grading algorithm and our code can be applied to grade other
#'inspected entities; and percentile cutoffs can be sought in subunits of a
#'jurisdiction that are not ZIP codes. For example, it may make sense to search
#'for percentile cutoffs in an inspector's allocated inspection area or within a
#'census tract. We chose to work with ZIP codes in our work because of the fact
#'that area assignments for inspectors in King County (WA) tend to be single or
#'multiple ZIP codes, and we desired to assign grades based on how a
#'restaurant's scores compare to other restaurants assessed by the same
#'inspector.  We could have calculated percentile cutoffs in an inspector's
#'allocated area, but we also desired to create a grading system that was
#'readily explainable, and the process for allocating an area to an inspector is
#'non-trivial. Where ''ZIP code'' is referenced, please read ''ZIP code or other
#'subunit of a jurisdiction'' and ''restaurant'' should read ''restaurant or
#'other entity to be graded''.
#'
#'\code{findCutoffs} takes in a matrix of restaurants' scores and a vector
#'corresponding to restaurants' ZIP codes, and outputs a data frame of cutoff
#'scores to be used in grade classification. The returned ZIP code cutoff data
#'frame has one row for each unique ZIP code and has \code{(length(gamma)+1)}
#'columns, corresponding to one column for the ZIP code name, and
#'\code{(length(gamma))} cutoff scores separating the \code{(length(gamma)+1)}
#'grading categories.  Across each ZIP code's row, cutoff scores increase and we
#'assume, as in the King County (WA) case, that greater risk is associated with
#'larger inspection scores. (If the inspection system of interest associates
#'larger inspection scores with reduced risk, it will be necessary to perform a
#'transformation of inspection scores before utilizing \code{findCutoffs}, or
#'other functions in the \code{DineSafeR} package. However a simple function
#'such as \code{f(score) = - score} would perform the necessary transformation
#'on the input score matrix.)
#'
#'The returned ZIP code data frame can be used with the
#'\code{\link{gradeAllBus}} function to assign each restaurant a grade: a
#'restaurant's most recent or mean inspection score is compared to the cutoff
#'values in its ZIP code.  We find the smallest cutoff value that the
#'restaurant's score is less than or equal to, and we find the index of this
#'cutoff when the ZIP code's cutoffs are ordered from smallest to largest.  This
#'index will then index the alphabet in order to return the letter grade to be
#'assigned to the restaurant in question.  If the restaurant's score is greater
#'than all cutoff scores, it is assigned the \code{(length(gamma)+1)}th letter
#'of the alphabet as its letter grade - the worst grade in the grading scheme.
#'
#'The way in which cutoff scores are calculated for each ZIP code depends on the
#'value of the \code{type} variable.  The \code{type} variable can take one of
#'four values (see later) and the default value of \code{type} is set to
#'\code{type = "adj"}.
#'
#'@section Modes: \code{type = "unadj"} creates a ZIP code cutoff data frame
#'  with the same cutoff scores (meaningful values in a jurisdiction's
#'  inspection system that are contained in the vector \code{gamma}) for all ZIP
#'  codes. This ZIP code data frame can then be used to carry out ''unadjusted''
#'  grading, in which a restaurant's most recent routine inspection score is
#'  compared to these cutoffs.
#'@section Modes: \code{type = "perc"} takes in a vector of percentiles,
#'  \code{gamma}, and returns a data frame of the scores in each ZIP code
#'  corresponding to these percentiles (using R Type = 1 definition of
#'  \code{\link[stats]{quantile}}).
#'@section Modes: \code{type = "perc.resolve.ties"} takes in a vector of
#'  percentiles, \code{gamma}, and instead of returning (for B/C cutoffs) the
#'  scores in each ZIP code that result in \emph{at least} (\code{gamma[2]} x
#'  100)\% of restaurants in the ZIP code scoring less than or equal to these
#'  cutoffs, \code{type = "perc.resolve.ties"} takes into account the fact that
#'  ties exist in ZIP codes. Returned scores for A/B cutoffs are those that
#'  result in the \emph{closest} percentage of restaurants in the ZIP code
#'  scoring less than or equal to the A/B cutoff to the desired percentage,
#'  (\code{gamma[1]} x 100)\%. Similarly, B/C cutoffs are the scores in the ZIP
#'  code that result in the \emph{closest} percentage of restaurants in the ZIP
#'  code scoring less than or equal to the B/C cutoff and more than the A/B
#'  cutoff to the desired percentage, (\code{(gamma[2] - gamma[1])} x 100)\%.
#'
#'@section Modes: \code{type = "adj"} takes in a vector of uniform absolute
#'  cutoff scores, \code{gamma}, and, in the first instance, carries out
#'  unadjusted grading by comparing restaurants' most recent routine inspection
#'  scores to these cutoffs (see: \code{type = "unadj"}). Grade proportions in
#'  this scheme are then used as initial percentiles to find percentile cutoffs
#'  in each ZIP code (or percentile cutoffs accommodating for the presence of
#'  score ties in the ZIP code, depending on the value of \code{resolve.ties};
#'  see: \code{type = "perc"} or \code{type = "perc.resolve.ties"}). Restaurants
#'  are then graded with the ZIP code percentile cutoffs, and grading
#'  proportions are compared with grading proportions from the unadjusted
#'  system. Percentiles are iterated over (by the \code{\link{percentileSeek}}
#'  function) until grading proportions with ZIP code percentile cutoffs are
#'  within a certain tolerance (as determined by \code{restaurant.tol}) of the
#'  unadjusted grading proportions.
#'
#'@section Warning: \code{findCutoffs} will produce cutoff scores even for ZIP
#'  codes with only one restaurant - situations in which a percentile adjustment
#'  shouldn't be used. It is the job of the user to ensure that, if using the
#'  \code{findCutoffs} function in mode \code{type = "perc"}, \code{type =
#'  "perc.resolve.ties"} or \code{type = "adj"}, it makes sense to do so.  This
#'  may involve only performing the percentile adjustment on larger ZIP codes
#'  and providing absolute cutoff points for smaller ZIP codes, or may involve
#'  aggregating smaller ZIP codes into a larger geographical unit and then
#'  performing the percentile adjustment on the larger area.
#'@section Warning: As mentioned previously, \code{findCutoffs} was created for
#'  an inspection system that associates greater risk with larger inspection
#'  scores. If the inspection system of interest associates greater risk with
#'  reduced scores, it will be neccessary to perform a transformation of the
#'  scores matrix before utilizing the \code{findCutoffs} function. However a
#'  simple function such as \code{f(score) = - score} would perform the
#'  necessary transformation.
#'
#'
#'
#'@param X Numeric matrix of size \code{n} x \code{p}, where \code{n} is the
#'  number is restaurants to be graded and \code{p} is the number of inspections
#'  to be used in grade assignment.  Entry  \code{X[i,j]} represents the
#'  inspection score for the \code{i}th restaurant in the \code{j}th most recent
#'  inspection.
#'@param z Character vector of length \code{n} representing ZIP codes (or other
#'  subunits within a jurisdiction).  \code{z[i]} is the ZIP code corresponding
#'  to the restaurant with inspection scores in row \code{i} of \code{X}.
#'@param gamma Numeric vector representing absolute grade cutoffs or
#'  percentiles, depending on \code{type} variable value. Entries in gamma
#'  should be increasing, with \code{gamma[1] <= gamma[2]} etc (this is related
#'  to the "Warning" section and larger scores being associated with higher
#'  risk). If \code{type = "perc"} or \code{type = "perc.resolve.ties"}, gamma
#'  values represent percentiles and should take on values between 0 and 1.
#'@param type A character string that is one of \code{"adj"}, \code{"unadj"},
#'  \code{"perc"}, or \code{"perc.resolve.ties"}, and that indicates the grading
#'  algorithm to be implemented.
#'@param restaurant.tol Only relevant in the \code{type = "adj"} case (the
#'  default case). An integer indicating the maximum difference in the number of
#'  restaurants in a grading category between the unadjusted and adjusted
#'  grading algorithms (for the top \code{length(gamma)} grading categories).
#'@param max.iterations An integer only relevant in the \code{type = "adj"} case
#'  (the default case).  The maximum number of iterations that the
#'  \code{\link{percentileSeek}} iterative algorithm should run in order to find
#'  percentiles to be applied to ZIP codes to find cutoffs that result in the
#'  same global grading proportions (within the \code{restaurant.tol} level) as
#'  in the unadjusted grading system.
#'@param resolve.ties Boolean value only relevant in the \code{type = "adj"}
#'  case (the default case). If \code{resolve.ties = TRUE}, the intermediate
#'  algorithm used to find ZIP cutoffs (after \code{percentileSeek} has
#'  identified the global percentiles to be applied across all ZIP codes), is
#'  the \code{type = "perc.resolve.ties"} algorithm. Otherwise the \code{type =
#'  "perc"} algorithm is used.
#'
#'
#' @examples
#'
#' # Adjusted Grading (without ties resolution):
#'  zipcode.cutoffs.df <- findCutoffs(X.kc, zips.kc, gamma = c(0, 30))
#'  mean.scores <- rowMeans(X.kc, na.rm = TRUE)
#'  adj.grades <- gradeAllBus(mean.scores, zips.kc, zipcode.cutoffs.df)
#'
#' # Adjusted Grading (with ties resolution):
#'  cutoffs.Ties.df <- findCutoffs(X.kc, zips.kc, gamma = c(0, 30), resolve.ties = TRUE)
#'  grades.Ties <- gradeAllBus(mean.scores, zips.kc, cutoffs.Ties.df)
#'
#' # Unadjusted Grading:
#'  unadj.cutoffs <- findCutoffs(X.kc, zips.kc, gamma = c(0, 30), type = "unadj")
#'  unadj.grades <- gradeAllBus(scores = X.kc[,c(1)], zips.kc, zip.cutoffs = unadj.cutoffs)
#'
#' # Proportion A/B/C in each ZIP code
#' # Unadjusted
#'  foo1 <- round(t(table(unadj.grades, zips.kc))/apply(table(unadj.grades, zips.kc), 2, sum), 2)
#' # Adjusted (with ties resolution)
#'  foo2 <- round(t(table(adj.grades, zips.kc))/apply(table(adj.grades, zips.kc), 2, sum), 2)
#' # Adjusted (without ties resolution)
#'  foo3 <- round(t(table(grades.Ties, zips.kc))/apply(table(grades.Ties, zips.kc), 2, sum), 2)
#'
#' # Correlation plots of unadjusted vs. adjusted (with resolution of ties) grade proportions
#' # in ZIP codes for different grades
#' # Proportions A
#'  plot(foo1[,1], foo2[,1], xlim=range(cbind(foo1[,1],foo2[,1])),
#'  ylim=range(cbind(foo2[,1],foo1[,1])), pch=16,
#'  cex=sqrt(apply(table(adj.grades, zips.kc), 2, sum)/pi)*0.3,
#'  main = "Proportion A in ZIP Codes",
#'  xlab = "Unadjusted", ylab = "Adjusted")
#' # Proportions B
#'  plot(foo1[,2], foo2[,2],xlim=range(cbind(foo1[,2],foo2[,2])),
#'  ylim=range(cbind(foo2[,2],foo1[,2])),pch=16,
#'  cex=sqrt(apply(table(adj.grades,zips.kc),2,sum)/pi)*0.3,
#'  main = "Proportion B in ZIP Codes", xlab = "Unadjusted", ylab = "Adjusted")
#' # Proportions C
#'  plot(foo1[,3], foo2[,3],xlim=range(cbind(foo1[,3],foo2[,3])),
#'  ylim=range(cbind(foo2[,3],foo1[,3])),pch=16,
#'  cex=sqrt(apply(table(adj.grades,zips.kc),2,sum)/pi)*0.3,
#'  main = "Proportion C in ZIP Codes", xlab = "Unadjusted", ylab = "Adjusted")
#'
#'@export
#'
findCutoffs <- function(X, z, gamma, type = "adj", restaurant.tol = 10, max.iterations = 20, resolve.ties = TRUE) {
    ## Preliminary Checks
    #Check X, z, gamma are all of the correct class types; if not, convert.
    X <- matrix(as.numeric(X),nrow = NROW(X))
    z <- as.character(z)
    gamma <- as.numeric(gamma)
    #Check that length of z and number of rows of X match.  If not, throw an error.
    if (NROW(X) != length(z))
      stop("number of rows in X and length of z do not match!")
    #Check that type takes on one of the desired values
    if (!type %in% c("unadj","perc","perc.resolve.ties","adj"))
      stop("type incorrectly specified!")
    #Check that gamma vector is sorted in increasing order
    if(is.unsorted(gamma)){
      stop("gamma vector should be sorted in increasing order!")
    }
    #Check that entries in gamma vector, if type == "perc" or "perc.resolve.ties", are values between 0 and 1
    if ((type == "perc"|type =="perc.resolve.ties") &
        FALSE %in% c(gamma >= 0, gamma <= 1)){
      stop(
        "gamma values, for mid-adjusted scheme, represent percentiles and so should be values between 0 and 1!"
      )
    }
    #Check if any of the ZIP codes have less than, say 10 restaurants, in which
    #case a percentile adjustment may not be the most appropriate grading system
    if ((type == "perc" | type == "perc.resolve.ties"|
         type == "adj") &
        TRUE %in% (data.frame(table(z))$Freq < 10))
      warning(
        "at least one ZIP code has less than 10 restaurants. A percentile adjustment in this case may not be the most appropriate form of grading.", call. = FALSE
      )

    ## Initialize zip.cutoffs.df data frame - the object to be returned with a
    ## row for each unique ZIP code and the number of columns equal to the
    ## number of grades (one column for ZIPs, the remainder of columns for the
    ## (no.grades-1) ZIP code cutoff points)
    no.grades <- length(gamma) + 1
    no.unique.zips <- length(unique(z))
    zip.cutoffs.df <-
      data.frame(matrix(NA, nrow = no.unique.zips, ncol = no.grades))
    ## Names for columns in zip.cutoffs.df
    cutoff.names <- paste("Gamma", LETTERS[1:(no.grades - 1)], sep = ".")
    cutoff.names <- append(cutoff.names, "ZIP", after = 0)
    names(zip.cutoffs.df) <- cutoff.names

    mean.scores <- rowMeans(X, na.rm = T)

    ## If type=="unadj", the grading algorithm to be applied uses the same grade
    ## cutoffs for all ZIP codes
    if (type == "unadj") {
      ## First column is column of unique ZIP codes
      zip.cutoffs.df[,"ZIP"] <- unique(z)
      for (i in 1:length(gamma)) {
        zip.cutoffs.df[,c(i + 1)] <- rep(gamma[i], no.unique.zips)
      }
      return(zip.cutoffs.df)
    }

    ## If type=="perc", the grading algorithm uses percentile grade cutoffs
    ## for each ZIP code and gamma is a vector of percentiles
    if (type == "perc") {
      mean.scores.z <- data.frame(mean.scores, z)
      zip.cutoffs.df <-
        aggregate(
          data = mean.scores.z, mean.scores ~ z, FUN = function(mean.scores.vec)
            quantile(
              mean.scores.vec, probs = gamma, type = 1, na.rm = T
            )
        )
      zip.cutoffs.df <- do.call(data.frame, zip.cutoffs.df)
      names(zip.cutoffs.df) <- cutoff.names
      return(zip.cutoffs.df)
    }

    ## If type == "perc.resolve.ties"
    if (type == "perc.resolve.ties") {
        mean.scores.z <- data.frame(mean.scores, z)
      # Distribution of scores in each ZIP code
        distribution.z <-  as.data.frame(table(mean.scores.z))
        # Convert ZIP code column in distribution.z to character type
          distribution.z$z <- as.character(distribution.z$z)
        # Remove scores which no restaurants in ZIP code obtain
          distribution.z <- distribution.z[-which(distribution.z$Freq == 0),]
        # Number of restaurants in each ZIP code
          no.rests.z <- as.data.frame(table(mean.scores.z$z))
          names(no.rests.z) <- c("z", "no.rests")
          distribution.z <- merge(distribution.z, no.rests.z, by = "z")
        # Proportion of restaurants with a given score
          distribution.z$prop.score <- distribution.z$Freq/distribution.z$no.rests
      # Update cutoffs one at a time - finding A/B cutoff first and then finding other cutoffs
        for(i in 1:length(gamma)){
          # A/B Cutoff
            if(i == 1){
              # Desired percentage of restaurants with A grades:
              desired.perc <- gamma[i]
              # Initialize zip.cutoffs.df frame
              zip.cutoffs.df <- as.data.frame(unique(distribution.z$z))
              names(zip.cutoffs.df) <- "ZIP"
            } else{ # B/C cutoffs and beyond
              # Desired percentage of restaurants with "B" grade (in case of i == 2 and B/C cutoff calculation) is difference between current gamma value,
              # which is a percentile, and gamma[i-1]
              desired.perc <- gamma[i] - gamma[i - 1]
            }
          # Current cutoff name
            current.cutoff <- cutoff.names[i + 1]
          # Cumulative sum of proportions
            cumulative.sums <- aggregate(data = distribution.z, prop.score ~ z, FUN = cumsum)
            distribution.z$cum.sum <- as.numeric(unlist(cumulative.sums$prop.score))
          # Absolute difference between cumulative sum of proportions and desired percentage
            distribution.z$diff <- abs(distribution.z$cum.sum - desired.perc)
          # Find minimum absolute difference between cumulative sum of proportions
          # and desired percentage for each restaurant
            min.diffs <- aggregate(data = distribution.z, diff ~ z, FUN = min)
            min.diffs$row.number <- row.names(min.diffs)
            # Cutoff.locs contains the row numbers in distribution.z which correspond to the
            # minimum differences for each ZIP code
            cutoff.locs <- as.data.frame(which(outer(distribution.z$z,
                                                     min.diffs$z, "==") & outer(distribution.z$diff,
                                                                                min.diffs$diff, "=="), arr.ind = TRUE))
            cutoff.locs <- merge(cutoff.locs, min.diffs[,-which(colnames(min.diffs) == "diff")],
                                 by.x = "col", by.y = "row.number", all.x = TRUE)
            # Are there multiple scores for a ZIP code minimizing difference?
            # If so, choose larger score for cutoff (so that more restaurants receive higher grade in ZIP code)
            cutoff.locs <- aggregate(data = cutoff.locs, row~z, FUN = max)
            cutoff.locs$ZIP <- cutoff.locs$z
          # Identify cutoff values and append to zip.cutoffs.df
            cutoffs <- distribution.z[cutoff.locs$row, c("z", "mean.scores")]
            zip.cutoffs.df <- merge(zip.cutoffs.df, cutoffs, by.x = "ZIP", by.y = "z", all.x = TRUE)
            names(zip.cutoffs.df)[ncol(zip.cutoffs.df)] <- current.cutoff
          # Identify the starting row numbers for each ZIP code in distribution.z
            start.zips <- data.frame(ZIP = unique(distribution.z$z), start.loc = match(unique(distribution.z$z), distribution.z$z))
          # Prepare distribution.z for next for-loop iteration - delete those scores in each ZIP code
          # which now have grades associated with them
            rows.for.deletion <- merge(start.zips,cutoff.locs[,c("row", "ZIP")], by = "ZIP", all = TRUE)
            to.delete <- unlist(lapply(1:nrow(rows.for.deletion), FUN = function(x) rows.for.deletion$start.loc[x]:rows.for.deletion$row[x]))
            distribution.z <- distribution.z[-to.delete,]
        }
      # zip.cutoffs.df to required format
        zip.cutoffs.df$ZIP <- as.character(zip.cutoffs.df$ZIP)
        zip.cutoffs.df[,2:ncol(zip.cutoffs.df)] <- sapply(zip.cutoffs.df[,2:ncol(zip.cutoffs.df)], FUN = function(x)as.numeric(as.character(x)))
      # Replace any NA values with the value of the last non-NA cutoff in row
        na.locations <- which(is.na(zip.cutoffs.df), arr.ind = TRUE)
        if(length(na.locations) > 0){
          na.locations <- as.data.frame(na.locations)
          last.non.na <- aggregate(data = na.locations, col ~ row, FUN = min)
          last.non.na$col <- last.non.na$col - 1
          names(last.non.na) <- c("row", "last.nonna")
          na.locations <- merge(na.locations, last.non.na, by = "row")
          for(i in 1:nrow(na.locations)){
            zip.cutoffs.df[na.locations[i,"row"], na.locations[i,"col"]] <- zip.cutoffs.df[na.locations[i,"row"], na.locations[i,"last.nonna"]]
          }
        }
      # Convert ZIP column to factor type to be consistent with other cases
        zip.cutoffs.df$ZIP <- as.factor(zip.cutoffs.df$ZIP)
      return(zip.cutoffs.df)
    }

    ## If type=="adj", the grading algorithm to be applied uses absoulte grade
    ## cutoffs and finds the proportions of each grade in the unadjusted grading
    ## scheme.  It then applies the percentileSeek function in order to find
    ## the relevant percentiles to be applied to ZIP codes in order that the
    ## adjusted system proportions match the unadjusted proportions.
    if (type == "adj") {
      ## Check how the restaurant.tol compares to the number of restaurants to be graded.
      unadj.cutoffs <- findCutoffs(X, z, gamma, type = "unadj")
      ## Unadjusted grading compares most recent inspection scores to uniform absolute cutoff values
      unadj.grades <-
        gradeAllBus(scores = X[,c(1)], z, zip.cutoffs = unadj.cutoffs)
      unadj.grades <- factor(unadj.grades, levels = LETTERS[1:no.grades])
      grade.proportions <-
        as.data.frame(table(unadj.grades) / length(unadj.grades[which(!is.na(unadj.grades))]))
      if (nrow(grade.proportions) == 0)
        stop("grade proportions data frame for unadjusted grading has 0 rows")
      names(grade.proportions) <- c("grade", "proportion")
      desired.props <- grade.proportions$proportion
      ## Run percentileSeek function in order to find the percentiles that should be applied across all ZIP codes in order to match uniform absolute proportions
      gamma.updated <-
        percentileSeek(
          mean.scores, z, desired.props, restaurant.tol = restaurant.tol, max.iterations = max.iterations, resolve.ties = resolve.ties
        )
      #print(paste("final gamma vector =", gamma.updated))
      ## Run the findCutoffs function with the new percentiles
      if(resolve.ties == TRUE){
        zip.cutoffs.df <-
          findCutoffs(X, z, gamma.updated, type = "perc.resolve.ties")
      } else{
        zip.cutoffs.df <-
          findCutoffs(X, z, gamma.updated, type = "perc")
      }
      return(zip.cutoffs.df)
    }
  }
















