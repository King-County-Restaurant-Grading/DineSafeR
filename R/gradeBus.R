#' Grade Businesses.
#'
#' \code{gradeAllBus} takes in a vector of business inspection scores, business
#' ZIP codes and a data frame of ZIP code cutoff scores and returns a vector of
#' business grades.
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
#' \code{gradeAllBus} takes a vector of inspection scores (one score per
#' restaurant - this may be a mean or the result of a single inspection), a
#' vector of ZIP codes and a dataframe of ZIP code cutoffs.  It compares each
#' restaurant's inspection score to cutoff scores in the restaurant's ZIP code.
#' It finds the smallest cutoff score in the restaurant's ZIP code that the
#' restaurant's inspection score is less than or equal to - let's say this is
#' the (\code{letter.index})th cutoff score - and returns the
#' (\code{letter.index})th letter of the alphabet as the grade for the
#' restaurant. The returned vector of grades maintains the order of businesses
#' in vector inputs \code{scores} and in \code{z}).
#'
#' @param scores Numeric vector of length \code{n}, where \code{n} is the number is
#'   restaurants to be graded.  Each entry is the inspection score for one
#'   business.
#' @param z Character vector of length \code{n}, where each entry is the ZIP
#'   code (or other geographic area) of a business.  The order of businesses in
#'   \code{z} is the same as the order of businesses in \code{scores}.
#' @param zip.cutoffs A dataframe with the first column containing all of the
#'   ZIP codes in z and later columns containing cutoff scores for each ZIP code
#'   for grade classification.  Cutoff scores for each ZIP code should be
#'   ordered from lowest score in column 2 (representing the cutoff for the best
#'   grade) to the largest cutoff score in the final column (representing the
#'   cutoff inspection score for the second worst grade).
#' @return A character vector of length n, with each entry corresponding to the
#'   grade that the restaurant received.

#' @examples
#'
#' # Adjusted Grading (without tie resolution):
#'  zipcode.cutoffs.df <- findCutoffs(X.kc, zips.kc, gamma = c(0, 30))
#'  mean.scores <- rowMeans(X.kc, na.rm = TRUE)
#'  adj.grades <- gradeAllBus(mean.scores, zips.kc, zipcode.cutoffs.df)
#'
#' # Unadjusted Grading:
#'  unadj.cutoffs <- findCutoffs(X.kc, zips.kc, gamma = c(0, 30), type = "unadj")
#'  unadj.grades <- gradeAllBus(scores = X.kc[,c(1)], zips.kc, zip.cutoffs = unadj.cutoffs)
#'
#' # Proportion A/B/C in each ZIP code
#' # Unadjusted
#'  foo1 <- round(t(table(unadj.grades, zips.kc))/apply(table(unadj.grades, zips.kc), 2, sum), 2)
#' # Adjusted
#'  foo2 <- round(t(table(adj.grades, zips.kc))/apply(table(adj.grades, zips.kc), 2, sum), 2)
#'
#' # Correlation plots of unadjusted vs. adjusted grade proportions
#' # in ZIP codes for different grades
#' # Proportions A
#'  plot(foo1[,1], foo2[,1], xlim=range(cbind(foo1[,1],foo2[,1])),
#'  ylim=range(cbind(foo2[,1],foo1[,1])), pch=16,
#'  cex=sqrt(apply(table(adj.grades, zips.kc), 2, sum)/pi)*0.3,
#'  main = "Proportion A in ZIP Codes", xlab = "Unadjusted", ylab = "Adjusted")
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
#' @export

  gradeAllBus<- function(scores, z, zip.cutoffs){
      #============
      # Preliminary Checks
      X<- as.numeric(scores)
      z<- as.character(z)
      zip.cutoffs<- as.data.frame(zip.cutoffs)
      #Check that length of z and number of rows of X match.  If not, throw an error.
      if(length(X) != length(z)) stop("length of X and length of z do not match!")
      #=================
      # Grading all businesses
      # Use gradeBus function and apply to all businesses
      all.bus.grades<- lapply(1:length(X), FUN = function(i) gradeBus(scores[i], z[i], zip.cutoffs))
      # Return a character vector of grades
      return(as.character(all.bus.grades))
  }



#' Grade a Business.
#'
#' \code{gradeBus} takes in the inspection score for one restaurant, the ZIP
#' code for the restaurant, a data frame of ZIP
#' code cutoff information and returns the grade for the business in question.
#'
#' \code{gradeBus} takes one inspection score for a restaurant (this may be a
#' mean or the result of a single inspection), the restaurant's ZIP code and a
#' dataframe of ZIP code cutoffs. It compares each restaurant's inspection score
#' to cutoff scores in the restaurant's ZIP code.  It finds the smallest cutoff
#' score in the restaurant's ZIP code that the restaurant's inspection score is
#' less than or equal to - let's say this is the (\code{letter.index})th cutoff
#' score - and returns the (\code{letter.index})th letter of the alphabet as the
#' grade for the restaurant.  \code{gradeBus} is the function called by
#' \code{\link{gradeAllBus}} in order to grade all businesses.
#'
#' @param x.bar.i Numeric inspection score (or mean score) for restaurant in
#'   question.
#' @param z.i Character representing ZIP code (or other geographic area) of
#'   business in question.
#' @param zip.cutoffs A dataframe with the first column containing ZIP codes and
#'   later columns containing grade cutoff scores for each ZIP code. Cutoff
#'   scores for each ZIP code should be ordered from lowest score in column 2
#'   (representing the cutoff for the best grade) to largest cutoff score in the
#'   final column (representing the cutoff inspection score for the second worst
#'   grade).
#' @return A character representing the grade assigned to the restaurant in
#'   question ('A', 'B', 'C' etc).

#' @export
  gradeBus<- function(x.bar.i, z.i, zip.cutoffs){
      #=====
      # Preliminary Checks
      x.bar.i<- as.numeric(x.bar.i)
      z.i<- as.character(z.i)
      zip.cutoffs<- as.data.frame(zip.cutoffs)
      #Check that across each row, if there is more than 1 cutoff, that
      # scores are increasing - i.e. that the ZIP
      #code cutoff frame is correctly specified to be interpretted as we expect
      #(col 2 represents cutoff for A grade, col 3 represents cutoff for B grade
      #and so on)
      if(ncol(zip.cutoffs) >= 3){
        for(j in 2:(ncol(zip.cutoffs)-1)){
          if(FALSE %in% (as.numeric(zip.cutoffs[,c(j)]) <= as.numeric(zip.cutoffs[,c(j+1)]))) stop("incorrect specification of ZIP code cutoff frame - cutoffs are not ordered from lowest to highest for each ZIP!")
        }
      }
      #======
      # Grading
      zip.of.interest<- zip.cutoffs[which(zip.cutoffs[,c(1)] == z.i),]
      no.grades<- ncol(zip.cutoffs)
      ## if no ZIP exists for restaurant, we cannot score restaurant
      if(is.na(z.i)==TRUE){
        bus.grade=NA
      } else if(is.na(x.bar.i)==TRUE){ ## if no score exists for restaurant, we cannot score restaurant
        bus.grade= NA
      } else{
      ## compare x.bar.i to cutoff values in zip.of.interest:
      ## what is the smallest cutoff value that x.bar.i is less than or equal to?
      ## find the index of the smallest cutoff value in zip.of.interest frame and map to letter grade for restaurant of interest
      letter.index<-match(TRUE, x.bar.i<= as.numeric(zip.of.interest[,c(2:ncol(zip.cutoffs))]), nomatch = ncol(zip.cutoffs))
      bus.grade<- LETTERS[letter.index]
      }
      return(bus.grade)
  }



