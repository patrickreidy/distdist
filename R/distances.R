
#' @include distribution.R
NULL


# p-norm and ilk                                                                ##########

#' p-Metric
#'
#' Compute the \emph{p}-metric between two probability distributions.
#'
#' Let \code{p1} and \code{p2} denote the vectors of probability mass assigned
#' by two distributions defined on the same state space. Then, the \emph{p}-metric
#' between \code{p1} and \code{p2} is given by \code{(sum(abs(p1 - p2)^p))^(1/p)}.
#'
#' @param p1,p2 \code{\link[=Distribution-class]{Distributions}}.
#' @param p An atomic numeric greater than or equal to 1 that determines the
#'   power of the norm.
#'
#' @return The \emph{p}-metric between distributions \code{p1} and \code{p2}.
#'
#' @name pMetric
#' @export
methods::setGeneric(
  name = "pMetric",
  def = function(p1, p2, ...) {
    standardGeneric("pMetric")
  }
)

#' @usage \S4method{pMetric}{Distribution,Distribution}(p1, p2, p)
#' @name pMetric,Distribution,Distribution-method
#' @rdname pMetric
methods::setMethod(
  f = "pMetric",
  signature = c(p1 = "Distribution", p2 = "Distribution"),
  definition = function(p1, p2, p) {
    p1 <- Arrange(p1)
    p2 <- Arrange(p2)
    if (!Identical(p1, p2, names = TRUE, states = TRUE, probs = FALSE, arrange = FALSE)) {
      depth <- rlang::call_depth() - 1
      stop(stringr::str_c(substitute(p1, env = rlang::call_frame(depth)[["env"]]),
                          "and",
                          substitute(p2, env = rlang::call_frame(depth)[["env"]]),
                          "must be defined on the same state space.",
                          sep = " "))
    }
    if (p < 1) {
      stop(stringr::str_interp("Parameter p (= ${p}) must be greater than or equal to 1."))
    }
    p_metric <- (sum(abs(p1$P - p2$P)^p))^(1/p)
    return(p_metric)
  }
)





#' Manhattan Distance
#'
#' Compute the Manhattan distance between two probability distributions. This
#' distance is also known as the taxicab distance, the city block distance,
#' the rectilinear distance, and the snake distance.
#'
#' Let \code{p1} and \code{p2} denote the vectors of probability mass assigned
#' by two distributions defined on the same state space. Then, the Manhattan
#' distance between \code{p1} and \code{p2} is given by \code{sum(abs(p1 - p2))}.
#'
#' The Manhattan distance is equal to the 1-metric. It is also equal to twice
#' the trace distance.
#'
#' @param p1,p2 \code{\link[=Distribution-class]{Distributions}}.
#'
#' @return The Manhattan distance between distributions \code{p1} and \code{p2}.
#'
#' @seealso \code{\link[=pMetric]{p-metric}}, \code{\link{TotalVariation}}
#'
#' @name Manhattan
#' @export
methods::setGeneric(
  name = "Manhattan",
  def = function(p1, p2, ...) {
    standardGeneric("Manhattan")
  }
)

#' @usage \S4method{Manhattan}{Distribution,Distribution}(p1, p2)
#' @name Manhattan,Distribution,Distribution-method
#' @rdname Manhattan
methods::setMethod(
  f = "Manhattan",
  signature = c(p1 = "Distribution", p2 = "Distribution"),
  definition = function(p1, p2) {
    return(pMetric(p1 = p1, p2 = p2, p = 1))
  }
)




#' Total Variation Distance
#'
#' Compute the total variation distance between two probability distributions.
#'
#' Let \code{p1} and \code{p2} denote the vectors of probability mass assigned
#' by two distributions defined on the same state space. Then, the total variation
#' distance between \code{p1} and \code{p2} is given by \code{0.5 * sum(abs(p1 - p2))}.
#'
#' The total variation distance is equal to one half of either the 1-metric or
#' the Manhattan distance.
#'
#' @param p1,p2 \code{\link[=Distribution-class]{Distributions}}.
#'
#' @return The total variation distance between distributions \code{p1} and \code{p2}.
#'
#' @seealso \code{\link[=pMetric]{p-metric}}, \code{\link{Manhattan}}
#'
#' @name TotalVariation
#' @export
methods::setGeneric(
  name = "TotalVariation",
  def = function(p1, p2, ...) {
    standardGeneric("TotalVariation")
  }
)

#' @usage \S4method{TotalVariation}{Distribution,Distribution}(p1, p2)
#' @name TotalVariation,Distribution,Distribution-method
#' @rdname TotalVariation
methods::setMethod(
  f = "TotalVariation",
  signature = c(p1 = "Distribution", p2 = "Distribution"),
  definition = function(p1, p2) {
    return(0.5 * Manhattan(p1 = p1, p2 = p2))
  }
)





#' Euclidean Distance
#'
#' Compute the Euclidean distance between two probability distributions. (Some
#' authors also refer to the Euclidean distance on distributions as the
#' Patrick-Fischer distance.)
#'
#' Let \code{p1} and \code{p2} denote the vectors of probability mass assigned
#' by two distributions defined on the same state space. Then, the Euclidean
#' distance between \code{p1} and \code{p2} is given by
#' \code{(sum(abs(p1 - p2)^2))^(1/2)}.
#'
#' The Euclidean distance is equal to the 2-metric.
#'
#' @param p1,p2 \code{\link[=Distribution-class]{Distributions}}.
#'
#' @return The Euclidean distance between distributions \code{p1} and \code{p2}.
#'
#' @seealso \code{\link[=pMetric]{p-metric}}
#'
#' @name Euclidean
#' @export
methods::setGeneric(
  name = "Euclidean",
  def = function(p1, p2, ...) {
    standardGeneric("Euclidean")
  }
)

#' @usage \S4method{Euclidean}{Distribution,Distribution}(p1, p2)
#' @name Euclidean,Distribution,Distribution-method
#' @rdname Euclidean
methods::setMethod(
  f = "Euclidean",
  signature = c(p1 = "Distribution", p2 = "Distribution"),
  definition = function(p1, p2) {
    return(pMetric(p1 = p1, p2 = p2, p = 2))
  }
)









# Fidelity similarity and ilk                                                   ##########

#' Fidelity Similarity
#'
#' Compute the fidelity similarity between two probability distributions. This
#' similarity is also known as the Bhattacharya coefficient (not to be confused
#' with the Bhattacharya distances) and the Hellinger affinity (not to be
#' confused with the Hellinger metric).
#'
#' Let \code{p1} and \code{p2} denote the vectors of probability mass assigned
#' by two distributions defined on the same state space. Then, the fidelity
#' similarity between \code{p1} and \code{p2} is given by \code{sum(sqrt(p1 * p2))}.
#'
#' The Hellinger metric and the Bhattacharya distances may both be defined in
#' terms of the fidelity similarity.
#'
#' @param p1,p2 \code{\link[=Distribution-class]{Distributions}}.
#'
#' @return The fidelity similarity between distributions \code{p1} and \code{p2}.
#'
#' @seealso \code{\link{Bhattacharya1}}, \code{\link{Bhattacharya2}}, \code{\link{Hellinger}}, \code{\link{JeffriesMatusita}}
#'
#' @name Fidelity
#' @export
methods::setGeneric(
  name = "Fidelity",
  def = function(p1, p2, ...) {
    standardGeneric("Fidelity")
  }
)

#' @usage \S4method{Fidelity}{Distribution,Distribution}(p1, p2)
#' @name Fidelity,Distribution,Distribution-method
#' @rdname Fidelity
methods::setMethod(
  f = "Fidelity",
  signature = c(p1 = "Distribution", p2 = "Distribution"),
  definition = function(p1, p2) {
    p1 <- Arrange(p1)
    p2 <- Arrange(p2)
    if (!Identical(p1, p2, names = TRUE, states = TRUE, probs = FALSE, arrange = FALSE)) {
      depth <- rlang::call_depth() - 1
      stop(stringr::str_c(substitute(p1, env = rlang::call_frame(depth)[["env"]]),
                          "and",
                          substitute(p2, env = rlang::call_frame(depth)[["env"]]),
                          "must be defined on the same state space.",
                          sep = " "))
    }
    fidelity <- sum(sqrt(p1 * p2))
    return(fidelity)
  }
)





#' Bhattacharya Distances
#'
#' Compute the Bhattacharya distance 1 or the Bhattacharya distance 2 between
#' two probability distributions.
#'
#' Let \code{p1} and \code{p2} denote the vectors of probability mass assigned
#' by two distributions defined on the same state space. Let \code{rho(p1, p2)}
#' denote the fidelity similarity between \code{p1} and \code{p2}. Then, the
#' Bhattacharya distance 1 between \code{p1} and \code{p2} is given by
#' \code{acos(rho(p1, p2))^2}; the Bhattacharya distance 2 between \code{p1}
#' and \code{p2} is given by \code{-log(rho(p1, p2))}.
#'
#' @param p1,p2 \code{\link[=Distribution-class]{Distributions}}.
#'
#' @return A Bhattacharya distance between distributions \code{p1} and \code{p2}.
#'
#' @seealso \code{\link{Fidelity}}
#'
#' @name Bhattacharya
NULL

#' @name Bhattacharya1
#' @rdname Bhattacharya
#' @export
methods::setGeneric(
  name = "Bhattacharya1",
  def = function(p1, p2, ...) {
    standardGeneric("Bhattacharya1")
  }
)

#' @usage \S4method{Bhattacharya1}{Distribution,Distribution}(p1, p2)
#' @name Bhattacharya1,Distribution,Distribution-method
#' @rdname Bhattacharya
methods::setMethod(
  f = "Bhattacharya1",
  signature = c(p1 = "Distribution", p2 = "Distribution"),
  definition = function(p1, p2) {
    (acos(Fidelity(p1, p2)))^2
  }
)

#' @name Bhattacharya2
#' @rdname Bhattacharya
#' @export
methods::setGeneric(
  name = "Bhattacharya2",
  def = function(p1, p2, ...) {
    standardGeneric("Bhattacharya2")
  }
)

#' @usage \S4method{Bhattacharya2}{Distribution,Distribution}(p1, p2)
#' @name Bhattacharya2,Distribution,Distribution-method
#' @rdname Bhattacharya
methods::setMethod(
  f = "Bhattacharya2",
  signature = c(p1 = "Distribution", p2 = "Distribution"),
  definition = function(p1, p2) {
    -log(Fidelity(p1, p2))
  }
)





#' Hellinger metric
#'
#' Compute the Hellinger metric between two probability distributions.
#'
#' Let \code{p1} and \code{p2} denote vectors of probability mass assigned by
#' two distributions defined on the same state space. Let \code{rho(p1, p2)}
#' denote the fidelity similarity between \code{p1} and \code{p2}. Then, the
#' Hellinger metric between \code{p1} and \code{p2} is given by
#' \code{sqrt(1 - rho(p1, p2))}.
#'
#' The Hellinger metric may also be expressed in terms of the Euclidean distance:
#' \code{(1 / sqrt(2)) * Euclidean(sqrt(p1), sqrt(p2))}.
#'
#' @param p1,p2 \code{\link[=Distribution-class]{Distributions}}.
#'
#' @return The Hellinger metric between distributions \code{p1} and \code{p2}.
#'
#' @seealso \code{\link{Fidelity}}
#'
#' @name Hellinger
#' @export
methods::setGeneric(
  name = "Hellinger",
  def = function(p1, p2, ...) {
    standardGeneric("Hellinger")
  }
)

#' @usage \S4method{Hellinger}{Distribution,Distribution}(p1, p2)
#' @name Hellinger,Distribution,Distribution-method
#' @rdname Hellinger
methods::setMethod(
  f = "Hellinger",
  signature = c(p1 = "Distribution", p2 = "Distribution"),
  definition = function(p1, p2) {
    sqrt(1 - Fidelity(p1, p2))
  }
)





#' Jeffries-Matusita Distance
#'
#' Compute the Jeffries-Matusita distance between two probability distributions.
#'
#' Let \code{p1} and \code{p2} denote vectors of probability mass assigned by
#' two distributions defined on the same state space. Let \code{rho(p1, p2)}
#' denote the fidelity similarity between \code{p1} and \code{p2}. Then, the
#' Jeffries-Matusita distance between \code{p1} and \code{p2} is given by
#' \code{sqrt(2 * (1 - Fidelity(p1, p2)))}.
#'
#' The Jeffries-Matusita distance may also be expressed in terms of any of the
#' following: Euclidean distance, Bhattacharya distance 2, and Hellinger metric.
#' It is straightforward to arrive at any of these expressions.
#'
#' @param p1,p2 \code{\link[=Distribution-class]{Distributions}}.
#'
#' @return The Jeffries-Matusita distance between distributions \code{p1} and
#'   \code{p2}.
#'
#' @seealso \code{\link{Fidelity}}
#'
#' @name JeffriesMatusita
#' @export
methods::setGeneric(
  name = "JeffriesMatusita",
  def = function(p1, p2, ...) {
    standardGeneric("JeffriesMatusita")
  }
)

#' @usage \S4method{JeffriesMatusita}{Distribution,Distribution}(p1, p2)
#' @name JeffriesMatusita,Distribution,Distribution-method
#' @rdname JeffriesMatusita
methods::setMethod(
  f = "JeffriesMatusita",
  signature = c(p1 = "Distribution", p2 = "Distribution"),
  definition = function(p1, p2) {
    sqrt(2 * (1 - Fidelity(p1, p2)))
  }
)









# Kullback-Leibler divergence and ilk                                           ##########

KL <- function(p1, p2) {sum(p1 * log(p1 / p2))}



#' Kullback-Leibler Divergence
#'
#' Compute the Kullback-Leibler divergence of one probability distribution
#' from another. This divergence is also known as the relative entropy, the
#' information deviation, and the information gain.
#'
#' Let \code{p1} and \code{p2} denote the vectors of probability mass assigned
#' by two distributions defined on the same state space; furthermore, let
#' these distributions be strictly positively-valued. Then, the Kullback-Leibler
#' divergence of \code{p2} from \code{p1} is given by \code{sum(p1 * log(p1 / p2))}.
#'
#' Note that the terminology "divergence of \code{p2} from \code{p1}" indicates
#' that \code{p1} is the reference distribution against which the distribution
#' \code{p2} is evaluated.
#'
#' Kullback-Leibler divergence is not a symmetric function. That is, it is not
#' generally true that \code{KullbackLeibler(p1, p2) = KullbackLeibler(p2, p1)}.
#' Symmetric functions based on the Kullback-Leibler divergence are available
#' through the Jeffrey and Topsoe distance.
#'
#' @param p1,p2 \code{\link[=Distribution-class]{Distributions}}.
#'
#' @return The Kullback-Leibler divergence of \code{p2} from \code{p1}.
#'
#' @seealso \code{\link[=JensenShannon]{Jensen-Shannon}}, \code{\link{Jeffrey}}, \code{\link{Topsoe}}
#'
#' @name KullbackLeibler
#' @export
methods::setGeneric(
  name = "KullbackLeibler",
  def = function(p1, p2, ...) {
    standardGeneric("KullbackLeibler")
  }
)

#' @usage \S4method{KullbackLeibler}{Distribution,Distribution}(p1, p2)
#' @name KullbackLeibler,Distribution,Distribution-method
#' @rdname KullbackLeibler
methods::setMethod(
  f = "KullbackLeibler",
  signature = c(p1 = "Distribution", p2 = "Distribution"),
  definition = function(p1, p2) {
    if (0 %in% p1$P) {
      depth <- rlang::call_depth() - 1
      stop(stringr::str_c(substitute(p1, env = rlang::call_frame(depth)[["env"]]),
                          "must not be zero-valued for any random-variable states.",
                          sep = " "))
    }
    if (0 %in% p2$P) {
      depth <- rlang::call_depth() - 1
      stop(stringr::str_c(substitute(p2, env = rlang::call_frame(depth)[["env"]]),
                          "must not be zero-valued for any random-variable states.",
                          sep = " "))
    }
    p1 <- Arrange(p1)
    p2 <- Arrange(p2)
    if (!Identical(p1, p2, names = TRUE, states = TRUE, probs = FALSE, arrange = FALSE)) {
      depth <- rlang::call_depth() - 1
      stop(stringr::str_c(substitute(p1, env = rlang::call_frame(depth)[["env"]]),
                          "and",
                          substitute(p2, env = rlang::call_frame(depth)[["env"]]),
                          "must be defined on the same random-variable states.",
                          sep = " "))
    }
    return(KL(p1$P, p2$P))
  }
)





#' Jensen-Shannon Divergence
#'
#' Compute Jensen-Shannon divergences of two probability distributions.
#'
#' Let \code{p1} and \code{p2} denote the vectors of probability mass assigned
#' by two distributions defined on the same state space; furthermore, let
#' these distributions be strictly positively-valued. Let \code{p3 = a*p1 + (1-a)*p2},
#' for a constant \code{0 < a < 1}. Then, the Jensen-Shannon divergence is
#' given by \code{a*KullbackLeibler(p1, p3) + (1-a)*KullbackLeibler(p2, p3)}.
#'
#' Jensen-Shannon divergence is a symmetric function only if \code{a = 0.5}.
#'
#' @param p1,p2 \code{\link[=Distribution-class]{Distributions}}.
#' @param a A numeric constant between 0 and 1.
#'
#' @seealso \code{\link[=KullbackLeibler]{Kullback-Leibler}}, \code{\link{Topsoe}}.
#'
#' @name JensenShannon
#' @export
methods::setGeneric(
  name = "JensenShannon",
  def = function(p1, p2, ...) {
    standardGeneric("JensenShannon")
  }
)

#' @usage \S4method{JensenShannon}{Distribution,Distribution}(p1, p2, a)
#' @name JensenShannon,Distribution,Distribution-method
#' @rdname JensenShannon
methods::setMethod(
  f = "JensenShannon",
  signature = c(p1 = "Distribution", p2 = "Distribution"),
  definition = function(p1, p2, a) {
    if (0 %in% p1$P) {
      depth <- rlang::call_depth() - 1
      stop(stringr::str_c(substitute(p1, env = rlang::call_frame(depth)[["env"]]),
                          "must not be zero-valued for any random-variable states.",
                          sep = " "))
    }
    if (0 %in% p2$P) {
      depth <- rlang::call_depth() - 1
      stop(stringr::str_c(substitute(p2, env = rlang::call_frame(depth)[["env"]]),
                          "must not be zero-valued for any random-variable states.",
                          sep = " "))
    }
    p1 <- Arrange(p1)
    p2 <- Arrange(p2)
    if (!Identical(p1, p2, names = TRUE, states = TRUE, probs = FALSE, arrange = FALSE)) {
      depth <- rlang::call_depth() - 1
      stop(stringr::str_c(substitute(p1, env = rlang::call_frame(depth)[["env"]]),
                          "and",
                          substitute(p2, env = rlang::call_frame(depth)[["env"]]),
                          "must be defined on the same random-variable states.",
                          sep = " "))
    }
    p3 <- (a * p1$P) + ((1-a) * p2$P)
    jensen_shannon <- (a * KL(p1$P, p3)) + ((1-a) * KL(p2$P, p3))
    return(jensen_shannon)
  }
)





#' Jeffrey Distance
#'
#' Compute the Jeffrey distance between two probability distributions.
#'
#' Let \code{p1} and \code{p2} denote the vectors of probability mass assigned
#' by two distributions defined on the same state space; furthermore, let
#' these distributions be strictly positively-valued. Then, the Jeffrey distance
#' between \code{p1} and \code{p2} is given by
#' \code{KullbackLeibler(p1, p2) + KullbackLeibler(p2, p1)}.
#'
#' Jeffrey distance is a symmetric function based on Kullback-Leibler divergence.
#' It is the sum of the Kullback-Leibler divergence of each distribution from
#' the other.
#'
#' @param p1,p2 \code{\link[=Distribution-class]{Distributions}}.
#'
#' @return The Jeffrey distance between \code{p1} and \code{p2}.
#'
#' @seealso \code{\link[=KullbackLeibler]{Kullback-Leibler}}
#'
#' @name Jeffrey
#' @export
methods::setGeneric(
  name = "Jeffrey",
  def = function(p1, p2, ...) {
    standardGeneric("Jeffrey")
  }
)

#' @usage \S4method{Jeffrey}{Distribution,Distribution}(p1, p2)
#' @name Jeffrey,Distribution,Distribution-method
#' @rdname Jeffrey
methods::setMethod(
  f = "Jeffrey",
  signature = c(p1 = "Distribution", p2 = "Distribution"),
  definition = function(p1, p2) {
    if (0 %in% p1$P) {
      depth <- rlang::call_depth() - 1
      stop(stringr::str_c(substitute(p1, env = rlang::call_frame(depth)[["env"]]),
                          "must not be zero-valued for any random-variable states.",
                          sep = " "))
    }
    if (0 %in% p2$P) {
      depth <- rlang::call_depth() - 1
      stop(stringr::str_c(substitute(p2, env = rlang::call_frame(depth)[["env"]]),
                          "must not be zero-valued for any random-variable states.",
                          sep = " "))
    }
    p1 <- Arrange(p1)
    p2 <- Arrange(p2)
    if (!Identical(p1, p2, names = TRUE, states = TRUE, probs = FALSE, arrange = FALSE)) {
      depth <- rlang::call_depth() - 1
      stop(stringr::str_c(substitute(p1, env = rlang::call_frame(depth)[["env"]]),
                          "and",
                          substitute(p2, env = rlang::call_frame(depth)[["env"]]),
                          "must be defined on the same random-variable states.",
                          sep = " "))
    }
    jeffrey <- KL(p1$P, p2$P) + KL(p2$P, p1$P)
    return(jeffrey)
  }
)





#' Topsoe Distance
#'
#' Compute the Topsoe distance between two probability distributions.
#'
#' Let \code{p1} and \code{p2} denote the vectors of probability mass assigned
#' by two distributions defined on the same state space; furthermore, let
#' these distributions be strictly positively-valued. Let \code{p3 = (p1 + p2) / 2}.
#' Then, the Topsoe distance between \code{p1} and \code{p2} is given by
#' \code{KullbackLeibler(p1, p3) + KullbackLeibler(p2, p3)}.
#'
#' The Topsoe distance is a symmetric function based on Kullback-Leibler divergence.
#' It is the sum of the Kullback-Leibler divergence of the distribution
#' interpolated between \code{p1} and \code{p2}, from either distribution.
#' Equivalently, the Topsoe distance is twice the Jensen-Shannon divergence
#' with \code{a = 1/2}.
#'
#' The Topsoe distance is not a metric (i.e., it does not satisfy the triangle
#' inequality); however, its square root is a metric.
#'
#' @param p1,p2 \code{\link[=Distribution-class]{Distributions}}.
#' @param metric A logical. If \code{TRUE}, then the square root of the Topsoe
#'   distance is returned, which satisfies the properties of a metric.
#'
#' @return The Topsoe distance or the Topsoe metric (if \code{metric = TRUE})
#'   between \code{p1} and \code{p2}.
#'
#' @seealso \code{\link[=KullbackLeibler]{Kullback-Leibler}}, \code{\link[=JensenShannon]{Jensen-Shannon}}
#'
#' @name Topsoe
#' @export
methods::setGeneric(
  name = "Topsoe",
  def = function(p1, p2, ...) {
    standardGeneric("Topsoe")
  }
)

#' @usage \S4method{Topsoe}{Distribution,Distribution}(p1, p2, metric = FALSE)
#' @name Topsoe,Distribution,Distribution-method
#' @rdname Topsoe
methods::setMethod(
  f = "Topsoe",
  signature = c(p1 = "Distribution", p2 = "Distribution"),
  definition = function(p1, p2, metric = FALSE) {
    if (0 %in% p1$P) {
      depth <- rlang::call_depth() - 1
      stop(stringr::str_c(substitute(p1, env = rlang::call_frame(depth)[["env"]]),
                          "must not be zero-valued for any random-variable states.",
                          sep = " "))
    }
    if (0 %in% p2$P) {
      depth <- rlang::call_depth() - 1
      stop(stringr::str_c(substitute(p2, env = rlang::call_frame(depth)[["env"]]),
                          "must not be zero-valued for any random-variable states.",
                          sep = " "))
    }
    p1 <- Arrange(p1)
    p2 <- Arrange(p2)
    if (!Identical(p1, p2, names = TRUE, states = TRUE, probs = FALSE, arrange = FALSE)) {
      depth <- rlang::call_depth() - 1
      stop(stringr::str_c(substitute(p1, env = rlang::call_frame(depth)[["env"]]),
                          "and",
                          substitute(p2, env = rlang::call_frame(depth)[["env"]]),
                          "must be defined on the same random-variable states.",
                          sep = " "))
    }
    p3 <- (p1$P + p2$P) / 2
    topsoe <- KL(p1$P, p3) + KL(p2$P, p3) %>%
      (function(.t) {ifelse(metric, sqrt(.t), .t)})
    return(topsoe)
  }
)
