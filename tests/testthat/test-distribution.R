
suppressMessages(library(magrittr))



x1 <- c(0, 1, 2, 3)
x2 <- c(0, 1, 2, 3)
x3 <- c(0, 1, 2, 4)

p1 <- c(0.1, 0.2, 0.3, 0.4)
p2 <- 1/16

d1a <- c(0.0, 0.2, 0.3, 0.4)
d1b <- c(0.1, 0.2, 0.3, 0.5)
d2a <- 1/32
d2b <- 1/8

a1 <- as.character(p1)
a2 <- as.character(p2)

rv1_tbl <- tibble::tibble(X1 = x1)
rv2_tbl <- tidyr::crossing(X1 = x1, X2 = x2)
rv1_df <- as.data.frame(rv1_tbl)
rv2_df <- as.data.frame(rv2_tbl)

al1_tbl <- tibble::tibble(X1 = x1, P = a1)
al2_tbl <- tidyr::crossing(X1 = x1, X2 = x2) %>% dplyr::mutate(P = a2)
al1_df <- as.data.frame(al1_tbl)
al2_df <- as.data.frame(al2_tbl)

dv1a_tbl <- tibble::tibble(X1 = x1, P = d1a)
dv1b_tbl <- tibble::tibble(X1 = x1, P = d1b)
dv2a_tbl <- tidyr::crossing(X1 = x1, X2 = x2) %>% dplyr::mutate(P = d2a)
dv2b_tbl <- tidyr::crossing(X1 = x1, X2 = x2) %>% dplyr::mutate(P = d2b)
dv1a_df <- as.data.frame(dv1a_tbl)
dv1b_df <- as.data.frame(dv1b_tbl)
dv2a_df <- as.data.frame(dv2a_tbl)
dv2b_df <- as.data.frame(dv2b_tbl)

y1 <- c(1, 1, 1, 1)
y2 <- c(1, 1, 2, 3)

ms1_tbl <- tibble::tibble(Y1 = y1)
ms2_tbl <- tibble::tibble(Y1 = y1, Y2 = y2)
ms1_df <- as.data.frame(ms1_tbl)
ms2_df <- as.data.frame(ms2_tbl)

sv1_tbl <- tibble::tibble(Y1 = y1, P = p1)
sv2_tbl <- tibble::tibble(Y1 = y1, Y2 = y2, P = p1)
sv1_df <- as.data.frame(sv1_tbl)
sv2_df <- as.data.frame(sv2_tbl)

dt1_tbl <- tibble::tibble(X1 = x1, P = p1)
dt2_tbl <- tidyr::crossing(X1 = x1, X2 = x2) %>% dplyr::mutate(P = p2)
dt1_df <- as.data.frame(dt1_tbl)
dt2_df <- as.data.frame(dt2_tbl)

ar1_tbl <- tibble::tibble(X1 = rev(x1), P = rev(p1))
ar2_tbl <- tidyr::crossing(X2 = rev(x2), X1 = rev(x1)) %>% dplyr::mutate(P = p2)
ar1_df <- as.data.frame(ar1_tbl)
ar2_df <- as.data.frame(ar2_tbl)

z1_tbl <- tibble::tibble(X1 = c(0, 1, 2, 3, 5, 4), P = 1/6)
z2a_tbl <- tibble::tibble(X1 = c(0, 1, 2, 3, 4, 0), X2 = c(0, 1, 2, 3, 4, 5), P = 1/6)
z2b_tbl <- tibble::tibble(X2 = c(0, 1, 2, 3, 4, 5), X1 = c(0, 1, 2, 3, 4, 0), P = 1/6)
z1_df <- as.data.frame(z1_tbl)
z2a_df <- as.data.frame(z2a_tbl)
z2b_df <- as.data.frame(z2b_tbl)

dt2b_tbl <- tibble::tibble(X1 = x1, X2 = x2, P = p1)
dt2b_df <- as.data.frame(dt2b_tbl)

ex1_df <- data.frame(X1 = c(0, 1, 2, 3, 4, 5), P = c(0.1, 0.2, 0.3, 0.4, 0.0, 0.0))
ex2_df <- data.frame(X1 = c(0, 1, 2, 3, 0, 4), X2 = c(0, 1, 2, 3, 5, 4), P = c(0.1, 0.2, 0.3, 0.4, 0.0, 0.0))





testthat::context(":: Distribution class validity")

testthat::test_that("A Distribution assigns numeric probability mass that sums to 1.", {
  # Missing column named 'P'.
  testthat::expect_error(
    object = methods::new(Class = "Distribution", rv1_df),
    regexp = "A Distribution must contain a numeric variable named 'P', whose values are positive and sum to 1"
  )
  testthat::expect_error(
    object = methods::new(Class = "Distribution", rv1_tbl),
    regexp = "A Distribution must contain a numeric variable named 'P', whose values are positive and sum to 1"
  )
  testthat::expect_error(
    object = methods::new(Class = "Distribution", rv2_df),
    regexp = "A Distribution must contain a numeric variable named 'P', whose values are positive and sum to 1"
  )
  testthat::expect_error(
    object = methods::new(Class = "Distribution", rv2_tbl),
    regexp = "A Distribution must contain a numeric variable named 'P', whose values are positive and sum to 1"
  )
  # Column 'P' not numeric.
  testthat::expect_error(
    object = methods::new(Class = "Distribution", al1_df),
    regexp = "A Distribution must contain a numeric variable named 'P', whose values are positive and sum to 1"
  )
  testthat::expect_error(
    object = methods::new(Class = "Distribution", al1_tbl),
    regexp = "A Distribution must contain a numeric variable named 'P', whose values are positive and sum to 1"
  )
  testthat::expect_error(
    object = methods::new(Class = "Distribution", al2_df),
    regexp = "A Distribution must contain a numeric variable named 'P', whose values are positive and sum to 1"
  )
  testthat::expect_error(
    object = methods::new(Class = "Distribution", al2_tbl),
    regexp = "A Distribution must contain a numeric variable named 'P', whose values are positive and sum to 1"
  )
  # Column 'P' does not sum to 1.
  testthat::expect_error(
    object = methods::new(Class = "Distribution", dv1a_df),
    regexp = "A Distribution must contain a numeric variable named 'P', whose values are positive and sum to 1"
  )
  testthat::expect_error(
    object = methods::new(Class = "Distribution", dv1a_tbl),
    regexp = "A Distribution must contain a numeric variable named 'P', whose values are positive and sum to 1"
  )
  testthat::expect_error(
    object = methods::new(Class = "Distribution", dv1b_df),
    regexp = "A Distribution must contain a numeric variable named 'P', whose values are positive and sum to 1"
  )
  testthat::expect_error(
    object = methods::new(Class = "Distribution", dv1b_tbl),
    regexp = "A Distribution must contain a numeric variable named 'P', whose values are positive and sum to 1"
  )
  testthat::expect_error(
    object = methods::new(Class = "Distribution", dv2a_df),
    regexp = "A Distribution must contain a numeric variable named 'P', whose values are positive and sum to 1"
  )
  testthat::expect_error(
    object = methods::new(Class = "Distribution", dv2a_tbl),
    regexp = "A Distribution must contain a numeric variable named 'P', whose values are positive and sum to 1"
  )
  testthat::expect_error(
    object = methods::new(Class = "Distribution", dv2b_df),
    regexp = "A Distribution must contain a numeric variable named 'P', whose values are positive and sum to 1"
  )
  testthat::expect_error(
    object = methods::new(Class = "Distribution", dv2b_tbl),
    regexp = "A Distribution must contain a numeric variable named 'P', whose values are positive and sum to 1"
  )
})

testthat::test_that("A Distribution assigns probability mass over a random variable.", {
  # Missing variable other than 'P'.
  testthat::expect_error(
    object = methods::new(Class = "Distribution", data.frame(P = p1)),
    regexp = "A Distribution must contain at least one variable other than 'P'"
  )
  testthat::expect_error(
    object = methods::new(Class = "Distribution", tibble::tibble(P = p1)),
    regexp = "A Distribution must contain at least one variable other than 'P'"
  )
  # Repeated points in space defined by variables other than 'P'.
  testthat::expect_error(
    object = methods::new(Class = "Distribution", ms1_df),
    regexp = "The values of the random variable .* must be unique"
  )
  testthat::expect_error(
    object = methods::new(Class = "Distribution", ms1_tbl),
    regexp = "The values of the random variable .* must be unique"
  )
  testthat::expect_error(
    object = methods::new(Class = "Distribution", ms2_df),
    regexp = "The values of the random variable .* must be unique"
  )
  testthat::expect_error(
    object = methods::new(Class = "Distribution", ms2_tbl),
    regexp = "The values of the random variable .* must be unique"
  )
  testthat::expect_error(
    object = methods::new(Class = "Distribution", sv1_df),
    regexp = "The values of the random variable .* must be unique"
  )
  testthat::expect_error(
    object = methods::new(Class = "Distribution", sv1_tbl),
    regexp = "The values of the random variable .* must be unique"
  )
  testthat::expect_error(
    object = methods::new(Class = "Distribution", sv2_df),
    regexp = "The values of the random variable .* must be unique"
  )
  testthat::expect_error(
    object = methods::new(Class = "Distribution", sv2_tbl),
    regexp = "The values of the random variable .* must be unique"
  )
})





testthat::context(":: Coercing Distributions to tibbles")

testthat::test_that("Distributions can be coerced to tibbles", {
  testthat::expect_identical(
    object = as_tibble(methods::new(Class = "Distribution", dt1_df)),
    expected = dt1_tbl
  )
  testthat::expect_identical(
    object = as_tibble(methods::new(Class = "Distribution", dt1_tbl)),
    expected = dt1_tbl
  )
  testthat::expect_identical(
    object = as_tibble(methods::new(Class = "Distribution", dt2_df)),
    expected = dt2_tbl
  )
  testthat::expect_identical(
    object = as_tibble(methods::new(Class = "Distribution", dt2_tbl)),
    expected = dt2_tbl
  )
})





testthat::context(":: Arranging Distributions")

testthat::test_that("Distributions can be arranged by column and row", {
  testthat::expect_identical(
    object = Arrange(methods::new(Class = "Distribution", ar1_df)),
    expected = methods::new(Class = "Distribution", dt1_df)
  )
  testthat::expect_identical(
    object = Arrange(methods::new(Class = "Distribution", ar1_tbl)),
    expected = methods::new(Class = "Distribution", dt1_df)
  )
  testthat::expect_identical(
    object = Arrange(methods::new(Class = "Distribution", ar2_df)),
    expected = methods::new(Class = "Distribution", dt2_df)
  )
  testthat::expect_identical(
    object = Arrange(methods::new(Class = "Distribution", ar2_tbl)),
    expected = methods::new(Class = "Distribution", dt2_df)
  )
})





testthat::context(":: Extending Distributions")

testthat::test_that("Distributions can be extended to new states", {
  testthat::expect_identical(
    object = Extend(methods::new(Class = "Distribution", dt1_df),
                    to = methods::new(Class = "Distribution", z1_df)),
    expected = methods::new(Class = "Distribution", ex1_df)
  )
  testthat::expect_identical(
    object = Extend(methods::new(Class = "Distribution", dt1_df),
                    to = methods::new(Class = "Distribution", z1_tbl)),
    expected = methods::new(Class = "Distribution", ex1_df)
  )
  testthat::expect_identical(
    object = Extend(methods::new(Class = "Distribution", dt1_tbl),
                    to = methods::new(Class = "Distribution", z1_df)),
    expected = methods::new(Class = "Distribution", ex1_df)
  )
  testthat::expect_identical(
    object = Extend(methods::new(Class = "Distribution", dt1_tbl),
                    to = methods::new(Class = "Distribution", z1_tbl)),
    expected = methods::new(Class = "Distribution", ex1_df)
  )
  testthat::expect_identical(
    object = Extend(methods::new(Class = "Distribution", dt2b_df),
                    to = methods::new(Class = "Distribution", z2a_df)),
    expected = methods::new(Class = "Distribution", ex2_df)
  )
  testthat::expect_identical(
    object = Extend(methods::new(Class = "Distribution", dt2b_df),
                    to = methods::new(Class = "Distribution", z2b_df)),
    expected = methods::new(Class = "Distribution", ex2_df)
  )
  testthat::expect_identical(
    object = Extend(methods::new(Class = "Distribution", dt2b_df),
                    to = methods::new(Class = "Distribution", z2a_tbl)),
    expected = methods::new(Class = "Distribution", ex2_df)
  )
  testthat::expect_identical(
    object = Extend(methods::new(Class = "Distribution", dt2b_df),
                    to = methods::new(Class = "Distribution", z2b_tbl)),
    expected = methods::new(Class = "Distribution", ex2_df)
  )
  testthat::expect_identical(
    object = Extend(methods::new(Class = "Distribution", dt2b_tbl),
                    to = methods::new(Class = "Distribution", z2a_df)),
    expected = methods::new(Class = "Distribution", ex2_df)
  )
  testthat::expect_identical(
    object = Extend(methods::new(Class = "Distribution", dt2b_tbl),
                    to = methods::new(Class = "Distribution", z2b_df)),
    expected = methods::new(Class = "Distribution", ex2_df)
  )
  testthat::expect_identical(
    object = Extend(methods::new(Class = "Distribution", dt2b_tbl),
                    to = methods::new(Class = "Distribution", z2a_tbl)),
    expected = methods::new(Class = "Distribution", ex2_df)
  )
  testthat::expect_identical(
    object = Extend(methods::new(Class = "Distribution", dt2b_tbl),
                    to = methods::new(Class = "Distribution", z2b_tbl)),
    expected = methods::new(Class = "Distribution", ex2_df)
  )
})





testthat::context(":: Testing identity between Distributions")

testthat::test_that("Tests of identity between distributions are not strict", {
  testthat::expect_true(
    object = Identical(methods::new(Class = "Distribution", dt1_df),
                       methods::new(Class = "Distribution", dt1_tbl),
                       names = TRUE, states = FALSE, probs = FALSE)
  )
  testthat::expect_true(
    object = Identical(methods::new(Class = "Distribution", dt1_df),
                       methods::new(Class = "Distribution", dt1_tbl),
                       names = FALSE, states = TRUE, probs = FALSE)
  )
  testthat::expect_true(
    object = Identical(methods::new(Class = "Distribution", dt1_df),
                       methods::new(Class = "Distribution", dt1_tbl),
                       names = FALSE, states = FALSE, probs = TRUE)
  )
  testthat::expect_true(
    object = Identical(methods::new(Class = "Distribution", dt1_df),
                       methods::new(Class = "Distribution", dt1_tbl),
                       names = TRUE, states = TRUE, probs = FALSE)
  )
  testthat::expect_true(
    object = Identical(methods::new(Class = "Distribution", dt1_df),
                       methods::new(Class = "Distribution", dt1_tbl),
                       names = TRUE, states = FALSE, probs = TRUE)
  )
  testthat::expect_true(
    object = Identical(methods::new(Class = "Distribution", dt1_df),
                       methods::new(Class = "Distribution", dt1_tbl),
                       names = FALSE, states = TRUE, probs = TRUE)
  )
  testthat::expect_true(
    object = Identical(methods::new(Class = "Distribution", dt1_df),
                       methods::new(Class = "Distribution", dt1_tbl),
                       names = TRUE, states = TRUE, probs = TRUE)
  )
  testthat::expect_true(
    object = Identical(methods::new(Class = "Distribution", dt2_df),
                       methods::new(Class = "Distribution", dt2_tbl),
                       names = TRUE, states = FALSE, probs = FALSE)
  )
  testthat::expect_true(
    object = Identical(methods::new(Class = "Distribution", dt2_df),
                       methods::new(Class = "Distribution", dt2_tbl),
                       names = FALSE, states = TRUE, probs = FALSE)
  )
  testthat::expect_true(
    object = Identical(methods::new(Class = "Distribution", dt2_df),
                       methods::new(Class = "Distribution", dt2_tbl),
                       names = FALSE, states = FALSE, probs = TRUE)
  )
  testthat::expect_true(
    object = Identical(methods::new(Class = "Distribution", dt2_df),
                       methods::new(Class = "Distribution", dt2_tbl),
                       names = TRUE, states = TRUE, probs = FALSE)
  )
  testthat::expect_true(
    object = Identical(methods::new(Class = "Distribution", dt2_df),
                       methods::new(Class = "Distribution", dt2_tbl),
                       names = TRUE, states = FALSE, probs = TRUE)
  )
  testthat::expect_true(
    object = Identical(methods::new(Class = "Distribution", dt2_df),
                       methods::new(Class = "Distribution", dt2_tbl),
                       names = FALSE, states = TRUE, probs = TRUE)
  )
  testthat::expect_true(
    object = Identical(methods::new(Class = "Distribution", dt2_df),
                       methods::new(Class = "Distribution", dt2_tbl),
                       names = TRUE, states = TRUE, probs = TRUE)
  )
  testthat::expect_true(
    object = Identical(methods::new(Class = "Distribution", dt1_df),
                       methods::new(Class = "Distribution",
                                    tibble::tibble(X1 = x3, P = p1)),
                       names = TRUE, states = FALSE, probs = FALSE)
  )
  testthat::expect_false(
    object = Identical(methods::new(Class = "Distribution", dt1_df),
                       methods::new(Class = "Distribution",
                                    tibble::tibble(X1 = x3, P = p1)),
                       names = TRUE, states = TRUE, probs = FALSE)
  )
  testthat::expect_false(
    object = Identical(methods::new(Class = "Distribution", dt1_df),
                       methods::new(Class = "Distribution",
                                    tibble::tibble(X1 = x3, P = 1/4)),
                       names = TRUE, states = TRUE, probs = TRUE)
  )
  testthat::expect_true(
    object = Identical(methods::new(Class = "Distribution", dt2b_df),
                       methods::new(Class = "Distribution",
                                    tibble::tibble(X2 = rev(x2), X1 = rev(x1), P = p1)),
                       names = TRUE, states = TRUE, probs = FALSE)
  )
  testthat::expect_false(
    object = Identical(methods::new(Class = "Distribution", dt2b_df),
                       methods::new(Class = "Distribution",
                                    tibble::tibble(X2 = rev(x2), X1 = rev(x1), P = p1)),
                       names = TRUE, states = TRUE, probs = TRUE)
  )
  testthat::expect_true(
    object = Identical(methods::new(Class = "Distribution", dt2b_df),
                       methods::new(Class = "Distribution",
                                    tibble::tibble(X2 = rev(x2), X1 = rev(x1), P = rev(p1))),
                       names = TRUE, states = TRUE, probs = TRUE)
  )
  testthat::expect_false(
    object = Identical(methods::new(Class = "Distribution", dt2b_df),
                       methods::new(Class = "Distribution",
                                    tibble::tibble(X2 = rev(x2), X1 = rev(x1), P = rev(p1))),
                       names = TRUE, states = TRUE, probs = TRUE, arrange = FALSE)
  )
})

