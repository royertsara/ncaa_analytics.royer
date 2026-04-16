# DataFrame for the functions test:

df_test <- tibble::tibble(
  TEAM = c("A", "B", "C"),
  G = c(10, 20, 30),
  W = c(5, 15, 20),
  ADJOE = c(110, 105, 120),
  ADJDE = c(95, 90, 100),
  EFG_O = c(0.55, 0.60, 0.52),
  EFG_D = c(0.48, 0.50, 0.47),
  `2P_O` = c(0.52, 0.55, 0.50),
  `3P_O` = c(0.35, 0.38, 0.33),
  TORD = c(15, 12, 18),
  ORB = c(30, 28, 32),
  DRB = c(70, 68, 72),
  ADJ_T = c(70, 68, 72),
  CONF = c("ACC", "SEC", "ACC")
)

test_that("best_10_winpct fonctionne", {
  res <- best_10_winpct(df_test)

  expect_true(is.data.frame(res))
  expect_true(all(c("TEAM", "G", "W", "win_pct") %in% names(res)))

})

test_that("top_offense fonctionne", {
  res <- top_offense(df_test, n = 2, metric = "ADJOE")

  expect_true(is.data.frame(res))
  expect_equal(nrow(res), 2)
  expect_true(all(c("TEAM", "ADJOE") %in% names(res)))
})

test_that("top_offense renvoie une erreur si metric invalide", {
  expect_error(top_offense(df_test, metric = "BAD_METRIC"))
})

test_that("top_defense fonctionne", {
  res <- top_defense(df_test, n = 2, metric = "ADJDE")

  expect_true(is.data.frame(res))
  expect_equal(nrow(res), 2)
  expect_true(all(c("TEAM", "ADJDE") %in% names(res)))
})

test_that("top_defense renvoie une erreur si metric invalide", {
  expect_error(top_defense(df_test, metric = "BAD_METRIC"))
})


test_that("team_profil retourne un ggplot", {
  res <- team_profil(df_test, team = "A")

  expect_true(inherits(res, "ggplot"))
})

test_that("team_profil renvoie une erreur si team invalide", {
  expect_error(team_profil(df_test, team = "ZZZ"))
})

test_that("filter_conference fonctionne", {
  res <- filter_conference(df_test, conf = "ACC")

  expect_true(is.data.frame(res))
  expect_true(all(res$CONF == "ACC"))
})

test_that("filter_conference renvoie une erreur si conf invalide", {
  expect_error(filter_conference(df_test, conf = "BAD_CONF"))
})

test_that("top_conference fonctionne", {
  res <- top_conference(df_test, metric = "ADJOE")

  expect_true(is.data.frame(res))
  expect_true(all(c("CONF", "ADJOE", "ADJDE", "EFG_O", "EFG_D") %in% names(res)))
})

test_that("top_conference renvoie une erreur si metric invalide", {
  expect_error(top_conference(df_test, metric = "BAD_METRIC"))
})


