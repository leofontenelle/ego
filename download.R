library(jsonlite)
library(rtoot)
my.id = "109399675152989746"

# verify_envvar()

# Functions ----

check.account = function(account) {
  is.list(account) &&
    !is.null(account$id) &&
    !is.null(account$following_count) &&
    !is.null(account$followers_count)
}

get.path = function(id, folder) file.path(folder, sprintf("%s.json", id))
get.account.path = function(id) get.path(id, "accounts")
get.followers.path  = function(id) get.path(id, "followers")
get.followings.path = function(id) get.path(id, "followings")
get.move.path = function(id) get.path(id, "moves")

get.account = function(id, verbose = TRUE) {
  account.path = get.account.path(id)
  if (file.exists(account.path)) {
    account = fromJSON(get.account.path(id))
    if (check.account(account)) return(account)
    if (isTRUE(verbose)) {
      message(sprintf("Account %s was not saved properly", id))
      # If the account was moved, we are going to write the new location,
      # not the old one.
      file.remove(get.account.path(id))
    }
  }
  if (isTRUE(verbose)) message(sprintf("Getting account %s", id))
  account = get_account(id, parse = FALSE)
  if (!is.null(account$moved)) {
    move = list(from = id, to = account$moved$id)
    if (isTRUE(verbose)) message(sprintf("Account %s moved to %s", move$from, move$to))
    write_json(move, get.move.path(move$from))
    account.path = get.account.path(account$moved$id)
    account = account$moved
  }
  write_json(account, account.path)
  return(account)
}

get.follows = function(id, limit = 40L, what, verbose = TRUE) {
  stopifnot(what %in% c("followings", "followers"))
  follows.path = get.path(id, what)
  if (file.exists(follows.path)) {
    l = fromJSON(follows.path)
    ids = switch(what, followings = l$following, followers = l$follower)
    if (length(ids) > 0) return(ids)
    if (isTRUE(verbose)) message(sprintf(
      "There are no %s for %s, downloading again just to be sure", what, id
    ))
  }
  fun = switch(what,
               followings = get_account_following,
               followers = get_account_followers)
  max_id = NULL
  page.size = 40L
  pages = ceiling(limit/page.size)
  if (pages < 2) verbose = FALSE
  if (verbose) {
    pb = txtProgressBar(min = 0, max = pages, style = 3)
    message(sprintf("\nDownloading %s of %s", what, id))
  }
  ids = vector(mode = "list", length = pages)
  for (i in seq.int(pages)) {
    api_response = fun(id, max_id, parse = FALSE)
    ids[[i]] = sapply(api_response, write.account.or.not)
    if (verbose) setTxtProgressBar(pb, i)
    Sys.sleep(3)
    if (rtoot:::break_process_request(api_response, TRUE, verbose)) break
    max_id = attr(api_response, "headers")$max_id
  }
  if (verbose) cat("\n")
  ids = unlist(ids)
  if (is.null(ids) || (length(ids) == 0)) {
    l = list(follower = character(0), following = character(0))
  } else {
    l = switch(what,
               followings = list(follower = id, following = ids),
               followers =  list(follower = ids, following = id))
  }
  write_json(l, follows.path)
  return(ids)
}

write.account.or.not = function(account) {
  path = get.account.path(account$id)
  if (!file.exists(path)) write_json(account, path)
  return(account$id)
}

# Proceed ----

invisible(sapply(
  c("accounts", "followings", "followers", "moves"),
  \(x) if(!dir.exists(x)) dir.create(x)
))
my.account = get.account(my.id)
my.followings.id = get.follows(my.id, my.account$following_count, "followings")
my.followers.id = get.follows(my.id, my.account$followers_count, "followers")
follows = unique(c(my.followings.id, my.followers.id))

# Make it so we can resume where the 503 error stopped us
whereami = list(i = NULL, what = NULL, page = NULL, max_id = NULL)
if (file.exists("whereami.rds")) whereami = readRDS("whereami.rds")
ids = NULL
if (file.exists("ids.rds")) ids = readRDS("ids.rds")

for (i in (whereami$i %||% 1):length(follows)) {
  whereami$i = i; saveRDS(whereami, "whereami.rds")
  account = get.account(follows[i])
  if (interactive()) message(sprintf("Downloading follows for %s (%d of %d)", account$id, i, length(follows)))
  for (what in c("followings", "followers")) {
    if (what == "followings" && !is.null(whereami$what) && whereami$what == "followers") next
    whereami$what = what; saveRDS(whereami, "whereami.rds")
    count = switch(what, followings = account$following_count, followers = account$followers_count)
    follows.path = get.path(account$id, what)
    if (file.exists(follows.path)) {
      next
    } else if (count == 0) {
      # code duplication to avoid deep nesting
      l = list(from = character(0), to = character(0))
      write_json(l, follows.path)
      ids = NULL; saveRDS(ids, "ids.rds")
      next
    }
    fun = switch(what, followings = get_account_following, followers = get_account_followers)
    pages = ceiling(count/40L)
    if (is.null(ids)) ids = vector(mode = "list", length = pages)
    if (interactive()) pb = txtProgressBar(min = 0, max = pages, style = 3)
    starting.page = whereami$page %||% 1
    for (page in starting.page:pages) {
      whereami$page = page; saveRDS(whereami, "whereami.rds")
      api_response = fun(account$id, whereami$max_id, parse = FALSE)
      ids[[page]] = sapply(api_response, write.account.or.not); saveRDS(ids, "ids.rds")
      if (interactive()) setTxtProgressBar(pb, page)
      Sys.sleep(3)
      if (rtoot:::break_process_request(api_response, TRUE, verbose)) break
      whereami$max_id = attr(api_response, "headers")$max_id; saveRDS(whereami, "whereami.rds")
    }
    whereami$max_id = NULL; saveRDS(whereami, "whereami.rds")
    whereami$page = NULL; saveRDS(whereami, "whereami.rds")
    if (interactive()) {close(pb); cat("\n")}
    ids = unlist(ids); saveRDS(ids, "ids.rds")
    if (is.null(ids) || (length(ids) == 0)) {
      l = list(from = character(0), to = character(0))
    } else {
      l = switch(what,
                 followings = list(follower=account$id, following=ids),
                 followers =  list(follower=ids, following=account$id))
    }
    write_json(l, follows.path)
    ids = NULL; saveRDS(ids, "ids.rds")
  }
  whereami$what = NULL
  if (identical(i, length(follows))) whereami$i = NULL
  saveRDS(whereami, "whereami.rds")
}
