# Initial set-up ----

library(jsonlite)
library(rtoot)
my.id = "109399675152989746"

verify_envvar()

# Functions ----

get.account = function(id, verbose = TRUE) {
  account.path = get.account.path(id)
  if (file.exists(account.path)) {
    return(fromJSON(get.account.path(id)))
  } 
  if (isTRUE(verbose)) message("Getting account %s", id)
  account = get_account(id)
  stopifnot(is.null(account$moved))
  write_json(account, account.path)
  NULL
}

get.follows.or.not = function(account, what, verbose = TRUE) {
  stopifnot(what %in% c("followings", "followers"))
  follows.path = get.path(account$id, what)
  if (file.exists(follows.path)) {
    # Already done
    follows = fromJSON(follows.path)
    return(switch(what,
                  followings = follows$to,
                  followers = follows$from))
  }
  follows.fun = switch(what, 
                       followings = get_account_following,
                       followers = get_account_followers)
  follows.pages = switch(what,
                         followings = account$following_count,
                         followers = account$followers_count)
  
  if (length(follows) == 0) {
    write_json(list(fom = character(0), to = character(0)),
               follows.path)
    return(NULL)
  }
  stopifnot(is.data.frame(follows))
  stopifnot(is.null(follows$moved))
  apply(follows, 1, write.account.or.not)
  # Writing as a list to preserve column-major order and save space
  l = switch(what, 
             followings = list(from = id, to = follows$id),
             followers =  list(from = follows$id, to = id))
  write_json(l, follows.path)
  return(follows$id)  
}

get.path = function(id, folder) file.path(folder, sprintf("%s.json", id))
get.account.path = function(id) get.path(id, "accounts")
# These could be CSV but let's keep the symmetry
get.followers.path  = function(id) get.path(id, "followers")
get.followings.path = function(id) get.path(id, "followings")

write.account.or.not = function(account) {
  path = get.account.path(account$id)
  if (!file.exists(path)) write_json(account, path)
  return(account$id)
}

get.follows = function(id, limit = 40L, what, verbose = TRUE) {
  stopifnot(what %in% c("followings", "followers"))
  follows.path = get.path(id, what)
  if (file.exists(follows.path)) {
    l = fromJSON(follows.path)
    return(switch(what, followings = l$to, followers = l$from))
  }
  fun = switch(what, 
               followings = get_account_following, 
               followers = get_account_followers)
  max_id = NULL
  page.size = 40L
  pages = ceiling(limit/page.size)
  if (pages < 2) verbose = FALSE
  if (verbose) pb = txtProgressBar(min = 0, max = pages, style = 3)
  ids = vector(mode = "list", length = pages)
  for (i in seq.int(pages)) {
    api_response = fun(id, max_id, parse = FALSE)
    ids[[i]] = sapply(api_response, write.account.or.not)
    if (verbose) setTxtProgressBar(pb, i)
    if (rtoot:::break_process_request(api_response, TRUE, verbose)) break
    max_id = attr(api_response, "headers")$max_id
  }
  if (verbose) cat("\n")
  ids = unlist(ids)
  l = switch(what, 
             followings = list(from = id, to = ids),
             followers =  list(from = ids, to = id))
  write_json(l, follows.path)
  return(ids)
}

# Proceed ----

my.account = get.account(my.id)
my.followings.id = get.follows(my.id, my.account$following_count, "followings")
my.followers.id = get.follows(my.id, my.account$followers_count, "followers")
follows = unique(c(my.followings.id, my.followers.id))
global.pb = txtProgressBar(max = length(follows), style = 3)
for (i in seq_along(follows)) {
  id = follows[1]
  account = get.account(id)
  followings.id = get.follows(id, account$following_count, "followings")
  followers.id = get.follows(id, account$followers_count, "followers")
  setTxtProgressBar(global.pb, i)
}
close(global.pb)
cat("\n")