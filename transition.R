# Initial set-up ----

library(jsonlite)
my.id = "109399675152989746"


# Functions ----

decide.which.account = function(a, b) {
  a.ok = !is.null(a$last_status_at) && !is.na(a$last_status_at) && (length(a$last_status_at) > 0)
  b.ok = !is.null(a$last_status_at) && !is.na(a$last_status_at) && (length(a$last_status_at) > 0)
  larger = ifelse(a$statuses_count >= b$statuses_count, a, b)
  if (a.ok && b.ok) larger
  else if (a.ok) a
  else if (b.ok) b
  else larger
}

get.path = function(id, folder) file.path(folder, sprintf("%s.json", id))
get.path.old = function(id, folder) file.path("../ego", get.path(id, folder))
get.account.path = function(id) get.path(id, "accounts")
get.account.path.old = function(id) file.path("../ego", get.account.path(id))
get.followers.path.old  = function(id) file.path("../ego", get.path(id, "followers"))
get.followings.path.old = function(id) file.path("../ego", get.path(id, "followings"))
# These could be CSV but let's keep the symmetry
get.followers.path  = function(id) get.path(id, "followers")
get.followings.path = function(id) get.path(id, "followings")

transition.account = function(id) {
  account.path.old = get.account.path.old(id)
  account.path = get.account.path(id)
  if (!file.exists(account.path.old)) return(NULL) # Download later
  if (file.exists(account.path)) return(NULL) # Already done
  account = fromJSON(get.account.path.old(id))
  stopifnot(is.null(account$moved))
  write_json(account, account.path)
}

transition.follows = function(id, what) {
  stopifnot(what %in% c("followings", "followers"))
  follows.path.old = get.path.old(id, what)
  follows.path = get.path(id, what)
  if (!file.exists(follows.path.old)) return(NULL) # Download later
  if (file.exists(follows.path)) {
    # Already done
    follows = fromJSON(follows.path)
    return(switch(what,
                  followings = follows$to,
                  followers = follows$from))
  }
  follows = fromJSON(follows.path.old)
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

write.account.or.not = function(new.account) {
  id = new.account$id
  old.path = get.account.path.old(id)
  new.path = get.account.path(id)
  if (file.exists(new.path)) {
    # pass
  } else if (!file.exists(old.path)) {
    write_json(new.account, new.path)
  } else {
    old.account = fromJSON(old.path)
    write_json(decide.which.account(old.account, new.account), new.path)
  }
}

# Proceed ----

sapply(c("accounts", "followings", "followers"),
       \(x) if(!dir.exists(x)) dir.create(x))

transition.account(my.id)
followings.id = transition.follows(my.id, "followings")
followers.id = transition.follows(my.id, "followers")
follows = unique(c(followings.id, followers.id))
for (id in follows) {
  transition.account(id)
  followings.id = transition.follows(id, "followings")
  followers.id = transition.follows(id, "followers")
}