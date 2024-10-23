Just a playful attempt at analyzing my ego network on the Fediverse.

- *`accounts/`* --- JSON files with account data, downloaded from the instances' Mastodon API
- *`followers/`* --- JSON files with a (possibly long) list of "from" and a single "to" which is the same as the file name.
- *`followings/`* --- The reverse of `followers/`
- *`transition.R`* --- (Pun intended) a script to move files from a previous attempt and into this new one
- *`download.R`* --- A script to (hopefully) download the remaining (many hundred thousands of) account files