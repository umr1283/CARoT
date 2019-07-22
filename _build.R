#!/usr/local/bin/Rscript

# docker exec R360 Rscript -e 'remotes::install_github("mcanouil/CARoT", upgrade = "never")'

# git2r::config(user.name = "Mickaël Canouil", user.email = "mickael.canouil@cnrs.fr")
# use_git_credentials(credentials = git2r::cred_token())
check()
git2r::checkout(branch = "master")
commit <- paste('Built site for CARoT:', pkgdown:::read_desc()$get_version())
pkgdown::build_site(override = list(destination = tempdir()), preview = FALSE)
git2r::checkout(branch = "gh-pages")
file.copy(from = list.files(paste0(tempdir(), "/dev"), full.names = TRUE), to = ".", recursive = TRUE, overwrite = TRUE)
git2r::add(path = "*")
git2r::commit(message = commit, all = TRUE)
git2r::checkout(branch = "master")
git2r::push(name = "origin", refspec = "refs/heads/master", credentials = git2r::cred_token())
git2r::push(name = "origin", refspec = "refs/heads/gh-pages", credentials = git2r::cred_token())
