#!/usr/local/bin/Rscript

# docker exec R360 Rscript -e 'remotes::install_github("mcanouil/CARoT", upgrade = "never")'

devtools::check()
system("git checkout master")
commit <- paste('Built site for CARoT:', pkgdown:::read_desc()$get_version())
pkgdown::build_site(override = list(destination = tempdir()), preview = FALSE)
system("git checkout gh-pages")
file.copy(from = list.files(paste0(tempdir(), "/dev"), full.names = TRUE), to = ".", recursive = TRUE, overwrite = TRUE)
system("git add --all")
system(paste0('git -c "user.name=Mickaël Canouil" -c "user.email=mickael.canouil@cnrs.fr" commit -m "', commit, '"'))
unlink(paste(tempdir(), "/dev"), recursive = TRUE)
system("git checkout master")
