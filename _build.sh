#!/bin/sh

# docker exec R360 Rscript -e 'remotes::install_github("mcanouil/CARoT", upgrade = "never")'

Rscript -e 'devtools::check()'

git checkout master

Rscript -e 'pkgdown::build_site(override = list(destination = tempdir()), preview = FALSE)')

git checkout gh-pages

Rscript -e 'file.copy(from = list.files(paste0(tempdir(), "/dev"), full.names = TRUE), to = ".", recursive = TRUE, overwrite = TRUE)')

git add --all

Rscript -e 'Sys.setenv(pkg_commit = paste("Built site for CARoT:", pkgdown:::read_desc()$get_version()))'
git -c "user.name=Mickaël Canouil" -c "user.email=mickael.canouil@cnrs.fr" commit -m $pkg_commit

Rscript -e 'unlink(paste(tempdir(), "/dev"), recursive = TRUE)')

git checkout master


