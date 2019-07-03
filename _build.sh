docker exec R360 Rscript -e 'remotes::install_github("mcanouil/CARoT", upgrade = "never")'


devtools::check()
system("git checkout master")
pkgdown::build_site(override = list(destination = tempdir()), preview = FALSE)
system("git checkout gh-pages")
file.copy(from = list.files(paste0(tempdir(), "/dev"), full.names = TRUE), to = ".", recursive = TRUE, overwrite = TRUE)
system('git add --all; git -c "user.name=Mickaël Canouil" -c "user.email=mickael.canouil@cnrs.fr" commit -m "Built site for CARoT: 0.3.0.9000"')
unlink(paste(tempdir(), "/dev"), recursive = TRUE)
system("git checkout master")

