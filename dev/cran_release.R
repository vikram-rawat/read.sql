# step 1
devtools::test()

# step 2
devtools::document()

# step 3
devtools::check(
  env_vars = c("_R_CHECK_SYSTEM_CLOCK_" = "0")
)

# step 4
devtools::spell_check()
# spelling::update_wordlist()

# step 5
# rhub::rhub_setup(overwrite = TRUE)
rhub::rhub_doctor()
rhub::rhub_check(platforms = c("linux", "windows")) # 1,5

# step 6
# check windows builder
devtools::check_win_devel()
devtools::build()
# upload the tar file on this website https://win-builder.r-project.org/upload.aspx
# wait for the email

# step 7
usethis::use_release_issue()
devtools::release()

# step 8
# you will recieve an email for confirmation and
# then it will be submitted to cran.
# all other conversations will be on registered emails.
