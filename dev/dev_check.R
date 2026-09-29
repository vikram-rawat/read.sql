# step 1
devtools::document()

# step 2
devtools::check(
  env_vars = c("_R_CHECK_SYSTEM_CLOCK_" = "0")
)

# step 3
devtools::spell_check()
# spelling::update_wordlist()

# step 4
rhub::rhub_setup(overwrite = TRUE)
rhub::rhub_doctor()
rhub::rhub_check()

# step 4
# check windows builder
devtools::build()
# upload the tar file on this website https://win-builder.r-project.org/upload.aspx
# wait for the email

# step 5
devtools::release()
