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
rhub::rhub_check() # 1,5

# step 6
# check windows builder
devtools::build()
# upload the tar file on this website https://win-builder.r-project.org/upload.aspx
# wait for the email

# step 7
devtools::release()
