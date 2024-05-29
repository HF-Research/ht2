# DO NOT RUN THIS SCRIPT DIRECTLY, SOURCE THE CALL_DEPLOY_LOCAL SCRIPT

# Run this when deploying a development version to test. Comment out when
# deploying to production
rsconnect::deployApp(appName = 'ht-dev', forceUpdate = TRUE)

# Only run this when you are sure everything works, and have deployed a test
# version first
# rsconnect::deployApp(appName = 'HjerteTal', forceUpdate = TRUE)
