# Set options here
options(golem.app.prod = FALSE, shiny.port = 8080,
        # auth0_config_file = system.file("app/_auth0.yml", package = "golem.auth0"),
        # whenever there is one account token found, use the cached token
        gargle_oauth_email = TRUE,
        # specify auth tokens  should be stored in a hidden directory ".secrets"
        gargle_oauth_cache = "./app_data/.secrets") # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
rm(list = ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# Run the application
run_app()
