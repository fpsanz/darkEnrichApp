library(shinymanager)
## para crear una tabla inicial de usuarios
credentials <- data.frame(
  user = c("fpsanz"),
  password = c("fps379725"),
  # password will automatically be hashed
  admin = c(TRUE),
  start = NA,
  expire = NA,
  applications = c("darkEnrichApp;enrichapp_listable"),
  stringsAsFactors = FALSE
)

# library(keyring)
# key_set_with_value("R-fpsanz-key", "obiwankenobi", "1235")
# para crear la base de datos de usuarios
create_db(
  credentials_data = credentials,
  sqlite_path = "users.sqlite",
  passphrase = "fps379725"
)
