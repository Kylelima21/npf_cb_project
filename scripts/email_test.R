
library(blastula)

my_email_object <- render_email('outputs/emailtest.Rmd')

print(my_email_object)

# create_smtp_creds_key(
#   id = "kmail",
#   user = "klima@schoodicinstitute.org",
#   provider = "gmail",
#   #overwrite = TRUE
# )

smtp_send(my_email_object,
          from = "klima@schoodicinstitute.org",
          to = "klima@schoodicinstitute.org",
          subject = "test email",
          credentials = creds_key("kmail")
)




