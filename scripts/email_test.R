
library(blastula)

my_email_object <- render_email('outputs/emailtest.Rmd')

print(my_email_object)

# create_smtp_creds_key(
#   id = "gmail",
#   user = "klima@schoodicinstitute.org",
#   host = "smtp.gmail.com",
#   port = 465,
#   use_ssl = TRUE,
#   overwrite = TRUE
# )

smtp_send(my_email_object,
          from = "klima@schoodicinstitute.org",
          to = "klima@schoodicinstitute.org",
          subject = "Your email subject",
          credentials = creds_key("gmail")
)




