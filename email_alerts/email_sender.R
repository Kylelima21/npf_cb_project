## Script to produce and send the email alerts to those signed up
## Schoodic Institute at Acadia National Park


#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#
library(blastula)



#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#

## Can run to see example of what the email will look like
# my_email_object <- render_email('outputs/emailtest.Rmd')
# print(my_email_object)


## Create the credentials key needed to send from my email
# create_smtp_creds_key(
#   id = "kmail",
#   user = "klima@schoodicinstitute.org",
#   provider = "gmail",
#   #overwrite = TRUE
# )


## Produce and send the email
smtp_send(render_email('email_alerts/email_material.Rmd'),
          from = "klima@schoodicinstitute.org",
          to = "klima@schoodicinstitute.org",
          subject = "Your weekly citizen science report for species of interest",
          credentials = creds_key("kmail")
)




