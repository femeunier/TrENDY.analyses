rm(list = ls())

library(tidyverse)
library(readxl)
library(rtf)
library(dplyr)
library(mailR)

PIteams <- read.csv("./data/Contact_Trendy.csv")
PIs <- PIteams$Contact.Name

##### Definitely see this: https://datawookie.github.io/emayili/
##### and if using gmail then also accept to use less secure apps here #####https://myaccount.google.com/lesssecureapps

for (iPI in seq(6,6)){#},length(PIs))){

  cPI <- PIs[iPI]
  cmodel <- PIteams$Model.Name[iPI]
  cemail <- gsub(" ", "",PIteams$Email[iPI], fixed = TRUE)

  if (cemail == "") next()

  print(paste(iPI,cemail))



    send.mail(from = "felicien.meunier@gmail.com",
              to = cemail,
              subject = paste("Question about TrENDY model",cmodel),
              body=paste0(
  "Dear ",cPI,',',
  "

  I am working as postdoc at Ghent University under the supervision of prof. Hans Verbeeck.
  For a project that I lead with Marc Peaucelle, we are interested in using the outputs of TrENDY-v11
  to evaluate the performance of vegetation models to reproduce carbon sink and its components in tropical
  forests. We are comparing the model outputs with a series of field inventories across the tropics.
  I am contacting you because we are referred to as the contact person for the ",cmodel, " model. I would
  like to know if you would be interested in collaborating with us in such a effort. In particular,
  I would like you to explain me how your specific model allocates the total NPP into its components (is the ratio
  fixed, dynamic? What is the mean value for tropical forest PFTs?). Is there a model description paper that I
  could use as a reference for your model. Or even better if you'd be willing to share the model outputs for
  above-ground NPP instead of total NPP, it would be very useful.

  Please let me know if you are interested, and if you wish I can of course give you more detail about this on-going
  work.

  Many thanks and best regards,

  Dr. Félicien Meunier"),
              html=FALSE,
              smtp=list(host.name = "smtp.gmail.com",
                        port = 465,
                        user.name = "felicien.meunier@gmail.com",
                        passwd = "xdixqjogjbuhnwke",
                        ssl = TRUE),
              authenticate=TRUE,
              send = TRUE)


}

