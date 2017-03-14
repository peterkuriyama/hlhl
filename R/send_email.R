#' Send email that run is done
#' Function to send an email, arguments won't match
#' @param ctl Control file
#' @param nbest Number of best/good sites to samle
#' @param nmed Number of medium sites to samle
#' @param nbest Number of bad sites to samle
#' @export

send_email <- function(from = "<pkuriyama@gmail.com>", to = "<pkuriyama@gmail.com>",
  subject = "run done", body = 'run done', mailcontrol = list(smtpServer="ASPMX.L.GOOGLE.COM")){
 
  sendmail(from=from, to=to, subject=subject, msg=body, control=mailcontrol)
}