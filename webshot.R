devtools::install_github("wch/webshot")
library(webshot)

URL <- "https://www.twitch.tv/misterrogers"
URL2 <- "https://player.twitch.tv/?channel=misterrogers"

webshot(URL, cliprect = "viewport", delay = 10)
webshot(URL2, cliprect = "viewport", delay = 10)
