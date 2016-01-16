#
# Przykladowe dane pod pivota
#
# Autor: Karol Kubicki

wymiary <- list(urzadzenie = c("komputor", "smartfon", "dumbfon"),
               klaster = 0:5,
               test = c("baza ortogonalna",
                  "megatron30000",
                  "fan",
                  "DataScienceRocks",
                  "CienkiBolek"),
               kategoria = c("SupaHot", "LatestNews", "ZawszeSmieszne"),
               system = c("Windows", "Linux", "Inny"),
               przegladarka = c("Firefox", "Chrome", "Inna"),
               data = c("2016-01-01","2016-01-02","2016-01-03","2016-01-04","2016-01-05","2016-01-06","2016-01-07")
            )

data <- expand.grid(wymiary, stringsAsFactors = FALSE)

data$kliki <-sample(500:2000, dim(data)[1], replace = TRUE)
data$odslony <- data$kliki + sample(500:3000, dim(data)[1], replace = TRUE, prob=500:3000)

data <- as.data.frame(data)

data$kliki[data$test == "CienkiBolek"] <- data$kliki[data$test == "CienkiBolek"]/1.2
data$kliki[data$test == "DataScienceRocks"] <- 1.05*data$kliki[data$test == "DataScienceRocks"]
ll <- sum(data$test == "baza ortogonalna")
data$kliki[data$test == "baza ortogonalna"] <- (1+rnorm(ll, 0, 0.08))*data$kliki[data$test == "baza ortogonalna"]
ll <- sum(data$test == "megatron30000")
data$kliki[data$test == "megatron30000"] <- (0.98+rexp(ll, 20))*data$kliki[data$test == "megatron30000"]
ll <- sum(data$urzadzenie == "komputor")
data$odslony[data$urzadzenie == "komputor"] <- (1.5+rnorm(ll, 0, 0.07))*data$odslony[data$urzadzenie == "komputor"]
data$kliki[data$urzadzenie == "komputor"] <- (1.5+rnorm(ll, 0, 0.07))*data$kliki[data$urzadzenie == "komputor"]

data$kliki[data$przegladarka == "Inna"] <- data$kliki[data$przegladarka == "Inna"]/1.4
data$kliki[data$system == "Linux"] <- data$kliki[data$system == "Linux"]/1.2

write.csv(data, file = "wyniki_algorytmow.csv", row.names = FALSE)
