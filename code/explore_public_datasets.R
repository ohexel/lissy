library(readstata13)

notnull <- function(x) {
    n <- length(x)
    n0 <- sum(x == 0)
    zeropct <- n0/n
    return(zeropct)
}

wave10 <- c("at16", "be16", "br16", "ca15", "ca16", "ca17", "cl15", "cl17",
            "co16", "cz16", "dk16", "fi16", "ge16", "de15", "de16", "gr16",
            "hu15", "ie15", "ie16", "ie17", "il16", "it16", "ci15", "lt15",
            "lt16", "lt17", "mx16", "ps17", "pa16", "py16", "pe16", "pl16",
            "ru15", "ru16", "ru17", "rs16", "si15", "za15", "za17", "es16",
            "ch15", "ch16", "ch17", "tw16", "uk16", "us15", "us16", "uy16")

# No HI52: CN13, CZ13, NO13
wave09 <- c("au14", "at13", "be13", "br13", "ca12", "ca13", "ca14", "cl13",
            "co13", "dk13", "eg12", "ee13", "fi13", "ge13", "de12", "de13",
            "de14", "gr13", "gt14", "hu12", "ie12", "ie13", "ie14", "il12",
            "il14", "it14", "jp13", "lt12", "lt13", "lt14", "lu13", "mx12",
            "mx14", "nl13", "pa13", "py13", "pe13", "pl13", "ru13", "ru14",
            "rs13", "sk13", "si12", "za12", "kr12", "es13", "ch12", "ch13",
            "ch14", "tw13", "uk13", "us12", "us13", "us14", "uy13", "vn13")
# No HI52: CZ10, NO10
wave08 <- c("au10", "at10", "be10", "br09", "br11", "ca10", "cl09", "cl11",
            "co10", "dk10", "ee10", "fi10", "fr10", "ge10", "de09", "de10",
            "de11", "gr10", "gt11", "hu09", "is10", "in11", "ie09", "ie10",
            "ie11", "il10", "it10", "jp10", "lt09", "lt10", "lt11", "lu10",
            "mx10", "nl10", "pa10", "py10", "pe10", "pl10", "ru10", "ru11",
            "rs10", "sk10", "si10", "za10", "kr10", "es10", "ch09", "ch10",
            "ch11", "tw10", "uk10", "us09", "us10", "us11", "uy10", "vn11")
# No HI52: CZ07, NO07
wave07 <- c("au08", "at07", "be07", "br06", "ca07", "cl06", "co07", "dk07",
            "do07", "ee07", "fi07", "de06", "de07", "de08", "gr07", "gt06",
            "hu07", "is07", "ie06", "ie07", "ie08", "il07", "it08", "ci08",
            "jp08", "lu07", "mx08", "nl07", "pa07", "py07", "pe07", "pl07",
            "ru07", "rs06", "sk07", "si07", "za08", "kr08", "es07", "ch06",
            "ch07", "ch08", "tw07", "uk07", "us06", "us07", "us08", "uy07")
# No HI52: CZ04, LU04
wave06 <- c("au03", "au04", "at04", "be04", "ca04", "cl03", "co04", "dk04",
            "ee04", "fi04", "fr05", "de03", "de04", "de05", "gr04", "hu05",
            "is04", "in04", "ie03", "ie04", "ie05", "il05", "it04", "mx04",
            "nl04", "no04", "py04", "pe04", "pl04", "ru04", "sk04", "si04",
            "es04", "se05", "ch04", "tw05", "uk04", "us03", "us04", "us05",
            "uy04")
# No HI52: CZ02
wave05 <- c("au01", "at00", "be00", "ca98", "ca00", "cl98", "cl00", "cn02",
            "dk00", "ee00", "fi00", "fr00", "de98", "de00", "de01", "de02",
            "gr00", "hu99", "ie00", "ie02", "il01", "it98", "it00", "ci02",
            "lu00", "mx98", "mx00", "mx02", "nl99", "no00", "py00", "pl99",
            "ru00", "si99", "es00", "se00", "ch00", "ch02", "tw00", "uk99",
            "us98", "us99", "us00", "us01", "us02")
# No HI52: AT95, CA94, CA97, CZ96, IL97, RO95, RO97
wave04 <- c("au95", "at94", "at97", "be95", "be97", "cl94", "cl96", "dk95",
            "fi95", "fr94", "de94", "de95", "gr95", "hu94", "ie94", "ie95",
            "ie96", "it93", "it95", "lu94", "lu97", "mx94", "mx96", "nl93",
            "no95", "pl95", "sk96", "si97", "es95", "se95", "tw95", "tw97",
            "uk94", "uk95", "us93", "us94", "us95", "us96", "us97")
# No HI52: CA91, CZ92, SK92, ES90
wave03 <- c("au89", "be88", "be92", "cl90", "cl92", "dk92", "fi91", "fr89",
            "de89", "de91", "hu91", "il92", "it89", "it91", "lu91", "mx89",
            "mx92", "nl90", "no91", "pl92", "se92", "ch92", "tw91", "uk91",
            "us91", "us92")
# No HI52: AT87, CA87, DE83, IT86, PL86, ES85
wave02 <- c("au85", "be85", "dk87", "fi87", "fr84", "de84", "de87", "ie87",
            "il86", "it87", "lu85", "mx84", "nl83", "nl87", "no86", "se87",
            "tw86", "uk86", "us86")
# No HI52: CA81, DE78, ES80, SE81
wave01 <- c("au81", "fr78", "de81", "il79", "no79", "ch82", "tw81", "uk79",
            "us79")
# No HI52: CA71, CA75, DE73, SE67
wave00 <- c("se75", "uk69", "uk74", "us74")


vars <- c("hi52", "hi521", "hi522", "hi53", "hi531", "hi532")

waves <- list(wave09, wave08, wave07, wave06, wave05, wave04, wave03, wave02,
              wave01, wave00)
for(w in waves){
    print("///////////////////////////////////////////////////////////////////")
    print("NEW WAVE")
    print("///////////////////////////////////////////////////////////////////")
    res <- data.frame(country = character(),
                      year = character(),
                      zero = numeric())
    for(c in w){
        print("---------------------------------------------------------------")
        print(paste("Country:", c))
        print("---------------------------------------------------------------")
        x <- read.LIS(paste0(c, "h"), vars = vars)
        year <- paste0(ifelse(substr(c, 3, 4) < 20, "20", "19"),
                       substr(c, 3, 4))
        country <- substr(c, 1, 2)
        tmp <- sapply(x, notnull)
        tmp <- data.frame(country = country,
                          year = year,
                          zero.hi52 = tmp[1][[1]],
                          zero.hi521 = tmp[2][[1]],
                          zero.hi522 = tmp[3][[1]],
                          zero.hi53 = tmp[4][[1]],
                          zero.hi531 = tmp[5][[1]],
                          zero.hi532 = tmp[6][[1]])
        res <- rbind(res, tmp)
    }

    for(row in 1:nrow(res)){
        country <- res[row, "country"]
        year <- res[row, "year"]
        zero52 <- 1 - res[row, "zero.hi52"]
        zero521 <- 1 - res[row, "zero.hi521"]
        zero522 <- 1 - res[row, "zero.hi522"]
        diff52 <- zero52 - zero521 - zero522
        print(paste(country, year, zero52, diff52, sep = ","))
    }
}
