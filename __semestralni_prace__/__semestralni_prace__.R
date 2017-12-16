###############################################################################
###############################################################################
###############################################################################

## instaluji a loaduji balíčky ------------------------------------------------

invisible(
    lapply(
        c(
            "xtable",
            "openxlsx",
            "tseries",
            "seasonal",
            "forecast",
            "lmtest",
            "car"
        ),
        function(my_package){
            
            if(!(my_package %in% rownames(installed.packages()))){
                
                install.packages(
                    my_package,
                    dependencies = TRUE,
                    repos = "http://cran.us.r-project.org"
                )
                
            }
            
            library(my_package, character.only = TRUE)
            
        }
    )
)


## ----------------------------------------------------------------------------

###############################################################################

## nastavuji handling se zipováním v R ----------------------------------------

Sys.setenv(R_ZIPCMD = "C:/Rtools/bin/zip") 


## ----------------------------------------------------------------------------

###############################################################################

## nastavuji pracovní složku --------------------------------------------------

while(!"__semestralni_prace__.R" %in% dir()){
    setwd(choose.dir())
}

mother_working_directory <- getwd()


## ----------------------------------------------------------------------------

###############################################################################

## vytvářím posložky pracovní složky ------------------------------------------

setwd(mother_working_directory)

for(my_subdirectory in c("vstupy", "vystupy")){
    
    if(!file.exists(my_subdirectory)){
        
        dir.create(file.path(
            
            mother_working_directory, my_subdirectory
            
        ))
        
    }
    
}


## ----------------------------------------------------------------------------

###############################################################################

## loaduji data ---------------------------------------------------------------

setwd(
    paste(mother_working_directory, "vstupy", sep = "/")
)

time_series_1 <- read.table(
    
    file = "karcinom_tlusteho_streva.txt",
    header = TRUE,
    sep = ";",
    dec = ","
    
)

time_series_2 <- readLines(
    
    con = "ewcitmeas.dat",
    encoding = "UTF-8"
    
)


setwd(mother_working_directory)


## ----------------------------------------------------------------------------

###############################################################################

## (pre)processing dat --------------------------------------------------------

#### upravuji druhou časovou řadu ---------------------------------------------

###### umazávám hashtag v headeru ---------------------------------------------

time_series_2 <- gsub("#", "", time_series_2)


###### nahrazuji hvězdičky chybějící hodnotou (NA) ----------------------------

#time_series_2 <- gsub("\\*", NA, time_series_2)


###### odmazávám iniciální mezeru, je-li přítomna -----------------------------

time_series_2 <- gsub("^\\s+", "", time_series_2)


###### všechny vícenásobné mezery měním na jednoduché a nahrazuji je
###### středníkem -------------------------------------------------------------

for(i in 1:length(time_series_2)){
    
    while(grepl("  ", time_series_2[i])){
        
        time_series_2[i] <- gsub("  ", " ", time_series_2[i])
        
    }
    
}

time_series_2 <- gsub(" ", ";", time_series_2)


###### vytvářím data.frame z "time_series_2" ----------------------------------

temp_time_series_2 <- NULL

for(i in 2:length(time_series_2)){
    
    temp_time_series_2 <- rbind(
        
        temp_time_series_2,
        strsplit(time_series_2[i], split = ";")[[1]]
        
    )
    
}


colnames(temp_time_series_2) <- strsplit(
    time_series_2[1], split = ";"
)[[1]]


temp_time_series_2 <- data.frame(
    
    temp_time_series_2,
    stringsAsFactors = FALSE
    
)


######## převádím dny, měsíce a roky na integery ------------------------------

for(i in 1:3){
    
    temp_time_series_2[, i] <- suppressWarnings(
        as.integer(
            temp_time_series_2[, i]
        )
    )
    
}


######## převádím ostatní hodnoty na reálná čísla -----------------------------

for(i in 4:dim(temp_time_series_2)[2]){
    
    temp_time_series_2[, i] <- suppressWarnings(
        as.numeric(
            temp_time_series_2[, i]
        )
    )
    
}


time_series_2 <- temp_time_series_2


###### agreguji data v "time_series_2" na měsíční řady ------------------------

temp_time_series_2 <- NULL

for(my_year in unique(time_series_2$YY)){
    
    my_data <- time_series_2[
        time_series_2[, "YY"] == my_year
        ,
    ]
    
    for(my_quarter in c(1, 2, 3, 4)){
        
        if(
            dim(
                my_data[
                    my_data[, "MM"] %in% c(
                        (3 * my_quarter - 2):(3 * my_quarter)
                    )
                    ,
                ]
            )[1] > 0
        ){
            
            temp_time_series_2 <- rbind(
                
                temp_time_series_2,
                c(
                    
                    "year" = paste("19", my_year, sep = ""),
                    "quarter" = my_quarter,
                    apply(
                        my_data[
                            my_data[, "MM"] %in% c(
                                (3 * my_quarter - 2):(3 * my_quarter)
                            ),
                            setdiff(colnames(my_data), c("DD", "MM", "YY"))
                        ],
                        2,
                        sum,
                        na.rm = TRUE
                    )
                    
                )
                
            )
            
        }
        
    }
    
}


time_series_2 <- data.frame(
    
    temp_time_series_2,
    stringsAsFactors = FALSE
    
)


for(i in 3:dim(time_series_2)[2]){
    
    time_series_2[, i] <- as.numeric(
        time_series_2[, i]
    )
    
}


## ----------------------------------------------------------------------------

###############################################################################

## převádím obě řady na objekt typu ts() --------------------------------------

ts_1 <- ts(
    
    time_series_1[, "incidence"],
    start = min(time_series_1[, "rok"]),
    end = max(time_series_1[, "rok"]),
    frequency = 1
    
)

ts_2 <- ts(
    
    time_series_2[, "London"],
    start = c(min(as.integer(time_series_2[, "year"])), 1),
    end = c(max(as.integer(time_series_2[, "year"])), 4),
    frequency = 4
    
)


## ----------------------------------------------------------------------------

###############################################################################

## vykresluji diagramy pro obě časové řady ------------------------------------

setwd(paste(mother_working_directory, "vystupy", sep = "/"))

for(i in c(1, 2)){
    
    cairo_ps(
        file = paste(
            "diagram_casova_rada_",
            i,
            ".eps",
            sep = ""
        ),
        width = 8,
        height = 4,
        pointsize = 12
    )
    
    par(mar = c(4.1, 4.1, 0.5, 0.1))
    
    plot(
        x = get(paste("ts_", i, sep = "")),
        xlab = "rok",
        ylab = if(
            i == 1
        ){
            "# nových případů / 100 000 osob"
        }else{
            "# případů celkově"
        },
        col = "blue"
    )
    
    dev.off()
    
}

setwd(mother_working_directory)


## ----------------------------------------------------------------------------

###############################################################################

## korelogramy ----------------------------------------------------------------

setwd(paste(mother_working_directory, "vystupy", sep = "/"))

for(my_correlogram_type in c("acf", "pacf")){
    
    for(i in c(1, 2)){
        
        cairo_ps(
            file = paste(
                "my_",
                my_correlogram_type,
                "_diagram_",
                i,
                ".eps",
                sep = ""
            ),
            width = 4,
            height = 3,
            pointsize = 12
        )
        
        par(mar = c(4.1, 4.1, 0.1, 0.1))
        
        do.call(
            what = my_correlogram_type,
            args = list(
                x = get(paste("ts_", i, sep = "")),
                xlab = "zpoždění (lags)",
                ylab = if(
                    my_correlogram_type == "acf"
                ){
                    "autokorelace"
                }else{
                    "parciální autokorelace"
                },
                main = "",
                ylim = c(-0.5, 1.0),
                lag.max = if(i == 1){20}else{80}
            )
        )
        
        dev.off()
        
    }
    
}

setwd(mother_working_directory)


## ----------------------------------------------------------------------------

###############################################################################

## řešení pro jednotlivé řady: ------------------------------------------------

## nesezónní řada -------------------------------------------------------------

#### Augmentovaný Dickey-Fullerův test ----------------------------------------

adf.test(ts_1)
adf.test(log(ts_1))     # nezamítáme H_0, takže nestacionarita
adf.test(diff(log(ts_1), diff = 1))
                        # zamítáme H_0, stacionarita

# Augmented Dickey-Fuller Test

# data:  diff(log(ts_1), diff = 1)
# Dickey-Fuller = -3.8287, Lag order = 3, p-value = 0.02878
# alternative hypothesis: stationary


##### ACF a PACF po diferenciaci a logaritmování řady -------------------------

acf(diff(log(ts_1), diff = 1), lag.max = 20)
pacf(diff(log(ts_1), diff = 1), lag.max = 20)


setwd(paste(mother_working_directory, "vystupy", sep = "/"))

for(my_correlogram_type in c("acf", "pacf")){
    
    cairo_ps(
        file = paste(
            "my_",
            my_correlogram_type,
            "_diagram_after_dif_1",
            ".eps",
            sep = ""
        ),
        width = 4,
        height = 3,
        pointsize = 12
    )
    
    par(mar = c(4.1, 4.1, 0.1, 0.1))
    
    do.call(
        what = my_correlogram_type,
        args = list(
            x = diff(log(ts_1), diff = 1),
            xlab = "zpoždění (lags)",
            ylab = if(
                my_correlogram_type == "acf"
            ){
                "autokorelace"
            }else{
                "parciální autokorelace"
            },
            main = "",
            ylim = c(-0.5, 1.0),
            lag.max = 20
        )
    )
    
    dev.off()

}

setwd(mother_working_directory)


#### vytvářím tabulku s možnými modely ----------------------------------------

d <- 1

my_table <- NULL

for(p in 0:4){
    
    for(q in 0:4){
        
        my_arima <- arima(
            log(ts_1),
            order = c(p, d, q)
        )
        
        my_table <- rbind(
            
            my_table,
            c(
                "p" = p,
                "d" = d,
                "q" = q,
                "AIC" = my_arima$aic,
                "ljung_box_p" = Box.test(
                    my_arima$residuals,
                    lag = 20,
                    type = "Ljung-Box",
                    fitdf = sum(c(p, d, q))
                )$p.value,
                "breusch_godfrey_p" = bgtest(
                    lm(residuals(my_arima) ~ 1)
                )$p.value,
                "shapiro_p" = shapiro.test(
                    my_arima$residuals
                )$p.value,                
                "breusch_pagan_p" = unname(
                    bptest(
                        residuals ~ year,
                        data = data.frame(
                            "residuals" = c(residuals(my_arima)),
                            "year" = c(
                                1:length(residuals(my_arima))
                            )
                        )
                    )$p.value
                )
            )
            
        )
        
    }
    
}


print(
    xtable(
        my_table,
        align = rep("", ncol(my_table) + 1),
        digits = c(0, 0, 0, 0, 3, 3, 3, 3, 3)
    ),
    floating = FALSE,
    tabular.environment = "tabular",
    hline.after = NULL,
    include.rownames = TRUE,
    include.colnames = TRUE,
    format.args = list(decimal.mark = ",")
)


#### tisknu ještě výstupy diagnostických testů pro nejvhodnější model ---------

p <- 2
d <- 1
q <- 2


my_arima <- arima(
    log(ts_1),
    order = c(p, d, q)
)


###### Ljung-Boxův test -------------------------------------------------------

Box.test(
    arima(
        log(ts_1),
        order = c(p, d, q)
    )$residuals,
    lag = 20,
    type = "Ljung-Box",
    fitdf = sum(c(p, d, q))
)


###### Breusch-Godfreyho test -------------------------------------------------

bgtest(
    lm(residuals(my_arima) ~ 1)
)


###### Shapiro-Wilkův test ----------------------------------------------------

shapiro.test(
    arima(
        log(ts_1),
        order = c(p, d, q)
    )$residuals
)


###### Breusch-Paganův test ---------------------------------------------------

bptest(
    residuals ~ guarter,
    data = data.frame(
        "residuals" = c(0, residuals(my_arima)),
        "guarter" = rep(1:4, 10)
    )
)


#### ukládám histogram reziduí ------------------------------------------------

setwd(paste(mother_working_directory, "vystupy", sep = "/"))

cairo_ps(
    file = paste(
        "my_histogram_1",
        ".eps",
        sep = ""
    ),
    width = 6,
    height = 4,
    pointsize = 12
)

par(mar = c(4.1, 4.1, 0.1, 0.1))

hist(
    x = residuals(my_arima),
    col = "lightgrey",
    xlim = c(-0.15, 0.15),
    xlab = "rezidua výsledného modelu",
    ylab = "absolutní četnost",
    main = "",
    ylim = c(0, 12)
)

lines(density(residuals(my_arima)), col = "red")

dev.off()

setwd(mother_working_directory)


#### ukládám ACF a PACF pro výsledný model ------------------------------------

my_arima <- arima(
    log(ts_1),
    order = c(p, d, q)
)


setwd(paste(mother_working_directory, "vystupy", sep = "/"))

for(my_correlogram_type in c("acf", "pacf")){
    
    cairo_ps(
        file = paste(
            "my_",
            my_correlogram_type,
            "_diagram_final_1",
            ".eps",
            sep = ""
        ),
        width = 4,
        height = 3,
        pointsize = 12
    )
    
    par(mar = c(4.1, 4.1, 0.1, 0.1))
    
    do.call(
        what = my_correlogram_type,
        args = list(
            x = my_arima$residuals,
            xlab = "zpoždění (lags)",
            ylab = if(
                my_correlogram_type == "acf"
            ){
                "autokorelace"
            }else{
                "parciální autokorelace"
            },
            main = "",
            ylim = c(-0.5, 1.0),
            lag.max = 20
        )
    )
    
    dev.off()

}

setwd(mother_working_directory)


#### tisknu výstup modelu -----------------------------------------------------

my_table <- unclass(coeftest(my_arima))

print(
    xtable(
        my_table,
        align = rep("", ncol(my_table) + 1),
        digits = c(0, 3, 3, 3, 3)
    ),
    floating = FALSE,
    tabular.environment = "tabular",
    hline.after = NULL,
    include.rownames = TRUE,
    include.colnames = TRUE,
    format.args = list(decimal.mark = ",")
)


## ----------------------------------------------------------------------------

###############################################################################

## řešení pro jednotlivé řady: ------------------------------------------------

## sezónní řada ---------------------------------------------------------------

#### Augmentovaný Dickey-Fullerův test ----------------------------------------

adf.test(log(ts_2))     # zamítáme H_0, stacionarita

# Augmented Dickey-Fuller Test

# data:  log(ts_2)
# Dickey-Fuller = -3.8143, Lag order = 5, p-value = 0.0201
# alternative hypothesis: stationary


##### ACF a PACF po logaritmování řady ----------------------------------------

acf(diff(log(ts_2), lag = 4, differences = 1), lag.max = 80)
pacf(log(ts_2), lag.max = 80)


setwd(paste(mother_working_directory, "vystupy", sep = "/"))

for(my_correlogram_type in c("acf", "pacf")){
    
    cairo_ps(
        file = paste(
            "my_",
            my_correlogram_type,
            "_diagram_after_dif_2",
            ".eps",
            sep = ""
        ),
        width = 4,
        height = 3,
        pointsize = 12
    )
    
    par(mar = c(4.1, 4.1, 0.1, 0.1))
    
    do.call(
        what = my_correlogram_type,
        args = list(
            x = log(ts_2),
            xlab = "zpoždění (lags)",
            ylab = if(
                my_correlogram_type == "acf"
            ){
                "autokorelace"
            }else{
                "parciální autokorelace"
            },
            main = "",
            ylim = c(-0.5, 1.0),
            lag.max = 80
        )
    )
    
    dev.off()

}

setwd(mother_working_directory)


#### hledám nejlepší model ----------------------------------------------------

my_sarima <- arima(
    log(ts_2),
    order = c(3, 1, 3),
    seasonal = list(order = c(1, 0, 0), period = NA)
)

coeftest(my_sarima)

acf(my_sarima$resid)
pacf(my_sarima$resid)


print(
    xtable(
        my_table,
        align = rep("", ncol(my_table) + 1),
        digits = c(0, 0, 0, 0, 3, 3, 3, 3, 3)
    ),
    floating = FALSE,
    tabular.environment = "tabular",
    hline.after = NULL,
    include.rownames = TRUE,
    include.colnames = TRUE,
    format.args = list(decimal.mark = ",")
)


#### tisknu ještě výstupy diagnostických testů pro nejvhodnější model ---------

###### Ljung-Boxův test -------------------------------------------------------

Box.test(
    my_sarima$residuals,
    lag = 20,
    type = "Ljung-Box",
    fitdf = sum(c(p, d, q))
)


###### Breusch-Godfreyho test -------------------------------------------------

bgtest(
    lm(residuals(my_sarima) ~ 1)
)


###### Shapiro-Wilkův test ----------------------------------------------------

shapiro.test(
    my_sarima$residuals
)


###### Breusch-Paganův test ---------------------------------------------------

bptest(
    residuals ~ guarter,
    data = data.frame(
        "residuals" = c(residuals(my_sarima)),
        "guarter" = rep(1:4, 40)
    )
)


#### ukládám histogram reziduí ------------------------------------------------

setwd(paste(mother_working_directory, "vystupy", sep = "/"))

cairo_ps(
    file = paste(
        "my_histogram_2",
        ".eps",
        sep = ""
    ),
    width = 6,
    height = 4,
    pointsize = 12
)

par(mar = c(4.1, 4.1, 0.3, 0.1))

hist(
    x = c(residuals(my_sarima)),
    col = "lightgrey",
    xlim = c(-1.2, 1.2),
    xlab = "rezidua výsledného modelu",
    ylab = "absolutní četnost",
    main = ""
)

dev.off()

setwd(mother_working_directory)


#### ukládám ACF a PACF pro výsledný model ------------------------------------

setwd(paste(mother_working_directory, "vystupy", sep = "/"))

for(my_correlogram_type in c("acf", "pacf")){
    
    cairo_ps(
        file = paste(
            "my_",
            my_correlogram_type,
            "_diagram_final_2",
            ".eps",
            sep = ""
        ),
        width = 4,
        height = 3,
        pointsize = 12
    )
    
    par(mar = c(4.1, 4.1, 0.1, 0.1))
    
    do.call(
        what = my_correlogram_type,
        args = list(
            x = my_sarima$residuals,
            xlab = "zpoždění (lags)",
            ylab = if(
                my_correlogram_type == "acf"
            ){
                "autokorelace"
            }else{
                "parciální autokorelace"
            },
            main = "",
            ylim = c(-0.5, 1.0),
            lag.max = 80
        )
    )
    
    dev.off()

}

setwd(mother_working_directory)


#### tisknu výstup modelu -----------------------------------------------------

my_table <- unclass(coeftest(my_sarima))

print(
    xtable(
        my_table,
        align = rep("", ncol(my_table) + 1),
        digits = c(0, 3, 3, 3, 3)
    ),
    floating = FALSE,
    tabular.environment = "tabular",
    hline.after = NULL,
    include.rownames = TRUE,
    include.colnames = TRUE,
    format.args = list(decimal.mark = ",")
)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





