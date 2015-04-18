exclude <- function ()
{
  deps2011 <- unique(d1$Person_ID)
  find  <- function (x)
  {
    if (x %in% deps2011) {
      TRUE
    } else {
      FALSE
    }
  }
  find2  <- function (x)
  {
    if (x %in% deps2014) {
      TRUE
    } else {
      FALSE
    }
  }
  pres <- sapply(d2$Person_ID,find)
  d2 <<- d2[pres,]
  deps2014 <- unique(d2$Person_ID)
  pres <- sapply(d1$Person_ID,find2)
  d1 <<- d1[pres,]
}

countcars <- function (year)
{
  if (year == 2011) 
  {
    t <- cars2011
  } else 
  {
    if (year == 2013)
    {
      t <- cars2013
    } else
    {
      return
    }
  }
  faction <- sort(unique(as.character(t$faction_title)))
  ret <- data.frame()
  for (i in 1:length(faction)) 
  {
    m2 <- as.character(t$transport_mark[t$faction_title == faction[i]])
    marks <- sort(unique(m2))
    ncars <- numeric(0)
    automarks <- character(0)
    others <- 0
    for (j in 1:length(marks))
    {
      if (sum(m2 == marks[j])>3)  
      {
        ncars <- c(ncars, sum(m2 == marks[j]))
        automarks <- c(automarks, marks[j])
      } else
      {
        others <- others + sum(m2 == marks[j])
      }
    }
    ncars <- c(ncars, sum(m2 == marks[j]))
    automarks <- c(automarks, "Others")
    f <- rep(faction[i], length(automarks))
    ret <- rbind(ret, data.frame(f,automarks,ncars))
    
    
    #print(p)
    #p <- p + facet_grid(facets=. ~ gender)
    #pie(data.frame(marks,ncars)[,2],labels = data.frame(marks,ncars)[,1])
  }
  p <- ggplot(data = ret, 
              aes(x = factor(1), y = ncars, fill = factor(automarks)))
  p <- p + geom_bar(width = 1, stat = "identity") 
  p <- p + scale_color_manual(values = c("chartreuse4", "chocolate4", "azure4", "black", 
                                         "blue", "burlywood1", "darkgoldenrod", "darkmagenta", "darkolivegreen1", "darkorange", 
                                         "firebrick", "honeydew", "lavenderblush", "yellow", "violet", "navajowhite", 
                                         "lightpink4", "khaki", "lightcoral", "lavenderblush3", "mediumorchid4", "seagreen1"))
  p <- p + facet_grid(facets=. ~ f)
  p
}

changes_in_marks <- function (c2011, c2013)
{
  all_cars <- data.table(rbind(c2011, c2013))
  all_cars[, marksam := lapply(.SD,length), by = list(transport_mark, faction_title, declaration_year)]
  #comp <- data.table(unique(as.factor(all_cars$faction_title)), unique(as.factor(all_cars$transport_mark)))
  comp <- data.table(unique(data.table(as.factor(all_cars$faction_title), as.factor(all_cars$transport_mark))))
  setnames(comp, names(comp), c("faction_title", "transport_mark"))
  c1 <- numeric(0)
  c2 <- numeric(0)
  for (i in 1:length(comp$faction_title))
  {
    cam2011 <- unique(all_cars$marksam[(all_cars$faction_title ==  comp$faction_title[i]) & 
                              (all_cars$transport_mark ==  comp$transport_mark[i]) &
                                all_cars$declaration_year == 2011])
    cam2013 <- unique(all_cars$marksam[(all_cars$faction_title ==  comp$faction_title[i]) & 
                                         (all_cars$transport_mark ==  comp$transport_mark[i]) &
                                         all_cars$declaration_year == 2013])
    if (length(cam2011) == 0) {
      cam2011 <- 0
    }
    if (length(cam2013) == 0) {
      cam2013 <- 0
    }
    c1 <- c(c1,cam2011)
    c2 <- c(c2,cam2013)
  }
  comp[, changes := (c2 - c1)]
  comp <- comp[changes != 0,]
  comp[order(faction_title, changes)]
}

changes_incomes <- function(d1, d2, type = "family")
{
  inc1 <- data.table(d1[d1$chapter_num == 2,])
  inc2 <- data.table(d2[d2$chapter_num == 2,])
  if (type == "family") 
  {
    inc1 <- inc1[(inc1$index %% 1) > 0, ]
    inc2 <- inc2[(inc2$index %% 1) > 0, ]
  } 
  if (type == "declarer") 
  {
    inc1 <- inc1[(inc1$index %% 1) == 0, ]
    inc2 <- inc2[(inc2$index %% 1) == 0, ]
  }
  inc1 <- inc1[(inc1$index < 6) | (inc1$index >= 21),]
  inc2 <- inc2[(inc2$index < 6) | (inc2$index >= 21),]
  inc1[, suminc2011 := sum(numeric_value), by = Person_ID]
  inc2[, suminc2013 := sum(numeric_value), by = Person_ID]
  sinc <- data.table(unique(inc1[,c("Person_ID", "suminc2011"), with = FALSE]))
  sinc2 <- data.table(unique(inc2[,c("Person_ID", "suminc2013"), with = FALSE]))
  all <- data.table(unique(d1[,c("Person_ID", "person","faction_title")]))
  sinc <- merge(all, sinc, all.x = T, by = "Person_ID")
  sinc <- merge (sinc, sinc2, by = "Person_ID", all = TRUE)
  sinc$suminc2011[is.na(sinc$suminc2011)] <- 0
  sinc$suminc2013[is.na(sinc$suminc2013)] <- 0
  sinc[, changes := suminc2013 - suminc2011]
  sinc
}



library(ggplot2)
library(data.table)
setwd("/home/pavlo/decl_analysis")
d1 <- read.csv("decl_2011.csv")
d2 <- read.csv("decl_2013.csv")
load("factions.Rda")
exclude()
d1 <- merge(d1, factions, by.x = "Person_ID", by.y = "MP_ID")
d2 <- merge(d2, factions, by.x = "Person_ID", by.y = "MP_ID")
cars2011 <- d1[d1$index == 35 | d1$index == 40, 
    c("person", "transport_mark", "declaration_year", "transport_model",
      "transport_description","transport_year", "faction_title")] 
cars2013 <- d2[d2$index == 35 | d2$index == 40, 
    c("person", "transport_mark", "declaration_year", "transport_model",
      "transport_description","transport_year", "faction_title")] 
banks2011 <- d1[d1$index == 45 | d1$index == 51, c("person", "numeric_value", "faction_title")] 
banks2013 <- d2[d2$index == 45 | d2$index == 51, c("person", "numeric_value", "faction_title")]
