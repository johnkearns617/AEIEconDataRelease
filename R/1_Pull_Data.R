# EconDataReleases.R
# John Kearns and Grant Seiter
# 2022-05-05
# Last updated: 2022-06-08 \\ jdk

# set folders
master_dir = paste0("")

# load packages
require(tidyverse)
require(rvest)
require(fredr)
require(jsonlite)
require(sf)
require(reactablefmtr)
require(dataui)
require(magrittr)

with_tooltip <- function(value, tooltip) {
  tags$abbr(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
            title = tooltip, value)
}

# set FRED key
fred_key = "156b9cd1b9a52db3b9fc0bab8aca2b39"
beaKey = "F2533723-8E28-4A6B-B796-7295CF0406EE"

# initialize FRED link
fredr_set_key(fred_key)

# set release ids that we want to keep
selected_data = readxl::read_excel(paste0(master_dir,"selected_data_codes.xlsx"))

# pull release dates
pull_release_dates_json = function(key,date_start,date_end=Sys.Date() %m+% years(1)){

  require(lubridate)
  require(dplyr)

  req=data.frame()
  date_loop = date_end

  while(!any(duplicated(tail(date_loop,2)))){
    req = bind_rows(req,jsonlite::fromJSON(paste0("https://api.stlouisfed.org/fred/releases/dates?api_key=",key,"&include_release_dates_with_no_data=true&file_type=json&realtime_start=",date_start,"&realtime_end=",tail(date_loop,1),""))$release_dates)
    date_loop = append(date_loop,min(req$date))
    print(paste0(max(req$date)," to ",min(req$date)))
  }

  # drop duplicated releases
  req = req %>%
    distinct(release_id,date,.keep_all=TRUE)

  return(req)

}

release_dates = pull_release_dates_json(fred_key,"2022-01-01") %>%
  filter(release_id%in%selected_data$release_id) %>%
  bind_rows(data.frame(release_id=101,release_name="FOMC Press Conference",release_last_updated="2022-06-06",
                       date=paste0("2022-",c("01","03","05","06","07","09","11","12"),"-",c("26","16","04","15","27","21","02","14")))) # add FOMC press releases at beginning of year

# pull series from FRED
series_codes = readxl::read_excel(paste0(master_dir,"selected_data_codes.xlsx"),sheet="Series")

# functions to pull data
match_function = function(sid){

  if(series_codes$growth[series_codes$series_id==sid]=="m_growth"){
    df = m_growth_function(sid)
  }
  if(series_codes$growth[series_codes$series_id==sid]=="m_change"){
    df = m_change_function(sid)
  }
  if(series_codes$growth[series_codes$series_id==sid]=="w_growth"){
    df = w_growth_function(sid)
  }
  if(series_codes$growth[series_codes$series_id==sid]=="w_change"){
    df = w_change_function(sid)
  }
  if(series_codes$growth[series_codes$series_id==sid]=="q_growth"){
    df = q_growth_function(sid)
  }
  if(series_codes$growth[series_codes$series_id==sid]=="q_change"){
    df = q_change_function(sid)
  }
  if(series_codes$growth[series_codes$series_id==sid]=="d_growth"){
    df = d_growth_function(sid)
  }
  if(series_codes$growth[series_codes$series_id==sid]=="d_change"){
    df = d_change_function(sid)
  }
  if(series_codes$growth[series_codes$series_id==sid]=="FEDSEP"){
    df = fed_function(sid)
  }

  df = df %>% mutate(sid=sid1) %>% select(-sid1)

  return(df)
}


m_growth_function = function(sid){

  require(seasthedata)
  require(tidyverse)
  require(magrittr)

  data = fredr(sid) %>%
    mutate(level=value)

  if(sid%in%c("ACTLISCOUUS","MEDDAYONMARUS")){

    data = seasthedata::seasthedata(data %>% select(series_id,date,value) %>% group_by(series_id)) %>%
      ungroup() %>%
      left_join(data %>% select(date,level),by="date")

  }

  if(sid%in%c("CES0500000003")){

    cpi = fredr("CPIAUCSL") %>%
      select(date,value) %>%
      rename(cpi=value)

    real_data = data %>%
      left_join(cpi,by="date") %>%
      fill(cpi) %>%
      mutate(level=level/(cpi/mean(cpi[year(date)==year(last(date))],na.rm=TRUE)*100)*100)

    real_data = real_data %>%
      left_join(fredr_release_series(series_codes$release_id[series_codes$series_id==sid]) %>% select(id,title,observation_start,observation_end,units_short,seasonal_adjustment_short),by=c("series_id"="id")) %>%
      mutate(last_updated=max(release_dates$date[release_dates$release_id==series_codes$release_id[series_codes$series_id==sid]&release_dates$date<=Sys.Date()]),
             title=ifelse(is.na(title),series_codes$series_name[series_codes$series_id==sid],title),
             title=paste0("Real ",title),
             series_id=paste0(sid,"R"),
             frequency="Monthly",
             corr_units_short="%",
             units_short=case_when(
               sid=="ACTLISCOUUS"~"Units",
               sid=="PPIACO"~"Index (1982)=100",
               sid=="PPIFIS"~"Index (2009)=100",
               series_id=="CES0500000003R"~paste0(year(last(date))," $"),
               !is.na(units_short)~units_short
             ),
             corr_value=(value/dplyr::lag(value,12)-1)*100,sid1=series_id) %T>%
      write.csv(paste0(master_dir,"Data/data_save/",sid,"R.csv"))
    dfs[[paste0(sid,"R")]] <<- real_data

    real_data = real_data %>%
      summarize(title=paste0(title[1],"---",units_short[1]),sid1=sid1[1],
                source=series_codes$Source[series_codes$series_id==sid],
                type=series_codes$type[series_codes$series_id==sid],
                recent_values=list(na.omit(level[date>"2017-12-31"])),
                last_updated=as.Date(last_updated[1]),
                next_update=last(release_dates$date[release_dates$release_id==series_codes$release_id[series_codes$series_id==sid]&release_dates$date>Sys.Date()]),
                frequency=frequency[1],
                forecast=NA,
                last_update_for=last(date[!is.na(level)]),
                latest_value=round(level[n()],digits=2),
                previous_value = round(level[n()-1],digits=2),
                pop_change=paste0(round((nth(na.omit(level),-1)/nth(na.omit(level),-2)-1)*100,digits=2),corr_units_short[1]),
                d365_change=paste0(round((nth(na.omit(level),-1)/nth(na.omit(level),-13)-1)*100,digits=2),corr_units_short[1]),
                series_max=paste0(round(max(level,na.rm=TRUE),digits=2)," on ",date[which.max(level)]),
                series_min=paste0(round(min(level,na.rm=TRUE),digits=2)," on ",date[which.min(level)]),
                growth_max=paste0(round(max((level/dplyr::lag(level,12)-1)*100,na.rm=TRUE),digits=2),corr_units_short[1]," on ",date[which.max((level/dplyr::lag(level,12)-1)*100)]),
                growth_min=paste0(round(min((level/dplyr::lag(level,12)-1)*100,na.rm=TRUE),digits=2),corr_units_short[1]," on ",date[which.min((level/dplyr::lag(level,12)-1)*100)])) %>%
      ungroup()

  }

  data = data %>%
    left_join(fredr_release_series(series_codes$release_id[series_codes$series_id==sid]) %>% select(id,title,observation_start,observation_end,units_short,seasonal_adjustment_short),by=c("series_id"="id")) %>%
    mutate(last_updated=max(release_dates$date[release_dates$release_id==series_codes$release_id[series_codes$series_id==sid]&release_dates$date<=Sys.Date()]),
           title=ifelse(is.na(title),series_codes$series_name[series_codes$series_id==sid],title),
           frequency="Monthly",
           corr_units_short="%",
           units_short=case_when(
             sid=="ACTLISCOUUS"~"Units",
             sid=="PPIACO"~"Index (1982)=100",
             sid=="PPIFIS"~"Index (2009)=100",
             !is.na(units_short)~units_short
           ),
           corr_value=(value/dplyr::lag(value,12)-1)*100,sid1=series_id) %T>%
    write.csv(paste0(master_dir,"Data/data_save/",sid,".csv"))
  dfs[[paste0(sid)]] <<- data


  data = data %>%
    summarize(title=paste0(title[1],"---",units_short[1]),sid1=sid1[1],
              source=series_codes$Source[series_codes$series_id==sid],
              type=series_codes$type[series_codes$series_id==sid],
              recent_values=list(na.omit(level[date>"2017-12-31"])),
              last_updated=as.Date(last_updated[1]),
              next_update=last(release_dates$date[release_dates$release_id==series_codes$release_id[series_codes$series_id==sid]&release_dates$date>Sys.Date()]),
              frequency=frequency[1],
              forecast=NA,
              last_update_for=last(date[!is.na(level)]),
              latest_value=round(level[n()],digits=2),
              previous_value = round(level[n()-1],digits=2),
              pop_change=paste0(round((nth(na.omit(value),-1)/nth(na.omit(value),-2)-1)*100,digits=2),corr_units_short[1]),
              d365_change=paste0(round((nth(na.omit(value),-1)/nth(na.omit(value),-13)-1)*100,digits=2),corr_units_short[1]),
              series_max=paste0(round(max(level,na.rm=TRUE),digits=2)," on ",date[which.max(level)]),
              series_min=paste0(round(min(level,na.rm=TRUE),digits=2)," on ",date[which.min(level)]),
              growth_max=paste0(round(max((value/dplyr::lag(value,12)-1)*100,na.rm=TRUE),digits=2),corr_units_short[1]," on ",date[which.max((value/dplyr::lag(value,12)-1)*100)]),
              growth_min=paste0(round(min((value/dplyr::lag(value,12)-1)*100,na.rm=TRUE),digits=2),corr_units_short[1]," on ",date[which.min((value/dplyr::lag(value,12)-1)*100)])) %>%
    ungroup()

  if(sid%in%c("CES0500000003")){
    data=bind_rows(data,real_data)
  }
  return(data)

}

m_change_function = function(sid){

  require(seasthedata)
  require(tidyverse)
  require(magrittr)

  data = fredr(sid) %>%
    mutate(level=value)

  if(sid%in%c("ACTLISCOUUS","MEDDAYONMARUS")){

    data = seasthedata::seasthedata(data %>% select(series_id,date,value) %>% group_by(series_id)) %>%
      ungroup() %>%
      left_join(data %>% select(date,level),by="date")

  }

  data = data %>%
    left_join(fredr_release_series(series_codes$release_id[series_codes$series_id==sid]) %>% select(id,title,observation_start,observation_end,units_short,seasonal_adjustment_short),by=c("series_id"="id")) %>%
    mutate(last_updated=max(release_dates$date[release_dates$release_id==series_codes$release_id[series_codes$series_id==sid]&release_dates$date<=Sys.Date()]),
           title=ifelse(is.na(title),series_codes$series_name[series_codes$series_id==sid],title),
           frequency="Monthly",
           corr_units_short=case_when(
             units_short=="Rate"~" perc. points",
             grepl("Index",units_short)~"",
             sid=="MEDDAYONMARUS"~" days",
             sid=="BBKMGDP"~" perc. points",
             units_short=="Percent, Seasonally Adjusted"~" perc. points",
             sid%in%c("UNRATE","CIVPART","EMRATIO","LNS11300060","LNS11300002","U6RATE","PSAVERT","MICH","TCU")~" perc. points"
           ),
           units_short=case_when(
             sid=="MEDDAYONMARUS"~"Days",
             sid=="TCU"~"% of total capacity",
             sid%in%c("UNRATE","CIVPART","EMRATIO","LNS11300060","LNS11300002","U6RATE","PSAVERT")~"%",
             !is.na(units_short)~units_short
           ),
           corr_value = value-dplyr::lag(value,12),sid1=series_id) %T>%
    write.csv(paste0(master_dir,"Data/data_save/",sid,".csv"))
  dfs[[paste0(sid)]] <<- data

  data = data %>%
    summarize(title=paste0(title[1],"---",units_short[1]),sid1=sid1[1],
              source=series_codes$Source[series_codes$series_id==sid],
              type=series_codes$type[series_codes$series_id==sid],
              recent_values=list(na.omit(level[date>"2017-12-31"])),
              last_updated=as.Date(last_updated[1]),
              next_update=last(release_dates$date[release_dates$release_id==series_codes$release_id[series_codes$series_id==sid]&release_dates$date>Sys.Date()]),
              frequency=frequency[1],
              forecast=NA,
              last_update_for=last(date[!is.na(level)]),
              latest_value=round(level[n()],digits=2),
              previous_value = round(level[n()-1],digits=2),
              pop_change=paste0(round((nth(na.omit(value),-1)-nth(na.omit(value),-2)),digits=2),corr_units_short[1]),
              d365_change=paste0(round((nth(na.omit(value),-1)-nth(na.omit(value),-13)),digits=2),corr_units_short[1]),
              series_max=paste0(round(max(level,na.rm=TRUE),digits=2)," on ",date[which.max(level)]),
              series_min=paste0(round(min(level,na.rm=TRUE),digits=2)," on ",date[which.min(level)]),
              growth_max=paste0(round(max((value-dplyr::lag(value,12)),na.rm=TRUE),digits=2),corr_units_short[1]," on ",date[which.max((value-dplyr::lag(value,12)))]),
              growth_min=paste0(round(min((value-dplyr::lag(value,12)),na.rm=TRUE),digits=2),corr_units_short[1]," on ",date[which.min((value-dplyr::lag(value,12)))])) %>%
    ungroup()

  return(data)

}

w_growth_function = function(sid){

  require(seasthedata)
  require(tidyverse)
  require(magrittr)

  data = fredr(sid) %>%
    mutate(level=value)

  if(sid%in%c("ACTLISCOUUS","MEDDAYONMARUS")){

    data = seasthedata::seasthedata(data %>% select(series_id,date,value) %>% group_by(series_id)) %>%
      ungroup() %>%
      left_join(data %>% select(date,level),by="date")

  }

  data = data %>%
    left_join(fredr_release_series(series_codes$release_id[series_codes$series_id==sid]) %>% select(id,title,observation_start,observation_end,units_short,seasonal_adjustment_short),by=c("series_id"="id")) %>%
    mutate(last_updated=max(release_dates$date[release_dates$release_id==series_codes$release_id[series_codes$series_id==sid]&release_dates$date<=Sys.Date()]),
           title=ifelse(is.na(title),series_codes$series_name[series_codes$series_id==sid],title),
           frequency="Weekly",
           corr_units_short="%",
           units_short=case_when(
             sid%in%c("WALCL","WTREGEN","WSHOMCB","WORAL","SWPT","WRBWFRBL","WLRRAL","TREAST")~"$Millions",
             !is.na(units_short)~units_short
           ),
           corr_value=(value/dplyr::lag(value,52)-1)*100,sid1=series_id) %T>%
    write.csv(.,paste0(master_dir,"Data/data_save/",sid,".csv"))
  dfs[[paste0(sid)]] <<- data

  data= data %>%
    summarize(title=paste0(title[1],"---",units_short[1]),sid1=sid1[1],
              source=series_codes$Source[series_codes$series_id==sid],
              type=series_codes$type[series_codes$series_id==sid],
              recent_values=list(na.omit(level[date>"2017-12-31"])),
              last_updated=as.Date(last_updated[1]),
              next_update=last(release_dates$date[release_dates$release_id==series_codes$release_id[series_codes$series_id==sid]&release_dates$date>Sys.Date()]),
              frequency=frequency[1],
              forecast=NA,
              last_update_for=last(date[!is.na(level)]),
              latest_value=round(level[n()],digits=2),
              previous_value = round(level[n()-1],digits=2),
              pop_change=paste0(round((nth(na.omit(value),-1)/nth(na.omit(value),-2)-1)*100,digits=2),corr_units_short[1]),
              d365_change=paste0(round((nth(na.omit(value),-1)/nth(na.omit(value),-53)-1)*100,digits=2),corr_units_short[1]),
              series_max=paste0(round(max(level,na.rm=TRUE),digits=2)," on ",date[which.max(level)]),
              series_min=paste0(round(min(level,na.rm=TRUE),digits=2)," on ",date[which.min(level)]),
              growth_max=paste0(round(max((value/dplyr::lag(value,52)-1)*100,na.rm=TRUE),digits=2),corr_units_short[1]," on ",date[which.max((value/dplyr::lag(value,12)-1)*100)]),
              growth_min=paste0(round(min((value/dplyr::lag(value,52)-1)*100,na.rm=TRUE),digits=2),corr_units_short[1]," on ",date[which.min((value/dplyr::lag(value,12)-1)*100)])) %>%
    ungroup()

  return(data)

}

w_change_function = function(sid){

  require(seasthedata)
  require(tidyverse)
  require(magrittr)

  data = fredr(sid) %>%
    mutate(level=value)

  if(sid%in%c("ACTLISCOUUS","MEDDAYONMARUS")){

    data = seasthedata::seasthedata(data %>% select(series_id,date,value) %>% group_by(series_id)) %>%
      ungroup() %>%
      left_join(data %>% select(date,level),by="date")

  }

  if(sid%in%c("GASREGW")){

    cpi = fredr("CPIAUCSL") %>%
      select(date,value) %>%
      rename(cpi=value) %>%
      mutate(month=month(date),
             year=year(date)) %>%
      select(-date)

    real_data = data %>%
      mutate(month=month(date),
             year=year(date)) %>%
      left_join(cpi,by=c("month","year")) %>%
      fill(cpi) %>%
      mutate(level=level/(cpi/mean(cpi[year(date)==year(last(date))],na.rm=TRUE)*100)*100) %>%
      select(-c(month,year))

    real_data = real_data %>%
      left_join(fredr_release_series(series_codes$release_id[series_codes$series_id==sid]) %>% select(id,title,observation_start,observation_end,units_short,seasonal_adjustment_short),by=c("series_id"="id")) %>%
      mutate(last_updated=max(release_dates$date[release_dates$release_id==series_codes$release_id[series_codes$series_id==sid]&release_dates$date<=Sys.Date()]),
             title=ifelse(is.na(title),series_codes$series_name[series_codes$series_id==sid],title),
             title=paste0("Real ",title),
             series_id=paste0(sid,"R"),
             frequency="Weekly",
             corr_units_short=case_when(
               grepl("GAS",sid)~"",
               sid=="MORTGAGE30US"~" perc. points",
               sid=="ANFCI"~" points",
               series_id=="GASREGWR"~paste0(last(date)," $")
             ),
             units_short=case_when(
               series_id=="GASREGWR"~paste0(year(last(date))," $ per gallon"),
               !is.na(units_short)~units_short
             ),
             corr_value=(value-dplyr::lag(value,52)),sid1=series_id) %T>%
      write.csv(.,paste0(master_dir,"Data/data_save/",sid,"R.csv"))
    dfs[[paste0(sid,"R")]] <<- real_data

    real_data = real_data %>%
      summarize(title=paste0(title[1],"---",units_short[1]),sid1=sid1[1],
                source=series_codes$Source[series_codes$series_id==sid],
                type=series_codes$type[series_codes$series_id==sid],
                recent_values=list(na.omit(level[date>"2017-12-31"])),
                last_updated=as.Date(last_updated[1]),
                next_update=last(release_dates$date[release_dates$release_id==series_codes$release_id[series_codes$series_id==sid]&release_dates$date>Sys.Date()]),
                frequency=frequency[1],
                forecast=NA,
                last_update_for=last(date[!is.na(level)]),
                latest_value=round(level[n()],digits=2),
                previous_value = round(level[n()-1],digits=2),
                pop_change=paste0(round((nth(na.omit(level),-1)-nth(na.omit(level),-2)),digits=2),corr_units_short[1]),
                d365_change=paste0(round((nth(na.omit(level),-1)-nth(na.omit(level),-53)),digits=2),corr_units_short[1]),
                series_max=paste0(round(max(level,na.rm=TRUE),digits=2)," on ",date[which.max(level)]),
                series_min=paste0(round(min(level,na.rm=TRUE),digits=2)," on ",date[which.min(level)]),
                growth_max=paste0(round(max((level-dplyr::lag(level,52)),na.rm=TRUE),digits=2),corr_units_short[1]," on ",date[which.max((level-dplyr::lag(level,12)))]),
                growth_min=paste0(round(min((level-dplyr::lag(level,52)),na.rm=TRUE),digits=2),corr_units_short[1]," on ",date[which.min((level-dplyr::lag(level,12)))])) %>%
      ungroup()


  }

  data = data %>%
    left_join(fredr_release_series(series_codes$release_id[series_codes$series_id==sid]) %>% select(id,title,observation_start,observation_end,units_short,seasonal_adjustment_short),by=c("series_id"="id")) %>%
    mutate(last_updated=max(release_dates$date[release_dates$release_id==series_codes$release_id[series_codes$series_id==sid]&release_dates$date<=Sys.Date()]),
           title=ifelse(is.na(title),series_codes$series_name[series_codes$series_id==sid],title),
           frequency="Weekly",
           corr_units_short=case_when(
             grepl("GAS",sid)~"",
             sid=="MORTGAGE30US"~" perc. points",
             sid=="ANFCI"~" points"
           ),
           units_short=case_when(
             !is.na(units_short)~units_short
           ),
           corr_value=(value-dplyr::lag(value,52)),sid1=series_id) %T>%
    write.csv(.,paste0(master_dir,"Data/data_save/",sid,".csv"))
  dfs[[paste0(sid)]] <<- data

  data = data %>%
    summarize(title=paste0(title[1],"---",units_short[1]),sid1=sid1[1],
              source=series_codes$Source[series_codes$series_id==sid],
              type=series_codes$type[series_codes$series_id==sid],
              recent_values=list(na.omit(level[date>"2017-12-31"])),
              last_updated=as.Date(last_updated[1]),
              next_update=last(release_dates$date[release_dates$release_id==series_codes$release_id[series_codes$series_id==sid]&release_dates$date>Sys.Date()]),
              frequency=frequency[1],
              forecast=NA,
              last_update_for=last(date[!is.na(level)]),
              latest_value=round(level[n()],digits=2),
              previous_value = round(level[n()-1],digits=2),
              pop_change=paste0(round((nth(na.omit(value),-1)-nth(na.omit(value),-2)),digits=2),corr_units_short[1]),
              d365_change=paste0(round((nth(na.omit(value),-1)-nth(na.omit(value),-53)),digits=2),corr_units_short[1]),
              series_max=paste0(round(max(level,na.rm=TRUE),digits=2)," on ",date[which.max(level)]),
              series_min=paste0(round(min(level,na.rm=TRUE),digits=2)," on ",date[which.min(level)]),
              growth_max=paste0(round(max((value-dplyr::lag(value,52)),na.rm=TRUE),digits=2),corr_units_short[1]," on ",date[which.max((value-dplyr::lag(value,12)))]),
              growth_min=paste0(round(min((value-dplyr::lag(value,52)),na.rm=TRUE),digits=2),corr_units_short[1]," on ",date[which.min((value-dplyr::lag(value,12)))])) %>%
    ungroup()

  if(sid%in%c("GASREGW")){
    data=bind_rows(data,real_data)
  }

  return(data)

}

q_growth_function = function(sid){

  require(seasthedata)
  require(tidyverse)
  require(magrittr)

  data = fredr(sid) %>%
    mutate(level=value)

  if(sid%in%c("ACTLISCOUUS","MEDDAYONMARUS")){

    data = seasthedata::seasthedata(data %>% select(series_id,date,value) %>% group_by(series_id)) %>%
      ungroup() %>%
      left_join(data %>% select(date,level),by="date")

  }

  if(sid%in%c("ECIALLCIV")){

    cpi = fredr("CPIAUCSL") %>%
      select(date,value) %>%
      rename(cpi=value) %>%
      mutate(quarter=quarter(date),
             year=year(date)) %>%
      group_by(year,quarter) %>%
      summarize(date=date[1],
                cpi=mean(cpi,na.rm=TRUE)) %>%
      ungroup() %>%
      select(-c(year,quarter))


    real_data = data %>%
      left_join(cpi,by="date") %>%
      fill(cpi) %>%
      mutate(level=level/(cpi)*100)

    real_data = real_data %>%
      rowwise() %>%
      mutate(level=ifelse(sid=="ECIALLCIV",level/real_data$level[real_data$date=="2005-10-01"]*100,level)) %>%
      ungroup()

    real_data = real_data %>%
      left_join(fredr_release_series(series_codes$release_id[series_codes$series_id==sid]) %>% select(id,title,observation_start,observation_end,units_short,seasonal_adjustment_short),by=c("series_id"="id")) %>%
      mutate(last_updated=max(release_dates$date[release_dates$release_id==series_codes$release_id[series_codes$series_id==sid]&release_dates$date<=Sys.Date()]),
             title=ifelse(is.na(title),series_codes$series_name[series_codes$series_id==sid],title),
             frequency="Quarterly",
             title=paste0("Real ",title),
             series_id=paste0(sid,"R"),
             corr_units_short="%",
             units_short=case_when(
               !is.na(units_short)~units_short
             ),
             corr_value=(value/dplyr::lag(value,4)-1)*100,sid1=series_id) %T>%
      write.csv(.,paste0(master_dir,"Data/data_save/",sid,"R.csv"))
    dfs[[paste0(sid,"R")]] <<- real_data

    real_data = real_data %>%
      summarize(title=paste0(title[1],"---",units_short[1]),sid1=sid1[1],
                source=series_codes$Source[series_codes$series_id==sid],
                type=series_codes$type[series_codes$series_id==sid],
                recent_values=list(na.omit(level[date>"2017-12-31"])),
                last_updated=as.Date(last_updated[1]),
                next_update=last(release_dates$date[release_dates$release_id==series_codes$release_id[series_codes$series_id==sid]&release_dates$date>Sys.Date()]),
                frequency=frequency[1],
                forecast=NA,
                last_update_for=last(date[!is.na(level)]),
                latest_value=round(level[n()],digits=2),
                previous_value = round(level[n()-1],digits=2),
                pop_change=paste0(round((nth(na.omit(level),-1)/nth(na.omit(level),-2)-1)*100,digits=2),corr_units_short[1]),
                d365_change=paste0(round((nth(na.omit(level),-1)/nth(na.omit(level),-5)-1)*100,digits=2),corr_units_short[1]),
                series_max=paste0(round(max(level,na.rm=TRUE),digits=2)," on ",date[which.max(level)]),
                series_min=paste0(round(min(level,na.rm=TRUE),digits=2)," on ",date[which.min(level)]),
                growth_max=paste0(round(max((level/dplyr::lag(level,4)-1)*100,na.rm=TRUE),digits=2),corr_units_short[1]," on ",date[which.max((level/dplyr::lag(level,4)-1)*100)]),
                growth_min=paste0(round(min((level/dplyr::lag(level,4)-1)*100,na.rm=TRUE),digits=2),corr_units_short[1]," on ",date[which.min((level/dplyr::lag(level,4)-1)*100)])) %>%
      ungroup()

  }

  data = data %>%
    left_join(fredr_release_series(series_codes$release_id[series_codes$series_id==sid]) %>% select(id,title,observation_start,observation_end,units_short,seasonal_adjustment_short),by=c("series_id"="id")) %>%
    mutate(last_updated=max(release_dates$date[release_dates$release_id==series_codes$release_id[series_codes$series_id==sid]&release_dates$date<=Sys.Date()]),
           title=ifelse(is.na(title),series_codes$series_name[series_codes$series_id==sid],title),
           frequency="Quarterly",
           corr_units_short="%",
           units_short=case_when(
             !is.na(units_short)~units_short
           ),
           corr_value=(value/dplyr::lag(value,4)-1)*100,sid1=series_id) %T>%
    write.csv(.,paste0(master_dir,"Data/data_save/",sid,".csv"))
  dfs[[paste0(sid)]] <<- data

  data = data %>%
    summarize(title=paste0(title[1],"---",units_short[1]),sid1=sid1[1],
              source=series_codes$Source[series_codes$series_id==sid],
              type=series_codes$type[series_codes$series_id==sid],
              recent_values=list(na.omit(level[date>"2017-12-31"])),
              last_updated=as.Date(last_updated[1]),
              next_update=last(release_dates$date[release_dates$release_id==series_codes$release_id[series_codes$series_id==sid]&release_dates$date>Sys.Date()]),
              frequency=frequency[1],
              forecast=NA,
              last_update_for=last(date[!is.na(level)]),
              latest_value=round(level[n()],digits=2),
              previous_value = round(level[n()-1],digits=2),
              pop_change=paste0(round((nth(na.omit(value),-1)/nth(na.omit(value),-2)-1)*100,digits=2),corr_units_short[1]),
              d365_change=paste0(round((nth(na.omit(value),-1)/nth(na.omit(value),-5)-1)*100,digits=2),corr_units_short[1]),
              series_max=paste0(round(max(level,na.rm=TRUE),digits=2)," on ",date[which.max(level)]),
              series_min=paste0(round(min(level,na.rm=TRUE),digits=2)," on ",date[which.min(level)]),
              growth_max=paste0(round(max((value/dplyr::lag(value,4)-1)*100,na.rm=TRUE),digits=2),corr_units_short[1]," on ",date[which.max((value/dplyr::lag(value,4)-1)*100)]),
              growth_min=paste0(round(min((value/dplyr::lag(value,4)-1)*100,na.rm=TRUE),digits=2),corr_units_short[1]," on ",date[which.min((value/dplyr::lag(value,4)-1)*100)])) %>%
    ungroup()

  if(sid%in%c("ECIALLCIV")){
    data=bind_rows(data,real_data)
  }

  return(data)

}

q_change_function = function(sid){

  require(seasthedata)
  require(tidyverse)
  require(magrittr)

  data = fredr(sid) %>%
    mutate(level=value)

  if(sid%in%c("ACTLISCOUUS","MEDDAYONMARUS")){

    data = seasthedata::seasthedata(data %>% select(series_id,date,value) %>% group_by(series_id)) %>%
      ungroup() %>%
      left_join(data %>% select(date,level),by="date")

  }

  data = data %>%
    left_join(fredr_release_series(series_codes$release_id[series_codes$series_id==sid]) %>% select(id,title,observation_start,observation_end,units_short,seasonal_adjustment_short),by=c("series_id"="id")) %>%
    mutate(last_updated=max(release_dates$date[release_dates$release_id==series_codes$release_id[series_codes$series_id==sid]&release_dates$date<=Sys.Date()]),
           title=ifelse(is.na(title),series_codes$series_name[series_codes$series_id==sid],title),
           frequency="Quarterly",
           corr_units_short=case_when(
             grepl("NOW",sid)~" perc. points"
           ),
           units_short=case_when(
             !is.na(units_short)~units_short
           ),
           corr_value=(value-dplyr::lag(value,4)),sid1=series_id) %T>%
    write.csv(.,paste0(master_dir,"Data/data_save/",sid,".csv"))
  dfs[[paste0(sid)]] <<- data

  data = data %>%
    summarize(title=paste0(title[1],"---",units_short[1]),sid1=sid1[1],
              source=series_codes$Source[series_codes$series_id==sid],
              type=series_codes$type[series_codes$series_id==sid],
              recent_values=list(na.omit(level[date>"2017-12-31"])),
              last_updated=as.Date(last_updated[1]),
              next_update=last(release_dates$date[release_dates$release_id==series_codes$release_id[series_codes$series_id==sid]&release_dates$date>Sys.Date()]),
              frequency=frequency[1],
              forecast=NA,
              last_update_for=last(date[!is.na(level)]),
              latest_value=round(level[n()],digits=2),
              previous_value = round(level[n()-1],digits=2),
              pop_change=paste0(round((nth(na.omit(value),-1)-nth(na.omit(value),-2)),digits=2),corr_units_short[1]),
              d365_change=paste0(round((nth(na.omit(value),-1)-nth(na.omit(value),-5)),digits=2),corr_units_short[1]),
              series_max=paste0(round(max(level,na.rm=TRUE),digits=2)," on ",date[which.max(level)]),
              series_min=paste0(round(min(level,na.rm=TRUE),digits=2)," on ",date[which.min(level)]),
              growth_max=paste0(round(max((value-dplyr::lag(value,4)),na.rm=TRUE),digits=2),corr_units_short[1]," on ",date[which.max((value-dplyr::lag(value,4)))]),
              growth_min=paste0(round(min((value-dplyr::lag(value,4)),na.rm=TRUE),digits=2),corr_units_short[1]," on ",date[which.min((value-dplyr::lag(value,4)))])) %>%
    ungroup()

  return(data)

}

d_change_function = function(sid){

  require(seasthedata)
  require(tidyverse)
  require(magrittr)

  data = fredr(sid) %>%
    mutate(level=value)

  if(sid%in%c("ACTLISCOUUS","MEDDAYONMARUS")){

    data = seasthedata::seasthedata(data %>% select(series_id,date,value) %>% group_by(series_id)) %>%
      ungroup() %>%
      left_join(data %>% select(date,level),by="date")

  }

  data = data %>%
    left_join(fredr_release_series(series_codes$release_id[series_codes$series_id==sid]) %>% select(id,title,observation_start,observation_end,units_short,seasonal_adjustment_short),by=c("series_id"="id")) %>%
    mutate(last_updated=max(release_dates$date[release_dates$release_id==series_codes$release_id[series_codes$series_id==sid]&release_dates$date<=Sys.Date()]),
           title=ifelse(is.na(title),series_codes$series_name[series_codes$series_id==sid],title),
           frequency="Daily",
           corr_units_short=case_when(
             sid%in%c("BAMLH0A0HYM2","BAMLC0A4CBBB","DGS10","DGS2","DGS30","T10Y2Y","T5YIE","T10YIE")~" perc. points",
             sid%in%c("VIXCLS","VXEEMCLS")~" perc. points",
           ),
           units_short=case_when(
             sid%in%c("VIXCLS","VXEEMCLS")~"%",
             sid=="T10Y2Y"~" perc. points",
             !is.na(units_short)~units_short
           ),
           corr_value=(value-dplyr::lag(value,365)),sid1=series_id) %T>%
    write.csv(.,paste0(master_dir,"Data/data_save/",sid,".csv"))
  dfs[[paste0(sid)]] <<- data

  data = data %>%
    summarize(title=paste0(title[1],"---",units_short[1]),sid1=sid1[1],
              source=series_codes$Source[series_codes$series_id==sid],
              type=series_codes$type[series_codes$series_id==sid],
              recent_values=list(na.omit(level[date>"2017-12-31"])),
              last_updated=as.Date(last_updated[1]),
              next_update=last(release_dates$date[release_dates$release_id==series_codes$release_id[series_codes$series_id==sid]&release_dates$date>Sys.Date()]),
              frequency=frequency[1],
              forecast=NA,
              last_update_for=last(date[!is.na(level)]),
              latest_value=round(level[n()],digits=2),
              previous_value = round(level[n()-1],digits=2),
              pop_change=paste0(round((nth(na.omit(value),-1)-nth(na.omit(value),-2)),digits=2),corr_units_short[1]),
              d365_change=paste0(round((nth(na.omit(value),-1)-nth(na.omit(value),-366)),digits=2),corr_units_short[1]),
              series_max=paste0(round(max(level,na.rm=TRUE),digits=2)," on ",date[which.max(level)]),
              series_min=paste0(round(min(level,na.rm=TRUE),digits=2)," on ",date[which.min(level)]),
              growth_max=paste0(round(max((value-dplyr::lag(value,365)),na.rm=TRUE),digits=2),corr_units_short[1]," on ",date[which.max((value-dplyr::lag(value,365)))]),
              growth_min=paste0(round(min((value-dplyr::lag(value,365)),na.rm=TRUE),digits=2),corr_units_short[1]," on ",date[which.min((value-dplyr::lag(value,365)))])) %>%
    ungroup()

  return(data)

}

d_growth_function = function(sid){

  require(seasthedata)
  require(tidyverse)
  require(magrittr)

  data = fredr(sid) %>%
    mutate(level=value)

  if(sid%in%c("ACTLISCOUUS","MEDDAYONMARUS")){

    data = seasthedata::seasthedata(data %>% select(series_id,date,value) %>% group_by(series_id)) %>%
      ungroup() %>%
      left_join(data %>% select(date,level),by="date")

  }

  data = data %>%
    left_join(fredr_release_series(series_codes$release_id[series_codes$series_id==sid]) %>% select(id,title,observation_start,observation_end,units_short,seasonal_adjustment_short),by=c("series_id"="id")) %>%
    mutate(last_updated=max(release_dates$date[release_dates$release_id==series_codes$release_id[series_codes$series_id==sid]&release_dates$date<=Sys.Date()]),
           title=ifelse(is.na(title),series_codes$series_name[series_codes$series_id==sid],title),
           frequency="Daily",
           corr_units_short="%",
           units_short=case_when(
             sid%in%c("VIXCLS","VXEEMCLS")~"",
             !is.na(units_short)~units_short
           ),
           corr_value=(value/dplyr::lag(value,365)-1)*100,sid1=series_id) %T>%
    write.csv(.,paste0(master_dir,"Data/data_save/",sid,".csv"))
  dfs[[paste0(sid)]] <<- data

  data = data %>%
    summarize(title=paste0(title[1],"---",units_short[1]),sid1=sid1[1],
              source=series_codes$Source[series_codes$series_id==sid],
              type=series_codes$type[series_codes$series_id==sid],
              recent_values=list(na.omit(level[date>"2017-12-31"])),
              last_updated=as.Date(last_updated[1]),
              next_update=last(release_dates$date[release_dates$release_id==series_codes$release_id[series_codes$series_id==sid]&release_dates$date>Sys.Date()]),
              forecast=NA,
              frequency=frequency[1],
              last_update_for=last(date[!is.na(level)]),
              latest_value=round(level[n()],digits=2),
              previous_value = round(level[n()-1],digits=2),
              pop_change=paste0(round((nth(na.omit(value),-1)/nth(na.omit(value),-2)-1)*100,digits=2),corr_units_short[1]),
              d365_change=paste0(round((nth(na.omit(value),-1)/nth(na.omit(value),-366)-1)*100,digits=2),corr_units_short[1]),
              series_max=paste0(round(max(level,na.rm=TRUE),digits=2)," on ",date[which.max(level)]),
              series_min=paste0(round(min(level,na.rm=TRUE),digits=2)," on ",date[which.min(level)]),
              growth_max=paste0(round(max((value/dplyr::lag(value,365)-1)*100,na.rm=TRUE),digits=2),corr_units_short[1]," on ",date[which.max((value/dplyr::lag(value,365)-1)*100)]),
              growth_min=paste0(round(min((value/dplyr::lag(value,365)-1)*100,na.rm=TRUE),digits=2),corr_units_short[1]," on ",date[which.min((value/dplyr::lag(value,365)-1)*100)])) %>%
    ungroup()

  return(data)

}

fed_function = function(sid){

  require(seasthedata)
  require(tidyverse)
  require(magrittr)

  data = fredr(sid) %>%
    mutate(level=value)

  data = data %>%
    left_join(fredr_release_series(series_codes$release_id[series_codes$series_id==sid]) %>% select(id,title,observation_start,observation_end,units_short,seasonal_adjustment_short),by=c("series_id"="id")) %>%
    mutate(last_updated=max(release_dates$date[release_dates$release_id==series_codes$release_id[series_codes$series_id==sid]&release_dates$date<=Sys.Date()]),
           title=ifelse(is.na(title),series_codes$series_name[series_codes$series_id==sid],title),
           frequency=NA,
           corr_units_short=NA,
           units_short=case_when(
             sid%in%c("VIXCLS","VXEEMCLS")~"",
             !is.na(units_short)~units_short
           ),
           corr_value=NA,sid1=series_id) %T>%
    write.csv(.,paste0(master_dir,"Data/data_save/",sid,".csv"))
  dfs[[paste0(sid)]] <<- data

  data = data %>%
    summarize(title=paste0(title[1],"---",units_short[1]),sid1=sid1[1],
              source=series_codes$Source[series_codes$series_id==sid],
              type=series_codes$type[series_codes$series_id==sid],
              recent_values=list(na.omit(level[date>"2017-12-31"])),
              last_updated=as.Date(last_updated[1]),
              next_update=last(release_dates$date[release_dates$release_id==series_codes$release_id[series_codes$series_id==sid]&release_dates$date>Sys.Date()]),
              forecast=NA,
              frequency=frequency[1],
              last_update_for=first(date[!is.na(level)]),
              latest_value=round(level[!is.na(level)][1],digits=2),
              previous_value = NA,
              pop_change=NA,
              d365_change=NA,
              series_max=NA,
              series_min=NA,
              growth_max=NA,
              growth_min=NA) %>%
    ungroup()

  return(data)

}

# make function to plot data
plot_data = function(data,variable,start_date=-Inf,end_date=Inf){

  ggplot(data %>% filter(date>=start_date&date<=end_date&series_id==variable),aes_string(x="date",y="value")) +
    geom_line(size=2,color=rgb(0,140,204,maxColorValue=255)) +
    labs(x="",y=paste0((data %>% filter(date>=start_date&date<=end_date&series_id==variable))$title[1],"\n(",(data %>% filter(date>=start_date&date<=end_date&series_id==variable))$units_short[1],")"),caption=paste0("Data updated as of ",as.Date((data %>% filter(date>=start_date&date<=end_date&series_id==variable))$last_updated[1])))

}



# make function to generate summary table for a given release
release_table = function(sid){

  require(reactablefmtr)
  require(lubridate)
  require(htmltools)
  require(crosstalk)

  table_a = do.call("bind_rows",lapply(sid,match_function)) %>%
    relocate(recent_values,.after=title) %>%
    relocate(last_updated,.after=forecast) %>%
    relocate(type,.before=title) %>%
    mutate(color_pal=case_when((strftime(next_update, format = "%V")==strftime(Sys.Date(), format = "%V")&last_updated<Sys.Date()&frequency!="Daily"&frequency!="Weekly")|((next_update>Sys.Date())&(as.numeric(strftime(next_update, format = "%V"))==as.numeric(strftime(Sys.Date(), format = "%V"))+1)&frequency!="Daily"&frequency!="Weekly")~"#E75400",
                               (strftime(last_updated, format = "%V")==strftime(Sys.Date(), format = "%V")&last_updated<=Sys.Date()&frequency!="Daily"&frequency!="Weekly")~"#29AF7FFF",
                               TRUE~"#FFFFFF")) %>%
    mutate(vis_filter = ifelse(color_pal=="#29AF7FFF"&frequency!="Daily"&frequency!="Weekly"|(strftime(next_update, format = "%V")==strftime(Sys.Date(), format = "%V")&last_updated<Sys.Date()&frequency!="Daily"&frequency!="Weekly"),"Updated This Week",ifelse(color_pal!="#FFFFFF"&frequency!="Daily"&frequency!="Weekly","Next Week",NA)),
           sort_filter = ifelse(color_pal=="#29AF7FFF"&frequency!="Daily"&frequency!="Weekly"|(strftime(next_update, format = "%V")==strftime(Sys.Date(), format = "%V")&last_updated<Sys.Date()&frequency!="Daily"&frequency!="Weekly"),1,ifelse(color_pal!="#FFFFFF"&frequency!="Daily"&frequency!="Weekly",2,3))) %>%
    relocate(vis_filter,.before=type) %>%
    arrange(sort_filter,color_pal,next_update,desc(last_updated))

  assign('save_table_a',table_a,envir=parent.frame())

  data = SharedData$new(table_a)

  return(data)
}

vis_table = function(data,save_table_a,table_a){

  with_tooltip <- function(value, tooltip) {
    tags$abbr(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
              title = tooltip, value)
  }

  type_filter1 <- filter_checkbox(
    id = "type",
    label = "Type",
    sharedData = data,
    group =  ~ type,
    inline = TRUE)
  type_filter2 <- filter_checkbox(
    id = "vis_filter",
    label = "Updated?",
    sharedData = data,
    group =  ~ vis_filter,
    inline = TRUE)

  test = reactable(data,
                   defaultPageSize = 200,
                   theme=fivethirtyeight(),
                   resizable=TRUE,
                   #groupBy = "vis_filter",
                   filterable=TRUE,
                   compact=TRUE,
                   selection = "single",
                   selectionId = "tableid1",
                   onClick="select",
                   #defaultExpanded = TRUE,
                   columns = list(.selection=colDef(show=FALSE),
                                  color_pal=colDef(show=FALSE),
                                  title=colDef(name=""),
                                  type=colDef(show=FALSE),
                                  frequency=colDef(show=FALSE),
                                  sort_filter=colDef(show=FALSE),
                                  sid=colDef(show=FALSE),
                                  vis_filter=colDef(show=FALSE),
                                  next_update=colDef(name="Next update on:",cell=color_tiles(table_a,color_ref="color_pal"),header = with_tooltip("Next update on:", "Green if updated this week; Red if updated within 2 weeks")),
                                  last_updated=colDef(name="Last updated on:",cell=color_tiles(table_a,color_ref="color_pal"),header = with_tooltip("Last updated on:", "Green if updated this week; Red if updated within 2 weeks")),
                                  forecast=colDef(name="Market expectation",show=FALSE),
                                  last_update_for=colDef(name="For:"),
                                  latest_value=colDef(name="Most recent level"),
                                  previous_value=colDef(name="Previous level"),
                                  pop_change=colDef(name="PoP Change",header = with_tooltip("PoP Change", "Period-over-period change")),
                                  d365_change=colDef(name="YoY Change",header = with_tooltip("YoY Change", "Year-over-year change")),
                                  series_max=colDef(name="Hist. Max Level",show=FALSE),
                                  series_min=colDef(name="Hist. Min Level",show=FALSE),
                                  growth_max=colDef(name="Hist. Max Annual Change",show=FALSE),
                                  growth_min=colDef(name="Hist. Min Annual Change",show=FALSE),
                                  recent_values = colDef(name="",cell = react_sparkline(table_a,
                                                                                        height = 80,
                                                                                        decimals = 1,
                                                                                        statline = "mean",
                                                                                        statline_color = "red",
                                                                                        bandline = "innerquartiles",
                                                                                        bandline_color = "darkgreen"))))


  output = bscols(list(type_filter1,type_filter2,htmltools::tagList(test)))

  #temp_release <- tempfile(pattern = "test", fileext = ".html")
  #htmltools::save_html(output, temp_release)
  #rmarkdown::pandoc_self_contained_html(input = temp_release, output = paste0(master_dir,"release",Sys.Date(),".html"))

  return(output)

}

dfs <<- list()
data_release_table = release_table(series_codes$series_id[series_codes$growth=="m_growth"|series_codes$growth=="m_change"|series_codes$growth=="w_growth"|series_codes$growth=="w_change"|series_codes$growth=="q_growth"|series_codes$growth=="q_change"|series_codes$growth=="d_growth"|series_codes$growth=="d_change"])

save(data_release_table,dfs,save_table_a,vis_table,release_dates,series_codes,file=paste0(master_dir,"release_data.RData"))
