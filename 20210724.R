#パッケージをインストールしていない場合は先にインストール
install.packages("tidyverse")


#libraryの読み込み
library(tidyverse) #今回は特にdplyr,tidyr,stringr,purrrを使用
library(readxl) #excelの読み込みに使用


#施設概要表のデータの確認

#とりあえず1つ読み込んでみる
read_excel("input/summary/summary2019.xlsx") %>% 
  head()

read_excel("input/summary/summary2019.xlsx") %>% 
  select(1:3,施設名) %>% View() #改行や記号がいらない

#読み込む
summary2019 <- 
  read_excel("input/summary/summary2019.xlsx") %>% 
  select(1:3,施設名) %>% 
  mutate(年度 = 2019)
View(summary2019)

colnames_summary <- c("告示番号", "通番", "市町村番号", "施設名", "年度")
colnames(summary2019) <- colnames_summary
View(summary2019)


#スライド31へ

#これを関数にする
readsummary <- function(file){
  colnames_summary <- c("告示番号", "通番", "市町村番号", "施設名", "年度")
  input_dir <- "input/summary/"
  path <- paste0(input_dir,file)
  data <- 
    read_excel(path) %>% 
    select(1:3,施設名) %>% 
    mutate(年度 = 2019)
  colnames(data) <- colnames_summary
  return(data)
}


readsummary("summary2019.xlsx")
readsummary("summary2018.xlsx")

#自動的に年度を入れたい→ファイル名から年度を抽出できないか？→stringr
year <- str_extract("summary2019.xlsx","[\\d]{4}")
print(year)

#関数に加える
readsummary <- function(file){
  input_dir <- "input/summary/"
  path <- paste0(input_dir,file)
  colnames_summary <- c("告示番号", "通番", "市町村番号", "施設名", "年度")
  year <- str_extract(file,"[\\d]{4}")
  data <- 
    read_excel(path) %>% 
    select(1:3,施設名) %>% 
    mutate(年度 = year)
  colnames(data) <- colnames_summary
  return(data)
}

readsummary("summary2018.xlsx")

#スライド32へ

#joinでつなげる
readsummary("summary2019.xlsx") %>% 
  left_join(readsummary("summary2018.xlsx"),by=c("通番" = "告示番号")) %>% View()

readsummary("summary2019.xlsx") %>% 
  left_join(readsummary("summary2018.xlsx"),by=c("通番" = "告示番号"),suffix=c("","2018")) %>% View()

readsummary("summary2019.xlsx") %>% 
  left_join(readsummary("summary2018.xlsx"),by=c("通番" = "告示番号"),suffix=c("","2018")) %>% 
  left_join(readsummary("summary2017.xls"),by=c("通番2018" = "告示番号"),suffix=c("","2017"))  %>% View()

readsummary("summary2019.xlsx") %>% 
  left_join(readsummary("summary2018.xlsx"),by=c("通番" = "告示番号"),suffix=c("","2018")) %>% 
  left_join(readsummary("summary2017.xls"),by=c("通番2018" = "告示番号"),suffix=c("","2017")) %>% 
  left_join(readsummary("summary2016.xls"),by=c("通番2017" = "告示番号"),suffix=c("","2016")) %>% View()

summary_master <-
  readsummary("summary2019.xlsx") %>% 
  left_join(readsummary("summary2018.xlsx"),by=c("通番" = "告示番号"),suffix=c("","2018")) %>% 
  left_join(readsummary("summary2017.xls"),by=c("通番2018" = "告示番号"),suffix=c("","2017")) %>% 
  left_join(readsummary("summary2016.xls"),by=c("通番2017" = "告示番号"),suffix=c("","2016")) %>% 
  rename(
    告示番号2019=告示番号,
    告示番号2018=通番2018,
    告示番号2017=通番2017,
    告示番号2016=通番2016,
    施設名_基準 = 施設名) %>% 
  select(contains("告示番号"), 市町村番号, 施設名_基準) 
View(summary_master)


summary_master <-
  summary_master %>% 
  distinct() %>% #重複行を消す
  filter(str_detect(告示番号2019,"^[0-9]"))#数値で始まる行のみ残す
write.csv(summary_master,"output/summary/summary_master.csv",row.names = FALSE)

#スライド34へ

#MDC_02_8_07
#筋骨格系：MDC07

path <- "input/MDC_02_07/MDC_02_8_07_2019.xlsx.xlsx"

temp <- read_excel("input/MDC_02_07/MDC_02_8_07_2019.xlsx",
                   col_names = FALSE,
                   n_max = 4)
View(temp)

tibble(
  a=as.character(temp[1,]),
  b=as.character(temp[2,]),
  c=as.character(temp[3,]),
  d=as.character(temp[4,])) %>% 
  mutate(MDC="MDC") %>% 
  select(MDC,everything()) %>% View()

temp_colname <- 
  tibble(
    a=as.character(temp[1,]),
    b=as.character(temp[2,]),
    c=as.character(temp[3,]),
    d=as.character(temp[4,])) %>% 
  mutate(MDC="MDC") %>% 
  select(MDC,everything())

#fillで埋めてuniteでつなげる
temp_colname %>%
  fill(everything()) %>% 
  unite(col = "name",everything()) %>% View()

temp_colname %>%
  fill(everything()) %>% 
  unite(col = "name",everything()) %>% pull(name)

colnames <- 
  temp_colname %>%
  fill(everything()) %>% 
  unite(col = "name",everything()) %>% pull(name)

colnames %>% 
  str_replace_all(.,"MDC_NA_NA_NA_","") %>% 
  str_replace_all(.,"[。\\s]","")

#関数にする
get_colname <- 
  function(file){
    temp <- read_excel(file,
                       col_names = FALSE,
                       n_max = 4)
    
    temp_colname <- 
      tibble(
        a=as.character(temp[1,]),
        b=as.character(temp[2,]),
        c=as.character(temp[3,]),
        d=as.character(temp[4,])) %>% 
      mutate(MDC="MDC") %>% 
      select(MDC,everything())
    
    colnames <- 
      temp_colname %>%
      fill(everything()) %>% 
      unite(col = "name",everything()) %>% .$name
    
    col_names <- 
      colnames %>% 
      str_replace_all(.,"MDC_NA_NA_NA_","") %>% 
      str_replace_all(.,"[。\\s]","")
    
    return(col_names)}
get_colname("input/MDC_02_07/MDC_02_8_07_2019.xlsx")

#スライド42へ

#表記揺れを探す
temp2016 <- c(get_colname("input/MDC_02_07/MDC_02_8_07_1_2016.xls"),get_colname("input/MDC_02_07/MDC_02_8_07_2_2016.xls"))
temp2017 <- c(get_colname("input/MDC_02_07/MDC_02_8_07_1_2017.xls"),get_colname("input/MDC_02_07/MDC_02_8_07_2_2017.xls"))
temp2018 <- get_colname("input/MDC_02_07/MDC_02_8_07_2018.xlsx")
temp2019 <- get_colname("input/MDC_02_07/MDC_02_8_07_2019.xlsx")

setdiff(temp2016,temp2017)
setdiff(temp2017,temp2016)
setdiff(temp2016,temp2018)
setdiff(temp2018,temp2016)
setdiff(temp2016,temp2019)
setdiff(temp2019,temp2016)
setdiff(temp2017,temp2018)
setdiff(temp2018,temp2017)
setdiff(temp2017,temp2019)
setdiff(temp2019,temp2017)
setdiff(temp2018,temp2019)
setdiff(temp2019,temp2018)

#データの読み込み
input_dir <- "input/MDC_02_07/"
input_file <- "MDC_02_8_07_2019.xlsx"
path <- paste0(input_dir,input_file)
temp <- read_excel(path,
                   col_names = get_colname(path),
                   skip = 4)
temp %>% View()

#スライド46へ

#いらない列を外す
temp %>% 
  select(!contains("在院日数")) %>% 
  select(!contains("再掲")) %>% 
  mutate(suppressWarnings(across(contains("件数"), parse_number))) %>% 
  mutate(across(contains("件数"),~replace_na(.x,0))) %>% View()

#これでもOK
temp %>% 
  select(!contains("在院日数")) %>% 
  select(!contains("再掲")) %>% 
  mutate(suppressWarnings(across(contains("件数"), ~parse_number(.x)))) %>% 
  mutate(across(contains("件数"),~replace_na(.x,0))) %>% View()

#関数にする
WranglingMDC02x <- 
  function(file){
    input_dir <- "input/MDC_02_07/"
    file_name <- file
    path <- paste0(input_dir,file)
    temp <- read_excel(path,
                       col_names = get_colname(paste0(input_dir,file_name)),
                       skip = 4)
    temp <- 
      temp %>% 
      select(!contains("在院日数")) %>% 
      select(!contains("再掲")) %>% 
      mutate(suppressWarnings(across(contains("件数"), parse_number))) %>% 
      mutate(across(contains("件数"),~replace_na(.x,0)))
    return(temp)
  }

#スライド52へ

#施設概要表とjoin。一番右に最新年の施設名が載る
input_file <- "MDC_02_8_07_2_2017.xls"

WranglingMDC02x(input_file) %>% 
  left_join(summary_master, by=c("告示番号"="告示番号2017")) %>% View()

temp <- 
  WranglingMDC02x(input_file) %>% 
  select(-施設名) %>% #施設名を抜く
  mutate(年度=2017) %>% #年度をつける
  left_join(summary_master, by=c("告示番号"="告示番号2017")) %>% #告示番号を合わせてleft_join
  rename(施設名 = 施設名_基準) %>% #施設名_基準を施設名にする
  select(告示番号,市町村番号,施設名,年度,starts_with("MDC")) #いるデータだけ選び並び替える
View(temp)

#csvファイルを作ってみる
output_dir <- "output/MDC_02_07/"
output_file <- str_extract(input_file,"MDC.*[\\d]{4}")
output_path <- paste0(output_dir,output_file,".csv")
write.csv(temp,output_path,row.names = FALSE) 

#csvファイルを作る関数にする
ReadMDC02x <-
  function(input_file,summary){
    input_dir <- "input/MDC_02_07/"
    path <- paste0(input_dir,input_file)
    year <- str_extract(input_file,"[\\d]{4}")
    
    temp <-
      WranglingMDC02x(input_file) %>% 
      select(-施設名) %>% #施設名を抜く
      mutate(年度=year) %>% #年度をつける
      left_join(summary, by=c("告示番号"=paste0("告示番号",year))) %>% #告示番号を合わせてleft_join
      rename(施設名 = 施設名_基準) %>% #施設名_基準を施設名にする
      select(告示番号,市町村番号,施設名,年度,starts_with("MDC")) #いるデータだけ選び並び替える
    
    output_dir <- "output/MDC_02_07/"
    output_file <- str_extract(input_file,"MDC.*[\\d]{4}")
    output_file_path <- paste0(output_dir,output_file,".csv")
    
    return(write.csv(temp,output_file_path,row.names = FALSE, fileEncoding = "UTF-8"))
    #return(write.csv(temp,output_file_path,row.names = FALSE, fileEncoding = "CP932")) 文字化けするならUTF-8をCP932に変更
  }

ReadMDC02x("MDC_02_8_07_2019.xlsx",summary_master)

#スライド54へ

list.files("input/MDC_02_07/")


list.files("input/MDC_02_07/", pattern = "x.*") #pattern=を入れると選択できる
files <- list.files("input/MDC_02_07/", pattern = "x.*") 

#フォルダ内にあるExcelファイルを全部csvファイルにする
map(files,~ReadMDC02x(.x,summary_master))

temp <- read_csv("output/MDC_02_07/MDC_02_8_07_2_2017.csv")
temp %>% View()

#スライド57へ


temp %>% 
  pivot_longer(cols = starts_with("MDC"),names_to = c("names"),values_to = "件数")  %>% View()


temp %>% 
  pivot_longer(
    cols = starts_with("MDC"),
    names_sep = "_",
    names_to = c("a","MDC","病名","b","サブ"), 
    values_to = "件数")  %>% View()


#スライド64へ

temp %>% 
  pivot_longer(
    cols = starts_with("MDC"),
    names_sep = "_",
    names_to = c("a","MDC","病名","b","サブ"), 
    values_to = "件数")  %>% 
  select(-a,-b) %>% 
  mutate(病名=ifelse((年度 %in% c(2016,1017) & MDC =="070560"),"重篤な臓器病変を伴う全身性自己免疫疾患",病名)) %>% 
  mutate(手術=ifelse(サブ==99,"なし","あり")) %>% View()



list.files("output/MDC_02_07/")
csvfiles <- list.files("output/MDC_02_07/", pattern = "csv")

input_dir <- "output/MDC_02_07/"
MDC_02_07_all <-
  map_dfr(csvfiles,~
            read_csv(paste0(input_dir,.x)) %>% 
            pivot_longer(
              cols = starts_with("MDC"),
              names_sep = "_",
              names_to = c("a","MDC","病名","b","サブ"), 
              values_to = "件数")  %>% 
            select(-a,-b) %>% 
            mutate(病名=ifelse((年度 %in% c(2016,1017) & MDC =="070560"),"重篤な臓器病変を伴う全身性自己免疫疾患",病名)) %>% 
            mutate(手術=ifelse(サブ==99,"なし","あり")))

View(MDC_02_07_all)

#スライド
