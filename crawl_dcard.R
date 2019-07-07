# source('/Users/dtseng02/Documents/Initial Func(1).R')
library(tidyverse)
library(rvest)
library(httr)
library(beepr)

get_colnames <- function(df, .collapse = ", ") {
  final <- df %>% colnames() %>% str_c(collapse = .collapse)
  return(final)
}

###1. yahoo相關關鍵字丟到dcard search
url_raw <- "https://www.dcard.tw/search/general?query=yahoo"
html_raw <- url_raw %>% read_html()

###2. index data frame 創建, 加上標題, 連結, id等column
index_title <- html_raw %>% html_nodes(".PostEntry_unread_2U217-") %>% html_text()
index_link <- html_raw %>% html_nodes(".PostEntry_root_V6g0rd") %>% html_attr("href")
index_board <- html_raw %>% html_nodes(".PostEntry_forum_1m8nJA") %>% html_text()
index_author <- html_raw %>% html_nodes(".hSAyoj") %>% #加這個是避免跑到上面的話題跟卡稱
  html_nodes(".PostAuthor_root_3vAJfe") %>% html_text()
# index_reacations <- html_raw %>% html_nodes(".hlvyVg") %>% html_text()
# index_reply <- html_raw %>% html_nodes(".PostEntry_comments_2iY8V3") %>% html_text()
# 有些文沒有回應所以換個方式
index_meta <- html_raw %>% html_nodes(".PostEntry_meta_1lGUFm") %>% html_text()
# index_excerpt <- html_raw %>% html_nodes(".PostEntry_excerpt_2eHlNn") %>% html_text()
# index_reply <- html_raw %>% html_nodes(".PostEntry_reply_1oU-6z") %>% html_text()
# 有些摘要跑出來是文字摘要但有些是特定文章回應所以換個方式
index_content <- html_raw %>% html_nodes(".PostEntry_content_g2afgv") %>% html_text()

df_index <- tibble(index_title,index_link,index_board,index_author,index_meta,index_content) %>%
  mutate(index_link = str_c("https://www.dcard.tw", index_link)) %>%
  mutate(index_content = str_remove(index_content, index_title)) %>%
  mutate(index_content = str_remove(index_content, index_meta)) %>%
  mutate(index_content = str_remove_all(index_content, "\\\n")) %>%
  rename(index_excerpt = index_content) %>%
  mutate(id = row_number()) %>%
  select(index_board, index_title, index_excerpt, index_author, index_meta, index_link, id)


###3. 拿index去抓每篇文, 記得加上ID

#前置作業: crawler function and df pre-wrote
p_read_html <- possibly(read_html, otherwise = NULL)

df_article <- tibble(article_link='', article_board='', article_title='', article_author='', article_author_school='', article_text='', article_love='', article_reply='', article_date="") %>% filter(row_number()<1)
df_comment <- list()#tibble(comment_floor='', comment_author_school='', comment_love='', comment_text='', comment_time="") %>% filter(row_number()<1)
id_gg <- vector()

#for loop
for(i in 1:2) {
  # i = 2
  j = (i*10) - 9
  k = j + 9
  
  #raw html
  html_raw = df_index[j:k,] %>% pull(index_link) %>% 
    map(function(x){x %>% p_read_html()}) %>% 
    set_names(pull(df_index[j:k,"id"])) %>% compact()
  
  # 測試讀不到的網站
  # html_raw_t <- c(df_index[j:k,] %>% pull(index_link),
  #   "https://www.dcard.tw/f/mood/p/231592200") %>% p_read_html()
  # "https://www.dcard.tw/f/mood/p/231592200" %>% p_read_html()
  
  #怕有些連結死掉了雖然real time的應該不太會但是保險起見
  html_raw_index <- html_raw %>% 
    map(function(x){x %>% html_nodes(".Post_title_2O-1el") %>% html_text()}) %>%
    map_lgl(function(x){!is.na(x)})
  # id_gg <- c(html_raw_index[!html_raw_index] %>% names(), id_gg)
  
  html_raw_f <- html_raw[html_raw_index]
  
  #article df
  article_id <- names(html_raw_f)
  article_title <- html_raw_f %>% 
    map(function(x){x %>% html_nodes(".Post_title_2O-1el") %>% html_text()})
  article_author <- html_raw_f %>% 
    map(function(x){x %>% html_nodes(".PostHeader_uid_3g_pzg") %>% html_text()}) %>%
    map(function(x){if(length(x)==0) x = NA_character_ else(x)})
  article_author_school <- html_raw_f %>% 
    map(function(x){x %>% html_nodes(".PostHeader_author_3AAMDh .PostAuthor_root_3vAJfe") %>% html_text()}) %>%
    map(function(x){if(length(x)==0) x = NA_character_ else(x)})
  article_board <- html_raw_f %>% 
    map(function(x){x %>% html_nodes(".Post_forum_1YYMfp") %>% html_text()})
  article_text <- html_raw_f %>% 
    map(function(x){x %>% html_nodes(".Post_root_23_VRn") %>% html_text()})
  article_love <- html_raw_f %>% 
    map(function(x){x %>% html_nodes(".jTOuHc") %>% html_text()})
  article_reply <- html_raw_f %>% 
    map(function(x){x %>% html_nodes(".DJrdA") %>% html_text()})
  article_date <- html_raw_f %>% 
    map(function(x){x %>% html_nodes(".Post_date_2ipeYS") %>% html_text()})
  #article_link <- df_index[j:k,] %>% pull(index_link)
  
  df_article_tmp <- tibble(article_board=unlist(article_board),
                           article_title=unlist(article_title),
                           article_author=unlist(article_author),
                           article_author_school=unlist(article_author_school),
                           article_text=unlist(article_text),
                           article_love=unlist(article_love),
                           article_reply=unlist(article_reply),
                           article_date=unlist(article_date),
                           article_id = article_id)
  
  comment_floor <-  html_raw_f %>% 
    map(function(x){x %>% html_nodes(".CommentEntry_floor_VtDUyr") %>% html_text()})
  comment_author_school <-  html_raw_f %>% #.PostAuthorHeader_author_3O30xu span
    map(function(x){x %>% html_nodes(".CommentEntry_author_2BO0i1 .PostAuthorHeader_author_3O30xu span") %>% html_text()})
  comment_love <-  html_raw_f %>% 
    map(function(x){x %>% html_nodes(".jnzHNd") %>% html_text()})
  comment_text <-  html_raw_f %>% 
    map(function(x){x %>% html_nodes(".CommentEntry_content_1ATrw1") %>% html_text()})
  comment_time <-  html_raw_f %>% 
    map(function(x){x %>% html_nodes(".CommentEntry_date_2E4vYF") %>% html_text()})
  
  #comment_love暫時不加入，因為還沒辦法處理刪文的愛心
  df_comment_tmp <- pmap(list(comment_floor,comment_author_school,comment_text,comment_time),function(a,b,c,d)
    {bind_cols(comment_floor=unlist(a),comment_author_school=unlist(b),comment_text=unlist(c),comment_time=unlist(d))})
  
  # comment_author_school %>% map(function(x){x %>% length()}) %>% unlist() 
  # comment_love %>% map(function(x){x %>% length()}) %>% unlist() 
  # comment_floor %>% map(function(x){x %>% length()}) %>% unlist() 
  # comment_text %>% map(function(x){x %>% length()}) %>% unlist() 
  # comment_time %>% map(function(x){x %>% length()}) %>% unlist() 
  
  df_comment <- c(df_comment, df_comment_tmp)
  df_article <- df_article %>% bind_rows(df_article_tmp)
  print(i)
  
  tmsleep <- sample(5:7,1)
  Sys.sleep(tmsleep)
}

#關掉用不到的連線
closeAllConnections()
gc()

###4. 分成主文跟回文的data frame
df_index %>% dim()
df_article %>% dim()
df_comment %>% length()

df_index %>% 
  mutate(hyperlink = str_c('=HYPERLINK("',index_link,'","',index_title,'")')) %>%
  select(-id) %>%
  write_csv("/Users/dtseng02/Documents/CRM/cralwer/df_index.csv")
df_article %>% write_csv("/Users/dtseng02/Documents/CRM/cralwer/df_article.csv")

df_article_comment_raw <- 
  df_article %>%
  left_join(df_comment %>% bind_rows(.id = "article_id"), by = "article_id")

df_article_check <- read_csv("/Users/dtseng02/Documents/CRM/cralwer/df_article.csv")
###5. 資料清洗後上傳到google drive
library(googledrive)
library(googlesheets4)

gd_crawler_dcard <- drive_get("~/crawler/crawler_dcard")
gd_crawler_dcard <- drive_get("~/crawler/crawler_dcard")
gd_crawler_dcard %>% drive_update(media = "/Users/dtseng02/Documents/CRM/cralwer/df_index.csv")

###6. 每小時重複task
library(cronR)
df_article %>% dim()

###7. 寫信通知
install.packages("gmailr")

df_article %>% clipr::write_clip()

http://www.cninsights.com/news/detail.aspx?tid=1601
