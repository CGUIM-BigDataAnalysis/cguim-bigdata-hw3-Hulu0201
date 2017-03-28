長庚大學 大數據分析方法 作業三
================

網站資料爬取
------------

``` r
#install.packages("rvest")
library(rvest)
```

    ## Warning: package 'rvest' was built under R version 3.3.3

    ## Loading required package: xml2

    ## Warning: package 'xml2' was built under R version 3.3.3

``` r
p1<- "https://www.ptt.cc/bbs/LoL/index.html"
p2<- "https://www.ptt.cc/bbs/LoL/index7078.html"
p3<- "https://www.ptt.cc/bbs/LoL/index7077.html"
p4<- "https://www.ptt.cc/bbs/LoL/index7076.html"
p5<- "https://www.ptt.cc/bbs/LoL/index7075.html"
p6<- "https://www.ptt.cc/bbs/LoL/index7074.html"
PttLOL <-c(p1,p2,p3,p4,p5,p6)
dataframAll <-NULL
for(i in PttLOL){
    PttLOLContent<-read_html(i)
post_title <- PttLOLContent %>% 
html_nodes(".title") %>% 
html_text()
post_pushnumber<- PttLOLContent %>% 
html_nodes(".nrec") %>% 
html_text()
post_author<- PttLOLContent %>% 
html_nodes(".author") %>% 
html_text()
    datafram <- data.frame(Title = post_title, PushNum=post_pushnumber, 
                           Author=post_author)
    dataframAll <- rbind(dataframAll,datafram)
    next 
}

PttLOL_posts <- dataframAll
#knitr::kable(
#    PttLOL_posts[1:100,c("Title","PushNum","Author")])
```

爬蟲結果呈現
------------

``` r
knitr::kable(
    PttLOL_posts[1:100,c("Title","PushNum","Author")]) ##請將iris取代為上個步驟中產生的爬蟲資料資料框
```

| Title                                             | PushNum | Author       |
|:--------------------------------------------------|:--------|:-------------|
| \[閒聊\] 同人圖分享-板上陰氣太重 陽氣太少         | 2       | f222051618   |
| \[實況\] maki睡不飽 好聲音台                      |         | Andy7577272  |
| \[實況\] 細雪緋悠 馬爾音樂台 (鄉民NG歡樂場        |         | gzzzneww     |
| \[實況\] 打趴你的電競夢 上野韓服RANK              |         | luckylin1798 |
| Re: \[問題\] 為什麼天賦要點一點吸血?              | 5       | gunman321    |
| \[公告\] LoL 樂透開獎                             |         | \[彩券\]     |
| \[公告\] LoL 板 開始舉辦樂透!                     | 3       | rainnawind   |
| Re: \[問題\] 為什麼天賦要點一點吸血?              |         | ccrhalp      |
| \[公告\] LoL║英雄 ┌──┐３月閒聊區┌→                | 93      | rainnawind   |
| \[公告\] LoL║ 聯盟┘板規└─────┘                    |         | rainnawind   |
| \[電競\] 近期賽事                                 | 42      | superRKO     |
| Re: \[閒聊\] 媽寶的實力                           | 3       | RayShigeno   |
| Re: \[閒聊\] 優秀的打野選手需要哪些能力?          | 2       | s505015      |
| \[實況\] EggRoIl / FW Betty 正在打韓服喔          | 23      | maybe158003  |
| \[閒聊\] 像超粒方支持LOL選手可行嗎                | X1      | alecklu      |
| \[問題\] 有沒有加里歐的大絕<英靈之門>的八卦??!    | 16      | MiamiKotori  |
| \[實況\] 辛吉德了便宜還賣乖                       | 1       | Tiamat6716   |
| \[實況\] 爆到生活無法自理 113最強輔助             |         | sky082       |
| \[閒聊\] 英雄聯盟FB                               | 14      | TrashTalking |
| \[閒聊\] XG siusiu這個選手                        | 8       | weizen16     |
| \[實況\]悼文吟誦者 大師中輔台                     |         | jack41725    |
| \[閒聊\] 如果要選世界前五中路，大家的名單是？     | 19      | yilafi       |
| (本文已被刪除) \[YoruHentai\]                     |         | -            |
| \[閒聊\] 貝克講解 LCK W9D4 SKT VS SSG GAME 2      | 4       | toaykoyo     |
| \[問題\] S3的TPA與JTeam的相似之處                 |         | xup654z      |
| \[閒聊\] 西門是努力型的天才嗎?                    |         | kiversonx17  |
| (本文已被刪除) \[Asiankappa\]                     |         | -            |
| \[揪團\] NG歡樂場！！OWO                          | 5       | asthedew     |
| (本文已被刪除) \[iamwhoim\]                       | 6       | -            |
| \[實況\] 加藤鷹架 總算要過200000了                |         | pepsihong10  |
| \[閒聊\] HKE的問題                                | 1       | koloerty     |
| Re: \[閒聊\] 三娜之中，角色操作難度順序？         | 3       | fdfdfdfd51   |
| Re: \[閒聊\] FOFO跟西門交易的話 算是兩全其美嗎?   |         | lovekula     |
| \[實況\] 漂亮弟弟 台南周子瑜清純開台              |         | bleachman    |
| Re: \[閒聊\] 克萊門 : 丁特若回歸 HKE保證第三      |         | s111228s     |
| Re: \[閒聊\] 有鑽框白金框是不是一種原罪           | 4       | rainyday1908 |
| \[問題\] 該怎麼教導新手打英雄                     | 11      | jack880828   |
| \[實況\] 吉力老師，久沒開台打個小號壓壓驚         | 1       | fufu1818     |
| \[問題\] 西門到底哪裡出了問題?                    | 20      | cotter87     |
| \[揪團\] 打打彈性積分                             |         | a22873756    |
| \[閒聊\] 【滑新聞】服務費立法為工資？商總：定     | X8      | tigerclamp   |
| \[實況\] 我在減肥只是看起來不大像/MiSTakE         | 9       | PibaoN       |
| \[揪團\] 單雙積分衝大師<sub>~</sub>               | 7       | andy31313    |
| Re: \[閒聊\] 有鑽框白金框是不是一種原罪           | 1       | cchou0114    |
| Re: \[閒聊\] 有鑽框白金框是不是一種原罪           | 7       | fdfdfdfd51   |
| Re: \[閒聊\] 【滑新聞】服務費立法為工資？商總：定 |         | BHAIRE       |
| \[閒聊\] 西門會怎麼經營youtube呢?                 | 9       | Comebuy      |
| \[閒聊\] Lck第八周比賽精華                        |         | Batterygod   |
| \[揪團\] 開心玩就好                               | 2       | lonetimeago  |
| \[閒聊\] FW FB (聊聊峽谷W7)                       | 11      | bobyhsu      |
| \[實況\] TPS King / GodJJ                         | 19      | eqer         |
| \[閒聊\] 靠北實況主 &lt;有搞頭嗎 ?                |         | SODAECHO1    |
| Re: \[閒聊\] 克萊門 : 丁特若回歸 HKE保證第三      | 9       | asyan        |
| \[實況\] 口口口 口口口 / EDG Meiko                | 4       | rosalic0423  |
| Re: \[閒聊\] 為何Maple可以一直維持在顛峰狀態?     | 20      | VVVV5555     |
| Re: \[閒聊\] 優秀的打野選手需要哪些能力?          | 8       | yowhatsupsli |
| \[揪團\] 鑽V單雙積分                              | 20      | jacky199703  |
| \[閒聊\] 中國主辦世界大賽會不會播放義勇軍進行曲   | 27      | hochengyuan  |
| \[閒聊\] 早上看NA比賽下路暗影島雙人回來了??!      | 10      | MiamiKotori  |
| \[閒聊\] 有鑽框白金框是不是一種原罪               | 5       | PUAstyle     |
| \[情報\] 英雄透視鏡：禦國巨像 ─ 加里歐！          | 8       | BRITAINCAT   |
| \[閒聊\] 丁特 FB                                  | X2      | YamCha       |
| \[閒聊\] FOFO跟西門交易的話 算是兩全其美嗎?       | 19      | qo6su3fm4    |
| Re: \[閒聊\] 為何Maple可以一直維持在顛峰狀態?     | 19      | gakkag       |
| \[閒聊\] 職業戰隊該有課外活動嗎?有什麼活動?       | 4       | godshibainu  |
| \[問題\] 艾希帶新軍閥                             | 10      | minibaby1121 |
| \[實況\] 小小關 報復台服 不配合直接送頭           |         | lovesaber    |
| \[實況\] 口口口口 / EDG Scout (團練關台           | 1       | Fishkeke     |
| \[閒聊\] 三娜之中，角色操作難度順序？             | 80      | racksold     |
| Re: \[閒聊\] 為何Maple可以一直維持在顛峰狀態?     | 1       | wsxwsx0426   |
| Re: \[閒聊\] 三娜之中，角色操作難度順序？         | 13      | diefish5566  |
| Re: \[問題\] 歐洲人是不是比較有創造力？           | 3       | coox         |
| \[問題\] 加里歐JG好不好用                         | 27      | ss8901234    |
| \[閒聊\] 厄薩斯最近是不是回穩了？                 | 18      | Sasamumu     |
| Re: \[問題\] 歐洲人是不是比較有創造力？           | 10      | ccyaztfe     |
| \[實況\] 路過的饅頭 金一開爬                      |         | Tulipsiaoyu  |
| \[實況\] 噯卑彌呼，韓服電梯向上！？               | 1       | Destinyandy  |
| \[問題\] 如果沒有lol動主播該何去何從？            | 30      | loveHyeri    |
| \[閒聊\] 虐實況主有多爽                           | X3      | KENDO777     |
| \[問題\] 國動的角色池是不是太淺了                 | X1      | ArtemXis     |
| (本文已被刪除) \[da8855\]                         | X4      | -            |
| \[問題\] 要怎麼改革潛伏在台服的港仔？             | 16      | GTOyoko5566  |
| \[閒聊\] 優秀的打野選手需要哪些能力?              | 82      | e807761566   |
| \[閒聊\] 團練時的教練                             |         | yoyododi     |
| \[實況\] MayJay 2017AD教學台                      | 4       | cat0201      |
| \[實況\] 大同嗨彼得 鑽4JG午安台                   |         | kingkingkong |
| \[實況\] FW Winds                                 | 17      | kalinlove    |
| \[情報\] 全新艾克百寶袋 加碼一週更優惠！          | 16      | rainyday1908 |
| \[閒聊\] SKT還是世界第一名嗎                      |         | zindqq       |
| \[閒聊\] 為何Maple可以一直維持在顛峰狀態?         | 71      | tw689        |
| Re: \[問題\] 歐洲人是不是比較有創造力？           | 15      | Flyroach     |
| \[電競\] 2017 LCS NA Spring W9D3 \#2 最後一天     | 23      | rainnawind   |
| \[發錢\] 阿卡阿卡莉 (截止)                        | 20      | diefish5566  |
| \[閒聊\] NA季賽最後一天競爭依舊激烈               | 16      | rainnawind   |
| \[閒聊\] 四年了，心也死了                         | X1      | daric9180    |
| \[公告\] LoL 板 開始舉辦樂透!                     | 1       | rainnawind   |
| \[問題\] 加里歐有BUG?                             | 17      | TokyoKind    |
| (本文已被刪除) \[duck03228\]                      |         | -            |
| \[招生\] 北美最大的線上語音系統上線啦             | 3       | Asiankappa   |
| \[公告\] LoL 樂透開獎                             | 2       | \[彩券\]     |

解釋爬蟲結果
------------

``` r
str(PttLOL_posts)
```

    ## 'data.frame':    111 obs. of  3 variables:
    ##  $ Title  : Factor w/ 97 levels "\n\t\t\t\n\t\t\t\t[公告] LoL 板 開始舉辦樂透!\n\t\t\t\n\t\t\t",..: 5 7 9 8 10 2 1 10 4 3 ...
    ##  $ PushNum: Factor w/ 33 levels "","2","3","42",..: 2 1 1 1 5 1 3 1 6 1 ...
    ##  $ Author : Factor w/ 94 levels "[彩券]","Andy7577272",..: 4 2 6 7 5 1 8 3 8 8 ...

``` r
#總共爬出111篇文章,其中置底的公告,討論,閒聊文也包含在內
table(PttLOL_posts$Author)
```

    ## 
    ##       [彩券]  Andy7577272      ccrhalp   f222051618    gunman321 
    ##            2            1            1            1            2 
    ##     gzzzneww luckylin1798   rainnawind     superRKO            - 
    ##            1            1            7            1            6 
    ##      alecklu     asthedew    jack41725  kiversonx17     koloerty 
    ##            1            1            1            1            1 
    ##  maybe158003  MiamiKotori  pepsihong10   RayShigeno      s505015 
    ##            1            2            1            1            1 
    ##       sky082   Tiamat6716     toaykoyo TrashTalking     weizen16 
    ##            1            1            1            1            1 
    ##      xup654z       yilafi    a22873756    andy31313   Batterygod 
    ##            1            1            1            1            1 
    ##       BHAIRE    bleachman      bobyhsu    cchou0114      Comebuy 
    ##            1            1            1            1            1 
    ##     cotter87         eqer   fdfdfdfd51     fufu1818   jack880828 
    ##            1            1            2            1            1 
    ##  lonetimeago     lovekula       PibaoN rainyday1908     s111228s 
    ##            1            1            1            2            1 
    ##   tigerclamp        asyan   BRITAINCAT  diefish5566     Fishkeke 
    ##            1            1            1            2            1 
    ##       gakkag  godshibainu  hochengyuan  jacky199703    lovesaber 
    ##            1            1            1            1            1 
    ## minibaby1121     PUAstyle    qo6su3fm4     racksold  rosalic0423 
    ##            1            1            1            1            1 
    ##    SODAECHO1     VVVV5555   wsxwsx0426       YamCha yowhatsupsli 
    ##            1            1            1            1            1 
    ##     ArtemXis      cat0201     ccyaztfe         coox  Destinyandy 
    ##            1            1            1            1            1 
    ##   e807761566     Flyroach  GTOyoko5566    kalinlove     KENDO777 
    ##            1            1            1            1            1 
    ## kingkingkong    loveHyeri     Sasamumu    ss8901234  Tulipsiaoyu 
    ##            1            1            1            1            1 
    ##        tw689     yoyododi       zindqq           A6       Aatrox 
    ##            1            1            1            1            1 
    ##   Asiankappa    daric9180    FenixShou mickeykiller  peter840606 
    ##            1            1            1            1            1 
    ##  Shushusnail     spicyway    TokyoKind   ym19950822 
    ##            1            1            1            1

``` r
#rainnawind所發的文章數最多,共有7篇.另外有一個數字是6,這是已被刪除的文章數目.
```

解釋解釋解釋解釋

``` r
#前情提要:我沒有再打LOL,跟他不熟
#置底的公告文的作者rainnawind推測應該是LOL版的版主
#已被刪除的文章,作者ID不會出現在Author的欄位 e.g.(本文已被刪除) [Asiankappa]
#LOL板跟有些版不太一樣,好像沒規定標題[黑特]才可以噓文,感覺不容易被板龜咬
#在計算哪個作者發文數最多時,觀察到好少有人發文篇數是大於2,推斷是LOL版的活躍度相當高,常常有人在上面發問,閒聊,開實況,或是揪團. 
#討論的氣氛熱絡,除了推文回覆,更多是以直接回覆整篇文,可以避免推文海同時可以將資訊更有效率的表達
#這個版也盛行[樂透],可以讓更多人來參與活動賺P幣
```

解釋解釋解釋解釋 人工結論與解釋解釋解釋解釋解釋解釋解釋
