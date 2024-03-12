options(stringsAsFactors = FALSE)

library(sqldf)
library(dplyr)
library(data.table)

Tags <- read.csv("travel_stackexchange_com/Tags.csv.gz")
Badges <- read.csv("travel_stackexchange_com/Badges.csv.gz")
Comments <- read.csv("travel_stackexchange_com/Comments.csv.gz")
Posts <- read.csv("travel_stackexchange_com/Posts.csv.gz")
PostLinks <- read.csv("travel_stackexchange_com/PostLinks.csv.gz")
Users <- read.csv("travel_stackexchange_com/Users.csv.gz")
Votes <- read.csv("travel_stackexchange_com/Votes.csv.gz")

BadgesDT <- data.table(Badges)
CommentsDT <- data.table(Comments)
TagsDT <- data.table(Tags)
PostsDT <- data.table(Posts)
UsersDT <- data.table(Users)
VotesDT <- data.table(Votes)
PostLinksDT <- data.table(PostLinks)


#################### Zadanie 1 #################################

# rozwiazanie za pomoca jezyka SQL
df_sql_1 <- function(Tags) {
  
  sqldf::sqldf("
  SELECT TagName, Count
  FROM Tags
  ORDER BY Count DESC
  LIMIT 10
  ")
}

# rozwiazanie za pomoca funkcji bazowych R-a
df_base_1 <- function(Tags) {
  
  df_base <- Tags[, c("TagName", "Count") ]       # wybieram dwie kolumny z ramki Tags
  sort <- order(df_base$Count, decreasing = TRUE) # sortuje kolumne Count malejaco
  df_base <- df_base[sort[1:10], ]                # wybieram pierwszych 10 wyników
  row.names(df_base) <- NULL                      # zmieniam numery wierszy
  df_base
}

# rozwiazanie za pomoca pakietu dplyr, jest analogiczne jak wyżej
df_plyr_1 <- function(Tags) {
  
  df_plyr <- select(Tags, TagName, Count) # wybieramy odpowiednie kolumny
  sort <- arrange(df_plyr, desc(Count)) # sortujemy wybrane kolumny po Count malejaco
  df_plyr <- top_n(sort, 10, Count)       # wybieramy 10 pierwszych wierszy
  df_plyr
}

# rozwiazanie za pomoca pakietu data.table, jest analogiczne, jak wyżej
df_table_1 <- function(TagsDT){
  
  df_t <- TagsDT[,.(TagName, Count)]
  df_t <- setorder(df_t, -Count)  # jest znak "-" bo sortujemy malejaco
  df_t <- df_t[1:10]
  df_t
}

# Sprawdzamy poprawnosc otrzymanych wynikow
all_equal(df_sql_1(Tags), df_base_1(Tags))
all_equal(df_sql_1(Tags), df_plyr_1(Tags))
all_equal(df_sql_1(Tags), df_table_1(TagsDT))

# Mierzymy czas
microbenchmark::microbenchmark(
  
  sqldf = df_sql_1(Tags),
  base = df_base_1(Tags),
  dplyr = df_plyr_1(Tags),
  data.table = df_table_1(TagsDT)
)


############################Zadanie 2#############################


# rozwiazanie za pomoca jezyka SQL
df_sql_2 <- function(Users, Posts){
  
  sqldf::sqldf("
  SELECT Users.DisplayName, Users.Age, Users.Location,
  AVG(Posts.Score) AS PostsMeanScore,
  MAX(Posts.CreationDate) AS LastPostCreationDate
  FROM Posts
  JOIN Users ON Users.AccountId=Posts.OwnerUserId
  WHERE OwnerUserId != -1
  GROUP BY OwnerUserId
  ORDER BY PostsMeanScore DESC
  LIMIT 10
")
}

# rozwiazanie za pomoca funkcji bazowych R-a
df_base_2 <- function(Users, Posts){
  
  # na poczatku chce stworzyc ramke danych ktora potem bede laczyl joinem z ramka Users
  
  #zliczam srednia Score, grupujac po OwnerUserId
  tmp1 <- aggregate( Posts["Score"], by = Posts["OwnerUserId"], FUN = mean)
  colnames(tmp1) = c("OwnerUserId", "PostsMeanScore")
  
  #znajduje maksymalna wartosc CreationDate, grupujac po OwnerUserId
  tmp2 <- aggregate( Posts["CreationDate"], by = Posts["OwnerUserId"], FUN = max)
  colnames(tmp2) = c("OwnerUserId", "LastPostCreationDate")          
  tmp3 <- cbind(tmp1, tmp2)                                                         
  tmp3 <- tmp3[, c("OwnerUserId", "PostsMeanScore", "LastPostCreationDate")]           
  # mam utworzona ramke danych tmp3 ktora bede laczyl joinem z ramka Users
  
  df_base <- merge (x = Users, y = tmp3, by.x = "AccountId", by.y = "OwnerUserId") 
  # po polaczeniu joinem wybieramy odpowiednie wiersze gdzie AccountId jest 
  # rozny od -1
  df_base <- df_base[ df_base$AccountId != -1, ]
  # sortuje ramke danych malejaco po kolumnie PostsMeanScore
  df_base <- df_base[order(df_base$PostsMeanScore, decreasing = TRUE), ]
  df_base <- head(df_base[, c("DisplayName", "Age", "Location", "PostsMeanScore", 
                              "LastPostCreationDate")], 10)
  # wybieram odpowiednie kolumny i 10 pierwszych wierszy wynikowej ramki
  rownames(df_base) <- NULL        
  df_base
}

# rozwiazanie za pomoca pakietu dplyr, jest analogiczne jak wyżej
df_plyr_2 <- function(Users, Posts) {
  
  # na poczatku chce stworzyc ramke danych ktora potem bede laczyl joinem z ramka Users
  #zliczam srednia Score, grupujac po OwnerUserId
  tmp1 <- summarise_at(group_by(Posts, OwnerUserId), vars(Score),list(mean = mean))
  tmp1 <- rename(tmp1, PostsMeanScore = mean)
  #znajduje maksymalna wartosc CreationDate, grupujac po OwnerUserId
  tmp2 <- summarise_at(group_by(Posts, OwnerUserId), vars(CreationDate),list(max = max))
  tmp2 <- rename(tmp2, LastPostCreationDate = max)
  tmp2 <- select(tmp2, LastPostCreationDate)
  tmp3 <- bind_cols(tmp1, tmp2)
  # mam utworzona ramke danych tmp3 ktora bede laczyl joinem z ramka Users
  
  df_plyr <- inner_join(Users, tmp3, by=c("AccountId" = "OwnerUserId"))
  df_plyr <- filter(df_plyr, "OwnerUserId" != 1)
  # wybieram odpowiednie kolumny
  df_plyr <- select(df_plyr, DisplayName, Age, Location, PostsMeanScore, LastPostCreationDate)
  # sortuje ramke danych malejaco po kolumnie PostsMeanScore i wybieram
  # 10 pierwszych wierszy wynikowej ramki
  df_plyr <- top_n(arrange(df_plyr, desc(PostsMeanScore)), 10, PostsMeanScore)
  df_plyr
}

# rozwiazanie za pomoca pakietu data.table
df_table_2 <- function(UsersDT, PostsDT){
  
  # licze srednia Score oraz maksymalna wartosc CreationDate grupujac po OwnerUserId
  tmp <- PostsDT[, .(PostsMeanScore = mean(Score), 
                     LastPostCreationDate = max(CreationDate)), by = OwnerUserId]
  
  # ramke danych tmp lacze inner joinem z ramka UsersDT
  tmp2 <- UsersDT[tmp, on=c("AccountId"="OwnerUserId"), nomatch = 0]
  # wybieram nastepnie odpowiednie kolumny, wiersze
  tmp2 <- tmp2[, .(DisplayName, Age, Location, PostsMeanScore, LastPostCreationDate)]
  tmp2 <- tmp2["AccountId" != 1]
  # sortuje malejaco po kolumnie PostsMeanScore
  df_table <- setorder(tmp2, -PostsMeanScore)
  df_table <- df_table[1:10]  # wybieram pierwsze 10 wierszy
  df_table
}

# Sprawdzamy poprawnosc otrzymanych wynikow
all_equal(df_sql_2(Users, Posts), df_base_2(Users, Posts))
all_equal(df_sql_2(Users, Posts), df_plyr_2(Users, Posts))
all_equal(df_sql_2(Users, Posts), df_table_2(UsersDT, PostsDT))

# Mierzymy czas
microbenchmark::microbenchmark(
  
  sqldf = df_sql_2(Users, Posts),
  base = df_base_2(Users, Posts),
  dplyr = df_plyr_2(Users, Posts),
  data.table = df_table_2(UsersDT, PostsDT)
)



##########################Zadanie 3###########################


# rozwiazanie za pomoca jezyka SQL
df_sql_3 <- function(Users, Posts) {
  
  sqldf::sqldf("
  SELECT DisplayName, QuestionsNumber, AnswersNumber
  FROM
  (
    SELECT COUNT(*) as AnswersNumber, Users.DisplayName, Users.Id
    FROM Users JOIN Posts ON Users.Id = Posts.OwnerUserId
    WHERE Posts.PostTypeId = 1
    GROUP BY Users.Id
  ) AS Tab1
  JOIN
  (
    SELECT COUNT(*) as QuestionsNumber, Users.Id
    FROM Users JOIN Posts ON Users.Id = Posts.OwnerUserId
    WHERE Posts.PostTypeId = 2
    GROUP BY Users.Id
  ) AS Tab2
  ON Tab1.Id = Tab2.Id
  WHERE QuestionsNumber < AnswersNumber
  ORDER BY AnswersNumber DESC
  ")
}

# rozwiazanie za pomoca funkcji bazowych R-a
df_base_3 <- function(Users, Posts) {
  
  # lacze joinem ramki danych Users i Posts
  tmp1 <- merge(x = Users, y = Posts, by.x = "Id", by.y = "OwnerUserId")
  # wybieram kolumny DisplayName oraz Id tam gdzie PostTypeId jest rowny 1
  tmp1 <- tmp1[tmp1$PostTypeId == 1, c("DisplayName", "Id")] 
  # licze ilosc DisplayName grupujac po Id
  Tab1 <- aggregate(x = tmp1[("DisplayName")], by = tmp1["Id"], FUN = length)      
  colnames(Tab1) <- c("Id", "AnswersNumber")          # zmieniam nazwy kolumn
  
  # lacze inner joinem tmp1 i Tab1 i wybieram unikatowe wartosci grupujac po Id
  # bo w wyniku musze miec jeszcze kolumne DisplayName
  Tab1 <- unique(merge(x = Tab1, y = tmp1, by = "Id"))
  # zmieniam kolejnosc kolumn i numery wierszy
  Tab1 <- Tab1[c("AnswersNumber", "DisplayName", "Id")] 
  rownames(Tab1) <- NULL  
  # mam utworzona Tab1
  
  # lacze joinem ramki danych Users i Posts
  tmp2 <- merge(x = Users, y = Posts, by.x = "Id", by.y = "OwnerUserId")   
  tmp2 <- tmp2[tmp2$PostTypeId == 2,] 
  tmp2 <- tmp2["Id"]
  # wybralem odpowiednie wiersze i kolumne Id, nastepnie liczbe Id grupujac po Id
  Tab2 <- aggregate(x = tmp2[("Id")], by = tmp2["Id"], FUN = length) 
  # zmieniam nazwy kolumn i wybieram odpowiednie kolumny
  colnames(Tab2) <- c("Id", "QuestionsNumber")
  Tab2 <- Tab2[c("QuestionsNumber","Id")] 
  # mam utworzona Tab2
  
  # lacze inner joinem Tab1 i Tab2
  df_base <- merge(x = Tab1, y = Tab2, by = "Id")
  # zmieniam kolejnosc kolumn
  df_base <- df_base[c("DisplayName", "QuestionsNumber", "AnswersNumber")]
  # wybieram odpowiednie wiersze
  df_base <- df_base[df_base$QuestionsNumber < df_base$AnswersNumber, ]
  # sortuje malejoco po kolumnie AnswersNumber
  df_base <- df_base[order(df_base$AnswersNumber, decreasing = TRUE), ]
  rownames(df_base) <- NULL  
  df_base
}

# rozwiazanie za pomoca pakietu dplyr, analogiczne jak wyzej
df_plyr_3 <- function(Users, Posts) {
  
  # lacze joinem ramki danych Users i Posts
  tmp1 <- inner_join(Users, Posts, by=c("Id" = "OwnerUserId"))
  # wybieram kolumny DisplayName oraz Id tam gdzie PostTypeId jest rowny 1
  tmp1 <- filter(tmp1, PostTypeId == 1)
  tmp1 <- select(tmp1, DisplayName, Id)
  # licze ilosc DisplayName grupujac po Id
  tmp2 <- summarise_at(group_by(tmp1, Id), vars(DisplayName),list(length = length))
  tmp2 <- rename(tmp2, AnswersNumber = length)
  # chce miec teraz Tab1 - musze dolaczyc jeszcze do tmp2 kolumne DisplayName
  # z odpowiednimi wierszami - stosuje inner joina i wybieram unikatowe wartosci
  Tab1 <- distinct(inner_join(tmp2, tmp1, by="Id")) 
  Tab1 <- select(Tab1, AnswersNumber, DisplayName, Id)  # Tab1 z tresci  
  
  # lacze joinem ramki danych Users i Posts
  tmp3 <- inner_join(Users, Posts, by=c("Id" = "OwnerUserId"))
  # wybieram odpowiednie wiersze i kolumny
  tmp3 <- filter(tmp3, PostTypeId == 2)
  tmp4a <- select(tmp3, Id)
  tmp4b <- rename(tmp4a, Id2 = Id) # tworze kopie tmp4 z inna nazwa kolumny
  tmp5 <- bind_cols(tmp4a, tmp4b)  
  Tab2 <- summarise_at(group_by(tmp5, Id2), vars(Id),list(length = length)) 
  # liczymy ilosc Id grupujac po Id - funkcja nie dziala gdy do argumentow 
  # vars i by damy ta sama kolumne dlatego utworzylem ramke tmp5 z dwoma takimi 
  # samymi kolumnami o innych nazwach
  Tab2 <- rename(Tab2, Id = Id2, QuestionsNumber = length)
  Tab2 <- select(Tab2, QuestionsNumber, Id)  # Tab2 z zadania
  
  
  df_plyr <- inner_join(Tab1, Tab2, by="Id")   # lacze inner joinem 
  # wybieram nastepnie odpowiednie kolumny i wiersze
  df_plyr <- select(df_plyr, DisplayName, QuestionsNumber, AnswersNumber)       
  df_plyr <- filter(df_plyr, QuestionsNumber < AnswersNumber)
  # sortuje malejaco po AnswersNumber
  df_plyr <- arrange(df_plyr, desc(AnswersNumber))
  df_plyr
}

# rozwiazanie za pomoca pakietu data.table, analogiczne jak dwa poprzednie
df_table_3 <- function(UsersDT, PostsDT) {
  
  # lacze joinem ramki danych Users i Posts
  tmp1 <- UsersDT[PostsDT, on=c("Id" = "OwnerUserId"), nomatch = 0]
  # wybieram kolumny DisplayName oraz Id tam gdzie PostTypeId jest rowny 1
  tmp1 <- tmp1[PostTypeId == 1]
  tmp2 <- tmp1[, .(Id, DisplayName)]
  # licze ilosc DisplayName grupujac po Id
  tmp1 <- tmp1[, .(AnswersNumber = length(DisplayName)), by=Id]
  # chce miec teraz Tab1 - musze dolaczyc jeszcze do tmp1 kolumne DisplayName
  # z odpowiednimi wierszami - stosuje inner joina i wybieram unikatowe wartosci
  tmp2 <- unique( tmp1[tmp2, on="Id", nomatch = 0] )
  Tab1 <- tmp2[,.(AnswersNumber, DisplayName, Id)]   # Tab1 z zadania
  
  # lacze joinem ramki danych Users i Posts
  tmp3 <- UsersDT[PostsDT, on=c("Id" = "OwnerUserId"), nomatch = 0]
  # wybieram odpowiednie wiersze i kolumny
  tmp3 <- tmp3[PostTypeId == 2]
  tmp3 <- tmp3[, .(Id)]
  tmp3 <- tmp3[, Id2:= Id]  # dodaje do tmp3 kolumne o nazwie Id2 o wierszach
  # identycznych z kolumna Id, nastepnie licze ilosc Id grupujac po Id
  tmp3 <- tmp3[, .(QuestionsNumber = length(Id2)), by=Id]
  Tab2 <- tmp3[,.(QuestionsNumber, Id)]
  Tab2 <- setorder(Tab2, Id)  # Tab2  zadania
  
  # lacze inner joinem Tab1 i Tab2
  df_table <- Tab1[Tab2, on = "Id", nomatch = 0]
  # wybieram nastepnie odpowiednie kolumny i wiersze
  df_table <- df_table[,.(DisplayName, QuestionsNumber, AnswersNumber)]
  df_table <- df_table[QuestionsNumber < AnswersNumber]
  # sortuje malejaco po AnswersNumber
  df_table <- setorder(df_table, -AnswersNumber)
  df_table
}

# Sprawdzamy poprawnosc otrzymanych wynikow
all_equal(df_sql_3(Users, Posts), df_base_3(Users, Posts))
all_equal(df_sql_3(Users, Posts), df_plyr_3(Users, Posts))
all_equal(df_sql_3(Users, Posts), df_table_3(UsersDT, PostsDT))

# Mierzymy czas
microbenchmark::microbenchmark(
  
  sqldf = df_sql_3(Users, Posts),
  base = df_base_3(Users, Posts),
  dplyr = df_plyr_3(Users, Posts),
  data.table = df_table_3(UsersDT, PostsDT)
)


#################3#######Zadanie 4########################


# rozwiazanie za pomoca jezyka SQL
df_sql_4 <- function(Posts, Comments){
  
  sqldf::sqldf("
  SELECT
  Posts.Title, Posts.CommentCount,
  CmtTotScr.CommentsTotalScore,
  Posts.ViewCount
  FROM (
    SELECT
    PostID,
    UserID,
    SUM(Score) AS CommentsTotalScore
    FROM Comments
    GROUP BY PostID, UserID
  ) AS CmtTotScr
  JOIN Posts ON Posts.ID=CmtTotScr.PostID
  WHERE Posts.PostTypeId=1
  ORDER BY CmtTotScr.CommentsTotalScore DESC
  LIMIT 10
  ")
}

# rozwiazanie za pomoca funkcji bazowych R-a
df_base_4 <- function(Posts, Comments) {
  
  tab <- Comments[, c("PostId", "UserId")]
  tab <- tab[is.na(tab$UserId), ]       # wybieramy wiersze z wartoscia NA dla kolumny
  # UserId, w kolumnie PostId nie ma brakow danych
  
  # tworzeramke danych ktora zawiera wartosc Score dla wierszy w ktorych 
  # UserId jest rowne NA
  tab2 <- as.data.frame(Comments[rownames(tab), "Score"])     
  colnames(tab2) <- c("CommentsTotalScore")
  tab3 <- cbind(tab[c("PostId", "UserId")], tab2["CommentsTotalScore"])
  # licze sume CommentsTotalScore i grupuje tylko po PostsId bo UserId w tab3 wynosi NA
  tab4 <- aggregate(x = tab3$CommentsTotalScore, by = tab3["PostId"], FUN = sum)
  tab5 <- as.data.frame(rep(NA, length(tab4$PostId)))
  tmp <- cbind(tab4, tab5) # dolaczam kolumne UserId
  colnames(tmp) <- c("PostId", "CommentsTotalScore", "UserId")
  tmp <- tmp[c("PostId", "UserId", "CommentsTotalScore")]
  
  # licze sume CommentsTotalScore i grupuje po PostsId i UserId - funkcja obliczy 
  # nam szukana sume gdy UserId nie bedzie NA
  tmp2 <- aggregate(x = Comments$Score, by = Comments[c("PostId", "UserId")], 
                    FUN = sum)
  colnames(tmp2) <- c("PostId", "UserId", "CommentsTotalScore")
  
  # laczymy teraz w jedna ramka danych wyniki otrzymane liczac sume CommentsTotalScore
  # w sytuacjach gdy UserId bylo rowne NA i bylo rozne od NA
  CmtTotScr <- rbind(tmp, tmp2 ) 
  rownames(CmtTotScr) <- NULL
  
  # lacze inner joinem Posts i CmtTotScr
  df_base <- merge(x = Posts, y = CmtTotScr, by.x = "Id", by.y = "PostId")
  # wybieram odpowiednie wiersze i kolumny
  df_base <- df_base[df_base$PostTypeId == 1, ]
  df_base <- df_base[c("Title", "CommentCount", "CommentsTotalScore", "ViewCount")]
  # sortuje malejaco po kolumnie CommentsTotalScore
  df_base <- df_base[order(df_base$CommentsTotalScore, decreasing = TRUE), ]
  # wybieram 10 pierwszych wierszy
  df_base <- head(df_base, 10)
  rownames(df_base) <- NULL
  df_base
}

# rozwiazanie za pomoca pakietu dplyr
df_plyr_4 <- function(Posts, Comments) {
  
  tmp <- summarise_at(group_by(Comments, PostId, UserId), vars(Score),list(sum = sum)) 
  # liczymy sume Score grupujac po PostId, UsersId - tutaj nie przeszkadza nam fakt
  # że UsersId lub PostId moze miec wartosc NA
  
  # laczymy inner joinem Posts z otrzymana ramka danych tmp
  df_plyr <- inner_join(Posts, tmp, by=c("Id"="PostId"))
  # wybieramy odpowiednie wiersze i kolumny
  df_plyr <- filter(df_plyr, PostTypeId == 1)
  df_plyr <- select(df_plyr, Title, CommentCount, sum, ViewCount)
  # zmieniamy nazwe kolumn
  df_plyr <- rename(df_plyr, CommentsTotalScore = sum )
  # sortujemy malejaco po kolumnie CommentsTotalScore i wybieramy 10 pierwszych wierszow
  df_plyr <- top_n(arrange(df_plyr, desc(CommentsTotalScore)), 10, CommentsTotalScore)
  df_plyr
}

# rozwiazanie za pomoca pakietu data.table
df_table_4 <- function(PostsDT, CommentsDT) {
  
  tmp <- CommentsDT[, .(CommentsTotalScore = sum(Score)), by = c("PostId", "UserId")]
  # liczymy sume Score grupujac po PostId, UsersId - tutaj nie przeszkadza nam fakt
  # że UsersId lub PostId moze miec wartosc NA
  
  # laczymy inner joinem Posts z otrzymana ramka danych tmp
  df_table <- PostsDT[tmp, on=c("Id" = "PostId"), nomatch = 0]
  # wybieramy odpowiednie wiersze i kolumny
  df_table <- df_table[PostTypeId == 1]
  df_table <- df_table[,.(Title, CommentCount, CommentsTotalScore, ViewCount)]
  # sortujemy malejaco po kolumnie CommentsTotalScore i wybieramy 10 pierwszych wierszow
  df_table <- setorder(df_table, -CommentsTotalScore)
  df_table <- df_table[1:10]
  df_table
}

# Sprawdzamy poprawnosc otrzymanych wynikow
all_equal(df_sql_4(Posts, Comments), df_base_4(Posts, Comments))
all_equal(df_sql_4(Posts, Comments), df_plyr_4(Posts, Comments))
all_equal(df_sql_4(Posts, Comments), df_table_4(PostsDT, CommentsDT))

# Mierzymy czas
microbenchmark::microbenchmark(
  
  sqldf = df_sql_4(Posts, Comments),
  base = df_base_4(Posts, Comments),
  dplyr = df_plyr_4(Posts, Comments),
  data.table = df_table_4(PostsDT, CommentsDT)
)



#########################Zadanie 5###########################


# rozwiazanie za pomoca jezyka SQL
df_sql_5 <- function(Posts) {
  
  sqldf::sqldf("
  SELECT
  Questions.Id,
  Questions.Title,
  BestAnswers.MaxScore,
  Posts.Score AS AcceptedScore,
  BestAnswers.MaxScore-Posts.Score AS Difference
  FROM (
    SELECT Id, ParentId, MAX(Score) AS MaxScore
    FROM Posts
    WHERE PostTypeId==2
    GROUP BY ParentId
  ) AS BestAnswers
  JOIN (
    SELECT * FROM Posts
    WHERE PostTypeId==1
  ) AS Questions
  ON Questions.Id=BestAnswers.ParentId
  JOIN Posts ON Questions.AcceptedAnswerId=Posts.Id
  ORDER BY Difference DESC
  LIMIT 10
")
}

# rozwiazanie za pomoca funkcji bazowych R-a
df_base_5 <- function(Posts) {
  
  # wybieram wiersze, w ktorych PostTypeId jest rowne 2
  tmp <- Posts[Posts$PostTypeId==2, ]
  # licze maksymalna wartosc Score grupujac po ParentId 
  tmp2 <- aggregate(x = tmp$Score, by = tmp["ParentId"], FUN = max)    
  colnames(tmp2) <- c("ParentId", "MaxScore")
  
  tmp3 <- unique(tmp[c("ParentId", "Id")])
  # sortujemy tmp3 po ParentId i zapisujemy numery wierszy powstalej ramki danych
  tmp3 <- tmp3[order(tmp3$ParentId),]
  tmp4 <- rownames(tmp3)
  # wybieramy z Posts z kolumny Score odpowiednie wiersze
  tmp4 <- Posts[tmp4, "Score"]
  tmp5 <- cbind(tmp3, tmp4) # dzieki polaczeniu kolumn dostajemy ramke w ktorej 
  # dla kazdej pary Id, ParentId mamy wartosc Score z Posts ktora jej odpowiada
  colnames(tmp5) <- c("ParentId", "Id", "MaxScore")
  # zmieniamy nazwy i kolejnosc kolumn
  tmp5 <- tmp5[c("Id", "ParentId", "MaxScore")]
  # stosuje inner join po to aby do dwoch kolumn z tmp2 dolaczyc kolumne Id tak 
  # aby kazda kombinacja ParentId, MaxScore miala w jednym wierszu Id takie samo jak
  # w ramce wyjsciowej Posts
  tmp6 <- merge(x = tmp2, y = tmp5, by = c("ParentId", "MaxScore"))
  tmp6 <- tmp6[c("Id", "ParentId", "MaxScore")]
  # w tmp6 mamy jeszcze wiersze dla ktorych mamy rozne Id dla tych samych par 
  # MaxScore i Parent Id
  # Chcemy wybrac wtedy wiersz w ktorym Id jest najmniejsze
  tmp6 <- tmp6[order(tmp6$Id), ]
  tmp7 <- tmp6[c("ParentId", "MaxScore")]
  tmp7 <- unique(tmp7)
  # dla kazdej unikalnej pary ParentId, MaxScore bierzemy pierwsze (czyli najmniejsze
  # bo sortowalismy malejaco kolumne Id w tmp6) Id
  BestAnswers <- cbind(tmp7, (tmp6[rownames(tmp7), "Id"]))
  colnames(BestAnswers) <- c("ParentId", "MaxScore", "Id")
  BestAnswers <- BestAnswers[c("Id", "ParentId", "MaxScore")]
  # sortujemy po kolumnie ParentId
  BestAnswers <- BestAnswers[order(BestAnswers$ParentId), ]
  rownames(BestAnswers) <- NULL
  
  # tworze ramke danych Questions z zadania, przez wybranie odpowiednich wierszy
  Questions <- Posts[Posts$PostTypeId == 1, ]
  rownames(Questions) <- NULL
  
  # lacze inner joinem Questions z BestAnswers
  tab1 <- merge(y = BestAnswers, x = Questions, by.y = "ParentId", by.x = "Id",)
  # Powstala w ten sposob ramke danych lacze inner joinem z Posts
  tab2 <- merge(x = tab1, y = Posts, by.x = "AcceptedAnswerId", by.y = "Id", suffixes = c(".1", ".2"))
  tab2 <- tab2[c("Id", "Title.1", "MaxScore", "Score.2")]
  # Po wybraniu odpowiednich kolumn,
  #tworze kolumne Difference ktora jest roznica MaxScore i Score.2 
  tab3 <- as.data.frame(tab2$MaxScore - tab2$Score.2)
  df_base <- cbind(tab2, tab3) # dolaczam do tab2 kolumne Difference
  colnames(df_base) <- c("Id", "Title", "MaxScore", "AcceptedScore", "Difference")
  # sortuje po kolumnie Difference malejaco i wybieram 10 pierwszych wierszy
  df_base <- df_base[order(df_base$Difference, decreasing = TRUE), ]
  df_base <- head(df_base, 10)
  rownames(df_base) <- NULL
  df_base
}

# rozwiazanie za pomoca pakietu dplyr
df_plyr_5 <- function(Posts) {
  
  tmp1 <- select(Posts, ParentId, Score, Id, PostTypeId)
  tmp1 <- filter(tmp1, PostTypeId == 2)
  # obliczamy maksymalna wartosc Score grupujac po ParentId
  tmp2 <- summarise_at(group_by(tmp1, ParentId), vars(Score),list(max = max))
  tmp2 <- rename(tmp2, Score = max)
  tmp3 <- semi_join(tmp1, tmp2, by=c("ParentId", "Score"))       # teraz mamy tabele
  # w ktorej kombinacje Parentid i Score moga miec taka sama wartosc dla roznych Id
  BestAnswers <- summarise_at(group_by(tmp3, ParentId, Score), vars(Id), list(min = min))   # dla
  # dla tych samych kombinacji Parentid i Score wymieramy najmniejsze Id
  BestAnswers <- rename(BestAnswers, MaxScore = Score, Id = min)
  Questions <- filter(Posts, PostTypeId == 1)
  # lacze inner joinem Questions z BestAnswers
  tmp4 <- inner_join(Questions, BestAnswers, by=c("Id"="ParentId"))
  # Powstala w ten sposob ramke danych lacze inner joinem z Posts
  tmp5 <- inner_join(tmp4, Posts, by=c("AcceptedAnswerId"="Id"))
  tmp5 <- select(tmp5, Id, Title.x, MaxScore, Score.y)
  # Po wybraniu odpowiednich kolumn,
  #tworze kolumne Difference ktora jest roznica MaxScore i Score.2 
  tmp6 <- select(tmp5, MaxScore) - select(tmp5, Score.y) 
  tmp6 <- rename(tmp6, Difference = MaxScore)
  df_plyr <- bind_cols(tmp5, tmp6) # dolaczam do tmp5 kolumne Difference
  df_plyr <- rename(df_plyr, Title=Title.x, AcceptedScore = Score.y)
  # sortuje po malejaco po kolumnie Difference i wybieram 10 pierwszych wierszy
  df_plyr <- top_n(arrange(df_plyr, desc(Difference)), 10, Difference)
  df_plyr
}

# rozwiazanie za pomoca pakietu data.table
df_table_5 <- function(PostDT) {
  
  # wybieramy odpowiednie wiersze i kolumny
  tmp <- PostsDT[PostTypeId == 2]
  tmp <- tmp[,.(Id, ParentId, Score)]
  # liczymy maksymalna wartosc Score grupujac po ParentId
  tmp2 <- tmp[, .(Score = max(Score)), by=ParentId]
  # laczymy inner joinem odpowiednie ramki danych
  BestAnswers <- tmp[tmp2, .(Id, Score, ParentId), on=c("ParentId","Score")]
  
  # dla unikalnych wartosci ParentId i Score znajdujemy najmniejsze Id
  BestAnswers <- BestAnswers[, .(Id = min(Id)), by=c("ParentId", "Score")]
  BestAnswers <- setnames(BestAnswers, c("Score"), c("MaxScore"))
  # sortuje po kolumnie ParentId
  BestAnswers <- setorder(BestAnswers, ParentId)
  # tworze ramke danych Questions
  Questions <- PostsDT[PostTypeId == 1]
  # lacze inner joinem Questions z BestAnswers
  Questions <- Questions[BestAnswers, on=c("Id" = "ParentId"), nomatch = 0]
  # Powstala w ten sposob ramke danych lacze inner joinem z Posts
  df_table <- Questions[PostsDT, on=c("AcceptedAnswerId" = "Id"), nomatch = 0]
  df_table <- df_table[, .(Id, Title, MaxScore, i.Score)]
  # Po wybraniu odpowiednich kolumn, dolaczam do df_table nowa kolumne 
  # Difference ktora jest roznica MaxScore i i.Score 
  df_table <- df_table[, Difference:= MaxScore - i.Score]
  df_table <- setnames(df_table, c("i.Score"), c("AcceptedScore"))
  # sortuje malejaca po kolumnie Difference i wybieram 10 pierwszych wierszy
  df_table <- setorder(df_table, -Difference)
  df_table <- df_table[1:10]
}

# Sprawdzamy poprawnosc otrzymanych wynikow
all_equal(df_sql_5(Posts), df_base_5(Posts))
all_equal(df_sql_5(Posts), df_plyr_5(Posts))
all_equal(df_sql_5(Posts), df_table_5(PostsDT))

# Mierzymy czas
microbenchmark::microbenchmark(
  
  sqldf = df_sql_5(Posts),
  base = df_base_5(Posts),
  dplyr = df_plyr_5(Posts),
  data.table = df_table_5(PostsDT)
)


