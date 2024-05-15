
options(stringsAsFactors = FALSE)

library(sqldf)
library(dplyr)
library(data.table)

sqldf_1 <- function(Posts, Users){
  sqldf::sqldf("
  SELECT Location, COUNT(*) AS Count
  FROM (
  SELECT Posts.OwnerUserId, Users.Id, Users.Location
  FROM Users
  JOIN Posts ON Users.Id = Posts.OwnerUserId
  )
  WHERE Location NOT IN ('')
  GROUP BY Location
  ORDER BY Count DESC
  LIMIT 10
  ")
}

base_1 <- function(Posts, Users){
  
  # chce dla kazdego uzytkownika znalezc jego lokalizacje, lacze tabele po odpowiednim kluczu
  df_base <- merge(x = Users, y = Posts, by.x = "Id", by.y = "OwnerUserId") 
  df_base <- df_base[ df_base$Location != '', ]
  df_base <- df_base["Location"]
  # licze liczbe wystapien danej lokalizacji, nastepnie sortuje malejaco po liczbie wystapien
  df_base <- aggregate(df_base["Location"], by = df_base["Location"], FUN = length)
  colnames(df_base) <- c("Location", "Count")
  df_base <- df_base[order(df_base$Count, decreasing = T), ]
  # chce odswiezyc numery wierszy
  rownames(df_base) <- NULL
  df_base[1:10,]
}

dplyr_1 <- function(Posts, Users){
  
  # lacze tabele aby dla kazdego uzytkownika znalezc jego lokalizacje
  # licze liczbe wystapien danej lokalizacji
  tmp1 <- Users %>%
    inner_join(Posts, by = c("Id" = "OwnerUserId")) %>% 
    filter(Location != "") %>%
    select(Location) %>% 
    count(Location) %>%  
    rename(Count = n) %>% 
    arrange(desc(Count)) %>% 
    slice_head(n = 10) 
}

data.table_1 <- function(PostsDT, UsersDT){
  
  tmp1 <- UsersDT[PostsDT, on = .(Id = OwnerUserId)]  
  # lacze tabele aby dla kazdego uzytkownika znalezc jego lokalizacje
  tmp1 <- tmp1[Location != "", .(Count = .N), by = .(Location)] 
  # licze liczbe wystapien danej lokalizacji
  tmp1 <- tmp1[order(-Count)][1:10]
}

sqldf_2 <- function(Posts, PostLinks){
  sqldf::sqldf("
  SELECT Posts.Title, RelatedTab.NumLinks
  FROM
  (
    SELECT RelatedPostId AS PostId, COUNT(*) AS NumLinks
    FROM PostLinks
    GROUP BY RelatedPostId
  ) AS RelatedTab
  JOIN Posts ON RelatedTab.PostId=Posts.Id
  WHERE Posts.PostTypeId=1
  ORDER BY NumLinks DESC
  ")
}

base_2 <- function(Posts, PostLinks){
  
  # agregujemy aby miec odpowiednia liczbe powiazanych linkow
  tmp1 <- aggregate(PostLinks["RelatedPostId"], by = PostLinks["RelatedPostId"], FUN = length)
  colnames(tmp1) <- c("PostId", "NumLinks")
  tmp2 <- merge(tmp1, Posts, by.x = "PostId", by.y = "Id")
  # znajdujemy posty typu pytanie
  tmp2 <- tmp2[tmp2$PostTypeId == 1, c("Title", "NumLinks")]
  # sortujemy malejaco po liczbie powiazan
  result <- tmp2[order(-tmp2$NumLinks),]
  row.names(result) <- NULL
  result 
}

dplyr_2 <- function(Posts, PostLinks){
  
  tmp1 <- PostLinks %>%
    group_by(RelatedPostId) %>%
    summarise(NumLinks = n()) %>%
    inner_join(Posts, by = c("RelatedPostId" = "Id")) %>%
    filter(PostTypeId == 1) %>%
    arrange(desc(NumLinks)) %>%
    select(Title, NumLinks)
}

data.table_2 <- function(PostsDT, PostLinksDT){
  
  # zliczam liczbe powiazan
  tmp1 <- PostLinksDT[, .(NumLinks = .N), by = RelatedPostId]
  tmp2 <- merge(tmp1, PostsDT, by.x = "RelatedPostId", by.y = "Id", all.x = TRUE)
  # biore pod uwage posty typu pytanie o PostTypeId = 1, wybieram kolumny, sortuje malejaco
  result <- tmp2[PostTypeId == 1, .(Title, NumLinks)][order(-NumLinks)]
}


sqldf_3 <- function(Comments, Posts, Users){
  sqldf::sqldf("
  SELECT Title, CommentCount, ViewCount, CommentsTotalScore,
  DisplayName, Reputation, Location
  FROM (
  SELECT Posts.OwnerUserId, Posts.Title, Posts.CommentCount, Posts.ViewCount,
  CmtTotScr.CommentsTotalScore
  FROM (
  SELECT PostId, SUM(Score) AS CommentsTotalScore
  FROM Comments
  GROUP BY PostId
  ) AS CmtTotScr
  JOIN Posts ON Posts.Id = CmtTotScr.PostId
  WHERE Posts.PostTypeId=1
  ) AS PostsBestComments
  JOIN Users ON PostsBestComments.OwnerUserId = Users.Id
  ORDER BY CommentsTotalScore DESC
  LIMIT 10
  ")
}

base_3 <- function(Comments, Posts, Users){

  # agreguje aby obliczyc laczny wynik z komentarzy 
  tmp1 <- aggregate(Score ~ PostId, data = Comments, FUN = sum )
  tmp2 <- merge(tmp1, Posts[Posts$PostTypeId == 1, ], by.x = "PostId", by.y = "Id")
  tmp3 <- merge(tmp2, Users, by.x = "OwnerUserId", by.y = "Id")
  # wybieram odpowiednie kolumny, zmieniam nazwy, sortuje po lacznym wyniku komentarzy
  result <- tmp3[, c("Title", "CommentCount", "ViewCount", "Score.x", "DisplayName", "Reputation", "Location")]
  colnames(result)[colnames(result) == "Score.x"] <- "CommentsTotalScore"
  result <- result[order(-result$CommentsTotalScore), ][1:10,]
  rownames(result) <- NULL
  result
}

dplyr_3 <- function(Comments, Posts, Users){
  tmp0 <- Posts %>% filter(PostTypeId == 1)
  # obliczam laczny wynik komentarzy, lacze ramki danych aby uzyskac potrzebne kolumny
  tmp1 <- Comments %>%
    group_by(PostId) %>%
    summarise(CommentsTotalScore = sum(Score)) %>% 
    inner_join(tmp0, by = c("PostId" = "Id")) 
  tmp1 <- tmp1 %>% 
    inner_join(Users, by = c("OwnerUserId" = "Id")) %>% 
    arrange(desc(CommentsTotalScore)) 
  # wybieram odpowiednie kolummy i pierwsze 10 wierszy 
  tmp2 <- select(tmp1, Title, CommentCount, ViewCount, CommentsTotalScore, DisplayName, Reputation, Location) %>% 
    slice_head(n = 10) 
  tmp2
}

data.table_3 <- function(CommentsDT, PostsDT, UsersDT){
  
  # laczny wynik z komentarzy 
  tmp1 <- CommentsDT[, .(CommentsTotalScore = sum(Score)), by = PostId]
  tmp2 <- merge(tmp1, PostsDT[PostTypeId == 1], by.x = "PostId", by.y = "Id", all.x = TRUE)
  tmp3 <- merge(tmp2, UsersDT, by.x = "OwnerUserId", by.y = "Id")
  # sortuje 
  setorder(tmp3, -CommentsTotalScore)
  # odpowiednie kolumny
  tmp3 <- tmp3[, .(Title, CommentCount, ViewCount, CommentsTotalScore, DisplayName, Reputation, Location)][1:10]
}


sqldf_4 <- function(Posts, Users){
  sqldf::sqldf("
  SELECT DisplayName, QuestionsNumber, AnswersNumber, Location,
  Reputation, UpVotes, DownVotes
  FROM (
    SELECT *
      FROM (
        SELECT COUNT(*) as AnswersNumber, OwnerUserId
        FROM Posts
        WHERE PostTypeId = 2
        GROUP BY OwnerUserId
      ) AS Answers
    JOIN
    (
      SELECT COUNT(*) as QuestionsNumber, OwnerUserId
      FROM Posts
      WHERE PostTypeId = 1
      GROUP BY OwnerUserId
    ) AS Questions
    ON Answers.OwnerUserId = Questions.OwnerUserId
    WHERE AnswersNumber > QuestionsNumber
    ORDER BY AnswersNumber DESC
    LIMIT 5
  ) AS PostsCounts
  JOIN Users
  ON PostsCounts.OwnerUserId = Users.Id
  ")
}

base_4 <- function(Posts, Users){
  
  #liczba odpowiedzi
  tmp1 <- aggregate(Id ~ OwnerUserId, data = Posts[Posts$PostTypeId == 2,], FUN = length)
  colnames(tmp1) <- c("OwnerUserId", "AnswersNumber")
  # liczba pytan
  tmp2 <- aggregate(Id ~ OwnerUserId, data = Posts[Posts$PostTypeId == 1,], FUN = length)
  colnames(tmp2) <- c("OwnerUserId", "QuestionsNumber")
  # wybor uzytkownikow o wiekszej liczbie odpowiedzi, laczenie danych 
  tmp3 <- merge(tmp1, tmp2, by = "OwnerUserId", all = TRUE)
  tmp3 <- tmp3[tmp3$AnswersNumber > tmp3$QuestionsNumber,]
  tmp3 <- tmp3[order(-tmp3$AnswersNumber),][1:5, ]
  tmp3 <- merge(tmp3, Users, by.x = "OwnerUserId", by.y = "Id")
  # wybor szukanych kolumn, sortowanie malejaco
  result <- tmp3[, c("DisplayName", "QuestionsNumber",
                        "AnswersNumber", "Location", "Reputation", "UpVotes", "DownVotes")]
  result <- result[order(-tmp3$AnswersNumber),]
  rownames(result) <- NULL
  result
  }

dplyr_4 <- function(Posts, Users){
  
  # liczba pytan
  tmp1 <- Posts %>%
    filter(PostTypeId == 2) %>%
    group_by(OwnerUserId) %>%
    summarise(AnswersNumber = n())
  
  # liczba odpowiedzi 
  tmp2 <- Posts %>%
    filter(PostTypeId == 1) %>%
    group_by(OwnerUserId) %>%
    summarise(QuestionsNumber = n())
  
  # wybor odpowiednich uzytkownikow 
  result <- inner_join(tmp1, tmp2, by = "OwnerUserId") %>%
    filter(AnswersNumber > QuestionsNumber) %>%
    arrange(desc(AnswersNumber)) %>% 
    inner_join(Users, by = c("OwnerUserId" = "Id")) %>%
    select(DisplayName, QuestionsNumber, AnswersNumber, Location, Reputation, UpVotes, DownVotes) %>%
    slice_head(n = 5)
}

data.table_4 <- function(PostsDT, UsersDT){
  
  # liczba pytan i odpowiedzi 
  tmp1 <- PostsDT[PostTypeId == 2, .(AnswersNumber = .N), by = OwnerUserId]
  tmp2 <- PostsDT[PostTypeId == 1, .(QuestionsNumber = .N), by = OwnerUserId]
  tmp3 <- tmp1[tmp2, on = "OwnerUserId"]
  # wybor odpowiednich uzytkownikow
  tmp3 <- tmp3[AnswersNumber > QuestionsNumber]
  result <- merge(tmp3, UsersDT, by.x = "OwnerUserId", by.y = "Id")
  # wybor kolumn, sortowanie
  result2 <- result[, .(DisplayName, QuestionsNumber, AnswersNumber, Location, Reputation, UpVotes, DownVotes)]
  result2 <- result2[order(-AnswersNumber), ][1:5]
}


sqldf_5 <- function(Posts, Users){
  sqldf::sqldf("
  SELECT
  Users.AccountId,
  Users.DisplayName,
  Users.Location,
  AVG(PostAuth.AnswersCount) as AverageAnswersCount
  FROM
  (
    SELECT
    AnsCount.AnswersCount,
    Posts.Id,
    Posts.OwnerUserId
    FROM (
      SELECT Posts.ParentId, COUNT(*) AS AnswersCount
      FROM Posts
      WHERE Posts.PostTypeId = 2
      GROUP BY Posts.ParentId
    ) AS AnsCount
    JOIN Posts ON Posts.Id = AnsCount.ParentId
  ) AS PostAuth
  JOIN Users ON Users.AccountId=PostAuth.OwnerUserId
  GROUP BY OwnerUserId
  ORDER BY AverageAnswersCount DESC
  LIMIT 10
  ")
}

base_5 <- function(Posts, Users){
  
  # liczba odpowiedzi 
  tmp1 <- aggregate(Id ~ ParentId, Posts[Posts$PostTypeId == 2,], FUN = length)
  colnames(tmp1) <- c("ParentId", "AnswersCount")
  tmp2 <- merge(Posts, tmp1, by.x = "Id", by.y = "ParentId")
  tmp3 <- merge(Users, tmp2, by.x = "AccountId", by.y = "OwnerUserId")
  pomocnicze <- tmp3[, c("AccountId", "DisplayName", "Location")]
  # srednia liczba odpowiedzi dla uzytkownika 
  tmp5 <- aggregate(tmp3["AnswerCount"], by = tmp3["AccountId"], FUN = mean)
  tmp6 <- merge(pomocnicze, tmp5, by = "AccountId")
  # sortowanie i resetowanie indeksow 
  result <- tmp6[order(-tmp6$AnswerCount, -tmp6$AccountId),]
  row.names(result) <- NULL
  colnames(result)[colnames(result) == "AnswerCount"] <- "AverageAnswersCount"
  result[1:10,]
}

dplyr_5 <- function(Posts, Users){
  
  # zliczanie odpowiedzi na pytanie
  tmp1 <- Posts %>%
    filter(PostTypeId == 2) %>%
    group_by(ParentId) %>%
    summarise(AnswersCount = n()) %>% 
    inner_join(Posts, by = c("ParentId" = "Id")) %>% 
    inner_join(Users, by = c("OwnerUserId" = "AccountId")) 
  tmp2 <- tmp1 %>% 
    group_by(OwnerUserId) %>% 
    summarise(AverageAnswersCount = mean(AnswersCount), .groups = "drop") %>% 
    inner_join(tmp1, by = "OwnerUserId") %>% 
    rename(AccountId = OwnerUserId) %>% 
    select(AccountId, DisplayName, Location, AverageAnswersCount) %>% 
    arrange(desc(AverageAnswersCount), desc(AccountId)) %>%
    mutate(AccountId = as.integer(AccountId)) %>%
    slice_head(n = 10)
}

data.table_5 <- function(PostsDT, UsersDT){
  
  # zliczanie odpowiedzi na pytania
  tmp1 <- PostsDT[PostTypeId == 2, .N, by = .(ParentId)]
  # laczenie z odpowiednimi tabelami
  tmp2 <- merge(PostsDT, tmp1, by.x = "Id", by.y = "ParentId")
  tmp3 <- merge(UsersDT, tmp2, by.x = "AccountId", by.y = "OwnerUserId")
  # srednia liczba odpowiedzi, sortowanie
  result <- tmp3[, .(AverageAnswersCount = mean(N)), by = .(AccountId, DisplayName, Location)]
  setorder(result, -AverageAnswersCount, -AccountId)[1:10]
}
