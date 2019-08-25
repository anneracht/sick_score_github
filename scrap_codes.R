
for (row in 1:nrow(stock)) {
  price <- stock[row, "apple"]
  date  <- stock[row, "date"]

  if(price > 117) {
    print(paste("On", date,
                "the stock price was", price))
  }
}



for(i in 1:nrow(sick_score_geo[41])){
  if (sick_score_geoi == "Detroit"){
    long[i] <- paste("postcode")
  }
}

for(i in 1:nrow(sick_score_geo)){
  if(sick_score_geo[i, 41] == "Detroit"){
    sick_score_geo[i, 51] <- paste("testing")
  }
}


for(i in min(sick_score_geo$City):max(sick_score_geo$City)){
  if(sick_score_geo[i, 41] == "Detroit"){
    sick_score_geo[i, 51] <- paste("testing")
  }
}



#this works?
sick_score_geo <- for(i in 1:nrow(sick_score_geo))
{
  if(isTRUE(sick_score_geo$City[i] == "Detroit")){
    sick_score_geo$lat[i] <- paste0("hello")
  }
}


for(i in seq_along(sick_score_geo))
{
  if(isTRUE(sick_score_geo$City[i] == "Detroit")){
    sick_score_geo$lat[i] <- paste0("hello")
  }
}


str(sick_score_geo$long)
sick_score_geo$long[2] <- paste0("testing")




paste(sick_score_geo["testing", 51])

sick_score_geo[2,41]
colnames(sick_score_geo[51])
