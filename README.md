# lake-inventory-data-capture
Shiny application for capturing lake biodiveristy inventory data


### user list
The file user list in ./data/input/ is on .gitignore and needs to be manually entered for every install. Can be done on the fly with e.g.:

```r
user_list <- data.frame(user_code=c("333","666"),
                        username=c("testuser","testuser2"))
write.csv(user_list,"./data/input/user_list.csv",row.names = FALSE)

```
