# library(RODBC)
# library(rlang)

## Connnection to sql server
conn <- odbcDriverConnect(connection = paste0("Driver={SQL Server Native client 11.0};server=localhost;database=Nifty;trusted_connection=yes;"))



get_Latest_day_Details = sqlQuery(conn,paste0("exec [dbo].[sp_get_all_link] 'To get latest day prices', 'Foreign'"))$Link_To_Call_all_companies
