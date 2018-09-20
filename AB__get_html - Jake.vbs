Set fso = CreateObject("Scripting.FileSystemObject")

dim xHttp: Set xHttp = createobject("Microsoft.XMLHTTP")
dim bStrm: Set bStrm = createobject("Adodb.Stream")
dim owners(10)
owners(1) = "andrew"
owners(2) = "jake"
owners(3) = "breanna"
owners(4) = "joseph"
owners(5) = "alyssa"
owners(6) = "chris"
owners(7) = "jordan"
owners(8) = "alex"
owners(9) = "colin"
owners(10) = "paul"


'''current_week = DatePart("ww",CDate(Date+i),3)-36
current_week = 2
for week = current_week-1 to current_week
'''for week = 1 to current_week
for team = 1 to 10
team_score = "http://games.espn.com/ffl/boxscorequick?leagueId=1336357&teamId=" & team & "&scoringPeriodId=" & week & "&seasonId=2018&view=scoringperiod&version=quick"

xHttp.Open "GET", team_score, False
xHttp.Send

with bStrm
    .type = 1
    .open
    .write xHttp.responseBody
    .savetofile "C:\Users\A097092\Desktop\Extra\Fantasy FB research\Teams Jake\" & owners(team) & "-" & week & ".txt", 2
    .close
end with

next
next

'''wscript.quit

xHttp.Open "GET", "http://games.espn.com/ffl/leaguerosters?leagueId=1336357", False
xHttp.Send

with bStrm
    .type = 1
    .open
    .write xHttp.responseBody
    .savetofile "C:\Users\A097092\Desktop\Extra\Fantasy FB research\Teams Jake.txt", 2
    .close
end with

Set bStrm = Nothing
Set xHttp = Nothing
Set fso = Nothing
Set fl = Nothing



'msgbox "Done"






