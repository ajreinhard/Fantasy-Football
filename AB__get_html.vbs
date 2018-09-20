Set fso = CreateObject("Scripting.FileSystemObject")
Set fl = fso.OpenTextFile("C:\Users\A097092\Desktop\Extra\Fantasy FB research\AC__Number Fire Master List.txt")

dim xHttp: Set xHttp = createobject("Microsoft.XMLHTTP")
dim bStrm: Set bStrm = createobject("Adodb.Stream")
dim owners(10)
owners(1) = "scott"
owners(2) = "cory"
owners(3) = "aj"
owners(4) = "devon"
owners(5) = "comp"
owners(6) = "chad"
owners(7) = "lucas"
owners(8) = "perry"
owners(9) = "matty"
owners(10) = "seth"


'''current_week = DatePart("ww",CDate(Date+i),3)-36
current_week = 2
for week = current_week-1 to current_week
'''for week = 1 to current_week
for team = 1 to 10
team_score = "http://games.espn.com/ffl/boxscorequick?leagueId=622218&teamId=" & team & "&scoringPeriodId=" & week & "&seasonId=2018&view=scoringperiod&version=quick"

xHttp.Open "GET", team_score, False
xHttp.Send

with bStrm
    .type = 1
    .open
    .write xHttp.responseBody
    .savetofile "C:\Users\A097092\Desktop\Extra\Fantasy FB research\Teams\" & owners(team) & "-" & week & ".txt", 2
    .close
end with

next
next

'''wscript.quit

do until fl.atendofstream

xHttp.Open "GET", fl.readline, False
xHttp.Send

with bStrm
    .type = 1
    .open
    .write xHttp.responseBody
    .savetofile "C:\Users\A097092\Desktop\Extra\Fantasy FB research\Players\" & fl.readline & ".txt", 2
    .close
end with

loop

fl.close

xHttp.Open "GET", "http://games.espn.com/ffl/leaguerosters?leagueId=622218", False
xHttp.Send

with bStrm
    .type = 1
    .open
    .write xHttp.responseBody
    .savetofile "C:\Users\A097092\Desktop\Extra\Fantasy FB research\Teams.txt", 2
    .close
end with

Set bStrm = Nothing
Set xHttp = Nothing
Set fso = Nothing
Set fl = Nothing



'msgbox "Done"






