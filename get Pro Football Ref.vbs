
file_path = "C:\Users\Owner\Documents\GitHub\Fantasy-Football\"

dim xHttp: Set xHttp = createobject("Microsoft.XMLHTTP")
dim bStrm: Set bStrm = createobject("Adodb.Stream")

for yr = 2013 to 2016
for x = 0 to 2000 step 100

xHttp.Open "GET", "https://www.pro-football-reference.com/play-index/pgl_finder.cgi?request=1&match=game&year_min=" & yr & "&year_max=" & yr & "&season_start=1&season_end=-1&pos=0&game_type=R&career_game_num_min=1&career_game_num_max=400&game_num_min=0&game_num_max=99&week_num_min=14&week_num_max=17&c1stat=fantasy_points&c1comp=gt&c1val=-15&c5val=1.0&order_by=fantasy_points&offset=" & x, False
xHttp.Send

with bStrm
    .type = 1
    .open
    .write xHttp.responseBody
    .savetofile file_path & "\FB Ref\" & yr & "-" & x & ".txt", 2
    .close
end with

next
next

Set bStrm = Nothing
Set xHttp = Nothing

msgbox "Done"