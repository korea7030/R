data(Andrew)
Andrew

# showTips - 풍선도움말 여부
# showLine - 점선 표시 여부
# enableScrollWheel - 마우스 휠 작동여부
# useMapTypeControl - 지도/위성 작동여부
# 
storm1 <- gvisMap(Andrew, "LatLong", "Tip", 
                  options=list(showTips=TRUE, showLine=TRUE, enableScrollWheel=TRUE, mapType="hybrid", useMapTypeControl=TRUE, width=800, hieght=400))

storm2 <- gvisMap(Andrew, "LatLong", "Tip", 
                  options=list(showTips=FALSE, showLine=FALSE, enableScrollWheel=FALSE, mapType="hybrid", useMapTypeControl=TRUE, width=800, hieght=400))

plot(storm1)

plot(storm2)
