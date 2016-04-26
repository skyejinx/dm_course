

kmlPrelude="""<?xml version="1.0" encoding="UTF-8"?>
<kml xmlns="http://www.opengis.net/kml/2.2">
"""
kmlContent=""
kmlEpilogue="</kml>"
with open("thirdtry.txt","r") as csvfile:
	lines=csvfile.readlines()
	for line in lines:
		linelist=line.split(",")
		lon=linelist[1]
		lat=linelist[0]
		label=linelist[2]
		newContent="""
<Placemark>
<name>"""+str(label)+"""</name>
<description>no description</description>
<Point>
  <coordinates>"""+lon+","+lat+""",0</coordinates>
</Point>
</Placemark>
		"""
		kmlContent+=(newContent)
with open("vis_results_fuckyouSkye.kml","w") as file:
	file.write(kmlPrelude+kmlContent+kmlEpilogue)
print "Fuck you Skye"