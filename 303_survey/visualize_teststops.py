

kmlPrelude="""<?xml version="1.0" encoding="UTF-8"?>
<kml xmlns="http://www.opengis.net/kml/2.2">
"""
kmlContent=""
kmlEpilogue="</kml>"
with open("thirdtry.txt","r") as csvfile:
	lines=csvfile.readlines()
	for line in lines:
		linelist=line.split(",")
		lon=linelist[2]
		lat=linelist[1]
		label=linelist[0]
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
with open("vis_results.kml","w") as file:
	file.write(kmlPrelude+kmlContent+kmlEpilogue)