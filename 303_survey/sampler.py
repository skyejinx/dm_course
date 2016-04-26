import random
import os
import csv

#this will sample a list of stops
def sampler(stops_list, loclist): 
	r_start = 1
	r_stop = len(stops_list)
	r_step = 1 #this way, we don't get stops right next to each other
	stop_number = 400 #for now
	sampled_stops = []
	i = 0
	while i <= stop_number:
		x = random.randrange(r_start, r_stop, r_step) 
	#	print "Stopname", stops_list[x][5]
		l = getLoc(stops_list[x], loclist)
		if l!= None: 
			sampled_stops.append(l)
			i += 1
	print sampled_stops, "finished!!!"
	return sampled_stops

#make sure the stop is in a neighborhood we care about
#the loclist was done by hand from a map, so extend each square a bit for coverage
def getLoc(stop, loclist): 
	for i in xrange(1, len(loclist)-1): 
		if float(stop[0]) <= (float(loclist[i][0])+.01) and float(stop[0]) >= (float(loclist[i][1])-.01): 
		#	print "yeah!"
			if float(stop[2]) <= (float(loclist[i][2])+.01) and float(stop[2]) >= (float(loclist[i][3])-.01):
	#			print "yes"
				return stop[5], loclist[i][0], loclist[i][2], loclist[i][4]
	return None

def main(file): 
	stopfile = open(FIL, 'r')
	datareader = csv.reader(stopfile, delimiter=',')
	stopslist = []
	for row in datareader:
		stopslist.append(row)
	print stopslist[0][0], stopslist[0][2]
	locfile = open(LOC, 'r')
	datareader = csv.reader(locfile, delimiter=',')
	loclist = []
	for row in datareader: 
		loclist.append(row)
	print loclist[0]
	print loclist[1]
	sampled_list=sampler(stopslist, loclist)
	with open('stops_secondpass.csv', 'w') as csvfile: 
		for i in xrange(0, len(sampled_list)): 
			writer = csv.writer(csvfile, delimiter=',')
			writer.writerow(sampled_list[i])
	print "wohoo!"
	
FIL = "C:/Users/Skye/SkyDrive/CMU/36-303 Sampling and Surveys/Survey/PAT Data/general_transit_bing/stops.txt"	
LOC = "C:/Users/Skye/SkyDrive/CMU/36-303 Sampling and Surveys/Survey/locations.txt"

main(FIL)