#imports
library('rjson')

#data import 
sfo <- read.csv('final_sfo_lines.csv')
crime <- fromJSON('crimeOSFO.json')


