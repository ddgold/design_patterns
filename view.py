import sys
import json

data = {}

def printClass():
	for obj in data["bcClass"]:
		print ("bcClass(" + str(obj["classID"]) + "," + str(obj["notInterface"]) + "," + obj["name"] + "," + obj["parent"] + ")")

def printImplements():
	for obj in data["bcImplements"]:
		print ("bcImplements(" + str(obj["interfaceID"]) + "," + obj["class"]  + "," + obj["interface"] + ")")

def printMembers():
	for obj in data["bcMembers"]:
		print ("bcMembers(" + str(obj["classID"]) + "," + str(obj["memberID"]) + "," + str(obj["isStatic"]) + "," + str(obj["isMethod"]) + "," + obj["type"] + "," + obj["extras"] + "," + obj["name"] + ")")

f = open(sys.argv[1], 'r')
data = json.load(f)
f.close()

print ("--- bcClass ---")
printClass()
print ()

print ("--- bcImplements ---")
printImplements()
print ()

print ("--- bcMembers ---")
printMembers()
print ()