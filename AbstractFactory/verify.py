import sys
import json

# parent = sys.argv[2]
children = []
factory = sys.argv[2]
success = True


# Load Data
f = open(sys.argv[1], 'r')
data = json.load(f)
f.close()


# Find AbstractFactory
for obj in data["bcClass"]:
	name = obj["name"]
	if (name[name.rfind(".") + 1 : ] == "AbstractFactory"):
		break
else:
	success = False
	print ("No AbstractFactory Found")

# Find ConcreteFactory root
for obj in data["bcClass"]:
	name = obj["name"]
	if (name[name.rfind(".") + 1 : ] == factory + "Factory"):
		break
else:
	success = False
	print ("No " + factory + "Factory Found")

# Check Each ConcreateFactory has correct functinos
for obj in data["bcClass"]:
	if (obj["parent"] == factory + "Factory"):
		check = [0, 1]
		for mem in data["bcMembers"]:
			if (mem["classID"] == obj["classID"]):
				if (mem["name"] == "createBox(String)"):
					check.remove(0)
				if (mem["name"] == "createButton(String)"):
					check.remove(1)
		if check != []:
			success = False
			print ("Invalid ConreateFactory: " + obj["name"][name.rfind(".") + 1 : ])
			break


if (success):
	print ("AbstractFactory is valid.")