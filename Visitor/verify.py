import sys
import json

parent = sys.argv[2]
children = []
function = sys.argv[3]
success = True


# Load Data
f = open(sys.argv[1], 'r')
data = json.load(f)
f.close()


# Find Visitor
for obj in data["bcClass"]:
	name = obj["name"]
	if (name[name.rfind(".") + 1 : ] == function + "Visitor"):
		break
else:
	success = False
	print ("No Visitor Found")

# Find Children
for obj in data["bcClass"]:
	if (obj["parent"] == parent):
		children.append(obj["classID"])
if (children == []):
	success = False
	print ("No Childen Found")

# Check Each Child
for child in children:
	for obj in data["bcMembers"]:
		if ((obj["classID"] == child) and (obj["name"] == "accept(" + function + "Visitor)")):
			break
	else:
		print ("Invalid Childen Found: " + str(child))
		break

if (success):
	print ("Visitor for " + function + " is valid.")