import xml.etree.ElementTree as ET
import urllib2
import string

URL = """http://en.wikipedia.org/w/api.php?format=xml&action=query&prop=revisions&rvprop=content&generator=allpages&gaplimit=%d&gapfilterredir=nonredirects&gapfrom=%s"""

prefix = "A"
gaplimit = 300


for prefix in string.uppercase:
	name = "%s-%d.txt" % (prefix, gaplimit)
	print name

	url = URL % (gaplimit, prefix)
	content = urllib2.urlopen(url).read()
	print content
	root = ET.fromstring(content)


	f = file(name, "w")
	for page in root[1][0]:
	#	print page.attrib["title"]
		#print page[0][0].text.encode("u8") + '\0'
		f.write(page[0][0].text.encode("u8") + '\0')

