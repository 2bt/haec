#!/usr/bin/env python
import xml.etree.ElementTree as ET
import urllib2
import string




prefix = "B"
gaplimit = 300


URL = """http://en.wikipedia.org/w/api.php?format=xml&action=query&prop=revisions&rvprop=content&generator=allpages&gaplimit=%d&gapfilterredir=nonredirects&gapfrom=%s"""

#for prefix in string.uppercase:

while True:

	name = "%s-%d.txt" % (prefix, gaplimit)
	print name

	url = URL % (gaplimit + 1, prefix)
	content = urllib2.urlopen(url).read()
	root = ET.fromstring(content)


	f = file(name, "w")

	for i in range(gaplimit):
		page = root[1][0][i]
		#title = page.attrib["title"]
		f.write(page[0][0].text.encode("u8") + '\0')

	prefix = urllib2.quote(root[1][0][gaplimit].attrib["title"].encode("utf-8"))

