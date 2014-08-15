#!/usr/bin/env python
from lxml import etree
import bz2
import sys

"""
This script lazily decompresses a wikipedia dump file.
Articles are written out, each followed by a NULL-byte indicating its end.

The latest dump file can be found here:
http://dumps.wikimedia.org/enwiki/latest/enwiki-latest-pages-articles.xml.bz2
"""


min_size = (1 << 30)

path = "enwiki-latest-pages-articles.xml.bz2"
fout = file("dump.txt", "w")
articles = 0

text_tag  = "{http://www.mediawiki.org/xml/export-0.8/}text"
title_tag = "{http://www.mediawiki.org/xml/export-0.8/}title"

for _, e in etree.iterparse(bz2.BZ2File(path)):
	if e.tag == text_tag and not e.text.startswith("#REDIRECT"):
		articles += 1
		fout.write(e.text.encode("u8") + '\0')
		if fout.tell() >= min_size: break
	e.clear()

	sys.stdout.write("%02d%%\r" % (fout.tell() * 100 / min_size))

print
print "size:", fout.tell()
print "articles:", articles
fout.close()
