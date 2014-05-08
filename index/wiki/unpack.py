#!/usr/bin/env python
from lxml import etree
import bz2

"""
This script lazily decompresses a wikipedia dump file.
Articles are written out, each followed by a NULL-byte indicating its end.
The script terminates when the output file size exceeds min_size.

The latest dump file can be found here:
http://dumps.wikimedia.org/enwiki/latest/enwiki-latest-pages-articles.xml.bz2
"""


path = "enwiki-latest-pages-articles.xml.bz2"
fout = file("test.txt", "w")
min_size = 2 * 2**30 # 2GB


text_tag = "{http://www.mediawiki.org/xml/export-0.8/}text"
title_tag = "{http://www.mediawiki.org/xml/export-0.8/}title"
articles = 0
for _, e in etree.iterparse(bz2.BZ2File(path)):
#	if e.tag == title_tag: print e.text.encode("u8")
	if e.tag == text_tag and not e.text.startswith("#REDIRECT"):
		articles += 1
		fout.write(e.text.encode("u8") + '\0')
		if fout.tell() >= min_size:
			print "size:", fout.tell()
			print "articles:", articles
			fout.close()
			break
	e.clear()
