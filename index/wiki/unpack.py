#!/usr/bin/env python
from lxml import etree
import bz2
import shutil

"""
This script lazily decompresses a wikipedia dump file.
Articles are written out, each followed by a NULL-byte indicating its end.

The latest dump file can be found here:
http://dumps.wikimedia.org/enwiki/latest/enwiki-latest-pages-articles.xml.bz2
"""



path = "enwiki-latest-pages-articles.xml.bz2"
fout = file("dump.txt", "w")
articles = 0
min_size = size_step = 2**24 # 16 MB step size

text_tag = "{http://www.mediawiki.org/xml/export-0.8/}text"
title_tag = "{http://www.mediawiki.org/xml/export-0.8/}title"
for _, e in etree.iterparse(bz2.BZ2File(path)):
#	if e.tag == title_tag: print e.text.encode("u8")
	if e.tag == text_tag and not e.text.startswith("#REDIRECT"):
		articles += 1
		fout.write(e.text.encode("u8") + '\0')
		if fout.tell() >= min_size:
			print "size:", fout.tell()
			print "articles:", articles
			fout.close()
			shutil.copyfile("dump.txt", "dump_%04d.txt" % (min_size >> 20))
			fout = file("dump.txt", "a")
			min_size += size_step
	e.clear()




