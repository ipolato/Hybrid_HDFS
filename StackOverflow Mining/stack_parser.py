import sys
import nltk
from nltk.tokenize import WordPunctTokenizer
from nltk.corpus import stopwords
from nltk.corpus import wordnet
from nltk.tokenize import word_tokenize
import xml.etree.cElementTree as ET
import re
hadoopl = ['hadoop','yarn','mapreduce']
hadoopy = set(hadoopl)

def parse_tags(tags):
	if tags == None:
		return set()
	else:
		return set(re.sub(r'[<>]',' ',tags).split(' '))

def retreiver(context):
	excp=0
	trap=0
        c=0
	count=0
	event, root = context.next()
	print '%s,%s,%s,%s,%s,%s,"%s","%s"' % tuple(['identifier','date', "yarnbody"]  + [x for x in hadoopy] + ['title','body'])
	for event, elem in context:
    		if trap==0 : ### root
			trap=1
			root.clear()
			continue

		tag=elem.attrib.get("Tags")
		tags = parse_tags(tag)
		if len(hadoopy.intersection(tags)) > 0:
			count=count+1
		else:
			continue
    		
    		identifier = elem.attrib.get("Id")
    		body= elem.attrib.get("Body")
    		title= elem.attrib.get("Title")
    		date = elem.attrib.get("CreationDate")+"Z"

		body=re.sub(r'[^\x00-\x7f]','',body) 

		title=re.sub(r'[^\x00-\x7f]',r'',title) 
    		title=re.sub('<[A-Za-z\/][^>]*>', '', str(title))
    		body=re.sub('<[A-Za-z\/][^>]*>', '', str(body))
    		elem.clear()
    		c=c+1
    		trap=0
		title=re.sub('"','',title)
		body=re.sub('"','',body)
		body=re.sub("\n",'',body)
		print '%s,%s,%s,%s,%s,%s,"%s","%s"' % tuple([identifier, date, "yarn" in body.lower() ]  + [x in tags for x in hadoopy] + [title,body])
				

file_path = "Posts.xml"
context = ET.iterparse(file_path, events=("start", "end"))

# turn it into an iterator
context = iter(context)
dict={}		
retreiver(context)

