#!/usr/bin/python

import os, re

aliases = os.listdir('/Users/greg/Documents/freex/')
aliases = [a.lower() for a in aliases if '.freex' in a]
aliases.sort(reverse=True)
aliases = [re.escape(a.lower()) for a in aliases]
aliasRegexpStr = '\\b'+'\\b|\\b'.join(aliases)+'\\b'
aliasRegexpStr = aliasRegexpStr.replace('\\ ', ' ?\\\n? *')
aliasRegexpStr = aliasRegexpStr.replace('\\ ', ' ?\\\n? *')
impLinkRegexp = re.compile(aliasRegexpStr,re.IGNORECASE|re.MULTILINE)


