import markdown,sys,re
from markdown.extensions.fenced_code import FencedCodeExtension
str = '\n'.join(map(lambda x: x.rstrip('\n'),
                     sys.stdin.readlines()))
print(markdown.Markdown(extensions=['footnotes','def_list','tables','toc']).convert(str))
