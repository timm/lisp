import markdown,sys
str = '\n'.join(map(lambda x: x.rstrip('\n'),
                     sys.stdin.readlines()))
print(markdown.Markdown(extensions=['footnotes',
           'codehilite','def_list','tables','toc']).convert(str))

