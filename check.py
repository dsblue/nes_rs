#!/usr/bin/python


golden = list()
test = list()

with open('nestest.log', 'r') as f:
    for line in f.readlines():
        golden.append(line[0:14] + " " + line[16:19] + line[47:73])

with open('run.log', 'r') as f:
    for line in f.readlines():
        test.append(line[11:25] + " " + line[27:31] + line[64:89])

i = 0
for (t, g) in zip(test, golden):
    if t != g:
        print(f"%08i|%s|%s|" % (i, t, g))
    i = i + 1
