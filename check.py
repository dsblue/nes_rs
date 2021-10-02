#!/usr/bin/python


golden = list()
test = list()

with open('nestest.log', 'r', encoding='utf-8') as f:
    for line in f.readlines():
        golden.append(line[0:14] + " " + line[16:19] + line[47:73])

try:
    f = open('run.log', 'r', encoding='utf-8')
    lines = f.readlines()
except UnicodeDecodeError:
    f = open('run.log', 'r', encoding='utf-16')
    lines = f.readlines()

for line in lines:
    test.append(line[11:25] + " " + line[27:31] + line[64:89])

i = 0
for (t, g) in zip(test, golden):
    if t != g:
        print(f"%08i|%s|%s|" % (i, t, g))
    i = i + 1
