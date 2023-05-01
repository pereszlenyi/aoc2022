10 rem solution for day 4: camp cleanup
20 print ""
30 print "program started"

40 rem data for animation
50 an%=1
60  an$(1)=chr$(157)+chr$(114)
70  an$(2)=chr$(157)+chr$(102)
80  an$(3)=chr$(157)+chr$(96)
90  an$(4)=chr$(157)+chr$(100)
100 an$(5)=chr$(157)+chr$(101)
110 an$(6)=chr$(157)+chr$(183)
120 an$(7)=chr$(157)+chr$(101)
130 an$(8)=chr$(157)+chr$(100)
140 an$(9)=chr$(157)+chr$(96)
150 an$(10)=chr$(157)+chr$(102)

160 rem counter for part 1
170 c1%=0
180 rem counter for part 2
190 c2%=0
200 rem opening input file
210 open 1, 8, 2, "input,seq,r"
220 print "processing input...  ";
230 for s=0 to 1
240   input# 1, a$, b$
250   x$ = a$
260   gosub 1000
270   al%=xl%: ar%=xr%
280   x$ = b$
290   gosub 1000
300   bl%=xl%: br%=xr%
310   rem checking for containment
320   if al%<=bl% and br%<=ar% then 2000
330   if bl%<=al% and ar%<=br% then 2000
340   rem checking for overlap
350   if al%<=bl% and bl%<=ar% then 3000
360   if bl%<=al% and al%<=br% then 3000
370   gosub 4000
380   s=255 and st
390   next s
400 close 1
410 print chr$(20) "done"

420 rem checking if reading input
430 rem was successful and
440 rem ending program if it wasn't
450 if s and 64 = 0 then end

460 rem printing result
470 print "# of full containments:";
480 print c1%
490 print "# of overlaps:";
500 print c2%
510 end

990 rem parsing a section assignment
1000 xl%=val(x$)
1010 rl%=len(x$)-len(str$(xl%))
1020 xr%=val(right$(x$,rl%))
1030 return

1990 rem incrmenting counter 1
2000 c1%=c1%+1
2010 goto 350

2990 rem incrmenting counter 2
3000 c2%=c2%+1
3010 goto 370

3990 rem progress animation
4000 print an$(an%);
4010 an%=an%+1
4020 if an%=11 then an% = 1
4030 return
