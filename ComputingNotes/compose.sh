#!/bin/bash
#for a in $(ls -1 *pdf); do
#pdfinfo $a > 1.temp ;
#echo $a;
#cat -v 1.temp | grep Pages | tr -s ' ' | cut -d' ' -f 2;
#done


echo "" | ps2pdf -sPAPERSIZE=a4 - blank.pdf

rm -f FIRST1.pdf SECOND1.pdf  SECOND2.pdf SECOND3.pdf  FS.pdf
pdftk  2052_001.pdf 2052_044.pdf 2053_001.pdf 2054_001.pdf 2055_001.pdf 2056_001.pdf 2057_001.pdf 2058_001.pdf 2059_001.pdf 2060_001.pdf 2060_028.pdf 2061_001.pdf 2062_001.pdf 2063_001.pdf 2063_033.pdf 2064_001.pdf 2065_001.pdf 2065_037.pdf 2066_001.pdf 2067_001.pdf 2068_001.pdf 2068_048.pdf  cat output FIRST1.pdf

pdftk  2069_001.pdf 2070_001.pdf 2071_001.pdf 2072_001.pdf 2073_001.pdf 2074_001.pdf 2075_001.pdf 2076_001.pdf 2077_001.pdf 2077_055.pdf 2078_001.pdf 2079_001.pdf cat output SECOND1.pdf

pdftk SECOND1.pdf cat end-1 output SECOND2.pdf


pdftk A=SECOND2.pdf B=blank.pdf cat A1-99 B1 A100-end output SECOND3.pdf

pdftk A=FIRST1.pdf B=SECOND3.pdf  shuffle A B  output FS.pdf




#rm -rf Separate
#mkdir -p Separate

#exit
#pdftk  FS.pdf cat 1-4 output     Separate/0Index.pdf
touch                            Separate/1Doesntexist.pdf
pdftk  FS.pdf cat 5-26 output    Separate/XAShortGuideTo.pdf
pdftk  FS.pdf cat 27-30 output   Separate/XImportantNote.pdf
pdftk  FS.pdf cat 31-34 output   Separate/2.pdf
pdftk  FS.pdf cat 35-42 output   Separate/3.pdf
pdftk  FS.pdf cat 43-44 output   Separate/4.pdf
pdftk  FS.pdf cat 45-50 output   Separate/5.pdf
pdftk  FS.pdf cat 51-54 output   Separate/6.pdf
#FIXME?
pdftk  FS.pdf cat 55-60 output   Separate/7.pdf
pdftk  FS.pdf cat 61-68 output   Separate/8.pdf
pdftk  FS.pdf cat 69-70 output   Separate/9.pdf
pdftk  FS.pdf cat 71-84 output   Separate/10.pdf
pdftk  FS.pdf cat 85-86 output   Separate/11.pdf
pdftk  FS.pdf cat 87-90 output   Separate/11a.pdf

pdftk  FS.pdf cat 91-94 output   Separate/12.pdf
pdftk  FS.pdf cat 95-100 output   Separate/12v2.pdf

pdftk  FS.pdf cat 101-102 output   Separate/13.pdf
pdftk  FS.pdf cat 103-106 output   Separate/14.pdf



pdftk  FS.pdf cat 107-110 output   Separate/14a.pdf
pdftk  FS.pdf cat 111-114 output   Separate/14c.pdf

pdftk  FS.pdf cat 114-120 output   Separate/14d.pdf





pdftk  FS.pdf cat 121-122 output   Separate/15.pdf

pdftk  FS.pdf cat 123-134 output   Separate/16.pdf

pdftk  FS.pdf cat 135-136 output   Separate/17.pdf

pdftk  FS.pdf cat 137-140 output   Separate/18.pdf


pdftk  FS.pdf cat 141-142 output   Separate/19.pdf




pdftk  FS.pdf cat 143-144 output   Separate/20.pdf



pdftk  FS.pdf cat 145-146 output   Separate/20a.pdf



pdftk  FS.pdf cat 147-152 output   Separate/21.pdf

pdftk  FS.pdf cat 153-200 output   Separate/22multiple.pdf



pdftk  FS.pdf cat 201-214 output   Separate/23.pdf


pdftk  FS.pdf cat 214-222 output   Separate/23a.pdf


pdftk  FS.pdf cat 223-224 output   Separate/23b.pdf



pdftk  FS.pdf cat 225-226 output   Separate/24.pdf



pdftk  FS.pdf cat 227-232 output   Separate/24a.pdf



pdftk  FS.pdf cat 233-234 output   Separate/25.pdf



pdftk  FS.pdf cat 235-238 output   Separate/26.pdf


pdftk  FS.pdf cat 239-242 output   Separate/27.pdf




pdftk  FS.pdf cat 243-248 output   Separate/28.pdf


pdftk  FS.pdf cat 249-252 output   Separate/29.pdf


pdftk  FS.pdf cat 253-258 output   Separate/30.pdf




pdftk  FS.pdf cat 259-262 output   Separate/31.pdf

pdftk  FS.pdf cat 263-264 output   Separate/XStatusOfTPDatasets.pdf




pdftk  FS.pdf cat 265-268 output   Separate/32.pdf





pdftk  FS.pdf cat 269-270 output   Separate/33.pdf





pdftk  FS.pdf cat 271-282 output   Separate/34.pdf





pdftk  FS.pdf cat 283-288 output   Separate/35.pdf



pdftk  FS.pdf cat 289-290 output   Separate/36.pdf



pdftk  FS.pdf cat 291-292 output   Separate/37.pdf




pdftk  FS.pdf cat 293-296 output   Separate/38.pdf



pdftk  FS.pdf cat 297-298 output   Separate/39.pdf

pdftk  FS.pdf cat 299-302 output   Separate/39a.pdf






pdftk  FS.pdf cat 303-304 output   Separate/40.pdf




pdftk  FS.pdf cat 305-312 output   Separate/41.pdf




pdftk  FS.pdf cat 313-314 output   Separate/42.pdf


pdftk  FS.pdf cat 315-320 output   Separate/43.pdf


pdftk  FS.pdf cat 321-322 output   Separate/44.pdf








pdftk  FS.pdf cat 323-332 output   Separate/45.pdf
pdftk  FS.pdf cat 333-334 output   Separate/45a.pdf


pdftk  FS.pdf cat 335-338 output   Separate/46.pdf


pdftk  FS.pdf cat 339-340 output   Separate/47.pdf


pdftk  FS.pdf cat 341-342 output   Separate/48.pdf





pdftk  FS.pdf cat 343-344 output   Separate/49.pdf


pdftk  FS.pdf cat 345-346 output   Separate/50.pdf


pdftk  FS.pdf cat 347-376 output   Separate/51.pdf
pdftk  FS.pdf cat 377-378 output   Separate/52.pdf

pdftk  FS.pdf cat 379-380 output   Separate/53e.pdf
#381 is a part of 65

pdftk  FS.pdf cat 383-390 output   Separate/54multiple.pdf

pdftk  FS.pdf cat 391-398 output   Separate/55.pdf
pdftk  FS.pdf cat 399-402 output   Separate/56.pdf





pdftk  FS.pdf cat 403-406 output   Separate/57.pdf



pdftk  FS.pdf cat 407-410 output   Separate/58.pdf









pdftk  FS.pdf cat 411-412 output   Separate/59.pdf



pdftk  FS.pdf cat 413-414 output   Separate/60.pdf

pdftk  FS.pdf cat 415-416 output   Separate/61.pdf

pdftk  FS.pdf cat 417-420 output   Separate/62.pdf


pdftk  FS.pdf cat 421-426 output   Separate/63.pdf
pdftk  FS.pdf cat 427-428 output   Separate/63a.pdf


pdftk  FS.pdf cat 429-430 output   Separate/64.pdf



pdftk  FS.pdf cat 431-432 output   Separate/65.pdf




pdftk  FS.pdf cat 433-446 output   Separate/66.pdf
pdftk  FS.pdf cat 447-448 output   Separate/66a.pdf

pdftk  FS.pdf cat 449-454 output   Separate/X56.pdf


pdftk  FS.pdf cat 455-476 output   Separate/67.pdf


pdftk  FS.pdf cat 477-482 output   Separate/68.pdf




pdftk  FS.pdf cat 483-490 output   Separate/69.pdf





pdftk  FS.pdf cat 491-498 output   Separate/70.pdf



pdftk  FS.pdf cat 499-500 output   Separate/72.pdf

pdftk  FS.pdf cat 500-506 output   Separate/73.pdf


pdftk  FS.pdf cat 507-520 output   Separate/74multiple.pdf



pdftk  FS.pdf cat 521-524 output   Separate/75.pdf




pdftk  FS.pdf cat 525-530 output   Separate/76.pdf





pdftk  FS.pdf cat 531-538 output   Separate/77.pdf


pdftk  FS.pdf cat 539-546 output   Separate/78.pdf



pdftk  FS.pdf cat 547-566 output   Separate/79.pdf




pdftk  FS.pdf cat 567-574 output   Separate/80.pdf








pdftk  FS.pdf cat 575-580 output   Separate/81.pdf
pdftk  FS.pdf cat 581-588 output   Separate/82.pdf
pdftk  FS.pdf cat 587-588 output   Separate/83.pdf
pdftk  FS.pdf cat 589-590 output   Separate/84.pdf


pdftk  FS.pdf cat 591-608 output   Separate/85.pdf


pdftk  FS.pdf cat 609-612 output   Separate/86.pdf


pdftk  FS.pdf cat 613-614 output   Separate/87a.pdf

pdftk  FS.pdf cat 615-622 output   Separate/87.pdf



pdftk  FS.pdf cat 623-628 output   Separate/88.pdf
pdftk  FS.pdf cat 629-632 output   Separate/89.pdf



pdftk  FS.pdf cat 633-636 output   Separate/90.pdf


pdftk  FS.pdf cat 637-660 output   Separate/91.pdf



pdftk  FS.pdf cat 661-672 output   Separate/92.pdf


pdftk  FS.pdf cat 673-676 output   Separate/93.pdf





pdftk  FS.pdf cat 677-692 output   Separate/94.pdf




pdftk  FS.pdf cat 693-696 output   Separate/95.pdf


pdftk  FS.pdf cat 697-698 output   Separate/96.pdf


pdftk  FS.pdf cat 699-702 output   Separate/97.pdf


pdftk  FS.pdf cat 703-822 output   Separate/98mixed.pdf



pdftk  FS.pdf cat 823-828 output   Separate/99.pdf





pdftk  FS.pdf cat 829-836 output   Separate/100.pdf



pdftk  FS.pdf cat 837-844 output   Separate/101.pdf



pdftk  FS.pdf cat 845-892 output   Separate/102multiple.pdf

