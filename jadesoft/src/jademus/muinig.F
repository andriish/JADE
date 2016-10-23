C   22/02/82 510091213  MEMBER NAME  MUINIG   (JADEMUS)     FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE MUINI
C-----------------------------------------------------------------------
C
C LAST CHANGE 20.00 12/03/84 C. BOWDERY  - TO MATCH MUINI CHANGES
C      CHANGE 12.30  9/03/84 C. BOWDERY  - TO MATCH MUINI CHANGES
C      CHANGE 21.45 21/02/84 C. BOWDERY  - TO MATCH MUINI CHANGES
C      CHANGE 23.00 22/02/82 HUGH MCCANN - FOR GRAPHICS, TO SUPPRESS
C                                          ERROR OUTPUT.
C
C-----------------------------------------------------------------------
C
C MUON INITIALISATION ROUTINE.
C
C SETS UP MUON STATISTICS ARRAYS.
C
C ALSO SETS MU ANALYSIS PARAMETERS - SEE BLOCK DATA STATEMENT.
C
C
C-----------------------------------------------------------------------
C
C                            COMMONS
C
      COMMON /CMUPRN/ MUPRIN,MUHPR,IANAC,ICOOR,IANAF,LANAC,LCOOR,LANAF,
     +                ITOTAL,LTOTAL,ICUTS,LCUTS
      COMMON /CMUIEV/ IEV,NEV,IRD,KRUN,KREC,
     +                ISECS,IMINS,IHOURS,IDAY,IMONTH,IYEAR
C
C------------------  C O D E  ------------------------------------------
C
C INITIALISE EVENT COUNTER FOR MU MESSAGES.
C
      IEV=0
C
C SET MUPRIN=0 FOR GRAPHICS.
C
      MUPRIN=0
C
C INITIALISE TOTAL MESSAGE COUNT FOR THIS JOB AND SET ITS LIMIT.
C
      ITOTAL=0
      LTOTAL=500
C
C INITIALISE ERROR COUNT FOR MUANAC,MUCOOR,MUANAF FOR THIS JOB.
C                            + MUCUTS.
      IANAC=0
      ICOOR=0
      IANAF=0
      ICUTS=0
C
C SET LIMITS ON ERROR MESSAGES FROM MUANAC,MUCOOR,MUANAF FOR THIS JOB.
C                            + MUCUTS.
      LANAC=20
      LCOOR=50
      LANAF=20
      LCUTS=20
C
C-----------------------------------------------------------------------
C
 99   CONTINUE
      RETURN
C
      END
C
C-----------------------------------------------------------------------
      BLOCK DATA
C-----------------------------------------------------------------------
C
C                            COMMONS
C
#include "cmustat.for"
#include "cmuanp.for"
C
C-----------------------------------------------------------------------
C
C DATA INITIALISATION STATEMENTS.
C
      DATA MUT1    /
     1 'MUCOOR  ','ALL HITS','NO HITS ','NO MUR1I','NO MUR20',
     2 'NO MUR2I','HITS>400','HMU LEN.','HMU BAD ','NO MUR10',
     3 'MU EVENT','MU TYP 1','MU TYP 5','MU TYP 9','    ..  ',
     4 '    ..  ','    ..  ','    ..  ','    ..  ','BAD DRFT',
     5 'BAD LONG','X PULLED','Z PULLED','MULT-PUL','HIT REC.',
     6 'HIT SEQ1','HIT SEQ2','HIT# ERR','NEW CH? ','SING ERR',
     7 '    ..  ','    ..  ','    ..  ','    ..  ','    ..  ',
     8 '    ..  ','    ..  ','    ..  ','    ..  ','DC-->HIT',
     9 'BAD CH# ','CH ORDER','CH-/->CR','SING BAD','    ..  ',
     A '    ..  ','    ..  ','    ..  ','    ..  ','    ..  '  /
      DATA MUT2    /
     1 '    ..  ','    ..  ','    ..  ','    ..  ','    ..  ',
     2 '    ..  ','    ..  ','    ..  ','    ..  ','BAD CR# ',
     3 'CR ORDER','NO T.REF','EOC REC.','CR. MARK','EOC ERR.',
     4 '    ..  ','    ..  ','    ..  ','    ..  ','    ..  ',
     5 'MCRATE 1','MCRATE 2','MCRATE 3','MCRATE 4','MCRATE 5',
     6 'MCRATE 6','MCRATE 7','MCRATE 8','MCRATE 9','MCRATE10',
     7 'MCRATE11','MCRATE12','MCRATE13','MCRATE14','NO EOC 1',
     8 'NO EOC 2','NO EOC 3','NO EOC 4','NO EOC 5','NO EOC 6',
     9 'NO EOC 7','NO EOC 8','NO EOC 9','NO EOC10','NO EOC11',
     A 'NO EOC12','NO EOC13','NO EOC14','    ..  ','    ..  '  /
C
C-----------------------------------------------------------------------
C
      DATA NMU/100*0/
C
C-----------------------------------------------------------------------
C
C CUTS ETC. USED IN MULINE.......
C CUTP: CUT ON PERPENDICULAR DISTANCE FROM HIT TO LINE IN CLUSTER
C   SEARCH IN MULINE.
      DATA CUTP/400./
C WRES: WEIGHT PER AMBIGUITY RESOLVED HIT. (REMEMBER UNRESOLVED HITS
C   GET A WEIGHT OF 2. SO MAKE WRES BIGGER THAN THIS.)
      DATA WRES/4./
C
C CUTS ETC. USED IN MULINA.......
C A) IN STEP 2, THROWING OUT WILD POINTS......
C     WILD=(D1.GT.FT*DMIN1.AND.D1.GT.FT*DRES).OR.D1.GT.FT*CUTT
C    *     .OR.(DL1.GT.FL*DLMIN1.AND.DL1.GT.FL*DLRES).OR.DL1.GT.FL*CUTL
      DATA CUTT/100./
      DATA CUTL/9999./
      DATA FT/3./
      DATA FL/3./
C DRES AND DLRES: ESTIMATED RESOLUTIONS, DRIFT AND LONG. RESPECTIVELY.
      DATA DRES/15./
      DATA DLRES/999./
C B) IN STEP 3, LOOKING FOR SMALL DRIFT DISTANCES....
C FS: IF DISTANCE BETWEEN LEFT AND RIGHT POINTS IS FS*BEST RMS DRIFT
C   DEVIATION, FIX AMBIGUITY AND REPEAT FROM STEP 1.
      DATA FS/2./
C C) IN STEP 4, CLASSIFYING CLUSTERS.......
C CUTC: CLUSTERS NOT ACCEPTED IF MINIMUM RMS DRIFT DEVIATION .GT. CUT1.
C    THE TOTAL RMS DEVIATION IS SET TO -1 (NO ACCEPTABLE PERMUTATIONS).
      DATA CUTC/35./
C FR: CLUSTERS NOT ACCEPTED IF 3RD BEST FIT .LT. FR*BEST FIT. THE TOTAL
C    RMS DEVIATION IS SET TO -2 (TOO MANY ACCEPTABLE PERMUTATIONS).
      DATA FR/2./
C D) IN STEP 5, ADDING LEFT OVER SINGLE HITS TO EXISTING CLUSTERS...
C FAT: ADD IF HIT .LT. FAT*RMS DEVIATION AWAY IN DRIFT DIRECTION .OR.
C FAL: ADD IF HIT .LT. FAL*RMS DEVIATION AWAY IN LONG. DIRECTION.
      DATA FAT/4./
      DATA FAL/4./
C CUTDT IS THE CUT (IN MM) APPLIED TO THE DISTANCE OF A HIT FROM A
C GIVEN LINE IN THE DRIFT DIRECTION
      DATA CUTDT/300.0/
C THE FOLLOWING CUTS ARE USED IN MULINE
C CUTDL IS THE CUT (IN MM) APPLIED TO THE DISTANCE OF A HIT FROM A
C GIVEN LINE IN THE LONGITUDINAL DIRECTION
      DATA CUTDL/9999.0/
C
C
C-----------------------------------------------------------------------
C
      END
