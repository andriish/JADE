      SUBROUTINE PRIPATR(IPPATR)
*
*   Print the content of the PATR bank, with pointer IPPATR
*   IPPATR points to the 4th word of the BOS header, i.e. to LENGTH
*   (this is BOS convention)   IPPATR is a pointer in array IEVENT
*
*   J. Olsson  14.12.2005
*   Last Update  10.06.2008   J.O.
*
C********** MACRO CEVENT **************
C   Event Buffer
C   The size corresponds to 2 MByte event size, maximum
C
      COMMON /CEVENT/IEVENT(525000)
      DIMENSION REVENT(525000)
      EQUIVALENCE (IEVENT(1),REVENT(1))
*=====================================
*
      CHARACTER CHPATR*4,CHASCI*4
*
*   J.Olsson   14.12.2005
* ----------------  CODE  ----------------------
*
      CHPATR = CHASCI(IEVENT(IPPATR-3))
      II = IPPATR
*
      WRITE(6,1001) CHPATR,IEVENT(II-2),IEVENT(II-1),IEVENT(II)
 1001 FORMAT(' ',A4,' ',2I6,'    Length ',I5)
      WRITE(6,1002)
 1002 FORMAT(' =================================')
*
      NRPATR = IEVENT(II-2)
*
      WRITE(6,1003) (IEVENT(II+J),J=1,8)
 1003 FORMAT(' Header ',3I8,2X,Z8,4I8)
      WRITE(6,1004)
 1004 FORMAT('         lheader nrtrcks ltrinfo   history nr.hits',
     $       ' unc.hts unc.elm  unused')
*--
      NTR = IEVENT(II+2)
      IF(NTR.LE.0) THEN
        WRITE(6,4545)
 4545   FORMAT('  No tracks! ')
        GO TO 9999
      ENDIF
*
      LENTR = IEVENT(II+3)
      LHEAD = IEVENT(II+1)
      KTR = 0
      II = II + LHEAD
      III = II - LENTR 
*---- 
 100  CONTINUE
      KTR = KTR + 1
      IF(KTR.GT.NTR) GO TO 9999
      III = III + LENTR
      II = III
      WRITE(6,1005) KTR
 1005 FORMAT('            Track nr. ',I3)
      WRITE(6,1006)
 1006 FORMAT('            *************')
*
      IF(NRPATR.NE.12) THEN
        WRITE(6,1007) IEVENT(II+1),IEVENT(II+2),IEVENT(II+3)
 1007   FORMAT('   patr track number ',I3,' program id ',Z8,
     $         '   patr date ',I10)
      ELSE
        CALL TWOIN1(IEVENT(II+2),IH1,IH2)
        WRITE(6,10071) IEVENT(II+1),IH1,IH2,IEVENT(II+3)
10071   FORMAT('   patr track number ',I3,'  MC 4-vect nr ',I6,
     $         ' MC part.id ',I6,'   patr date ',I10)
      ENDIF
*                                   PATR date is given as YYDDD
*                                   dx,dy,dz  are dir.cosines !
      II = II + 3
      WRITE(6,1009)
 1009 FORMAT(' First Measured Point:  Type, x y z   dx dy dz ')
      WRITE(6,1008) (IEVENT(II+J),J=1,7)
 1008 FORMAT('   ',I4,' hex: ', 6(2X,Z8))
*
*  conversion to IEEE FP
*
      DO 2001 J = 2,7
      CALL CNVIBM3E(IEVENT(II+J))
 2001 CONTINUE
*
      WRITE(6,10081) IEVENT(II+1),(REVENT(II+J),J=2,7)
10081 FORMAT(' ',I4,' ', 6(1X,F12.6))
*
      II = II + 7
      WRITE(6,1010)
 1010 FORMAT(' Last  Measured Point:  Type, x y z   dx dy dz ')
      WRITE(6,1008) (IEVENT(II+J),J=1,7)
*
*  conversion to IEEE FP
*
      DO 2002 J = 2,7
      CALL CNVIBM3E(IEVENT(II+J))
 2002 CONTINUE
*
      WRITE(6,10081) IEVENT(II+1),(REVENT(II+J),J=2,7)
*
      II = II + 7
      WRITE(6,1011) IEVENT(II+1)
 1011 FORMAT(' XY-plane fit-Type: (1) circle, (2) parabola   ',I6)
*
      WRITE(6,1012) 
 1012 FORMAT('  4 fit parameters, chi**2    nr.hits used in fit')
      WRITE(6,1013) (IEVENT(II+J),J=2,7)
 1013 FORMAT('  ',5(Z8,2X),I10)
*
*  conversion to IEEE FP
*
      DO 2003 J = 2,6
      CALL CNVIBM3E(IEVENT(II+J))
 2003 CONTINUE
      WRITE(6,10131) (REVENT(II+J),J=2,6),IEVENT(II+7)
10131 FORMAT(' ',5(F12.6,1X),I10)
*
      WRITE(6,1014)
 1014 FORMAT('  curv.   dcurv.   curvatures at 1st and lst point')
      WRITE(6,1015) (IEVENT(II+J),J=8,11)
 1015 FORMAT('  ',4(Z8,2X))
*
*  conversion to IEEE FP
*
      DO 2004 J = 8,11
      CALL CNVIBM3E(IEVENT(II+J))
 2004 CONTINUE
      WRITE(6,10151) (REVENT(II+J),J=8,11)
10151 FORMAT('  ',4(F12.6,2X))
*
      II = II + 11
      WRITE(6,1016) IEVENT(II+1)
 1016 FORMAT(' RZ plane fit ',I6)
*
      WRITE(6,1017) 
 1017 FORMAT('  2 fit parameters, chi**2    nr.hits used in fit')
      WRITE(6,1018) (IEVENT(II+J),J=2,5)
 1018 FORMAT('  ',3(Z8,2X),I10)
*
*  conversion to IEEE FP
*
      DO 2005 J = 2,4
      CALL CNVIBM3E(IEVENT(II+J))
 2005 CONTINUE
      WRITE(6,10181) (REVENT(II+J),J=2,4),IEVENT(II+5)
10181 FORMAT('  ',3(F12.6,2X),I10)
*
      WRITE(6,1019)
 1019 FORMAT('  Cell numbers with hits of this track')
      WRITE(6,1020) (IEVENT(II+J),J=6,11)
 1020 FORMAT('  ',6I6)
*
      II = II + 11
      WRITE(6,1021)
 1021 FORMAT('   Pointers to ass. LG-clus, mu-hits, ', 
     $       'TP-tr.bank, TOF-bank,    z-fit FLAG')
      WRITE(6,1022) (IEVENT(II+J),J=1,5)
 1022 FORMAT('   ',10X,4I8,10X,I6)
*
      II = II + 5
      WRITE(6,1023)
 1023 FORMAT('  first/last valid z-coord.   nr.ass.hits  tr.history')
      WRITE(6,1024)  (IEVENT(II+J),J=1,4)
 1024 FORMAT('        ',2(Z8,2X),'   ',I6,'  ',Z8)
*
*  conversion to IEEE FP
*
      DO 2006 J = 1,2
      CALL CNVIBM3E(IEVENT(II+J))
 2006 CONTINUE
      WRITE(6,10241) (REVENT(II+J),J=1,2),(IEVENT(II+J),J=3,4)
10241 FORMAT('  ',2(F12.6,2X),'  ',I6,'  ',Z8)
*
      II = II + 4
*
      WRITE(6,3346) II-III,LENTR
 3346 FORMAT('  debug    II-III  LENTR ',2I8)
*
      IF(LENTR.LE.48) GO TO 100
*  Come here if extended track area exists (LTR = 62, or larger)
*  In this case the word 44 (z-fit FLAG)  should be 2)
*    
      WRITE(6,1025)
 1025 FORMAT(' chi**2 RPHI-fit and 6 cov.elements')
      WRITE(6,1026) (IEVENT(II+J),J=1,7)
 1026 FORMAT('  ',7(Z8,2X))
*
*  conversion to IEEE FP
*
      DO 2007 J = 1,7
      CALL CNVIBM3E(IEVENT(II+J))
 2007 CONTINUE
      WRITE(6,10261) (REVENT(II+J),J=1,7)
10261 FORMAT('  ',7(F12.6,2X))
*
      II = II + 7
      WRITE(6,1027)
 1027 FORMAT(' chi**2 RZ-fit and 3 cov.elements')
      WRITE(6,1028) (IEVENT(II+J),J=1,4)
 1028 FORMAT('  ',4(Z8,2X))
*
*  conversion to IEEE FP
*
      DO 2008 J = 1,4
      CALL CNVIBM3E(IEVENT(II+J))
 2008 CONTINUE
      WRITE(6,10281) (REVENT(II+J),J=1,4)
10281 FORMAT('  ',4(F12.6,2X))
*
      II = II + 4
      WRITE(6,1029)
 1029 FORMAT(' Date of TP track fit     2 unused words ')
      WRITE(6,1030) (IEVENT(II+J),J=1,3)
 1030 FORMAT('  ',I10,'                  ',2I8)
*
      II = II + 3
*
      WRITE(6,3346) II-III,LENTR
*
      IF(II-III.LT.LENTR) THEN
        DO 3481 IEX = 1,LENTR-II+III    
          WRITE(6,3482) IEVENT(II+IEX),IEVENT(II+IEX)
 3482     FORMAT('  Extra word!  ',I12,3X,Z8)
 3481   CONTINUE
      ENDIF
      GO TO 100
*-----------------------
 9999 CONTINUE
      RETURN
      END
