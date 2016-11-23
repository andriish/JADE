      SUBROUTINE PRIZEHD(IPZEHD)
*
*   Print the content of the ZEHD bank, with pointer IPZEHD
*   IPPATR points to the 4th word of the BOS header, i.e. to LENGTH
*   (this is BOS convention)   IPZEHD is a pointer in array IEVENT
*      Format, see JCN 99
*
*   12.06.2008   J.O.
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
      CHARACTER CHZEHD*4,CHASCI*4,CHEBCD*4
      INTEGER IHALF(200)

*
*   J.Olsson   15.12.2005
*
* ----------------  CODE  ----------------------
*
      CHZEHD = CHASCI(IEVENT(IPZEHD-3))
      LEZEHD = IEVENT(IPZEHD)
      II = IPZEHD
*
*
      WRITE(6,1001) CHZEHD,IEVENT(II-2),IEVENT(II-1),IEVENT(II),
     $              IRUN,IEVN
 1001 FORMAT(' ',A4,' ',2I6,'    Length ',I5,' JADE Run/evnt ',2I8)
      WRITE(6,1002)
 1002 FORMAT(' ================================================',
     $       '================')
*
      DO 100 J = 1,3
      CALL TWOIN1(IEVENT(II+J),IHALF(J*2-1),IHALF(J*2))
 100  CONTINUE
*
      WRITE(6,1051) (IHALF(J),J=1,6)
 1051 FORMAT('  Vers.,MCflg,LZHD,LCUT,LMCH,dumm ',6I6)
* 
*  conversion to IEEE FP
*
      DO 2006 J = 4,5
       CALL CNVIBM3E(IEVENT(II+J))
 2006 CONTINUE
*        
      WRITE(6,1028) (REVENT(II+J),J=4,5)
 1028 FORMAT('   Unknown info ',2(1X,F12.6))
*
      CALL TWOIN1(IEVENT(II+6),IHALF1,IHALF2)
      WRITE(6,1025) IHALF1,IHALF2
 1025 FORMAT('  NRPHCT NRZCT  ',2I6)
*
      DO 2007 J = 7,15
       CALL CNVIBM3E(IEVENT(II+J))
 2007 CONTINUE
*
      WRITE(6,1032) (REVENT(II+J),J=7,9)
 1032 FORMAT(' RMINCT ZCT PCT ',3(1X,F12.6))
*
      WRITE(6,1033) (REVENT(II+J),J=10,12)
 1033 FORMAT(' unknown cuts ',3(1X,F12.6))
*
      WRITE(6,1034) (REVENT(II+J),J=13,15)
 1034 FORMAT(' ELGMIN ELGPHM "0.5"  ',3(1X,F12.6))
*
*-----------------------
 9999 CONTINUE
      RETURN
      END
