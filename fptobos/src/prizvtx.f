      SUBROUTINE PRIZVTX(IPZVTX)
*
*   Print the content of the ZVTX bank, with pointer IPZVTX
*   IPPATR points to the 4th word of the BOS header, i.e. to LENGTH
*   (this is BOS convention)   IPZVTX is a pointer in array IEVENT
*   Format Description, see  JCN 17
*     Note:  words 4,5  are reals, NOT integers! They must be converted
*            using the IBM3E conversion routine
*
*   Last Update  15.12.2005   J.O.     09.06.2008   J.O.
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
      CHARACTER CHZVTX*4,CHASCI*4
*
*   J.Olsson   15.12.2005
*
* ----------------  CODE  ----------------------
*
      CHZVTX = CHASCI(IEVENT(IPZVTX-3))
      II = IPZVTX
*
      WRITE(6,1001) CHZVTX,IEVENT(II-2),IEVENT(II-1),IEVENT(II)
 1001 FORMAT(' ',A4,' ',2I6,'    Length ',I5)
      WRITE(6,1002)
 1002 FORMAT(' =================================')
*---- 
 100  CONTINUE
      WRITE(6,1009)
 1009 FORMAT(' ',8X,
     $       'Z-vertex, distrib.width, error, NRHITS NBGHTS IFLG')
      WRITE(6,1008) (IEVENT(II+J),J=1,6)
 1008 FORMAT('  hex: ', 6(2X,Z8))
*
*  conversion to IEEE FP
*
      DO 2001 J = 1,5
      CALL CNVIBM3E(IEVENT(II+J))
 2001 CONTINUE
*
      WRITE(6,10081) (REVENT(II+J),J=1,3),
     $               (IFIX(REVENT(II+J)),J=4,5),
     $               (IEVENT(II+J),J=6,6)
10081 FORMAT(' ',3(1X,F12.6),2X,3I6)
      WRITE(6,10082) (REVENT(II+J),J=1,3),
     $               (IFIX(REVENT(II+J)),J=4,5),
     $               (IEVENT(II+J),J=6,6)
10082 FORMAT(' ',3(1X,E14.6),2X,3I6)
*
*-----------------------
 9999 CONTINUE
      RETURN
      END
