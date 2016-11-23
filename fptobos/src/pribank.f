      SUBROUTINE PRIHEAD(IPHEAD)
*
*   Print the content of the HEAD bank, with pointer IPHEAD
*   IPHEAD points to the 4th word of the BOS header, i.e. to LENGTH
*   (this is BOS convention)   IPHEAD is a pointer in array IEVENT
*
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
*   J.Olsson   15.12.2005
*
* ----------------  CODE  ----------------------
*
      CHHEAD = CHASCI(IEVENT(IPHEAD-3))
      II = IPHEAD
*
      WRITE(6,1001) CHHEAD,IEVENT(II-2),IEVENT(II-1),IEVENT(II)
 1001 FORMAT(' ',A4,' ',2I6,'    Length ',I5)
      WRITE(6,1002)
 1002 FORMAT(' =================================')
*

*-----------------------
 9999 CONTINUE
      RETURN
      END
