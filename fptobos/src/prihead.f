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
      CHARACTER CHHEAD*4,CHASCI*4
      INTEGER IHALF(200)
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
*   Convert the 16 bit word info in HEAD, to local array IHALF
*
      DO 100 J = 1,100
      CALL TWOIN1(IEVENT(II+J),IHALF(J*2-1),IHALF(J*2))
 100  CONTINUE
*
          IK = -9
 200      CONTINUE
          IK = IK+ 10
          IF(IK+9.GT.200) GO TO 9999
          JK = IK + 9
          WRITE(6,1110) IK,JK,(IHALF(IK+LL),LL=0,9)
1110      FORMAT(' ',I4,'-',I4,2X,10(1X,I7))
          GO TO 200
*-----------------------
 9999 CONTINUE
      RETURN
      END
