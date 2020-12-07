C   01/11/84            MEMBER NAME  PHNUMB   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE PHNUMB( IE, SH3, SLOPE )
C-----------------------------------------------------------------------
C
C    AUTHOR:   J. OLSSON        ?     :  DISPLAY LG PULSE HEIGTHS
C
C  LAST MOD:   J. HAGEMANN   10/10/84 :  NOW OWN MEMBER (FROM EVDISP)
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
C---------  MACRO CHSYM    SYMBOLS USED IN GRAPHICS WRITING -----
      COMMON /CHSYM/ HSYM(36)
C------ END MACRO CHSYM
C-----------------------------------------------------------------------
C                            MACRO CGRAPH .... GRAPHICS COMMON
C-----------------------------------------------------------------------
C
      LOGICAL DSPDTL,SSTPS,PSTPS,FREEZE
C
      COMMON / CGRAPH / JUSCRN,NDDINN,NDDOUT,IDATSV(11),ICREC,MAXREC,
     +                  LSTCMD,ACMD,LASTVW,ISTANV,
     +                  SXIN,SXAX,SYIN,SYAX,XMIN,XMAX,YMIN,YMAX,
     +                  DSPDTL(30),SSTPS(10),PSTPS(10),FREEZE(30),
     +                  IREADM,LABEL,LSTPS(10),IPSVAR
C
C------- END OF MACRO CGRAPH -------------------------------------------
C
C
      COMMON /CWORK1/ R,FI,R1,FI1,X1,Y1,R2,FI2,X2,Y2,ZET,X3,Y3,X4,Y4
     +               ,HMW(132)
C
C-----------------  C O D E  -------------------------------------------
C
      IF(IE.LT.1) RETURN
      DELTX = ABS(XMAX-XMIN)
      HUMP = 1
      IF(LASTVW.NE.13.AND.DELTX.LT.4000.) HUMP = 2
      IF(LASTVW.EQ.13.AND.DELTX.LT.5000.) HUMP = 2
      IF(HUMP.EQ.2) GO TO 200
C                "LOGARITMIC" DISPLAY MODE
      IDIV = 1
      DO 35 ICC = 1,5
      IDIV = IDIV*10
      JNDEX = IE/IDIV
      IF(JNDEX.GE.10) GO TO 35
      JNDEX = JNDEX + (ICC-1)*9 + 1
      GO TO 36
   35 CONTINUE
   36 IF(JNDEX.GT.36) JNDEX = 36
      HMW(1) = HSYM(JNDEX)
      CALL HEMSYM(X1,Y1,SH3,HMW,1,SLOPE)
      RETURN
C               WRITE FULL NUMBERS INTO BLOCK
200   CALL DNUM(IE,X1,Y1,SH3*.33,SLOPE)
      RETURN
      END
