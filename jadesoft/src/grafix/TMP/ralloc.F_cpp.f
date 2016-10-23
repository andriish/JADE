C   08/06/86 606080751  MEMBER NAME  RALLOC   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE RALLOC( OK )
C-----------------------------------------------------------------------
C
C    AUTHOR:   C. BOWDERY   8/06/86 :  ALLOCATE INPUT DATASET
C
C
C     ALLOCATES A DDNAME TO THE INPUT DSNAME WHICH IS GIVEN BY THE
C     USER. OK = .TRUE. IF THE ALLOCATION WAS SUCCESSFUL.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL  OK
C
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
      COMMON / CSVCW1 / NDDSVE,NRWR
C
C------------------  C O D E  -----------------------------------------
C
C                            ALLOCATE FORTRAN LUN TO INPUT DATA SET.
C
      NDDINN = 0
      CALL GETDS(NDDINN,'Please enter FULL NAME of the Dataset with the
     +Events  (without apostrophes)^',IDATSV,HERR)
C
      IF( HERR .EQ. 0 ) GO TO 85
C
C                            ERROR HAS OCCURRED ON ALLOCATION.
C
      WRITE(6,80)
  80  FORMAT(' Premature End of Graphics Program')
      OK = .FALSE.
      RETURN
C
C                            SUCCESSFULLY ALLOCATED DATA SET.
C
  85  MAXREC = 0
C
C                            NDDOUT SET TO ZERO HERE. IS SET TO O/P
C                            LUN AFTER FIRST CALL TO S/R WRIT.
C
      NDDOUT = 0
      NDDSVE = 0
      NRWR   = 0
C
      OK     = .TRUE.
C
      RETURN
      END
