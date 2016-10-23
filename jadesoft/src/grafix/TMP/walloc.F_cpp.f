C   05/08/84 507171442 MEMBER NAME  WALLOC   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE WALLOC
C-----------------------------------------------------------------------
C
C   AUTHOR:   J. OLSSON      ?     :  ALLOCATE OUTPUT DATASET
C
C        MOD: J. OLSSON    3/11/82 :
C   LAST MOD: C. BOWDERY  17/07/85 :  SMALL CHANGES, NEW NAME
C
C      ALLOCATE OUTPUT DATA SET IN GRAPHICS, JADE ROUTINE
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
      REAL*8 DDN
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
C==MACRO CIOUNI=========================================
      COMMON/CIOUNI/IUNIT,JUNIT,NCALI,KUNITA(10),LUNITA(10)
C==ENDMACRO CIOUNI========================================
C
      COMMON / CBSTR  / MODE,MAXRL
      COMMON / CSVCW1 / NDDSVE,NRWR
      COMMON / CWALLK / IOUTSV(11)
      DIMENSION IDATAL(11),HVOLSR(4)
      DATA ICONT /'CONT'/
      DATA HBLANK/'  '/
*** PMF 02/12/99
      INTEGER LI
      CHARACTER CDATAL*44
      EQUIVALENCE (CDATAL,IDATAL(1))
*** PMF (end)
C
C-------------------  C O D E  -----------------------------------------
C
   1  CALL TRMOUT(80,'Please enter FULL NAME of catalogued dataset for e
     $vent output:^')
      CALL TRMIN(44,IDATAL)
C
C                            TEST FOR THE STRING CONTINUE.
C
      IF(IDATAL(1).NE.ICONT) GO TO 9
      NDDOUT = 0
      GO TO 200
C
C                            ALLOCATE UNIT NUMBER TO OUTPUT DATA SET.
C
9     CONTINUE
      DO 111  I = 1,11
111   IOUTSV(I) = IDATAL(I)
   16 CONTINUE
      DO 3 I=1,4
    3 HVOLSR(I)=HBLANK
      NDDOUT=0
      CALL TRMOUT(80,'The dataset will now be allocated. Please be patie
     $nt for a few moments.^')
*** PMF 02/12/99: add suffix '.EVENT@' to the file name.
*     '@' tells GETPDD that the specified output file does
*     not need to exist before allocation.
      LI= MIN(INDEX(CDATAL,' '),38)
      CDATAL=CDATAL(1:LI-1)//'.EVENT@'
*** PMF(end)
      CALL GETPDD(IDATAL,HVOLSR,DDN,NDDOUT,HERR)
      IF(HERR.EQ.0) GO TO 5
C
C                            ERROR HAS OCCURRED ON ALLOCATION.
C
      CALL TRMOUT(80,'The name is incorrect or HMS/MSS is jammed.^')
44    CALL TRMOUT(80,'Please try again or continue session by "CONTINUE"
     $.^')
      GO TO 1
C---
C---     SUCCESSFULLY ALLOCATED DATA SET. NOW COUNT THE RECORDS
C---     AND LOAD THE FORTRAN REFERENCE NUMBER AND NUMBER OF RECORDS
C---     INTO THE TRANSFER COMMON FOR THE USE OF OTHER ROUTINES.
C---
    5 JUNIT = NDDOUT
      CALL TRMOUT(80,' Output dataset allocated.^')
      CALL TRMOUT(80,
     $'Do you want to overwrite the records on this dataset?^')
      CALL TRMOUT(80,'If not you will add records to the existing ds.^')
      CALL DECIDE(IANSW)
      IF(IANSW.EQ.1) GO TO 12
      CALL TRMOUT(80,' This option is presently not working, unfortunate
     $ly^')
      CALL TRMOUT(80,' Do you wish another output file? If not, you will
     $overwrite this one.^')
      CALL DECIDE(IANSW)
      IF(IANSW.EQ.1) GO TO 1
C     CALL TRMOUT(80,
C    $'IF YOU CHOOSE TO ADD EVENTS TO THE EXISTING ONES,^')
C     CALL TRMOUT(80,
C    $'THEN THE CURRENT EVENT WILL NOT BE WRITTEN. IS THIS OK?^')
C     CALL DECIDE(IANSW)
C     IF(IANSW.EQ.2) GO TO 12
C     CALL GNUREC(NDDOUT,NUMEVN,NUMREC)
C     NRWR = NUMEVN
C     WRITE(JUSCRN,101) NUMEVN,NUMREC
C 101 FORMAT(' DATA SET HAS NUMBER OF EVENTS, NUMBER OF RECORDS = ',2I4)
C     CALL TRMOUT(80,'EVENTS WILL BE ADDED TO THE ALREADY EXISTING ONES.
C    $^')
C     CALL STWADD(NDDOUT,*4)
C     GO TO 100
C4     CALL TRMOUT(80,'THE DATA SET GIVES READ ERROR OR IS EMPTY.^')
C      GO TO 44
C100   CALL BWRO(JUNIT,MAXRL,MODE)
12    CALL TRMOUT(80,'EVENTS WILL BE OVERWRITTEN. BOS S FORMAT IS DEFAUL
     $T.^')
      MODE = 0
      MAXRL = 33000
      NRWR = 0
      CALL BWRO(JUNIT,MAXRL,MODE)
200   CONTINUE
      RETURN
      END
