C   07/06/96 606071806  MEMBER NAME  BENT     (S4)          FORTG1
      SUBROUTINE BENT(NA,NB,NC,ND)
C     BOS SUBPROGRAM =0.1=
#include "acs.for"
      COMMON/BCS/IW(1)
      REAL RW(1)
      EQUIVALENCE (RW(1),IW(1))
#include "ccs.for"
      EXTERNAL UCOPY2
      INTEGER INIT/0/
C
C     LAYOUT OF BANK COMMON/BCS/
C
C     INDEX    IW( )             I   MEANING
C     ------------------------------------------------------
C         1    IW(1)             I+  POINTER TO FIRST BANKS
C         .    ...               I
C     NAMAX    IW(NAMAX)         I
C      NFRS    SPACE FOR BANKS   I+     *
C              ...               I      *
C              ...               I      *
C              ...               I      * BANK SPACE
C              ...               I      *
C              ...               I      *
C      NLST    SPACE FOR BANKS   I      *
C              NALIST(1)         I+  LIST OF NAMES
C              ...               I
C     ISLST    NALIST(NLIST)     I
C              NSLIST(1)         I+  SPECIAL LIST OF NAMES
C              ...               I
C     IMLST    NSLIST(NLIST)     I
C              NMLIST(1)         I+  MARKER FOR SPECIAL LIST
C              ...               I
C              NMLIST(NLIST)     I
C     INAMV    NAME              I+  NAME
C              NAMES(1)          I+  NAMES
C              ...               I
C     IOLST    NAMES(NAMAX)      I
C              IOLIST(1)         I+  IO-MARKER
C              ...               I
C     IPLST    IOLIST(NAMAX)     I
C              IK(1)             I+  POINTER FOR NAMES
C              ...               I
C              IK(NAMAX)         I
C              IK(NAMAX+1)       I
C              ...               I
C     NLAST    IK(NAMAX+NPRIM)   I
C
C
      IF(INIT.NE.0) GOTO 100
      INIT=1
      CALL VZERO(ICOND,80)
      CALL VZERO(IUND ,11)
      NLAST =NA
      NLAST1=NLAST
      CALL VZERO(IW,NLAST)
      NRECL =NA
      NDUMP =500
      NDUMP1=NDUMP
      NSPL  =NLAST/10
      NAMAX =100
      NLIST =100
      NERRL =10
      NEOTP =2
      IF(NB.GT. 0) NSPL=NB
      IF(NC.GE.10) NAMAX=NC
      IF(ND.GE.10) NLIST=ND
      MARKWR=-1
C
   10 NPRIM=(NAMAX/2)*2-1
   12 NPRIM=NPRIM+2
      DO 14 K=3,NPRIM,2
      L    =NPRIM/K
      IF(L.LT.K) GOTO 16
      IF(L*K.EQ.NPRIM) GOTO 12
   14 CONTINUE
C
   16 NAMAX1=NAMAX+1
      IPLST=NLAST-NAMAX-NPRIM
      IOLST=IPLST-NAMAX
      INAMV=IOLST-NAMAX
      IMLST=INAMV-NLIST-1
      ISLST=IMLST-NLIST
      NFRS =NAMAX1
      NEXT =NFRS
      NLST =ISLST-NLIST
      ILOW=NLST
C
  100 RETURN
C
      END
      SUBROUTINE BDMPA(IARG)
      IF(IARG.EQ. 2) STOP 1
      IF(IARG.EQ. 1) STOP 2
      IF(IARG.EQ.30) STOP 3
      STOP 4
      END
