      FUNCTION XFWVXC(IPHEAD) 
C-----------------------------------------------------------------------
C
C
C     RETURNS THE NUMBER OF RADIATION LENGTHS FROM THE INSIDE OF THE
C     BEAM PIPE TO THE FIRST GAS MOLECULE IN THE VERTEX CHAMBER.
C     BASED ON DATE IN HEAD BANK.
C     MC-DATA: NEW GEOMETRY IS FORCED IF FLAG LVTXC IS TRUE
C     ARGUMENT DUMMY IS IGNORED.
C
C   RADIATION LENGTH UPDATE, 9.6.1986    J.O.
C   RADIATION LENGTH UPDATE, 11.6.1986    J.O.
C
C       X0(BEAMPIPE)                0.03371
C       X0(1.KAPTON)                0.00011
C       X0(2.KAPTON+CU)             0.00148
C       X0(3.KAPTON+CU)             0.00262
C       X0(COOLING PIPES)           0.00107
C                                  =========
C                                   0.03899
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C PMF 03.11.98 
      LOGICAL TBIT
C
      LOGICAL NEWDET,LVTXC,LNHARD
C
C----------------------------------------------------------------------
C             MACRO CDATA .... BOS COMMON.
C
C             THIS MACRO ONLY DEFINES THE IDATA/HDATA/ADATA NAMES.
C             THE ACTUAL SIZE OF /BCS/ IS FIXED ON MACRO CBCSMX
C             OR BY OTHER MEANS. A DEFAULT SIZE OF 40000 IS GIVEN HERE.
C
C----------------------------------------------------------------------
C
      COMMON /BCS/ IDATA(40000)
      DIMENSION HDATA(80000),ADATA(40000),IPNT(50)
      EQUIVALENCE (HDATA(1),IDATA(1),ADATA(1)),(IPNT(1),IDATA(55))
      EQUIVALENCE (NWORD,IPNT(50))
C
C------------------------ END OF MACRO CDATA --------------------------
C
C                        --- MACRO CADMIN ---
C
      LOGICAL*1 LBREAD
C
      COMMON / CADMIN / IEVTP,NRREAD,NRWRIT,NRERR,LBREAD(4),IFLG,IERCAL,
     +                  ISMEAR,IJETCI,NFLAGS(10)
C
C                                 NFLAGS IS AN ARRAY OF GENERAL FLAGS
C                                   (1) : USED BY RDDATE
C                                   (2) : USED BY RDTRIG
C                                   (3) : USED BY RDTRIG
C                                   (4) : USED BY RDTRIG / PRSTAT
C                                   (5) : USED BY EVREAD -COUNTS RECORDS
C                                   (6) : USED BY SUPERV -COUNTS ERRORS
C                                   (7) : USED BY EVWRIT -'HEAD'LESS EVS
C                                   (8) : USED BY EVREAD/RDMTCO (EVWRIT)
C                                   (9) : USED BY RDMTCO/EVWRIT
C                                  (10) : FREE
C
C                                  BLOCK DATA SET IN MEMBER JADEBD
C
C
      COMMON / CVCEX  / LVTXC
C
C------------------  C O D E  ------------------------------------------
C
      IH2    = 2*IPHEAD
C
      IMONTH = HDATA(IH2+7)
      IYEAR  = HDATA(IH2+8)
C
C                            OLD GEOMETRY (PRIOR TO MAY 1984)
C
C
C   FOR VALUE OF RADIATION LENGTH, SEE JCN 87
C
      XFWVXC = 0.00000
C                            NEW DETECTOR HARDWARE IN MONTE-CARLO DATA?
C                            THEN LVTXC IS TRUE (FLAG IS CHECKED AND
C                                                SET IN RDMTCO)
C
      LNHARD = (IMONTH .GE. 5  .AND.  IYEAR .EQ. 1984)
     +                          .OR.  IYEAR .GE. 1985
      NEWDET = LVTXC .OR. (IEVTP.EQ.0 .AND. LNHARD)
C
      IF( .NOT. NEWDET ) RETURN
C
C                            NEW GEOMETRY
C
      XFWVXC = 0.03899
C
      IF ((IEVTP.NE.0).AND.(IYEAR.LE.1985)) XFWVXC = 0.03371
C
      RETURN
      END
