C   04/09/78 C8110201   MEMBER NAME  SHMAX    (JADELGS)     FORTRAN
      FUNCTION SHMAX(ETOT,JEG)
C
C     S.YAMADA   04-09-78  11:50
C     LAST MODIFICATION  05-10-78  20:55
C
C---- CALCULATE THE AVERAGE SHOWER DEPTH FOR THE SHOWER WITH ETOT(MEV)
C     INITIATED BY EL/POS(JEG=1) AND/OR GAMMA(JEG=0).
C
      COMMON /CLGDMS/ X0
      DATA ECUT/4.979/, GCUT/1.725/
      COMMON/ CLGVRN/ NVRSN(20)
      DATA NVCODE/178100520/
      NVRSN(18) = NVCODE
C
      SHMAX = 0.
      IF(JEG) 1,2,1
C---- ELECTRON/POSITRON
    1 REN = ETOT/ECUT
      IF(REN.LE.1.0) RETURN
      GO TO 3
C---- GAMMA
    2 REN = ETOT/GCUT
      IF(REN.LE.1.0) RETURN
    3 SHMAX = ALOG(REN)*X0
      RETURN
      END
