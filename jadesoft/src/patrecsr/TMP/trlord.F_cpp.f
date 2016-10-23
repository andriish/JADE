      SUBROUTINE TRLORD
      IMPLICIT INTEGER*2 (H)
C----------------------------------------------
C  MACRO CWORKPR .... PATTERN RECOGNITION CWORK
C----------------------------------------------
      COMMON /CWORK/ HPLAST,HPFREE,HPWRK(30),ADWRK(600),
     ,               HPRO,HNTR,HNTCEL(98),IPCL(200),NRHT(200),
     ,               NWR1(200),DS1(200),SL1(200),
     ,               NWR2(200),DS2(200),SL2(200),
     ,               LBL(200),NTREL(200),ICRO(200),
     ,               NTR,HNREL(100),HISTR(9,100),HRES(168),
     ,               NTRLM,RLMTR(3,5),
     ,               WRK(7000)
                     DIMENSION TRKAR(200,11),ITRKAR(200,11),
     ,                         LMRTR(3,5)
                     EQUIVALENCE (IPCL(1),TRKAR(1,1),ITRKAR(1,1))
                     EQUIVALENCE (LMRTR(1,1),RLMTR(1,1))
         DIMENSION IWRK(7000),HWRK(14000),IDWRK(600),HDWRK(1200)
                     EQUIVALENCE (IWRK(1),WRK(1),HWRK(1))
                     EQUIVALENCE (IDWRK(1),ADWRK(1),HDWRK(1))
C---------- END OF MACRO CWORKPR --------------
      DIMENSION HORD(9)
      CALL SETSL(HORD(1),0,18,0)
      IF(NTR.LE.0) RETURN
      DO 13000 I=1,NTR
      NELM=HNREL(I)
      IF(
     - NELM.GT.1
     -)THEN
      DO 13002 IJ=1,NELM
      ITK=HISTR(IJ,I)
      ITK=IABS(ITK)
      IF(
     - ITK.GT.0
     -)THEN
      IC=IPCL(ITK)
      IF(IC.LE.24) IRING=1
      IF(IC.GT.24.AND.IC.LE.48) IRING=2
      IF(IC.GT.48) IRING=3
      IW=NWR2(ITK)
      ISORT=ISHFTL(IRING,5)
      ISORT=ISORT+IW
      HORD(IJ)=ISORT
      ENDIF
13002 CONTINUE
13003 CONTINUE
      IT=NELM-1
      DO 13004 I1=1,IT
      ITMP=I1+1
      DO 13006 I2=ITMP,NELM
      IF(
     - HORD(I1).LT.HORD(I2)
     -)THEN
      IEMP=HORD(I1)
      HORD(I1)=HORD(I2)
      HORD(I2)=IEMP
      IEMP=HISTR(I1,I)
      HISTR(I1,I)=HISTR(I2,I)
      HISTR(I2,I)=IEMP
      ENDIF
13006 CONTINUE
13007 CONTINUE
13004 CONTINUE
13005 CONTINUE
      ENDIF
13000 CONTINUE
13001 CONTINUE
      NUM=100-NTR
      IF(
     - NUM.GT.0
     -)THEN
      DO 13008 I=1,NUM
      HNREL(NTR+I)=0
      DO 13010 J=1,9
      HISTR(J,NTR+I)=0
13010 CONTINUE
13011 CONTINUE
13008 CONTINUE
13009 CONTINUE
      ENDIF
      RETURN
      END
