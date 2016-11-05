C   24/04/81 104251629  MEMBER NAME  TCALCM   (CORLIB)      FORTRAN
C
C  --------------------------------------------------------------
       INTEGER *2 HADC,HTDC
      COMMON/CWORK/NR,RAW(5,42),NC,ICRT1(5,42),NTRK,ICRT2(50),TRK(5,50)
     - ,ITRC(50),NTC,ITRK(5,42),INFM(4),IRELT(14,50)
      COMMON/TFPED/ HADC(2,42),HTDC(2,42)
      COMMON/TMTP/TAUM(42),TAUP(42)
      DIMENSION IRAW(5,42),IW(704)
      EQUIVALENCE (IRAW(1,1),RAW(1,1)),(IW(1),INFM(1)),
     -             (IW(5),IRELT(1,1))
C  --------------------------------------------------------------
C
