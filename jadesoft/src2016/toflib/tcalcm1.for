C   24/04/81 409061122  MEMBER NAME  TCALCM1  (S)           FORTRAN
C
C  --------------------------------------------------------------
      INTEGER *2 HADC,HTDC,HSTAT,HON,HADCF,HTDCF,HFSTAT,HFON,
     - HTDC1,HON1,HTSPAR
      COMMON/TFPED/ HADC(2,42),HTDC(2,42),HSTAT(42),HON(42)
      COMMON/FWPED/ HADCF(2,16),HTDCF(2,16),HFSTAT(16),HFON(16)
      COMMON/TFPED1/HTDC1(2,42),HON1(42),HTSPAR(16)
      COMMON/CWORK/NR,RAW(5,42),NC,ICRT1(5,42),NTRK,ICRT2(50),TRK(5,50)
     - ,ITRC(50),NTC,ITRK(5,42),INFM(4),IRELT(14,50),RAW2(2,42)
      COMMON/TMTP/TAUM(42),TAUP(42)
      DIMENSION IRAW(5,42),IW(704),RE(14,50)
      EQUIVALENCE (IRAW(1,1),RAW(1,1)),(IW(1),INFM(1)),
     -             (IW(5),IRELT(1,1)),(RE(1,1),IRELT(1,1))
C  --------------------------------------------------------------
C
