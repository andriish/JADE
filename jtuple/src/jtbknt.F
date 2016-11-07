      subroutine jtbknt( lmc, langl )
      implicit none
      logical lmc, langl
#include "jtjtpl.inc"

C     Ntuple:
      CALL HBNT( 10, 'QQJT200', 'D' )
      CHDAGN(:)= ' '
      CALL QQFORM( CHDAGN, 'IRUN', 0, ' ', 0.0, 50000.0 )
      CALL QQFORM( CHDAGN, 'IEVNT', 0, ' ', 0.0, 0.0 )
      CALL QQFORM( CHDAGN, 'ITKMH', 0, ' ', 0.0, 1.0 )
      CALL QQFORM( CHDAGN, 'IGPMH', 0, ' ', 0.0, 1.0 )
      CALL QQFORM( CHDAGN, 'ISIST', 0, ' ', 0.0, 3.0 )
      CALL QQFORM( CHDAGN, 'ICVST', 0, ' ', 0.0, 3.0 )
      CALL QQFORM( CHDAGN, 'ICJST', 0, ' ', 0.0, 3.0 )
      CALL QQFORM( CHDAGN, 'ICZST', 0, ' ', 0.0, 3.0 )
      CALL QQFORM( CHDAGN, 'IEBST', 0, ' ', 0.0, 3.0 )
      CALL QQFORM( CHDAGN, 'IEEST', 0, ' ', 0.0, 3.0 )
      CALL QQFORM( CHDAGN, 'ISTG1', 0, ' ', 0.0, 1.0 )
      CALL QQFORM( CHDAGN, 'NTKD02', 0, ' ', 0.0, 500.0 )
      CALL QQFORM( CHDAGN, 'EBEAM', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAGN, 'PGCE(4)', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAGN, 'TVECTC(3)', 0, 'R', 0.0, 0.0 )
      PRINT *, ' '
      PRINT *, 'Print CHDAGN booking DAGNRL:'
      PRINT *, CHDAGN(1:INDEX(CHDAGN,' ')-1)
      CALL HBNAME( 10, 'DAGNRL', IRUN, CHDAGN(1:INDEX(CHDAGN,' ')-1) )
      chjdct(:)= ' '
      call qqform( chjdct, 'INO', 0, 'U', 0.0, 1000000.0 )
      call qqform( chjdct, 'INTTR', 0, 'U', 0.0, 100.0 )
      call qqform( chjdct, 'ILTR', 0, 'U', 0.0, 100.0 )
      call qqform( chjdct, 'ITAUCT', 0, 'U', 0.0, 1.0 )
      call qqform( chjdct, 'IMCRED', 0, 'U', -1.0, 1.0 )
      call qqform( chjdct, 'NJDCUT(2)', 0, 'U', 0.0, 1.0 )
      call qqform( chjdct, 'EB', 0, 'R', 0.0, 100.0 )
      call qqform( chjdct, 'EE(2)', 0, 'R', 0.0, 100.0 )
      call qqform( chjdct, 'ZVERT', 0, 'R', -2000.0, 2000.0 )
      print *, ' '
      print *, 'Print CHJDCT booking JDCUTS:'
      print *, chjdct(1:index(chjdct,' ')-1)
      call hbname( 10, 'JDCUTS', ino, chjdct(1:index(chjdct,' ')-1) )
      CHDAEV(:)= ' '
      CALL QQFORM( CHDAEV, 'TDTC', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'TMADTC', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'TMIDTC', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'MHDTC', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'MLDTC', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'BTDTC', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'BWDTC', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'CPDTC', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'DPDTC', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'SDTC', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'ADTC', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'ACPDTC', 0, 'R', 0.0, 0.0)
      CALL QQFORM( CHDAEV, 'TDT', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'TMADT', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'TMIDT', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'MHDT', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'MLDT', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'BTDT', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'BWDT', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'CPDT', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'DPDT', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'SDT', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'ADT', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'ACPDT', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'TDC', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'TMADC', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'TMIDC', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'MHDC', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'MLDC', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'BTDC', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'BWDC', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'CPDC', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'DPDC', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'SDC', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'ADC', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'ACPDC', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'TDMT', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'TMADMT', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'TMIDMT', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'MHDMT', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'MLDMT', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'BTDMT', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'BWDMT', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'CPDMT', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'DPDMT', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'SDMT', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'ADMT', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDAEV, 'ACPDMT', 0, 'R', 0.0, 0.0 )
      PRINT *, ' '
      PRINT *, 'Print CHDAEV booking DAEVSH:'
      PRINT *, CHDAEV(1:INDEX(CHDAEV,' ')-1)
      CALL HBNAME( 10, 'DAEVSH', TDTC, 
     &     CHDAEV(1:INDEX(CHDAEV,' ')-1) )
      IF( LANGL ) THEN
         CHDAAN(:)= ' '
         CALL QQFORM( CHDAAN, 'BZDTC', 0, 'R', 0.0, 0.0 )
         CALL QQFORM( CHDAAN, 'KSWDTC', 0, 'R', 0.0, 0.0 )
         CALL QQFORM( CHDAAN, 'NRDTC', 0, 'R', 0.0, 0.0 )
         CALL QQFORM( CHDAAN, 'C34DTC', 0, 'R', 0.0, 0.0 )
         CALL QQFORM( CHDAAN, 'BZDT', 0, 'R', 0.0, 0.0 )
         CALL QQFORM( CHDAAN, 'KSWDT', 0, 'R', 0.0, 0.0 )
         CALL QQFORM( CHDAAN, 'NRDT', 0, 'R', 0.0, 0.0 )
         CALL QQFORM( CHDAAN, 'C34DT', 0, 'R', 0.0, 0.0 )
         CALL QQFORM( CHDAAN, 'BZDC', 0, 'R', 0.0, 0.0 )
         CALL QQFORM( CHDAAN, 'KSWDC', 0, 'R', 0.0, 0.0 )
         CALL QQFORM( CHDAAN, 'NRDC', 0, 'R', 0.0, 0.0 )
         CALL QQFORM( CHDAAN, 'C34DC', 0, 'R', 0.0, 0.0 )
         CALL QQFORM( CHDAAN, 'BZDMT', 0, 'R', 0.0, 0.0 )
         CALL QQFORM( CHDAAN, 'KSWDMT', 0, 'R', 0.0, 0.0 )
         CALL QQFORM( CHDAAN, 'NRDMT', 0, 'R', 0.0, 0.0 )
         CALL QQFORM( CHDAAN, 'C34DMT', 0, 'R', 0.0, 0.0 )
         PRINT *, ' '
         PRINT *, 'Print CHDAAN booking DAANGL:'
         PRINT *, CHDAAN(1:INDEX(CHDAAN,' ')-1)
         CALL HBNAME( 10, 'DAANGL', BZDTC,
     &        CHDAAN(1:INDEX(CHDAAN,' ')-1) )
      ENDIF
      CHDAJT(:)= ' '
      CALL QQFORM(CHDAJT,'NXJDTC',0,' ',0.0,REAL(MAXJET))
      CALL QQFORM(CHDAJT,'NXJDT',0,' ',0.0,REAL(MAXJET))
      CALL QQFORM(CHDAJT,'NXJDC',0,' ',0.0,REAL(MAXJET))
      CALL QQFORM(CHDAJT,'NXJDMT',0,' ',0.0,REAL(MAXJET))
      CALL QQFORM(CHDAJT,'NXJETC',0,' ',0.0,REAL(MAXJET))
      CALL QQFORM(CHDAJT,'NXJET',0,' ',0.0,REAL(MAXJET))
      CALL QQFORM(CHDAJT,'NXJEC',0,' ',0.0,REAL(MAXJET))
      CALL QQFORM(CHDAJT,'NXJEMT',0,' ',0.0,REAL(MAXJET))
      CALL QQFORM(CHDAJT,'NXJCTC',0,' ',0.0,REAL(MAXJET))
      CALL QQFORM(CHDAJT,'NXJCT',0,' ',0.0,REAL(MAXJET))
      CALL QQFORM(CHDAJT,'NXJCC',0,' ',0.0,REAL(MAXJET))
      CALL QQFORM(CHDAJT,'NXJCMT',0,' ',0.0,REAL(MAXJET))
      CALL QQFORM(CHDAJT,'YDDTC(NXJDTC)',0,'R',0.0,0.0)
      CALL QQFORM(CHDAJT,'YEDTC(NXJETC)',0,'R',0.0,0.0)
      CALL QQFORM(CHDAJT,'YCDTC(NXJCTC)',0,'R',0.0,0.0)
      CALL QQFORM(CHDAJT,'NJCEDTC(*)',NBC,' ',-1.0,REAL(MAXJET))
      CALL QQFORM(CHDAJT,'NJCRDTC(*)',NBC,' ',-1.0,REAL(MAXJET))
      CALL QQFORM(CHDAJT,'YDDT(NXJDT)',0,'R',0.0,0.0)
      CALL QQFORM(CHDAJT,'YEDT(NXJET)',0,'R',0.0,0.0)
      CALL QQFORM(CHDAJT,'YCDT(NXJCT)',0,'R',0.0,0.0)
      CALL QQFORM(CHDAJT,'NJCEDT(*)',NBC,' ',-1.0,REAL(MAXJET))
      CALL QQFORM(CHDAJT,'NJCRDT(*)',NBC,' ',-1.0,REAL(MAXJET))
      CALL QQFORM(CHDAJT,'YDDC(NXJDC)',0,'R',0.0,0.0)
      CALL QQFORM(CHDAJT,'YEDC(NXJEC)',0,'R',0.0,0.0)
      CALL QQFORM(CHDAJT,'YCDC(NXJCC)',0,'R',0.0,0.0)
      CALL QQFORM(CHDAJT,'NJCEDC(*)',NBC,' ',-1.0,REAL(MAXJET))
      CALL QQFORM(CHDAJT,'NJCRDC(*)',NBC,' ',-1.0,REAL(MAXJET))
      CALL QQFORM(CHDAJT,'YDDMT(NXJDMT)',0,'R',0.0,0.0)
      CALL QQFORM(CHDAJT,'YEDMT(NXJEMT)',0,'R',0.0,0.0)
      CALL QQFORM(CHDAJT,'YCDMT(NXJCMT)',0,'R',0.0,0.0)
      CALL QQFORM(CHDAJT,'NJCEDMT(*)',NBC,' ',-1.0,REAL(MAXJET))
      CALL QQFORM(CHDAJT,'NJCRDMT(*)',NBC,' ',-1.0,REAL(MAXJET))
      PRINT*, ' '
      PRINT*, 'Print CHDAJT booking DAJETS:'
      PRINT*, CHDAJT(1:INDEX(CHDAJT,' ')-1)
      CALL HBNAME(10,'DAJETS',NXJDTC,CHDAJT(1:INDEX(CHDAJT,' ')-1))
      CHDACT(:)= ' '
      CALL QQFORM( CHDACT, 'NTRK', 0, ' ', 0.0, REAL(MAXTRK) )
      CALL QQFORM( CHDACT, 'ID02(NTRK)', 0, ' ', 0.0, 1.0 )
      CALL QQFORM( CHDACT, 'DEDX(NTRK)', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDACT, 'DDED(NTRK)', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDACT, 'NHDE(NTRK)', 0, ' ', 0.0, 200.0 )
      CALL QQFORM( CHDACT, 'DP(NTRK)', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDACT, 'PTRK(3,NTRK)', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDACT, 'ICHG(NTRK)', 0, ' ', -1.0, 1.0 )
      CALL QQFORM( CHDACT, 'NHCJ(NTRK)', 0, ' ', 0.0, 200.0 )
      CALL QQFORM( CHDACT, 'Z0(NTRK)', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDACT, 'D0(NTRK)', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDACT, 'NMTTRK', 0, ' ', 0.0, REAL(MAXTRK) )
      CALL QQFORM( CHDACT, 'IMTTRK(NMTTRK)', 0, ' ', 0.0, 
     &     REAL(MAXTRK) )
      CALL QQFORM( CHDACT, 'MTSCFT(NMTTRK)', 0, 'R', 0.0, 0.0 )
      PRINT *, ' '
      PRINT *, 'Print CHDACT booking DACTRK:'
      PRINT *, CHDACT(1:INDEX(CHDACT,' ')-1)
      CALL HBNAME( 10, 'DACTRK', NTRK, CHDACT(1:INDEX(CHDACT,' ')-1) )
      CHDACL(:)= ' '
      CALL QQFORM( CHDACL, 'NCLUS', 0, ' ', 0.0, REAL(MAXTRK) )
      CALL QQFORM( CHDACL, 'NMTCLS', 0, ' ', 0.0, REAL(MAXTRK) )
      CALL QQFORM( CHDACL, 'IMTCLS(NMTCLS)', 0, ' ', 0.0, 
     &     REAL(MAXTRK) )
      CALL QQFORM( CHDACL, 'NMTKIL', 0, ' ', 0.0, REAL(MAXTRK) )
      CALL QQFORM( CHDACL, 'IMTKIL(NMTKIL)', 0, ' ', 0.0, 
     &     REAL(MAXTRK) )
      CALL QQFORM( CHDACL, 'PCLUS(3,NCLUS)', 0, 'R', 0.0, 0.0 )
      CALL QQFORM( CHDACL, 'MTSCFC(NMTCLS)', 0, 'R', 0.0, 0.0 )
      PRINT *, ' '
      PRINT *, 'Print CHDACL booking DACLUS:'
      PRINT *, CHDACL(1:INDEX(CHDACL,' ')-1)
      CALL HBNAME( 10, 'DACLUS', NCLUS, 
     &     CHDACL(1:INDEX(CHDACL,' ')-1) )

C     Monte Carlo part:
      if( lmc ) then
         CHMCGN(:)= ' '
         CALL QQFORM( CHMCGN, 'IEVTYP', 0, ' ', 0.0, 8.0 )
         CALL QQFORM( CHMCGN, 'INONR', 0, ' ', 0.0, 1.0 )
         CALL QQFORM( CHMCGN, 'PISR(4)', 0, 'R', 0.0, 0.0 )
         CALL QQFORM( CHMCGN, 'NPRIMF', 0, ' ', 0.0, REAL(MAXPRF) )
         CALL QQFORM( CHMCGN, 'IFERID(NPRIMF)', 0, ' ', -16.0, 16.0 )
         CALL QQFORM( CHMCGN, 'PRIMF(4,NPRIMF)', 0, 'R', 0.0, 0.0 )
         PRINT *, ' '
         PRINT *, 'Print CHMCGN booking MCGNRL:'
         PRINT *, CHMCGN(1:INDEX(CHMCGN,' ')-1)
         CALL HBNAME( 10, 'MCGNRL', IEVTYP, 
     &        CHMCGN(1:INDEX(CHMCGN,' ')-1) )
         CHMCEV(:)= ' '
         CALL QQFORM( CHMCEV, 'TP', 0, 'R', 0.0, 0.0 )
         CALL QQFORM( CHMCEV, 'TMAP', 0, 'R', 0.0, 0.0 )
         CALL QQFORM( CHMCEV, 'TMIP', 0, 'R', 0.0, 0.0 )
         CALL QQFORM( CHMCEV, 'MHP', 0, 'R', 0.0, 0.0 )
         CALL QQFORM( CHMCEV, 'MLP', 0, 'R', 0.0, 0.0 )
         CALL QQFORM( CHMCEV, 'BTP', 0, 'R', 0.0, 0.0 )
         CALL QQFORM( CHMCEV, 'BWP', 0, 'R', 0.0, 0.0 )
         CALL QQFORM( CHMCEV, 'CPP', 0, 'R', 0.0, 0.0 )
         CALL QQFORM( CHMCEV, 'DPP', 0, 'R', 0.0, 0.0 )
         CALL QQFORM( CHMCEV, 'SP', 0, 'R', 0.0, 0.0 )
         CALL QQFORM( CHMCEV, 'AP', 0, 'R', 0.0, 0.0 )
         CALL QQFORM( CHMCEV, 'ACPP', 0, 'R', 0.0, 0.0 )
         CALL QQFORM( CHMCEV, 'TH', 0, 'R', 0.0, 0.0 )
         CALL QQFORM( CHMCEV, 'TMAH', 0, 'R', 0.0, 0.0 )
         CALL QQFORM( CHMCEV, 'TMIH', 0, 'R', 0.0, 0.0 )
         CALL QQFORM( CHMCEV, 'MHH', 0, 'R', 0.0, 0.0 )
         CALL QQFORM( CHMCEV, 'MLH', 0, 'R', 0.0, 0.0 )
         CALL QQFORM( CHMCEV, 'BTH', 0, 'R', 0.0, 0.0 )
         CALL QQFORM( CHMCEV, 'BWH', 0, 'R', 0.0, 0.0 )
         CALL QQFORM( CHMCEV, 'CPH', 0, 'R', 0.0, 0.0 )
         CALL QQFORM( CHMCEV, 'DPH', 0, 'R', 0.0, 0.0 )
         CALL QQFORM( CHMCEV, 'SH', 0, 'R', 0.0, 0.0 )
         CALL QQFORM( CHMCEV, 'AH', 0, 'R', 0.0, 0.0 )
         CALL QQFORM( CHMCEV, 'ACPH', 0, 'R', 0.0, 0.0 )
         PRINT *, ' '
         PRINT *, 'Print CHMCEV booking MCEVSH:'
         PRINT *, CHMCEV(1:INDEX(CHMCEV,' ')-1)
         CALL HBNAME( 10, 'MCEVSH', TP, 
     &        CHMCEV(1:INDEX(CHMCEV,' ')-1) )
         IF( LANGL ) THEN
            CHMCAN(:)= ' '
            CALL QQFORM( CHMCAN, 'BZP', 0, 'R', 0.0, 0.0 )
            CALL QQFORM( CHMCAN, 'KSWP', 0, 'R', 0.0, 0.0 )
            CALL QQFORM( CHMCAN, 'NRP', 0, 'R', 0.0, 0.0 )
            CALL QQFORM( CHMCAN, 'C34P', 0, 'R', 0.0, 0.0 )
            CALL QQFORM( CHMCAN, 'BZH', 0, 'R', 0.0, 0.0 )
            CALL QQFORM( CHMCAN, 'KSWH', 0, 'R', 0.0, 0.0 )
            CALL QQFORM( CHMCAN, 'NRH', 0, 'R', 0.0, 0.0 )
            CALL QQFORM( CHMCAN, 'C34H', 0, 'R', 0.0, 0.0 )
            PRINT *, ' '
            PRINT *, 'Print CHMCAN booking MCANGL:'
            PRINT *, CHMCAN(1:INDEX(CHMCAN,' ')-1)
            CALL HBNAME( 10, 'MCANGL', BZP, 
     &           CHMCAN(1:INDEX(CHMCAN,' ')-1) )
         ENDIF
         CHMCJT(:)= ' '
         CALL QQFORM(CHMCJT,'NXJDP',0,' ',0.0,REAL(MAXJET))
         CALL QQFORM(CHMCJT,'NXJDH',0,' ',0.0,REAL(MAXJET))
         CALL QQFORM(CHMCJT,'NXJEP',0,' ',0.0,REAL(MAXJET))
         CALL QQFORM(CHMCJT,'NXJEH',0,' ',0.0,REAL(MAXJET))
         CALL QQFORM(CHMCJT,'NXJCP',0,' ',0.0,REAL(MAXJET))
         CALL QQFORM(CHMCJT,'NXJCH',0,' ',0.0,REAL(MAXJET))
         CALL QQFORM(CHMCJT,'YDP(NXJDP)',0,'R',0.0,0.0)
         CALL QQFORM(CHMCJT,'YEP(NXJEP)',0,'R',0.0,0.0)
         CALL QQFORM(CHMCJT,'YCP(NXJCP)',0,'R',0.0,0.0)
         CALL QQFORM(CHMCJT,'NJCEP(*)',NBC,' ',-1.0,REAL(MAXJET))
         CALL QQFORM(CHMCJT,'NJCRP(*)',NBC,' ',-1.0,REAL(MAXJET))
         CALL QQFORM(CHMCJT,'YDH(NXJDH)',0,'R',0.0,0.0)
         CALL QQFORM(CHMCJT,'YEH(NXJEH)',0,'R',0.0,0.0)
         CALL QQFORM(CHMCJT,'YCH(NXJCH)',0,'R',0.0,0.0)
         CALL QQFORM(CHMCJT,'NJCEH(*)',NBC,' ',-1.0,REAL(MAXJET))
         CALL QQFORM(CHMCJT,'NJCRH(*)',NBC,' ',-1.0,REAL(MAXJET))
         PRINT *, ' '
         PRINT *, 'Print CHMCJT booking MCJETS:'
         PRINT *, CHMCJT(1:INDEX(CHMCJT,' ')-1)
         CALL HBNAME(10,'MCJETS',NXJDP,CHMCJT(1:INDEX(CHMCJT,' ')-1))
         CHMCPT(:)= ' '
         CALL QQFORM( CHMCPT, 'NTRKP', 0, ' ', 0.0, REAL(MAXPRT) )
         CALL QQFORM( CHMCPT, 'ILUCP(NTRKP)', 0, ' ', -6.0, 22.0 )
         CALL QQFORM( CHMCPT, 'PTRKP(4,NTRKP)', 0, 'R', 0.0, 0.0 )
         PRINT *, ' '
         PRINT *, 'Print CHMCPT booking MCPART:'
         PRINT *, CHMCPT(1:INDEX(CHMCPT,' ')-1)
         CALL HBNAME(10,'MCPART',NTRKP,CHMCPT(1:INDEX(CHMCPT,' ')-1))
         CHMCCT(:)= ' '
         CALL QQFORM(CHMCCT,'NTRKH',0,' ',0.0,REAL(MAXTRK))
         CALL QQFORM(CHMCCT,'NTRK2',0,' ',0.0,REAL(MAXTRK))
         CALL QQFORM(CHMCCT,'PTRKH(4,NTRKH)',0,'R',0.0,0.0)
         CALL QQFORM( CHMCCT, 'ILUCH(NTRKH)', 0, ' ', 0.0, 0.0 )
         CALL QQFORM(CHMCCT,'ILUC(NTRK2)',0,' ',-65536.0,65536.0)
         CALL QQFORM(CHMCCT,'ISTRT(NTRK2)',0,' ',0.0,12.0)
         CALL QQFORM(CHMCCT,'ICHGH(NTRKH)',0,' ',-1.0,1.0)
         PRINT *, ' '
         PRINT *, 'Print CHMCCT booking MCCTRK:'
         PRINT *, CHMCCT(1:INDEX(CHMCCT,' ')-1)
         CALL HBNAME(10,'MCCTRK',NTRKH,CHMCCT(1:INDEX(CHMCCT,' ')-1))
      ENDIF

C     Book histos:
      call hstaf( 'yes' )
      call hbook1( 100, 'Number of PATR tracks', 51, -0.5, 50.5, 0.0 )
      call hbook1( 101, 'Number of selected PATR tracks'
     &     , 51, -0.5, 50.5, 0.0 )      
      call hbook1( 102, 'Number of LGCL clusters'
     &     , 51, -0.5, 50.5, 0.0 )
      call hbook1( 103, 'Number of selected LGCL clusters'
     &     , 51, -0.5, 50.5, 0.0 )
      call hbook1( 104, 'Number of MT selected LGCL clusters'
     &     , 51, -0.5, 50.5, 0.0 )
      call hbook1( 105, 'EEE1 MT', 60, 0.0, 30.0, 0.0 )
      call hbook1( 106, 'EEE2 MT', 60, 0.0, 30.0, 0.0 )
      call hbook1( 107, 'EEEB MT', 60, 0.0, 60.0, 0.0 )
      call hbook1( 108, 'Tracks from vertex', 51, -0.5, 50.5, 0.0 )
      call hbook1( 109, 'Good tracks', 51, -0.5, 50.5, 0.0 )

      call hbook1( 110, 'Evis/Ebeam MT', 60, 0.0, 3.0, 0.0 )
      call hbook1( 111, 'Sum(pz)/Evis MT', 50, -1.0, 1.0, 0.0 )
      call hbook1( 112, 'z vertex', 50, -300.0, 300.0, 0.0 )
      call hbook1( 113, 'cos(ThetaT) MT', 50, -1.0, 1.0, 0.0 )
      call hbook1( 114, 'pmiss/Ebeam MT', 50, 0.0, 1.0, 0.0 )

      call hbook1( 120, 'PATR hits r-phi', 61, -0.5, 60.5, 0.0 )
      call hbook1( 121, 'PATR hits r-z', 61, -0.5, 60.5, 0.0 )
      call hbook1( 122, 'PATR zvert', 50, -200.0, 200.0, 0.0 )
      call hbook1( 123, 'PATR rmin', 50, -100.0, 100.0, 0.0 )
      call hbook1( 124, 'PATR ptot', 60, 0.0, 60.0, 0.0 )
      call hbook1( 125, 'PATR pxy', 60, 0.0, 30.0, 0.0 )

      call hbook1( 130, 'LGCL Etot', 60, 0.0, 60.0, 0.0 )
      call hbook1( 131, 'LGCL Etot MT', 60, 0.0, 60.0, 0.0 )
      call hbook1( 132, 'LGCL E', 60, 0.0, 30.0, 0.0 )
      call hbook1( 133, 'LGCL E MT', 60, 0.0, 30.0, 0.0 )

C     The End:
      return
      end
