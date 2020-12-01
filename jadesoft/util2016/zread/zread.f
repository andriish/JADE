C ZREAD 06/07/01 P.A. Movilla Fernandez
      PROGRAM ZREAD
      IMPLICIT NONE
*.***************************************************************
*.
*.  Standalone example routine for reading ZE4V JADE data.
*.  This routine can process both ASCII and binary formatted files. 
*.
*.  06/07/01   P.A. Movilla Fernandez
*.
*.  Called routines:
*.        JADEV  ... reads ONE complete JADE record
*.        ENDIAN ... checks endian format of binary ZE4V files
*.
*.
*.***************************************************************
C

C General I/O steering parameters
C (should be read in via Fortran read or FFREAD or whatever) 
      INTEGER IUIN,NF,IFORM,MAXEVT,IPROUT
      CHARACTER FNAM0*80
C --- Basename of the data file
C     (The complete file name has the extension .1, .2, etc...)
      DATA FNAM0/ 'jade.demosample.bin_ze4v' /
C      DATA FNAM0/ 'jade.demosample.ascii_ze4v' /
C --- Format of the data file: 0=ASCII,  <>0=binary ZE4V
C     Must be set manually!
      DATA IFORM/ 1 /
C --- Number of input files with same basename (<=999)
      DATA NF / 100 /
C --- Max. number of events to read in
      DATA MAXEVT/ 1200 /
C --- Number of events to print out after connection with a new input file
      DATA IPROUT/ 20 /
C --- Fortran LUN / CFIO descriptor
      DATA IUIN / 20 /
C Further stuff
      INTEGER NEVENT,IEVTNW,IFL,IOS,IER
      LOGICAL ENDIAN,LEX,LPROUT
      CHARACTER CFORM*12,FNAM*80,CI*3
C
C------- C o d e ----------------------------------
C
C INITIALIZATION
C
      NEVENT=0
      IF( ABS( IFORM ).EQ.1 ) THEN
         CFORM='UNFORMATTED'
      ELSE
         CFORM='FORMATTED'
      ENDIF
C
C BEGIN OF FILE LOOP
C
C    -Perform user specific initialization
C     CALL JINI
C
      NF=MIN( NF,999 )
      DO 2000 IFL=1, NF
         IEVTNW=0
C      -Complete input file name
         IF( IFL.LT.10                   ) WRITE( CI,'(I1)') IFL
         IF( IFL.GE.10  .AND. IFL.LT.100  ) WRITE( CI,'(I2)') IFL
         IF( IFL.GE.100 .AND. IFL.LT.1000 ) WRITE( CI,'(I3)') IFL
         FNAM=FNAM0(1:INDEX(FNAM0,' ')-1)//'.'//CI
         INQUIRE( FILE=FNAM, EXIST=LEX )
         IF( .NOT.LEX ) THEN
            WRITE(*,9000) FNAM(1:INDEX( FNAM,' ' )-1)
 9000       FORMAT(' ZREAD: File ',A,' does not exist. Will stop now!' )
            GOTO 3000
         ENDIF
C
C    CONNECT WITH DATA FILE
C
C      -Check if endian format of current platform matches endian format of binary input data
         IF( IFL.EQ.1 .AND. IFORM.NE.0 ) THEN 
            IF( .NOT.ENDIAN( FNAM ) ) IFORM=-ABS( IFORM )
         ENDIF
C
C      -Use FORTRAN I/O to handle ASCII data and binary data with same endian format
         IF( IFORM.GE.0 ) THEN
            OPEN(IUIN,FILE=FNAM,STATUS='OLD',FORM=CFORM,IOSTAT=IOS)
         ELSE
C
C      -Use CERNLIB package CFIO to handle binary data with different endian format
            CALL CFOPEN(IUIN,0,1,'r   ',0,FNAM,IOS )
         ENDIF
C
C      -Print some info
         WRITE(*,9001) IFL,IUIN,CFORM,IOS,FNAM(1:INDEX( FNAM,' ' )-1)
 9001    FORMAT(//,' ZREAD: Processing data file:'
     >        ,' NR=',I2,' LUN=',I3,' F=',A,' IOSTAT=',I2,' FILE=',A,//)
         IF( IOS.NE.0 ) THEN
            WRITE( *,9002 )
 9002       FORMAT(' ZREAD: Error while opening file!'
     >           ,/,'...will stop now!')
            STOP
         ENDIF
C
C      -Read file header present in JADE files with ASCII format
         CALL JADEV( IUIN,IFORM,1,IER )
         IF( IER.LT.0 ) THEN
            WRITE( *, 9010 ) NEVENT+1,IER
 9010       FORMAT(' ZREAD: Read error in EVENT=',I7,' IOSTAT=',I2)
            STOP
         ENDIF
C
C    BEGIN OF EVENT LOOP
C
 1000    CONTINUE
C
         NEVENT=NEVENT+1
C
C    READ JADE EVENT RECORD
C
         CALL JADEV( IUIN,IFORM,0,IER )
C      -Check if read error
         IF( IER.LT.0 ) THEN
            WRITE( *, 9010 ) NEVENT,IER
            STOP
C      -Check if EOF reached
         ELSEIF( IER.EQ.1 ) THEN
            IF( IFORM.GE.0 ) THEN
               CLOSE( IUIN )
            ELSE
               CALL CFCLOS( IUIN,0 )
            ENDIF
            NEVENT=NEVENT-1
            WRITE( *, 9011 ) NEVENT,IER
 9011       FORMAT(' ZREAD: End of file reached.',
     >           ' Current event number =',I7,'  IOSTAT=',I2)
            IF( NEVENT.LT.MAXEVT ) THEN 
               GOTO 2000        ! <--- read next file
            ELSE
               GOTO 3000
            ENDIF
         ENDIF
C
C      -Dump JADE event record
         IEVTNW=IEVTNW+1
         LPROUT=( IEVTNW.LE.IPROUT
     >        .OR. MOD(NEVENT,500).EQ.0
     >        .OR. NEVENT.GE.MAXEVT )
         IF( LPROUT ) CALL JADEPR
C
C      -Perform analysis of current event
C      (Use information stored in common block /XDATA/)
C       CALL JANA
C
C    END OF EVENT LOOP
C
         IF( NEVENT.LT.MAXEVT ) THEN
            GOTO 1000
         ELSE
            IF( IFORM.GE.0 ) THEN
               CLOSE(IUIN)
            ELSE
               CALL CFCLOS(IUIN,0)
            ENDIF
            GOTO 3000
         ENDIF
C
C END OF FILE LOOP
C
 2000 CONTINUE
C
C The End
 3000 CONTINUE
      WRITE(*,9030) NEVENT
 9030 FORMAT(' ZREAD: Event limit or EOF reached.'
     >     ,' Number of read events;',I7)
C
C    -Perform final analysis steps
C     CALL JFIN
C
      STOP
      END
C
C
C
      SUBROUTINE JADEV(IUIN,IFORM,IOPT,IER)
      IMPLICIT NONE
*.********************************************************************
*.
*.  Reads one JADE event record and stores the data into common /XDATA/.
*.
*.  P.A. Movilla Fernandez
*.                    20/01/98 for ASCII data
*.                    28/01/00 now also working for binary ZE4V data
*.                    01/12/00 binary ZE4V data with both big endian
*.                             AND little endian format readable!
*.
*.  Input:
*.    IUIN        ... logical fortran unit number associated with the data file  
*.    IFORM= 0    ... event record is in ASCII format
*.         = 1    ... event record is in binary ZE4V format, endian format
*.                    of current platform equals endian format of data
*.         =-1    ... event record is in binary ZE4V format, endian format
*.                    of current platform differs from endian format of data
*.    IOPT = 0    ... read JADE event record
*.         = 1    ... read header of ASCII file
*.  Output:
*.     IER =  0,1 ... event record successfully read in
*.         =  1   ... end of file reached
*.         = -1   ... read error
*.
*.  The event record is stored in the following variables and arrays.
*.
*.  Header section of the event record:
*.  -----------------------------------
*.     IXREVN                      running event number
*.     IXRUN                       run number
*.     IXEVENT                     event number
*.     IXMCR                       MCREDU flag
*.     XEBEAM                      beam energy
*.     XEVCH                       visible energy (tracks)
*.     XEVTOT                      visible energy (clusters)
*.     XSPH1X XSPH1Y XSPH1Z XSPH1  1. sphericity + value
*.     XSPH2X XSPH2Y XSPH2Z XSPH2  2. sphericity + value
*.     XSPH3X XSPH3Y XSPH3Z XSPH3  3. sphericity + value
*.     XTHRX XTHRY XTHRZ XTHR      thrust axis and value
*.     XACOX XACOY XACOZ XACO      acoplanarity + value
*.     IXNVER                      # vertices
*.     XVERX(I) XVERY(I) XVERZ(I)  coordinate vertex I
*.     IXNPAR                      # partons
*.     IXQFL                       quark flavour
*.     XPPX(I) XPPY(I) XPPZ(I) XPP(I)  4-momentum of parton I  
*.     IXNP                        number of particles
*.
*.  Particle section (particle number: I)
*.  -------------------------------------
*.  - general
*.     IXRPN(I)              Running particle number
*.     IXPATR(I)             0 or PATR number of PHOT partners (conversion)
*.     IXPC(I)               Particle code (JADE-MC-convention)*100 + identified typ
*.     IXVN(I)               0 or vertex number of particle origin
*.     IXV2N(I)              0 or number of sec. vertec
*.     IXPALL(I)             0 or particle number in PALL bank (MC only)
*.     IXEO(I)               Traceback code [ electron origin ] (MC only)
*.     IXTPTR(I)             0 or number of TPTR bank
*.     IXPTF(I)              Particle typ flag: 0=cluster, 1=tracks
*.     XEX(I) XEY(I) XEZ(I)  Particle direction
*.     XP(I)                 Total momentum
*.     XCH(I)                Charge
*.  - tracks ( IXPTF(I)=1 )
*.     XECL(I) XECLE(I)      Energy of the associated cluster and its error
*.     XECLC(I)              0 oder corrected cluster energy (Meier)
*.     XDEDX(I) XDEDXS(I)    dEdx + sigma
*.     XRMIN(I)              Rmin
*.     XDX(I) XDY(I) XDZ(I)  Doca point(?)
*.     IXDFLG(I)             Detector flag (TP convention)
*.     IXCLN(I)              Number of the 1. associated cluster 
*.                           + 100*(number of the 2. associated cluster)
*.     IXCTRN(I)             Number of the track in PATR Bank
*.     IXHITS(I)             # hits in r/phi * 100 + # hits in r/z
*.     IXMUQ(I)              0 oder myon quality
*.  - clusters ( IXPTF(I)=0 )
*.     XECL(I) XECLE(I)      Energy of cluster and its error
*.     XECLC(I)              0 oder corrected cluster energy
*.     IXLGP(I)              LG section: 0 = barrel, +/-1 = +/-endcap
*.     IXCLN(I)              Number of the cluster in LGCL bank + 100* number of the 2. cluster
*.     IXNBL1(I)             # LG blocks 1. cluster minus # ass. tracks
*.     IXNBL2(I)             # LG blocks 2. cluster
*.
*.     
*.  === Format of the binary ZE4V data ===
*.
*.     See JADE Computer Note 99.
*.     N.B.: The binary data has BOS format. It can be read on both
*.           big endian and little endian machines.
*.           
*.  === Format of the JADE ASCII data ===
*.
*.  running evn#    (I6)
*.  run#     evn#    MCREDU    (3I8)
*.  Ebeam  Evis(ch)  Evis(ch+ne)    (3E16.9E2)
*.  1st sphericity axis(x,y,z) + value    (4E16.9E2)
*.  2nd sphericity axis(x,y,z) + value    (4E16.9E2)
*.  3rd sphericity axis(x,y,z) + value    (4E16.9E2)
*.      thrust     axis(x,y,z) + value    (4E16.9E2)
*.   acoplanarity  axis(x,y,z) + value    (4E16.9E2)
*.  #vertices    (I3)
*.   1st vertex (x,y,z)    (3E16.9E2)
*.   2nd vertex (x,y,z)    (3E16.9E2)
*.          :
*.  #jets/partons  quarkflavor    (2I3)
*.   1st parton px,py,pz,abs(p)    (4E16.9E2)
*.   2nd parton px,py,pz,abs(p)    (4E16.9E2)
*.          :
*.  #particles    (I4)
*.  (********** for each particle: *************************)
*.    running particle #    (I4)
*.    PATR# partcode vertex# secvertex# PALL# elecorigin TPTR# Ptf  (8I6)
*.    particle direction x,y,z + ptot + charge    (4E16.9E2,F4.1)
*.   (if Ptf=1:)
*.    Eclust Eclust-err corr.Eclust dE/dx sigma-dE/dx    (5E16.9E2)
*.    Rmin    (E16.9E2)
*.    doca  (x,y,z)    (3E16.9E2)
*.    detflag   cluster#   track#   nrphi*100+nrz  muquality    (5I6)
*.   (if Ptf=0:)
*.    Eclust  Eclust-err  corr.Eclust    (3E16.9E2)
*.    LG-part  cluster#   #blocks(1st LG)  #blocks(2nd LG)    (4I6)
*.  (********************************************************)
*.   (empty line)
*.
*.***************************************************************
C
C------  JADE event record variables -------------------------------
C 
      INTEGER IXREVN,IXRUN,IXEVENT,IXMCR
      REAL XEBEAM,XEVCH,XEVTOT
      REAL XSPH1X,XSPH1Y,XSPH1Z,XSPH1
     +     ,XSPH2X,XSPH2Y,XSPH2Z,XSPH2
     +     ,XSPH3X,XSPH3Y,XSPH3Z,XSPH3
     +     ,XACOX,XACOY,XACOZ,XACO
     +     ,XTHRX,XTHRY,XTHRZ,XTHR
      INTEGER IXNVER
      REAL XVERX,XVERY,XVERZ
      INTEGER IXNPAR,IXQFL
      REAL XPPX,XPPY,XPPZ,XPP
      INTEGER IXNP,IXRPN,IXPATR,IXPC,
     +     IXVN,IXV2N,IXPALL,IXEO,IXTPTR,IXPTF
      REAL XEX,XEY,XEZ,XP,XCH,XECL,XECLE,XECLC,
     +     XDEDX,XDEDXS,XRMIN,XDX,XDY,XDZ
      INTEGER IXDFLG,IXCLN,IXCTRN,IXHITS,
     +     IXMUQ,IXLGP,IXNBL1,IXNBL2
      INTEGER IX
      PARAMETER( IX=400 )
      COMMON /XDATA/ 
C         general section
     +     IXREVN,IXRUN,IXEVENT,IXMCR
     +     ,XEBEAM,XEVCH,XEVTOT
     +     ,XSPH1X,XSPH1Y,XSPH1Z,XSPH1
     +     ,XSPH2X,XSPH2Y,XSPH2Z,XSPH2
     +     ,XSPH3X,XSPH3Y,XSPH3Z,XSPH3
     +     ,XACOX,XACOY,XACOZ,XACO
     +     ,XTHRX,XTHRY,XTHRZ,XTHR
     +     ,IXNVER
     +     ,XVERX(20),XVERY(20),XVERZ(20)
     +     ,IXNPAR,IXQFL
     +     ,XPPX(20),XPPY(20),XPPZ(20),XPP(20),IXNP
C         particle section
     +     ,IXRPN(IX),IXPATR(IX),IXPC(IX)
     +     ,IXVN(IX),IXV2N(IX),IXPALL(IX),IXEO(IX)
     +     ,IXTPTR(IX),IXPTF(IX)
     +     ,XEX(IX),XEY(IX),XEZ(IX),XP(IX),XCH(IX)
     +     ,XECL(IX),XECLE(IX),XECLC(IX)
     +     ,XDEDX(IX),XDEDXS(IX),XRMIN(IX),XDX(IX),XDY(IX),XDZ(IX)
     +     ,IXDFLG(IX),IXCLN(IX),IXCTRN(IX),IXHITS(IX)
     +     ,IXMUQ(IX),IXLGP(IX),IXNBL1(IX),IXNBL2(IX)
      INTEGER IXDATA( 28+60+2+80+1+IX*31 )
      EQUIVALENCE( IXDATA(1),IXREVN )
C
C -------------------------------------------------------------------
C
      INTEGER IER,IOPT,IFORM,IUIN
C BOS
      REAL RW(40000)
      INTEGER IW(-4:40000),NWORDS
      INTEGER*2 HW0(80000)
      EQUIVALENCE (RW(1),IW(1),HW0(1))
      SAVE IW
C ZE4V pointer and counter
      INTEGER LH,LT,LCH,LPH,LPR,NPTR
      INTEGER*2 HW(80000),H1,H2
C
      INTEGER I,I1,I2,IXREVS,NDUM,NREAD,IOS
      CHARACTER TXT
      SAVE IXREVS
C
C----------------- C o d e ------------------------------------
C
      IER=0
      CALL VZERO(IXDATA,28+60+2+80+1+IX*31)
C
C**************************************
C     Read data from ASCII files
C**************************************
C
C General section
      IF( IFORM.EQ.0 ) THEN

C        Read header of the file 
         IF( IOPT.EQ.1 ) THEN
            READ(IUIN,ERR=9991,END=9992,FMT='(A1)') (TXT,I=1,34)
            GOTO 9999
         ENDIF
C
         READ(IUIN,'(I6)',ERR=9991,END=9992) IXREVN
         READ(IUIN,'(3I8)') IXRUN,IXEVENT,IXMCR
         READ(IUIN,'(3E16.9E2)') XEBEAM,XEVCH,XEVTOT
         READ(IUIN,'(4E16.9E2)') XSPH1X,XSPH1Y,XSPH1Z,XSPH1 
         READ(IUIN,'(4E16.9E2)') XSPH2X,XSPH2Y,XSPH2Z,XSPH2
         READ(IUIN,'(4E16.9E2)') XSPH3X,XSPH3Y,XSPH3Z,XSPH3
         READ(IUIN,'(4E16.9E2)') XTHRX,XTHRY,XTHRZ,XTHR
         READ(IUIN,'(4E16.9E2)') XACOX,XACOY,XACOZ,XACO
         READ(IUIN,'(I3)') IXNVER
         DO I=1,IXNVER
            READ(IUIN,'(3E16.9E2)') XVERX(I),XVERY(I),XVERZ(I) 
         ENDDO
         READ(IUIN,'(2I3)') IXNPAR,IXQFL
         IF( IXNPAR.GT.0 ) THEN
            DO I=1,IXNPAR
               READ(IUIN,'(4E16.9E2)') XPPX(I),XPPY(I),XPPZ(I),XPP(I) 
            ENDDO
         ENDIF
         READ(IUIN,'(I4)') IXNP

C Particle section
         IF( IXNP.GT.IX ) THEN
            WRITE(*,'(A,I8)') 'JADEV: Too many particles, IXNP=',IXNP
            WRITE(*,'(A)') ' ...will stop now!'
            STOP
         ENDIF
         DO I=1,IXNP
C ... general
            READ(IUIN,'(I4)') IXRPN(I)
            READ(IUIN,'(8I6)') IXPATR(I),IXPC(I),IXVN(I),IXV2N(I)
     >           ,IXPALL(I),IXEO(I),IXTPTR(I),IXPTF(I)
            READ(IUIN,'(4E16.9E2,F4.1)')
     >           XEX(I),XEY(I),XEZ(I),XP(I),XCH(I)
C ... tracks
            IF( IXPTF(I).EQ.1 ) THEN        
               READ(IUIN,'(5E16.9E2)')
     >              XECL(I),XECLE(I),XECLC(I),XDEDX(I),XDEDXS(I) 
               READ(IUIN,'(E16.9E2)') XRMIN(I)
               READ(IUIN,'(3E16.9E2)') XDX(I),XDY(I),XDZ(I)
               READ(IUIN,'(5I6)')
     >              IXDFLG(I),IXCLN(I),IXCTRN(I),IXHITS(I),IXMUQ(I)
C ... clusters
            ELSEIF( IXPTF(I).EQ.0 ) THEN
               READ(IUIN,'(3E16.9E2)') XECL(I),XECLE(I),XECLC(I)
               READ(IUIN,'(4I6)') IXLGP(I),IXCLN(I),IXNBL1(I),IXNBL2(I)
            ENDIF
         ENDDO
C Last line (blank) 
         READ(IUIN,'(A)') TXT
C
C*****************************************
C     Read data from binary ZE4V files
C*****************************************
C
      ELSEIF( ABS(IFORM).EQ.1 .AND. IOPT.EQ.0 ) THEN

C Perform FORTRAN read of binary data, if the data were produced on
C a platform similar to the current one (w.r.t. to endian format)
         IF( IFORM.EQ.1 ) THEN
            READ(IUIN,ERR=9991,END=9992) NWORDS,(IW(I),I=-3,NWORDS-4)
            NWORDS=NWORDS-4
            CALL UCOPY(HW0,HW,NWORDS)
         ELSE
C Read of binary data using subroutine CFGET, if the data were produced 
C on a platform different from the current one (w.r.t. to endian format)
C     - get record length
            NREAD=1
            CALL CFGET(IUIN,0,1,NREAD,NWORDS,IOS)
            IF( IOS.EQ.-1 ) THEN
               GOTO 9992
            ELSEIF( IOS.NE.0 ) THEN
               GOTO 9991
            ENDIF
            CALL VXINVB(NWORDS,1)
            NWORDS=NWORDS/4
C     - read record
            CALL CFGET(IUIN,0,NWORDS,NREAD,IW(-4),IOS)
            IF( IOS.NE.0 .OR. NREAD.NE.NWORDS ) GOTO 9991
C     - read last word of fortran record (=record length)
            NREAD=1
            CALL CFGET(IUIN,0,1,NREAD,NDUM,IOS)
            IF( IOS.NE.0 ) GOTO 9991
C     - ignore record length and BOS header IW(-4 ... 0)
            NWORDS=NWORDS-5
C     - perform byte-by-byte inversion of arrays IW,RW
            CALL VXINVB(IW(1),NWORDS)
C     - perform also halfword-by-halfword inversion of array HW0
C       (equivalenced with IW ) and put result into array HW
            DO I=1,NWORDS*2, 2
               H1=HW0(I)
               H2=HW0(I+1)
               HW(I)=H2
               HW(I+1)=H1
            ENDDO
         ENDIF

         LH   = HW(  1 )        ! Length of header
         LT   = HW(  5 )        ! Length general particle section
         LCH  = HW(  7 )        ! Length track section
         LPH  = HW(  9 )        ! Length cluster section
         LPR  = HW( 11 )        ! Length private section

         NPTR=0

         IXREVS  = IXREVS+1
         IXREVN  = IXREVS       ! no corresponding entry in ZE4V Bank
         IXRUN   = HW( 13 )
         IXEVENT = HW( 14 )
         IXMCR   = IW( 31 )
         XEBEAM  = RW(  8 )
         XEVCH   = 0.           ! no corresponding entry in ZE4V Bank
         XEVTOT  = 0.           ! no corresponding entry in ZE4V Bank
         XSPH1X  = RW(  9 )
         XSPH1Y  = RW( 10 )
         XSPH1Z  = RW( 11 )
         XSPH1   = RW( 12 )
         XSPH2X  = RW( 13 )
         XSPH2Y  = RW( 14 )
         XSPH2Z  = RW( 15 )
         XSPH2   = RW( 16 )
         XSPH3X  = RW( 17 )
         XSPH3Y  = RW( 18 )
         XSPH3Z  = RW( 19 )
         XSPH3   = RW( 20 )
         XTHRX   = RW( 21 )
         XTHRY   = RW( 22 )
         XTHRZ   = RW( 23 )
         XTHR    = RW( 24 )
         XACOX   = RW( 25 ) 
         XACOY   = RW( 26 )
         XACOZ   = RW( 27 )
         XACO    = RW( 28 )
         IXNVER  = HW(  3 )

         DO I=1,IXNVER
            XVERX(I) = RW( 31+HW(2)*(I-1) + 1 )
            XVERY(I) = RW( 31+HW(2)*(I-1) + 2 )
            XVERZ(I) = RW( 31+HW(2)*(I-1) + 3 )
         ENDDO

         IF( HW(4).GT.0 ) THEN
            IXNPAR   = HW( 62+2*HW(3)*HW(2) + 1 )
            IXQFL    = HW( 62+2*HW(3)*HW(2) + 2 )
            IF( IXNPAR.GT.0 ) THEN
               DO I=1,IXNPAR
                  XPPX(I) = RW( 32+HW(3)*HW(2)+4*(I-1) + 1 )
                  XPPY(I) = RW( 32+HW(3)*HW(2)+4*(I-1) + 2 )
                  XPPZ(I) = RW( 32+HW(3)*HW(2)+4*(I-1) + 3 )
                  XPP(I)  = RW( 32+HW(3)*HW(2)+4*(I-1) + 4 )
               ENDDO
            ENDIF
         ELSE
            IXNPAR  = 0
            IXQFL   = 0
            XPPX(1) = 0.
            XPPY(1) = 0.
            XPPZ(1) = 0.
            XPP(1)  = 0.
         ENDIF
         IXNP   = HW(6)

         NPTR  = LH             ! update ZE4V pointer

C Particle section
         IF( IXNP.GT.IX ) THEN
            WRITE(*,'(A,I8)') 'JADEV: Too many particles, IXNP=',IXNP
            WRITE(*,'(A)') ' ...will stop now!'
            STOP
         ENDIF
         DO I=1,IXNP
            IXRPN(I) = I        ! no corresponding entry in ZE4V Bank

C ... general
            IXPATR(I) = HW( NPTR*2 +  7 )
            IXPC(I)   = HW( NPTR*2 +  8 )
            IXVN(I)   = HW( NPTR*2 +  9 )
            IXV2N(I)  = HW( NPTR*2 + 10 )
            IXPALL(I) = HW( NPTR*2 + 15 )
            IXEO(I)   = HW( NPTR*2 + 16 ) ! I'am not sure
            IXTPTR(I) = HW( NPTR*2 + 17 )
            IXPTF(I)  = HW( NPTR*2 + 18 )
            XEX(I)    = RW( NPTR   +  1 )
            XEY(I)    = RW( NPTR   +  2 )
            XEZ(I)    = RW( NPTR   +  3 )
            XP(I)     = RW( NPTR   +  6 )
            XCH(I)    = RW( NPTR   +  7 )

            NPTR  = NPTR + LT   ! update ZE4V pointer

C ... tracks
            IF( IXPTF(I).EQ.1 ) THEN        
               XECL(I)    = RW( NPTR   +  1 )
               XECLE(I)   = RW( NPTR   +  2 )  
               XECLC(I)   = RW( NPTR   +  3 )  
               XDEDX(I)   = RW( NPTR   +  9 )  
               XDEDXS(I)  = RW( NPTR   + 10 )  
               XRMIN(I)   = RW( NPTR   + 11 )
               XDX(I)     = RW( NPTR   +  6 )
               XDY(I)     = RW( NPTR   +  7 )
               XDZ(I)     = RW( NPTR   +  8 )
               IXDFLG(I)  = HW( NPTR*2 +  7 ) 
               IXCLN(I)   = HW( NPTR*2 +  8 ) 
               IXCTRN(I)  = HW( NPTR*2 +  9 ) 
               IXHITS(I)  = HW( NPTR*2 + 10 ) 
               IXMUQ(I)   = IW( NPTR   + 12 ) 

               NPTR  = NPTR + LCH ! update ZE4V pointer
            
C ... clusters
            ELSEIF( IXPTF(I).EQ.0 ) THEN
               XECL(I)    = RW( NPTR   +  1 )
               XECLE(I)   = RW( NPTR   +  2 )
               XECLC(I)   = RW( NPTR   +  3 )
               IXLGP(I)   = HW( NPTR*2 +  7 ) 
               IXCLN(I)   = HW( NPTR*2 +  8 ) 
               IXNBL1(I)  = HW( NPTR*2 +  9 ) 
               IXNBL2(I)  = HW( NPTR*2 + 10 ) 

               NPTR  = NPTR + LPH ! update ZE4V pointer
               
            ENDIF
         ENDDO

      ENDIF
C
      GOTO 9999
 9991 IER=-1
      GOTO 9999
 9992 IER=1
 9999 RETURN
C
      ENTRY JADEPR
C
C Print out event in readable format
C

C General part
      WRITE(*,5900) IXREVN
     +     ,IXRUN,IXEVENT
     +     ,XEBEAM,IXMCR,XEVCH,XEVTOT
     +     ,IXNVER,IXNPAR,IXQFL
      WRITE(*,5910)
     +     'Thrust',XTHR,XTHRX,XTHRY,XTHRZ
     +     ,'Spher1',XSPH1,XSPH1X,XSPH1Y,XSPH1Z
     +     ,'Spher2',XSPH2,XSPH2X,XSPH2Y,XSPH2Z
     +     ,'Spher3',XSPH3,XSPH3X,XSPH3Y,XSPH3Z
     +     ,'Aco',XACO,XACOX,XACOY,XACOZ
      IF( IXNPAR.GT.0 ) THEN
         WRITE(*,5904)
         DO I=1,IXNPAR
            WRITE(*,5905) I,XPPX(I),XPPY(I),XPPZ(I),XPP(I) 
         ENDDO
      ENDIF
      WRITE(*,5902) IXNP
C Particle section
      I1=0
      I2=0
      DO I=1, IXNP
         
         IF( IXPTF(I) .EQ. 1 ) THEN
            I1=I1+1
            IF( I1.EQ.1 ) WRITE(*, 6001)
            WRITE(*,6010)
C    ... general
     +           IXRPN(I)
     +           ,IXPATR(I),IXPALL(I),IXTPTR(I)
     +           ,IXVN(I),IXV2N(I)
     +           ,IXEO(I),IXPC(I),IXPTF(I)
     +           ,XEX(I),XEY(I),XEZ(I),XP(I),XCH(I)
C    ... tracks
     +           ,XECL(I),XECLE(I),XECLC(I)
     +           ,IXDFLG(I),IXCLN(I),IXCTRN(I)
     +           ,INT(IXHITS(I)/100),MOD(IXHITS(I),100),IXMUQ(I)
     +           ,XRMIN(I),XDX(I),XDY(I),XDZ(I)
     +           ,XDEDX(I),XDEDXS(I)
         ELSE IF( IXPTF(I) .EQ. 0 ) THEN
            I2=I2+1
            IF( I2.EQ.1 ) WRITE(*, 6002)
            WRITE(*,6011)
C    ... general
     +           IXRPN(I)
     +           ,IXPATR(I),IXPALL(I),IXTPTR(I)
     +           ,IXVN(I),IXV2N(I)
     +           ,IXEO(I),IXPC(I),IXPTF(I)
     +           ,XEX(I),XEY(I),XEZ(I),XP(I),XCH(I)
C    ... clusters
     +           ,XECL(I),XECLE(I),XECLC(I)
     +           ,IXLGP(I),IXCLN(I),IXNBL1(I),IXNBL2(I)
         ENDIF
      ENDDO
C
 5900 FORMAT ('JADEPR:',/,T2
     +     ,58('#'),'   JADE EVENT RECORD ',I7,'    ',58('#')
     +     ,/,' Run=',I6,' Event=',I6,' Ebeam=',F6.2
     +     ,' MCREDU=',I3,' Evis(chrgd)= ',F5.2,' Evis(tot)= ',F5.2
     +     ,/,T4,'#Vertices=',I3,' #Partons=',I3,' Quarkflavour=',I3)
 5910 FORMAT (T4,A6,'= ',F6.4,4X,'x= ',F6.4,4X,'y= ',F6.4,4X,'z= ',F6.4)
 5904 FORMAT(T2,'Parton#      px      py      pz     |p|')
 5905 FORMAT(T2,I7,4F8.4)
 5902 FORMAT(T2,'#Particles=',I3)
 6001 FORMAT('  # PAT PAL TPT vt1 vt2 eor  pc  pt'
     +     ,'     ex     ey     ez    |p|  Q'
     +     ,'   E(cl)     +-    cor '  
     +     ,' det c# t# rf rz mu'
     +     ,'   Rmin   docx   docy   docz   dEdx     +-')
 6002 FORMAT(T91,' LG c# #b1 #b2')
 6010 FORMAT(T2,I2,8I4,4F7.3,F4.0,3F7.3,I5,4I3,I3,6F7.3)
 6011 FORMAT(T2,I2,8I4,4F7.3,F4.0,3F7.3,I5,3I3)

 6900 FORMAT(' - - - - - ',I2
     +     ,/T5,'General:',' PATR#',I2,' PALL#',I2,' TPTR#',I2
     +     ,' vtx#',I2,' vtx(2nd)#',I2
     +     ,' e-ori:',I3,' p-code:',I2,' p-type:',I2
     +     ,/T13,' ex=',F6.4,' ey=',F6.4,' ez=',F6.4
     +     ,' |p|=',F7.3,' Q=',F4.1)
 6901 FORMAT(T5,'Track:  '
     +     ,' det:',I4,' cl#:',I2,' tr#:',I2
     +     ,' #rphi:',I2,' #rz:',I2,' Mu:',I2
     +     ,' Rmin=',F7.3,' dx=',F7.3,' dy=',F7.3,' dz=',F7.3
     +     ,/T13,' E=',F7.3,' +-',F7.3,' cor=',F7.3
     +     ,' dEdx=',F7.4,' +-',F7.4)
 6902 FORMAT(T5,'Ecal:   '
     +     ,' LG:',I4,' Cl#',I2,' #Bl1:',I2,' #Bl2:',I2
     +     ,' E=',F7.3,' +-',F7.3,' cor=',F7.3)
C      
      RETURN
      END
C
C
C
      LOGICAL FUNCTION ENDIAN(CNAM)
      IMPLICIT NONE
*.***************************************************************
*.
*.  01/12/00   P.A. Movilla Fernandez
*.
*.  This routine checks if the endian format of current platform
*.  matches the endian format of the BINARY ZE4V input data.
*.
*.  Input:
*.        CNAM             ... name of the data file
*.  On return:
*.        ENDIAN = .TRUE.  ... format of current platform does match data format
*.        ENDIAN = .FALSE. ... format of current platform does NOT match data format
*.
*.  This is only an 'educated guess'. The routine checks the first
*.  integer words of the event record:
*.      1 = size of the fortran record length in bytes
*.      2 = size of the BOS record in full words
*.      4 = ZE4V BOS bank number (=1)
*.  The values should generally not exceed 65535 if the endian format
*.  of the data matches the endian format of the current platform.
*.
*.***************************************************************
      CHARACTER CNAM*(*)
C Check endian format of current platform
      INTEGER IBIG
      INTEGER*2 HBIG(2)
      EQUIVALENCE (IBIG,HBIG)
      DATA IBIG /1/
C
      INTEGER IUIN,IOS,N(5),LIM,NREAD
      DATA LIM/65535/
C Test input file
      CALL CFOPEN(IUIN,0,1,'r   ',0,CNAM,IOS )
      IF( IOS.NE.0 ) THEN
         WRITE(*,'(2A)') ' ENDIAN: Error while opening file ',CNAM
         WRITE(*,'(A)') ' ...will stop now!'
         STOP
      ENDIF
C Read first words of the record
      NREAD=5
      CALL CFGET(IUIN,0,NREAD,NREAD,N,IOS)
C Check words 1,2 and 4
      IF( ABS(N(1)).LE.LIM.AND.ABS(N(2)).LE.LIM.AND.ABS(N(4)).LE.LIM )
     >     THEN
         ENDIAN=.TRUE.
      ELSEIF(ABS(N(1)).GT.LIM.AND.ABS(N(2)).GT.LIM.AND.ABS(N(4)).GT.LIM)
     >        THEN
         ENDIAN=.FALSE.
         CALL VXINVB(N,2)
         CALL VXINVB(N(4),2)
      ELSE
         WRITE(*,'(3A)') ' ENDIAN: Cannot decide endian format'
     >        ,' of input data file ',CNAM
         WRITE(*,'(A,2I8,A5,I8)') ' Record starts with:',N
         WRITE(*,'(A)') ' ...will stop now!'
         STOP
      ENDIF
C I/O format info
      WRITE(*,'(/,A,/,A,2I8,A5,2I8)') 'ENDIAN:'
     >     ,' Data record starts with:',N
      WRITE(*,'(A,$)') ' JADE data I/O format: '
      IF( ENDIAN ) THEN
         WRITE(*,'(A,$)') 'binary ZE4V data and current platform with'
     >  //' EQUAL endian format.'
      ELSE
         WRITE(*,'(A,$)') 'binary ZE4V data and current platform with'
     >  //' DIFFERENT endian format.'
      ENDIF
      IF( HBIG(1).EQ.1 ) THEN
         WRITE(*,'(A)') ' Current platform is LITTLE ENDIAN.'
      ELSE
         WRITE(*,'(A)') ' Current platform is BIG ENDIAN.'
      ENDIF
C Close input file
      CALL CFCLOS(IUIN,0)
      RETURN
      END

