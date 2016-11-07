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
*.    IUIN        ... logical fortran unit number associated with the 
*.                    data file  
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
*.     IXPC(I)               Particle code (JADE-MC-convention)*100 + 
*.                           identified typ
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
*.        according to ze4vpk it is # hits in r/phi + # hits in r/z * 100
*.     IXMUQ(I)              0 oder myon quality
*.  - clusters ( IXPTF(I)=0 )
*.     XECL(I) XECLE(I)      Energy of cluster and its error
*.     XECLC(I)              0 oder corrected cluster energy
*.     IXLGP(I)              LG section: 0 = barrel, +/-1 = +/-endcap
*.     IXCLN(I)              Number of the cluster in LGCL bank + 
*.                           100* number of the 2. cluster
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
#include "xdata.inc"
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
      end
