C   09/02/87 809301320  MEMBER NAME  TPTRTS   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE TPTRTS( IPATR, ITRK, BKG, NTPTR, HV, INPV, IER )
C-----------------------------------------------------------------------
C
C     Author:   C. Bowdery      22/07/87:  Create summary for one track
C
C          mod: C. Bowdery      26/07/87:  HNPV --> INPV
C          mod: C. Bowdery      11/01/88:  SETSL--> VZERO
C     Last mod: C. Bowdery      30/09/88:  New IER argument added
C
C     Routine to summarise one PATR track and handle vertex info
C     PATR track ITRK does not necessarily correspond to TPTR bank NTPTR
C
C     Based upon TPPATR by S. Yamada.
C
C-----------------   A R G U M E N T S      ----------------------------
C
C     IPATR     In       I*4      Pointer to required PATR bank
C     ITRK      In       I*4      PATR track number to be processed
C     BKG       In       R*4      Magnetic field in kilogauss
C     NTPTR     In / Out I*4      TPTR bank no. to be created and incr.
C     HV        In / Out I*2      Vertex working array
C     INPV      In / Out I*4      Pointer to HV for next track no. slot
C     IER            Out I*4      Error code from CCRE
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

#include "bosdata.for"

C                            NW   = no. of words in a TPTR bank
C                            NVLM = no. of words in the TPVX/1 bank

      PARAMETER  ( NW     =     80 )
      PARAMETER  ( NVLM   =    100 )
      PARAMETER  ( INPVMX = 2*NVLM )

C                            IW: work space for each track
C                            HV: work space for the event vertex

      DIMENSION  IW( NW ), HV( INPVMX )

      DIMENSION  HW( 2*NW ), AW( NW )

      EQUIVALENCE ( HW(1),IW(1) ), ( AW(1),IW(1) )

C------------------  C O D E  ------------------------------------------

      IER   = 0

C                            Get the pointer to this track

      NWHD  = IDATA( IPATR + 1 )
      NWTRK = IDATA( IPATR + 3 )
      NB    = IPATR + NWHD + ( ITRK - 1 )*NWTRK

C                            Check if track is part of a curling one
C                            Skip processing if it is.

      CALL PARCIR( IPATR, NB, NBPAR )

      IF( NBPAR .EQ. 0 ) THEN

C                            Clear IW working space

        CALL VZERO( IW, NW )

C                            First assume this track from primary vertex
        HW( 1 ) = 1

C                            Track is seen in the Jet Chamber: code 1000
        HW( 3 ) = 1000

C                            Pointer to the copied track in 'PATR'.
        HW( 4 ) = ITRK

C                            Quality of the track
C                              chi**2 and no. of hits used: xy then rz

        AW( 19 ) = ADATA( NB + 23 )
        IW( 20 ) = IDATA( NB + 24 )
        AW( 21 ) = ADATA( NB + 32 )
        IW( 22 ) = IDATA( NB + 33 )

C                            CRV is the curvature in 1/mm
        CRV  = ADATA( NB + 25 )
        IF( BKG .LT. 0.0 ) CRV = -CRV
        ACRV = ABS( CRV )

C                            Charge of the track (from sign of CRV)
C                            If ambiguous charge, multiply by 100.

        AW(23) = SIGN( 1.0, CRV )

        IF( ADATA( NB + 26 ) .GE. ACRV ) AW(23) = 100.0 * AW(23)

C                            Calculate PT, momentum transverse to beam

        IF( ACRV .EQ. 0.0 ) ACRV = 1.E-10
        PT = ABS( 0.2998E-4 * BKG / ACRV )

C                            Calculate direction in xy plane

        DXY = SQRT( ADATA( NB+8 )**2 + ADATA( NB+9 )**2 )
        IF( DXY .EQ. 0.0 ) DXY = 1.E-10

C                            Calculate total momentum and error

        AW(24) = PT/DXY
        AW(25) = AW(24)*ADATA( NB+26 )/ACRV

C                            Store 'type' of the track direction
C                            1 = the line direction from the
C                                vertex to the first hit point
        IW(26) = 1

C                            Store direction cosines from PATR

        AW(27) = ADATA( NB+ 8 )
        AW(28) = ADATA( NB+ 9 )
        AW(29) = ADATA( NB+10 )

C                            VTXC/1 bank info
C                            HV(22)  # of the secondary tracks
C                            HV(23)  # of the positive secondary tracks
C                            HV(24)  # of the negative secondary tracks
C                            HV(26)  # of ambiguous secondary tracks

        HV(22) = HV(22) + 1

        IF( ABS(AW(23)) .LT. 10.0 ) THEN

          IF(AW(23) .GE. 0.0 ) THEN
            HV(23) = HV(23) + 1
          ELSE
            HV(24) = HV(24) + 1
          ENDIF

        ELSE
          HV(26) = HV(26) + 1
        ENDIF
        INPV = INPV + 1
        IF( INPV .LE. INPVMX )  HV( INPV ) = NTPTR

C                            Create a new track bank

        CALL CCRE( ITPTR, 'TPTR', NTPTR,  NW, IER )

        IF( IER .NE. 0 ) THEN
          CALL TPERRH
        ELSE
          CALL BSTR( ITPTR, IW, NW )
C + + + +                                       debug start
C         CALL BPRS( 'TPTR', NTPTR )
C + + + +                                       debug end
        ENDIF

C                           Increment TPTR bank number for next CALL

        NTPTR = NTPTR + 1

      ENDIF

      RETURN
      END
