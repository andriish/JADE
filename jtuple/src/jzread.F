      program jzread
      implicit none
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

C     HBOOK:
      integer nwpaw
      parameter( nwpaw=1000000 )
      integer hmemor(nwpaw)
      common /pawc/ hmemor

C     FFREAD declarations:
      integer ffspac
      parameter( ffspac=20000 )
      real space
      common /cfread/ space(ffspac)
#include "jzffr.inc"

C General I/O steering parameters
C (should be read in via Fortran read or FFREAD or whatever) 
      integer iuin, iprout
C --- Number of events to print out after connection with a new input file
      data iprout/ 20 /
C --- Fortran LUN / CFIO descriptor
      data iuin / 20 /
C Further stuff
      integer nevent, ievtnw, ifile, ios, ier, icycle
      logical endian, lex, lprout
      character cform*12
      character*(namlen) fnam
C
C------- C o d e ----------------------------------
C
C INITIALIZATION
C
C     Initialise HBOOK:
      call hlimit( nwpaw )

C     Read FFREAD cards:
      call ffinit( 0 )
      call ffset( 'SIZE', 8 )
      call jzsetr

C     Local init:
      nevent= 0
      if( abs( iform ).eq.1 ) then
         cform= 'UNFORMATTED'
      else
         cform= 'FORMATTED'
      endif

C     Open hbook file:
C     options to surpress RZOUT warnings?
      print *, 'JZREAD: opening hbook file ' 
     &     , chbkf(1:index(chbkf,' ')-1), ', unit= ', iohbk
     &     , ' lrec= ', lrec
      call hropen( iohbk, 'JTUPLE', chbkf, 'N', lrec, ios )
      if( ios.ne.0 ) then
         print *, 'JZREAD: can''t open hbook file ', chbkf
     &        , 'on unit ', iohbk, ', error code ', ios
         stop
      endif

C     Perform user specific initialization
      call jzinit
C
C     Begin of file loop:
C
      do 2000 ifile= 1, nbfile
         ievtnw= 0
         fnam= cfname(ifile)(1:index(cfname(ifile),' ')-1)
         inquire( file=fnam, exist=lex )
         if( .not.lex ) then
            write(*,9000) fnam(1:index( fnam,' ' )-1)
 9000       format( ' JZREAD: file ', A
     &           ,' does not exist. will stop now!' )
            goto 3000
         endif
C
C     Connect with data file:
C
C     Check if endian format of current platform matches endian 
C     format of binary input data. iform set via FFREAD tells
C     us format of data:
         if( ifile.eq.1 .and. iform.ne.0 ) then 
            if( .not.endian( fnam ) ) iform= -abs( iform )
         endif
C     Use FORTRAN I/O to handle ASCII data and binary data with
C     same endian format but use CERNLIB package CFIO to handle
C     binary data with different endian format
         if( iform.ge.0 ) then
            open( iuin, file=fnam, status='OLD', form=cform, 
     &           iostat=ios )
         else
            call cfopen( iuin, 0, 1, 'R   ', 0, fnam, ios )
         endif
C
C     Print some info:
         write( *, 9001 ) ifile, iuin, cform, ios, 
     &        fnam(1:index( fnam,' ' )-1)
 9001    format(//,' JZREAD: Processing data file:'
     >        ,' NR=',I2,' LUN=',I3,' F=',A,' IOSTAT=',I2,' FILE=',A,//)
         if( ios.ne.0 ) then
            write( *,9002 )
 9002       format(' JZREAD: Error while opening file!'
     >           ,/,'...will stop now!')
            stop
         endif
C
C     Read file header present in JADE files with ASCII format:
         call jadev( iuin, iform, 1, ier )
         if( ier.lt.0 ) then
            write( *, 9010 ) nevent+1, ier
 9010       format(' JZREAD: read error in event=',i7,' iostat=',i2)
            stop
         endif
C
C     Begin of event loop:
C
 1000    continue
c
         nevent= nevent + 1
C
C     Read JADE ze4v event record:
C
         call jadev( iuin, iform, 0, ier )
C     Check if read error:
         if( ier.lt.0 ) then
            write( *, 9010 ) nevent,ier
            stop
C     Check if EOF reached:
         elseif( ier.eq.1 ) then
            if( iform.ge.0 ) then
               close( iuin )
            else
               call cfclos( iuin,0 )
            endif
            nevent= nevent - 1
            write( *, 9011 ) nevent,ier
 9011       format(' JZREAD: end of file reached.',
     >           ' current event number =',i7,'  iostat=',i2)
            if( nevent.lt.maxevt ) then 
               goto 2000        ! <--- read next file
            else
               goto 3000
            endif
         endif
C
C     Dump JADE event record:
         ievtnw= ievtnw+1
         lprout= ( ievtnw.le.iprout
     >        .or. mod(nevent,500).eq.0
     >        .or. nevent.ge.maxevt )
         if( lprout ) call jadepr
C
C     Perform analysis of current event:
C     (Use information stored in common block /XDATA/)
         call jzevnt
C
C     End Of Event Loop:
C
         if( nevent.lt.maxevt ) then
            goto 1000
         else
            if( iform.ge.0 ) then
               close(iuin)
            else
               call cfclos(iuin,0)
            endif
            goto 3000
         endif
C
C     End of file loop:
C
 2000 continue

C     Finish job:
 3000 continue
      write( *, 9030 ) nevent
 9030 format(' JZREAD: Event limit or EOF reached.'
     >     ,' Number of read events:',I7)

C     Perform final analysis steps:
      call jzfnsh

C     Close hbook file after flushing ntuple:
      call hrout( 0, icycle, ' ' )
      call hrend( 'JTUPLE' )
      close( iohbk )

C     The End:
      stop
      end

C     FFREAD plug-in subroutine for '*'-keys:
      subroutine ffuser( hkey )
      implicit none
      integer hkey(*)

C     FFREAD common:
#include "jzffr.inc"

      character*(namlen) ckey, spaces

C     Convert argument to character:
      call uhtoc( hkey, nbytew, ckey, namlen )

C     Handle *FILE cards:
      if( index(ckey,'ZE4VFILE').gt.0 ) then
         nbfile= nbfile + 1
         if( nbfile.le.maxfil ) then
            call uhtoc( hfname, nbytew, cfname(nbfile), namlen )
            cfname(nbfile)= spaces( cfname(nbfile), 1 )
         else
            print *, 'FFUSER: max. number of files (', maxfil
     &           , ') exceeded: ', nbfile
         endif
      endif

C     The End:
      return
      end
