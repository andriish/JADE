      program jtjob
      implicit none

C     BOS declarations:
#include "bos.inc"

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
#include "jtffr.inc"

C     I/O parameters set in BLOCK DATA JADEBD:
      integer iunit, junit, ncali, kunita, lunita
      common /ciouni/ iunit, junit, ncali, kunita(10), lunita(10)

C     Flags for reading the calibration constants in klread:
      integer lbmc
      common /cmccal/ lbmc(17)

C     Local declarations:
      integer irunit
      parameter( irunit=2 )
      integer ievnt, ios, nphead, irec, icycle, ifile
      character*(namlen) csvbsf

C     Initialise BOS and GEP (a la ze4vjb)
      call bint( mbos, mbos/2, mbos/8, 0 )
c      call gepic

C     Initialise HBOOK:
      call hlimit( nwpaw )

C     Read FFREAD cards:
      call ffinit( 0 )
      call ffset( 'SIZE', 8 )
      call jtsetr

C     Open calibration file:
      if( lcalib ) then
         open( lunita(2), file=ccalf, status='old', form='unformatted', 
     &        iostat=ios )
         if( ios.ne.0 ) then
            print *, 'JTJOB: can''t open calibration file: ', ccalf
            stop
         endif
C     Select 'MUCA' (Muon calibration?) bank:
         lbmc(1)= 1
      endif
      
C     Open hbook file:
C     options to surpress RZOUT warnings?
      print *, 'JTJOB: opening hbook file ', chbkf(1:index(chbkf,' ')-1)
     &     , ', unit= ', iohbk, ' lrec= ', lrec
      call hropen( iohbk, 'JTUPLE', chbkf, 'N', lrec, ios )
      if( ios.ne.0 ) then
         print *, 'JTJOB: can''t open hbook file ', chbkf
     &        , 'on unit ', iohbk, ', error code ', ios
         stop
      endif

C     Initialise job:
      call jtinit

c     Loop over list of input files:
      do ifile= 1, nbfile

C     Open Supervisor-BOS file:
         csvbsf= cbfile(ifile)(1:index(cbfile(ifile),' ')-1)
         print *, 'JTJOB: opening input bos file '
     &        , csvbsf(1:index(csvbsf,' ')-1)
         open( iunit, file=cbfile(ifile), status='old'
     &        , form='unformatted', iostat=ios )
         if( ios.ne.0 ) then
            print *, 'JTJOB: can''t open svbos file: '
     &        , csvbsf(1:index(csvbsf,' ')-1)
            stop
         endif
         
C     Event record loop (maxrec records per file):
         ievnt= 0
         do irec= 1, maxrec

C     Read next event record:
            call bread( irunit, *1000, *3000 )

C     Try to get HEAD bank first to check if this is a real event:
            nphead= iw( ibln( 'HEAD' ) )
            if( nphead.gt.0 ) then
               ievnt= ievnt + 1
               if( lcalib ) call klread
               call jtevnt( ievnt )
            else
               print *, 'JTJOB: no HEAD bank in BOS record ', irec,
     &              ', don''t process event' 
            endif
            
C     Not sure what these do, a la ze4vjb:
            call bslt
            call bdlg
            
         enddo

C     Jump from bread when EOF:
 3000    print *, 'JTJOB: EOF from bread on unit ', irunit, ' after '
     &        ,irec, ' records'
         print *, 'JTJOB: closing input bos file '
     &        , csvbsf(1:index(csvbsf,' ')-1)
         close( iunit )

C     End of loop over bos input files:
      enddo

C     Jump labels from bread when some error happened, need to be skipped
C     when we get here normally:
      goto 4000
 1000 print *, 'JTJOB: something wrong in bread after ',irec
     &     , ' records'
      goto 4000

C     Finish job:
 4000 call jtfnsh

C     Close hbook file after flushing ntuple:
      call hrout( 0, icycle, ' ' )
      call hrend( 'JTUPLE' )
      close( iohbk )

C     BOS statistics:
      call bsta
c      call gepw

C     The End:
      stop
      end

C     FFREAD plug-in subroutine for '*'-keys:
      subroutine ffuser( hkey )
      implicit none
      integer hkey(*)

C     FFREAD common:
#include "jtffr.inc"

      character*(namlen) ckey, spaces, csvbsf

C     Convert argument to character:
      call uhtoc( hkey, nbytew, ckey, namlen )

C     Handle *FILE cards:
      if( index(ckey,'SVBOSF').gt.0 ) then
         call uhtoc( svbosf, nbytew, csvbsf, namlen )
         csvbsf= spaces( csvbsf, 1 )
         nbfile= nbfile + 1
         if( nbfile.le.maxfil ) then
            cbfile(nbfile)= csvbsf
         else
            print *, 'FFUSER: max. number of files (', maxfil
     &           , ') exceeded: ', nbfile, csvbsf
         endif
      endif

C     The End:
      return
      end
