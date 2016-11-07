C  Print values of 1-T for all possibilities in data:
      real function prthr()
      implicit none
      include 'qqntup.inc'
      integer ifdim, maxtrk
      parameter( ifdim=4, maxtrk=500 )
      integer ierr, ntrak
      real ptrak(ifdim,maxtrk), tval(3), tvec(3,3)
C  Tracks and cluster:
      call gettc( ifdim, ptrak, ntrak )
      call pxlth4( ntrak, ifdim, ptrak, tval, tvec, ierr )
      print *, '1-T t+c:     ', tdtc, 1.0-tval(3), tdtc-(1.0-tval(3))
C  Tracks:
      call gettrk( ifdim, ptrak, ntrak )
      call pxlth4( ntrak, ifdim, ptrak, tval, tvec, ierr )
      print *, '1-T tracks:  ', tdt, 1.0-tval(3), tdt-(1.0-tval(3))
C  Cluster:
      call getcls( ifdim, ptrak, ntrak )
      call pxlth4( ntrak, 4, ptrak, tval, tvec, ierr )
      print *, '1-T cluster: ', tdc, 1.0-tval(3), tdc-(1.0-tval(3))
C  MT:
      call getmt( ifdim, ptrak, ntrak )
      call pxlth4( ntrak, 4, ptrak, tval, tvec, ierr )
      print *, '1-T MT:      ', tdmt, 1.0-tval(3), tdmt-(1.0-tval(3))
      prthr= 1.0
      return
      end
C  Compute 4-vector sum components:
      real function rpgce( ipos )
      implicit none
      integer ipos
      integer ifdim, maxtrk
      parameter( ifdim=4, maxtrk=500 )
      real ptrak(ifdim,maxtrk), psum(4)
      integer ntrak, i, j
      call getmt( ifdim, ptrak, ntrak )
      call vzero( psum, 4 )
      do i= 1, ntrak
         do j= 1, 4
            psum(j)= psum(j) + ptrak(j,i)
         enddo
      enddo
      rpgce= psum(ipos)
      return
      end
C  Compute 1-T for a given event for plotting:
      real function rcalct( opt )
      implicit none
      integer opt
      integer ifdim, maxtrk
      parameter( ifdim=4, maxtrk=500 )
      integer ierr, ntrak
      real ptrak(ifdim,maxtrk), tval(3), tvec(3,3)
      if( opt .eq. 1 ) then
        call gettc( ifdim, ptrak, ntrak )
      elseif( opt .eq. 2 ) then
        call gettrk( ifdim, ptrak, ntrak )
      elseif( opt .eq. 3 ) then
        call getcls( ifdim, ptrak, ntrak )
      elseif( opt .eq. 4 ) then
        call getmt( ifdim, ptrak, ntrak )
      endif
      rcalct= -1.0
      if( ntrak.gt.2 ) then
        call pxlth4( ntrak, ifdim, ptrak, tval, tvec, ierr )
        if( ierr.eq.0 ) rcalct= 1.0-tval(3)
      endif
      return
      end
C  Wrappers for tracks/clusters/MT:
      real function rtdtc()
      implicit none
      real rcalct
      rtdtc= rcalct( 1 )
      return
      end
      real function rtdmt()
      implicit none
      real rcalct
      rtdmt= rcalct( 4 )
      return
      end
      real function rtdt()
      implicit none
      real rcalct
      rtdt= rcalct( 2 )
      return
      end
      real function rtdc()
      implicit none
      real rcalct
      rtdc= rcalct( 3 )
      return
      end
C  Get tracks and clusters:
      subroutine gettc( ifdim, ptrak, ntrak )
      implicit none
      include 'qqntup.inc'
      integer ifdim, ntrak, ncls
      real ptrak(ifdim,*)
      call gettrk( ifdim, ptrak, ntrak )
      call getcls( ifdim, ptrak(ifdim+1,ntrak), ncls )
      ntrak= ntrak+ncls
      return
      end
C  Get cluster 4-vectors:
      subroutine getcls( ifdim, ptrak, ntrak )
      implicit none
      include 'qqntup.inc'
      integer ifdim, ntrak
      real ptrak(ifdim,*)
      integer i, j
      real sum
      do i= 1, nclus
        sum= 0.0
        do j= 1, 3
          ptrak(j,i)= pclus(j,i)
          sum= sum+pclus(j,i)**2
        enddo
        ptrak(4,i)= sqrt(sum)
      enddo        
      ntrak= nclus
      return
      end
C  Get track 4-vectors:
      subroutine gettrk( ifdim, ptrak, ntrak )
      implicit none
      include 'qqntup.inc'
      integer ifdim, ntrak
      real ptrak(ifdim,*)
      real mpi
      parameter( mpi=0.140 )
      integer i,j
      real sum
      ntrak= 0
      do i= 1, ntrk
        if( id02(i).eq.1 ) then
          ntrak= ntrak+1
          sum= 0.0
          do j= 1, 3
            ptrak(j,ntrak)= ptrk(j,i)
            sum= sum+ptrk(j,i)**2
          enddo
          ptrak(4,ntrak)= sqrt(sum+mpi**2)
        endif
      enddo
      return
      end
C  Get MT objects:
      subroutine getmt(  ifdim, ptrak, ntrak )
      implicit none
      include 'qqntup.inc'
      integer ifdim, ntrak
      real ptrak(ifdim,*)
      real mpi
      parameter( mpi=0.140 )
      integer i, j, k
      real sum
C  Tracks first, might possibly be scaled:
      ntrak= 0
      do i= 1, ntrk
        if( id02(i) .eq. 0 ) goto 10
        do j= 1, nmttrk
          if( imttrk(j) .eq. i ) then
            print *, 'Track MT scaled: ', i, j, imttrk(j), mtscft(j)
            ntrak= ntrak + 1
            sum= 0.0
            do k= 1, 3
              ptrak(k,ntrak)= ptrk(k,i)*mtscft(j)
              sum= sum + (ptrk(k,i)*mtscft(j))**2
            enddo
            ptrak(4,ntrak)= sqrt(sum+mpi**2)
            goto 10
          endif
        enddo
        ntrak= ntrak + 1
        sum= 0.0
        do j= 1, 3
          ptrak(j,ntrak)= ptrk(j,i)
          sum= sum + ptrk(j,i)**2
        enddo
        ptrak(4,ntrak)= sqrt(sum+mpi**2)
 10     continue
      enddo
C  Cluster, killed, scaled or just copied:
      do i= 1, nclus
        do j= 1, nmtkil
          if( imtkil(j) .eq. i ) then
            goto 20
          endif
        enddo
        do j= 1, nmtcls
          if( imtcls(j) .eq. i ) then
            ntrak= ntrak + 1
            sum= 0.0
            do k= 1, 3
              ptrak(k,ntrak)= pclus(k,i)*mtscfc(j)
              sum= sum + ptrak(k,ntrak)**2
            enddo
            ptrak(4,ntrak)= sqrt(sum)
            goto 20
          endif
        enddo
        ntrak= ntrak + 1
        sum= 0.0
        do j= 1, 3
          ptrak(j,ntrak)= pclus(j,i)
          sum= sum + ptrak(j,ntrak)**2
        enddo
        ptrak(4,ntrak)= sqrt(sum)
 20     continue
      enddo
      return
      end
