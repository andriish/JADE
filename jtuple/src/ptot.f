      real function ptot( id )
      implicit none
      integer id, i, j

      integer ifdim, maxtrk
      parameter( ifdim=4, maxtrk=500 )
      integer ntrak
      real ptrak(ifdim,maxtrk), psum(4)

      include 'qqntup.inc'
c      do i= 1, ntrk
c         ptot= sqrt(ptrk(1,i)**2+ptrk(2,i)**2+ptrk(3,i)**2)
c         call hfill( id, ptot, 0.0, 1.0 )
c      enddo
c      do i= 1, nclus
c         ptot= sqrt(pclus(1,i)**2+pclus(2,i)**2+pclus(3,i)**2)
c         call hfill( id, ptot, 0.0, 1.0 )
c      enddo
      call getmt( ifdim, ptrak, ntrak )
      call vzero( psum, 4 )
      do i= 1, ntrak
         do j= 1, 4
            psum(j)= psum(j) + ptrak(j,i)
         enddo
      enddo
      if( itkmh.eq.1 ) call hfill( id, psum(4), 0.0, 1.0 )
      ptot= psum(4)
      return
      end
