CDECK  ID>, QQCKBK.
      subroutine qqckbk( chform, rblock, iblock, ierr )
      IMPLICIT NONE
C  Check ntuple common block real values for numerical problems
C  Input:  chform   HBNAME's character string describing the ntuple block
C          rblock() ntuple common block as real array
C          iblock() ntuple common block as integer array
C  Output: ierr     error flag, .ne.0 means problems
C  Author: Stefan Kluth
C  Date: early 2002
C  Modification:
      character*(*) chform
      real rblock(*)
      integer iblock(*), ierr
      logical ldebug
      parameter( ldebug=.false. )
      integer lenocc, index, ifps, ifpscl
      integer chflen, maxent
      parameter( chflen=500, maxent=100 )
      logical lignore, lint, lreal
      integer nent, ilow, ihigh, maxidx(maxent), maxlen(maxent), 
     &     inam(maxent)
      integer ibrace1, ibrace2, icomma, icbrack, bidx, idim1, idim2,
     &     i, j, k, len, bidx2
      character*1 char
      character*20 chent(maxent), chdim, chdim1, chfmt
      character*21 class(0:9)
      data class / 'Positive normalized',
     &     'Negative normalized',
     &     'Positive zero',         
     &     'Negative zero',         
     &     'Positive infinity',     
     &     'Negative infinity',     
     &     'Positive denormalized', 
     &     'Negative denormalized', 
     &     'Signalling NaN',        
     &     'Quiet NaN' /
C  Initialise:
      ierr= 0
C  Find "," separated entries in chform:
      nent= 0
      ihigh= -1
      len= lenocc( chform )
      if( ldebug ) print *, chform(1:lenocc(chform))
      do i= 1, chflen
        if( chform(i:i) .eq. '(' .or. chform(i:i) .eq. '[' ) 
     &       lignore= .true.
        if( chform(i:i) .eq. ')' .or. chform(i:i) .eq. ']' ) 
     &       lignore= .false.
        if( .not.lignore .and. 
     &       ( chform(i:i) .eq. ',' .or. i.eq.len+1 ) ) then
          ilow= ihigh+2
          ihigh= i-1
          nent= nent+1
          chent(nent)= chform(ilow:ihigh)          
          if( ldebug ) print *, nent, ' ', chent(nent)
        endif
      enddo
C  Reset index array:
      do i= 1, maxent
        maxidx(i)= -1
      enddo
C  Decode each entry and test floats:
      bidx= 1
      do i= 1, nent
C     Extract the index of the name part:
        if( index( chent(i), '(' ).gt.0 ) then
          inam(i)= index( chent(i), '(' )-1
        elseif( index( chent(i), '[' ).gt.0 ) then
          inam(i)= index( chent(i), '[' )-1
        elseif( index( chent(i), ':' ).gt.0 ) then
          inam(i)= index( chent(i), ':' )-1
        else
          inam(i)= lenocc( chent(i) )
        endif
        if( ldebug ) print *, 'Name part: ', i, ' ', chent(i)(1:inam(i))
        lint= .false.
        lreal= .false.
C     Test types of entries:
        char= chent(i)(1:1)
        if( ( ( char .eq. 'I' .or. char .eq. 'J' .or. char .eq. 'K' .or.
     &       char .eq. 'L' .or. char .eq. 'M' .or. char .eq. 'N' ) .and.
     &       index( chent(i), ':' ).eq.0 ) .or. 
     &       index( chent(i), ':I' ).gt.0  ) then
C     An int entry:
          lint= .true.
          if( index( chent(i), '(' ).eq.0 .and. 
     &        index( chent(i), '[' ).gt.0 ) then
C     Single int entry, could be index for an array:
            maxidx(i)= iblock(bidx)
            icomma= index( chent(i), ',' )
            icbrack= index( chent(i), ']' )
            write( chfmt, '(''(i''i1'')'')' ) icbrack-icomma+1
            read( chent(i)(icomma+1:icbrack-1), chfmt ) maxlen(i)
            if( ldebug ) print *, 'Array index? ', maxidx(i), maxlen(i)
          endif
        elseif( index( chent(i), ':R' ).gt.0 ) then
C     A float entry:
          lreal= .true.
        else
          print *, 'QQCKBK: entry not identified: ', chent(i)
          ierr= 2
          return
        endif
C     Now analyse entries:
        ibrace1= index( chent(i), '(' )
        if( ibrace1.eq.0 ) then
C     A single valued entry:
          if( lreal .and. ifps( rblock(bidx) ).eq.0 ) then
            print *, chent(i)(1:inam(i)), ': ',
     &           class(ifpscl( rblock(bidx) ))
            ierr= 1
          endif
          if( ldebug ) then
            if( lint ) then
              print *, chent(i)(1:inam(i)), ' I ', bidx, iblock(bidx)
            elseif( lreal ) then
              print *, chent(i)(1:inam(i)), ' R ', bidx, rblock(bidx)
            endif
          endif
          bidx= bidx+1
        else
C     An array, find the actual and max lengths:
          ibrace2= index( chent(i), ')' )
          chdim= chent(i)(ibrace1+1:ibrace2-1)
C     Is it 1- or 2-d:
          idim1= 1
          icomma= index( chdim, ',' )
          if( icomma.gt.0 ) then
            chdim1= chdim(1:icomma-1)
            chdim= chdim(icomma+1:lenocc(chdim))
            write( chfmt, '(''(i''i1'')'')' ) lenocc( chdim1 )
            read( chdim1, chfmt ) idim1
          endif
C     Try to find dimension in previous entries,
C     i.e. the array is variable size:
          do j= 1, i-1
            if( chent(j)(1:inam(j)).eq.chdim ) then
              idim2= maxidx(j)
              maxlen(i)= maxlen(j)
              goto 10
            endif
          enddo
C     No match with previous names, its a simple number,
C     i.e. the array is fixed size:
          write( chfmt, '(''(i''i1'')'')' ) lenocc( chdim )
          read( chdim, chfmt ) idim2
          maxlen(i)= idim2
 10       continue
          if( ldebug ) print *, 'Array: ', idim1, idim2
C     Now test array contents:
          do j= 1, idim2
            do k= 1, idim1
              bidx2= bidx+(j-1)*idim1+k-1
              if( lreal .and. ifps( rblock(bidx2) ).eq.0 ) then
                print *, chent(i)(1:inam(i)), ' ', j, ' ',
     &               class(ifpscl( rblock(bidx2) ))
                ierr= 1
              endif
              if( ldebug ) then
                if( lint ) then
                  print *, k, bidx2, ' I ', iblock(bidx2)
                elseif( lreal ) then
                  print *, k, bidx2, ' R ', rblock(bidx2)
                endif
              endif
            enddo
          enddo
          bidx= bidx+maxlen(i)*idim1
        endif
      enddo
      return
      end
