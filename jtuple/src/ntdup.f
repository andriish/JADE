      subroutine ntdup( id1, id2, id3 )
      implicit none
      integer id1, id2, id3
      include 'qqntup.inc'
      integer noent, ievent, ierr
      call hnoent( id1, noent )
      do ievent= 1, noent
         call hgnt( id1, ievent, ierr )
         if( ierr.ne.0 ) then
            print *, 'ntdup: error reading ', id1
            goto 10
         else
            if( abs(iferid(1)).eq.5 ) then
               call hfnt( id3 )
            else
               call hfnt( id2 )
            endif
        endif
      enddo
 10   continue
      return
      end
