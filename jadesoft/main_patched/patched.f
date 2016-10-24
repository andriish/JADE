        function aaor(A,B)
        logical A,B, or
        or=.FALSE.
        if (A .OR. B) or=.TRUE.
        return
        end
        
        function aaand(A,B)
        logical A,B, and
        and=.FALSE.
        if (A .AND. B) and=.TRUE.
        return
        end
        
        subroutine aamachine
        INTEGER ITYPE,IST
        COMMON / CIST / ITYPE,IST(4)
        
        
        end
C         MVC(  I1,  3, X2, 7, 1 )
        subroutine aamvc(I1,i2, r1, i3, i4)
        integer i1,i2,i3,i4
        real r1
        
        end
        
        function hfix(r)
        integer*2 hfix
        real r
        end
        
        
        
