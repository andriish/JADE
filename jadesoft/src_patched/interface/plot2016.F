#if defined(JEXTERNISCERNLIB)
        subroutine itxn(x,y,C,n)
        call itx(x,y,C)
        end        
        subroutine drawmu(x,y)
        call drawmu_old(x,y)
        end
        subroutine drawpi(x,y)
        call drawpi_old(x,y)
        end
#endif
