#ifndef jadehelpers_h
#define jadehelpers_h
#include <math.h>
#include <cmath>

#include <vector>
template <class V4>  void set_momentum(V4 &vv, const float x, const float y, const float z, const float e);
template <class EVENT,class V4>
std::vector<V4> GetFromTracks(EVENT* A)
{
    std::vector<V4> ret;
    static const  float mpi2 = 0.13957018*0.13957018;
    float ptrack[4];
    for( size_t itrk = 0; itrk < A->Ntrk; itrk++)
        {
            if( A->Id02[itrk] == 0 ) continue;
            ptrack[3] = 0.0;
            for( size_t j = 0; j < 3; j++ )
                {
                    ptrack[j] = A->Ptrk[itrk][j];
                    ptrack[3] += std::pow( ptrack[j], 2 );
                }
            ptrack[3] = std::sqrt( ptrack[3] + mpi2 );
            V4 XX;
            set_momentum<V4>(XX, ptrack[0], ptrack[1], ptrack[2], ptrack[3]);
            ret.push_back(XX);
        }
    return ret;
}
#endif
