C----------------------------------------------------------------------
C-
C-   Created   5-JUN-1989   Harrison B. Prosper
C-   Updated   5-APR-1991   Andrei Kiryunin: include SAMUS 
C-
C----------------------------------------------------------------------
      INTEGER IDHELP                    
      PARAMETER( IDHELP = 6 )
C
      INTEGER IDZERO      
      PARAMETER( IDZERO = 7 )
C
      INTEGER IDCDC      
      PARAMETER( IDCDC  = 8 )
C
      INTEGER IDCAL      
      PARAMETER( IDCAL  = 9 )
C
      INTEGER IDMUO      
      PARAMETER( IDMUO  = 10 )
C
      INTEGER IDLV0      
      PARAMETER( IDLV0  = 11)
C
      INTEGER IDFDC      
      PARAMETER( IDFDC  = 12)
C
      INTEGER IDTRD      
      PARAMETER( IDTRD  = 13)
C
      INTEGER IDVTX      
      PARAMETER( IDVTX  = 14)
C
      INTEGER IDTBM      
      PARAMETER( IDTBM  = 15 )
C
      INTEGER IDSAM      
      PARAMETER( IDSAM  = 16 )
C
      INTEGER IDMAXI     
      PARAMETER( IDMAXI = 16 )
C
      INTEGER NMENU                     
      INTEGER NCOMD                     
      INTEGER NPAR                      
      COMMON /D0COM/ NMENU,NCOMD,NPAR
