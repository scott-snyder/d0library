      SUBROUTINE MUSCNT(NMOD,HSHAPE,NSPAR,SPAR,XPAR,ROTM
     +                 ,NBUF,IBUF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : returns geometory parameter for a muon
C-                         scintillator 
C-
C-   Inputs  : NMOD      : Scintillator address 
C-                       : #Scint*1000 + #Muon_module
C-   Outputs : HSHAPE    : volume shape. (Character*4, 'BOX ' )
C-             NSPAR     : number of parameters. (=3)
C-             SPAR      : shape parameters.(Dx,Dy,Dz, Half width at local
C-             XPAR      : coordinate of the center of volume in lab system.
C-             ROTM      : 3x3 rotation matrix.
C-             NBUF      : no of spacial parameters filled in this program.
C-             IBUF      : spacial parameters
C-                       : IBUF(1) : Number of P.M.
C-                       :  BUF(2) : X of 1st P.M. in global 
C-                       :  BUF(3) : Y of 1st P.M. in global 
C-                       :  BUF(4) : Z of 1st P.M. in global 
C-                       :  BUF(5) : X of 2nd P.M. in global 
C-                       :  BUF(6) : Y of 2nd P.M. in global 
C-                       :  BUF(7) : Z of 2nd P.M. in global 
C-                       : IBUF(8) : size index
C-         note:   If the requested volume dose not exits, returned
C-                 parameters will be,
C-                   HSHAPE= '    '    (i.e.  blank)
C-                   NSPAR = 0
C-   Controls: None
C-
C-   Created   3-JAN-1992   Atsushi Taketani
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
C  varables in i/o argument...
      INTEGER NMOD 
      REAL    SPAR(3),XPAR(3),ROTM(3,3) 
      CHARACTER*4 HSHAPE
      INTEGER NBUF,IBUF(*), NSPAR
      INTEGER  MMOD, NSCI
C  external valiables
      INTEGER GZMSGE, LMSGE, IROT
C  loacl use
      INTEGER I
C  rotation matrix 
      REAL INVROT(3,3,16)
      INTEGER ICOL,IROW
      DATA INVROT/
     1   0., 0., 1.,  -1., 0., 0.,   0.,-1., 0.,
     2   0., 0.,-1.,  -1., 0., 0.,   0., 1., 0.,
     3   1., 0., 0.,   0., 0., 1.,   0.,-1., 0.,
     4   1., 0., 0.,   0., 0.,-1.,   0., 1., 0.,
     5   0., 0.,-1.,   1., 0., 0.,   0.,-1., 0.,
     6   0., 0., 1.,   1., 0., 0.,   0., 1., 0.,
     7  -1., 0., 0.,   0., 0.,-1.,   0.,-1., 0.,
     8  -1., 0., 0.,   0., 0., 1.,   0., 1., 0.,
     9   0., 0.,-1.,   0.,-1., 0.,  -1., 0., 0.,
     A   0., 1., 0.,   0., 0.,-1.,  -1., 0., 0.,
     B   0., 0., 1.,   0., 1., 0.,  -1., 0., 0.,
     C   0.,-1., 0.,   0., 0., 1.,  -1., 0., 0.,
     D   0., 0., 1.,   0.,-1., 0.,   1., 0., 0.,
     E   0., 1., 0.,   0., 0., 1.,   1., 0., 0.,
     F   0., 0.,-1.,   0., 1., 0.,   1., 0., 0.,
     G   0.,-1., 0.,   0., 0.,-1.,   1., 0., 0./
C-
C----------------------------------------------------------------------
C get MSGE pointer
      MMOD = MOD( NMOD, 1000 )     ! #muon_module
      NSCI = (NMOD - MMOD)/1000    ! #scintilltor 
      LMSGE = GZMSGE(MMOD,NSCI)    ! scintillator pointer
C
      IF ( LMSGE.EQ.0 ) THEN   ! --- MSGE bank dose not exist... 
        HSHAPE='    '
        NSPAR=0
        CALL VFILL(SPAR,3,10000.)
        CALL VFILL(XPAR,3,10000.)
        CALL VFILL(ROTM,9,10000.)
        NBUF = 0
        GOTO 999
      ELSE 
        HSHAPE='BOX '
        NSPAR=3
        SPAR(1) = C(LMSGE+16)/2.0  ! thickness
        SPAR(2) = C(LMSGE+17)/2.0  ! width
        SPAR(3) = C(LMSGE+18)/2.0  ! length
        XPAR(1) = C(LMSGE+12)
        XPAR(2) = C(LMSGE+13)
        XPAR(3) = C(LMSGE+14)
        IROT = IC(LMSGE+11)        ! orientation index
        DO 100 ICOL=1,3            ! rotation matrix local->global
        DO 100 IROW=1,3
  100     ROTM(IROW,ICOL) = INVROT(ICOL,IROW,IROT)
        IBUF(1) = IC(LMSGE+10)
        IBUF(8) = IC(LMSGE+15)
        DO 110 I=1,3
          IBUF(I+1) = IC(LMSGE+19+I)
  110     IBUF(I+4) = IC(LMSGE+23+I)
        NBUF = 8
      END IF
C
  999 RETURN
      END
