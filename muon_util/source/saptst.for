      LOGICAL FUNCTION SAPTST(N_DIR,FIT,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check if road line lie in beam plane
C-
C-   Returned value  :  .TRUE.   -    road lie in beam plane
C-                      .FALSE.  -    don't lie
C-
C-   Inputs  : N_DIR   -    direction in SAMUS detector
C-             FIT     -    road parameters
C-
C-   Outputs : IFL     -    .TRUE. if road line pass thru A - station
C-                          .FALSE. in other case
C-   Controls: 
C-
C-   Created  15-MAR-1991   O.Eroshin
C-   Updated  14-OCT-1991   Daria Zieminska  use VERXYZ instead of GE_VERTEX 
C-   Updated  18-SEP-1992   Daria Zieminska  added protection against /0 
C-   Updated   2-OCT-1992   Daria Zieminska  get ZMAG from STP 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*4 HSHAPE
      INTEGER  I,N_DIR,IVER,NV,NSPAR,IBUF,NBUF
      REAL     SAPLTR
      LOGICAL  IFL,FIRST
      REAL     PT,DIF,DIF_MAX,VDOTN,ZPLN(2),ZMAG(2),
     +         VT(3),VTBL(3),VTPT(3),FIT(4),SMALL
      REAL XPAR(3),SPAR(6),ROTM(3,3)
C----------------------------------------------------------------------
      DATA     FIRST/.TRUE./
      DATA     DIF_MAX/0.15/
      DATA     VTBL/ 0.0, 0.0, 1.0/
      DATA     ZPLN/-401.72, 401.72/
C      DATA     ZMAG/-515.62, 515.62/
      DATA SMALL/1.0E-6/
C----------------------------------------------------------------------
C
C......   Define positions of A - stations
C
      IF (FIRST)                                        THEN
C        ZPLN(1) = SAPLTR( 1,'ZPL')
C        ZPLN(2) = SAPLTR(10,'ZPL')
        CALL GTSMAG(1,HSHAPE,NSPAR,SPAR,XPAR,ROTM,NBUF,IBUF)
        ZMAG(1)=XPAR(3)
        CALL GTSMAG(2,HSHAPE,NSPAR,SPAR,XPAR,ROTM,NBUF,IBUF)
        ZMAG(2)=XPAR(3)
        FIRST   = .FALSE.
      END IF
C
      SAPTST  = .FALSE.
C
C......   Get interaction point
C
      CALL VERXYZ(IVER,VTPT,NV)
C
      IFL     = .FALSE.
      DO I=1,2
        PT    = FIT(I)*ZMAG(N_DIR)+FIT(I+2)
        PT    = (PT-VTPT(I))/(ZMAG(N_DIR)-VTPT(3))*(ZPLN(N_DIR)-VTPT(3))
        IF (ABS(VTPT(I)+PT).GT.34.) IFL = .TRUE.
      END DO
C
C......   Make check
C
      VT(1)   = FIT(3)-VTPT(1)
      VT(2)   = FIT(4)-VTPT(2)
      VT(3)   = 0.0
C
      CALL CROSS(VT,VTBL,VTPT)
      PT=SQRT(VTPT(1)**2+VTPT(2)**2+VTPT(3)**2)
      IF (PT.LT.SMALL) THEN  ! protection against crashing in VDOTN)
        SAPTST=.TRUE.
        GO TO 999
      END IF
C
      VT(1)   = FIT(1)
      VT(2)   = FIT(2)
      VT(3)   = 1.
C
      DIF     = VDOTN(VT,VTPT,3)
C
      IF (ABS(DIF).LE.DIF_MAX) SAPTST = .TRUE.
C----------------------------------------------------------------------
  999 RETURN
      END
