      SUBROUTINE GETDX(POSG,POS,WAVELEN,WLEN,VOFF,IORENT,NVER,DX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :Calculate fine vernier pad corrections
C-
C-   Inputs  :
C-   Outputs :DX
C-   Controls:
C-
C-   Created  29-AUG-1991   C.R.MURPHY
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      REAL POS,POSG,VERN,WAVELEN,WLEN,VOFF,DX,ERROR,R,S,ABS
      INTEGER IORENT,N,NVER
      DATA ERROR/5./
C
      VERN=(POSG-WAVELEN)-POS
      N=INT(WLEN/60.96)
C      DX=VERN+WLEN/2.-VOFF+60.96
C      IF (DX.GT.30.48) DX=DX-60.96
C      IF (IORENT.LT.0) DX=DX-2.*VERN-FLOAT(N)*60.96
C      IF (DX.GT.30.48) DX=DX-60.96
C      IF (NVER.EQ.2) DX=-DX
C
      R=VOFF+N*60.96
      IF (R.GT.WLEN+ERROR) R=R-60.96
      S=R-WLEN/2.
      IF (NVER.EQ.1) THEN
        DX=ABS(-WLEN/2.+VOFF)-ABS(VERN)
        IF (DX.GT.30.48) DX=DX-60.96
        IF (DX.LT.-30.48) DX=DX+60.96
        IF (IORENT.LT.0) THEN
          DX=ABS(VERN)-S
          IF (DX.GT.30.48) DX=DX-60.96
          IF (DX.LT.-30.48) DX=DX+60.96
        ENDIF
      ELSE
        DX=ABS(VERN)-ABS(-WLEN/2.+VOFF)
        IF (DX.GT.30.48) DX=DX-60.96
        IF (DX.LT.-30.48) DX=DX+60.96
        IF (IORENT.LT.0) THEN
          DX=S-ABS(VERN)
          IF (DX.GT.30.48) DX=DX-60.96
          IF (DX.LT.-30.48) DX=DX+60.96
        ENDIF
        DX=-DX
      ENDIF  
C
  999 RETURN
      END
