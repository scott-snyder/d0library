      SUBROUTINE WRITE_TRDCOR (PLANE,TRACK,hit)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : writes corrections in common TRDCOR, for at
C-                         most 4 hit anodes on the track.
C-   Inputs  :  PLANE      integer     1,2,3 (anodes) 4,5,6 (cathodes)
C-              TRACK      integer     track number
C-   Outputs : none (writes directly in common TRDCOR)
C-   Controls: none
C-
C-   Created  15-JAN-1993   Alain PLUQUET
C-   Updated  15-JUL-1993   A. Zylberstejn  : change NBPNT to NBHWIR 
C-   Updated  16-JUL-1993   A. Zylberstejn  :add wire nb as argument 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PLANE,TRACK,HIT,i
      INCLUDE 'D0$INC:TRDCOR.INC'
C      INCLUDE 'D0$INC:TRTOBN.INC'
      INCLUDE 'D0$INC:TRENER.INC'
C      I=NBPNT(PLANE,TRACK)
c      I=NBHWIR(PLANE,TRACK)
       IF (HIT.LE.4.AND.PLANE.LE.3) THEN
        PAD(PLANE,HIT,1)=CELE
        PAD(PLANE,HIT,2)=CEPI
        PAD(PLANE,HIT,3)=CAPC
        PAD(PLANE,HIT,4)=CPED
        PAD(PLANE,HIT,5)=CSEC
        PAD(PLANE,HIT,6)=CWIR
        PAD(PLANE,HIT,7)=CHVT
        PAD(PLANE,HIT,8)=CANG
        PAD(PLANE,HIT,9)=CGAS
        PAD(PLANE,HIT,10)=TCAN
        PAD(PLANE,HIT,11)=TTRD
        PAD(PLANE,HIT,12)=PCAN
        PAD(PLANE,HIT,13)=PTRD
        PAD(PLANE,HIT,14)=GCAN
        PAD(PLANE,HIT,15)=HVA
        PAD(PLANE,HIT,16)=HVW
        PAD(PLANE,HIT,17)=HVP
        STATUS(PLANE,HIT,1)=EELE
        STATUS(PLANE,HIT,2)=EEPI
        STATUS(PLANE,HIT,3)=EAPC
        STATUS(PLANE,HIT,4)=EPED
        STATUS(PLANE,HIT,5)=ESEC
        STATUS(PLANE,HIT,6)=EWIR
        STATUS(PLANE,HIT,7)=EHVT
        STATUS(PLANE,HIT,8)=EANG
        STATUS(PLANE,HIT,9)=EGAS
c        print*,' in write_trdcor,plane,hit',plane,hit,' cped',cped,
c     &    ' pad(4)',(pad(plane,i,4),i=1,hit)
      ENDIF
      END
