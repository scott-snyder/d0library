      SUBROUTINE GETCEL(PX,PY,PZ,IETA,IPHI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find IETA and IPHI cell containing a track
C-
C-   Inputs  : Px,Py,Pz  3 momentum of particle
C-
C-   Outputs : IETA, IPHI
C-
C-   Created  14-FEB-1989   John Womersley
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      real eta,phi,px,py,pz
      integer ieta,iphi
      INCLUDE 'D0$INC:PI.DEF'
C----------------------------------------------------------------------

      CALL ETAPHI(PX,PY,PZ,ETA,PHI)

      IPHI=INT(PHI*64./TWOPI)+1

      IF (abs(ETA).le.3.2)THEN
        ieta=int(sign(10.*abs(eta)+1.,eta))
      elseif(abs(eta).le.3.42)then
        ieta=int(sign(33.,eta))
      elseif(abs(eta).le.3.7)then
        ieta=int(sign(34.,eta))
      elseif(abs(eta).le.4.1)then
        ieta=int(sign(35.,eta))
      elseif(abs(eta).le.4.45)then
        ieta=int(sign(36.,eta))
c      elseif(abs(eta).le.5.1)then ! temporary fix puts eta=infinity tracks
c      into last bin (37)
      else
        ieta=int(sign(37.,eta))
      endif
C
C ****  For IETA=33 onwards, only odd values of IPHI exist
C
      IF(ieta.ge.33.and.mod(iphi,2).eq.0)THEN
        iphi=iphi-1
      ENDIF

  999 RETURN
      END
