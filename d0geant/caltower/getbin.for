      SUBROUTINE GETBIN(X,Y,Z,P,ZV,IPART,KEY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find KEY vector corresponding to correct bins
C-                         of momentum, rapidity for shower library
C-
C-   Inputs  : X,Y,Z     coordinates of calorimeter entry position
C-             P         track momentum
C-             Zv        Vertex z-position
C-             IPART     GEANT (Not ISAJET) particle code
C-
C-   Outputs : KEY(3)    RZ key vector for shower library
C-
C-   Created  14-FEB-1989   John Womersley
C-   Updated  11-OCT-1989   John Womersley  change arguments to use x,y,z 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NMBIN,NVBIN
      PARAMETER (NMBIN=7,NVBIN=6)
      REAL MOMBIN(NMBIN+1)
      DATA MOMBIN/0.1,0.32,1.,3.2,10.,32.,100.,2000./
      REAL VTXBIN(NVBIN+1)
      DATA VTXBIN/-9999.,-30.,-12.5,0.,12.5,30.,9999./

      REAL P,X,Y,Z,ZV
      INTEGER IETA,IPHI
      LOGICAL EM,HAD
      INTEGER IPART,IBIN
      INTEGER KEY(3)
C----------------------------------------------------------------------
C
C ****  determine momentum bin KEY(1)
C ****  -1 to -nmbin for EM, 1 to nmbin for hadrons
C
      EM=.FALSE.
      HAD=.FALSE.
      IF(IPART.LE.3)EM=.TRUE.
      IF(IPART.GE.7.AND.IPART.LE.41)HAD=.TRUE.
      IF(IPART.GE.45.AND.IPART.LE.47)HAD=.TRUE.
C
      IF(P.LT.MOMBIN(1).OR.((.NOT.EM).AND.(.NOT.HAD)))GOTO 990
C
      DO 100 IBIN=1,NMBIN
        IF (MOMBIN(IBIN).LE.P)THEN
          IF(MOMBIN(IBIN+1).GT.P)THEN
            KEY(1)=IBIN
          ENDIF
        ENDIF
  100 CONTINUE
C
      IF(EM)KEY(1)=-1*KEY(1)
C
C ****  determine eta bin KEY(2)
C
      CALL GETCEL(X,Y,Z,IETA,IPHI)
      KEY(2)=ABS(IETA)
C
C ****  determine vertex position bin KEY(3)
C
      DO 300 IBIN=1,NVBIN
        IF (VTXBIN(IBIN).LE.ZV)THEN
          IF(VTXBIN(IBIN+1).GT.ZV)THEN
            KEY(3)=IBIN
          ENDIF
        ENDIF
  300 CONTINUE
C
  999 RETURN
C
  990 CALL UZERO(KEY,1,4)
      RETURN
      END
