      SUBROUTINE CDATSW ( HIT, NDIM, NHIT, IFADC )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create and fill the Zebra structure
C-                         (CDCH -- DLYR -- DSEC)--DCDA from the hits HIT
C-                         given by GEANT ( JHITS ) For the Sense Wires
C-
C-   Inputs  : HIT ( NDIM, NHIT ) = array of data from JHITS
C-             IFADC(3)           = Layer, Sector, Wire
C-   Outputs : none
C-
C-   Created  27-JAN-1988   Ghita Rahal-Callot
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZDALH.LINK'
      INCLUDE 'D0$INC:GCUNIT.INC'
C
C
      INTEGER NDIM, NHIT, IFADC(*)
      REAL HIT ( NDIM, NHIT )
C
      INTEGER NWORD, NMAX
      PARAMETER (NWORD = 18, NMAX = 50)
      INTEGER IHIT, ITIMES ( 2*NMAX )
      REAL DRFT (NMAX), ERDRFT (NMAX), CARSET ( 200 )
      REAL DATAS ( NWORD, NMAX )
C
      INTEGER LDALL, LDALS, IPT
      REAL X, Y, DIST
C
C----------------------------------------------------------------------
C
      LDALH = LC ( LSCDC - IZDALH )
      IF ( LDALH .LE. 0 ) THEN
        WRITE ( LOUT, * ) '****** CDHITS : bank LDALH not defined'
        CALL EXIT(1)
      ENDIF
      LDALL = LC ( LDALH - IFADC(1) - 1 )
      IF ( LDALL .LE. 0 ) THEN
        WRITE ( LOUT, * ) '****** CDHITS : bank LDALL not defined',
     &    ' for the layer', IFADC(1)
        CALL EXIT(1)
      ENDIF
      LDALS = LC ( LDALL - IFADC(2) - 1 )
      IF ( LDALS .LE. 0 ) THEN
        WRITE ( LOUT, * ) '****** CDHITS : bank LDALS not defined',
     &    ' for the sector ', IFADC(2)
        CALL EXIT(1)
      ENDIF
      IPT = LDALS + 6 + IC ( LDALS + 6 ) * IFADC(3)

C
C  ****  Loop through hits in this cell
C
      IF ( NHIT .LE. 0 ) GO TO 999
      DO 100 IHIT=1,NHIT
C
C ****  Compute the Drift distance in the cell
C
        X = (HIT(1,IHIT) + HIT(4,IHIT)) / 2.
        Y = (HIT(2,IHIT) + HIT(5,IHIT)) / 2.
        DIST=  (X-C(IPT+1)) * C(LDALS+3)  + (Y-C(IPT+2)) * C(LDALS+4)
        CALL CDRIFT (DIST, IFADC, DRFT(IHIT), ERDRFT(IHIT))
  100 CONTINUE
C
C ****  Order hits by time on the sense wires
C
      CALL SORTZV ( DRFT, ITIMES, NHIT, 1, 0, 0, 0, CARSET )
C
C ****  Get the array DATAS ( to fill the ZEBRA Data banks )
C
      CALL DVECDA ( HIT, NDIM, NHIT, DRFT, ERDRFT,
     &              ITIMES, IFADC(3), DATAS )
C
C ****  Put the hits in Zebra Structure CDCH -- DLYR -- DSEC --DCDA
C
      CALL FIDCDA ( DATAS, NHIT, IFADC )
C
  999 CONTINUE
      RETURN
      END
