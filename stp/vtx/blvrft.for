      SUBROUTINE BLVRFT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fills bank VRFT hanging from VGEH. Actually the
C-                         data are given in the routine itself.
C-
C-   Inputs  :
C-   Outputs :
C-
C-   Created  15-SEP-1988   Ghita Rahal-Callot
C-   Updated  13-OCT-1988   Ghita Rahal-Callot  :Corrected PHI0 of the layer 0 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVRFT.LINK'
      INTEGER LVRFT, NFORMA, IPT, I
      INTEGER NUWIR, NULAY
      PARAMETER (NUWIR=8)
      PARAMETER (NULAY=3)
      INTEGER NUSEC(NULAY), NDAT
      DATA NUSEC /16, 32, 32/
      REAL CELSIZ
      DATA CELSIZ /1.8288/
      REAL RFICEN(2*NULAY)
      DATA RFICEN /5.334, -9.8438, 10.0076, -2.8125, 14.6304, 0.0/
      INTEGER IOF(3)
      DATA IOF / 17, 19, 21 /
C
C
C ****  Nominal x position of each wire
C
      REAL XWI(NUWIR)
      DATA XWI / -1.6002, -1.1430, -0.6858, -0.2286, 0.2286, 0.6858,
     &  1.1430, 1.6002/
C
C ****  staggering of each wire
C
      REAL STAG(NUWIR)
      DATA STAG / .01, -.01, .01, -.01, .01, -.01, .01, -.01/
C----------------------------------------------------------------------
      CALL MZFORM ( 'VRFT', '4I 4F 3I 4F 3I 4F -F', NFORMA)
      NDAT = 1 + 7*NULAY + 2*NUWIR
      CALL MZBOOK ( IXSTP, LVRFT, LVGEH, -IZVRFT, 'VRFT', 0, 0,
     &              NDAT, NFORMA, 0 )

      IC ( LVRFT + 1 ) = NULAY
      IPT = LVRFT+2
      DO 100 I = 1 , NULAY
        IC(IPT)   = NUSEC(I)
        IC(IPT+1) = NUWIR
        IC(IPT+2) = IOF(I)
        C(IPT+3)  = CELSIZ
        C(IPT+4)  = 180. / NUSEC(I)
        CALL UCOPY(RFICEN(2*I-1), C(IPT+5), 2)
        IPT = IPT+7
  100 CONTINUE
      CALL UCOPY(XWI, C(LVRFT+23),NUWIR)
      CALL UCOPY(STAG,C(LVRFT+23+NUWIR), NUWIR)
  999 RETURN
      END
