      SUBROUTINE ENJETS(LJET0)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : ENJETS is called at the end of each event.  Its
C-              purpose is to finish filling in the JETS banks. It fills the
C-              bank version number, Et, theta, phi, eta, sig**2(Ex), and
C-              sig**2(Ey).
C-
C-   Inputs  : LJET0 the link to the first JETS bank
C-   Outputs : None
C-   Controls:
C-
C-   Created  20-OCT-1988   Z. Wolf
C-   Updated  20-JAN-1989   Z. Wolf
C-   Updated  24-MAR-1989   Z. Wolf
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C--   I/O
      INTEGER LJET0
C
C--   ZEBRA
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C
C--   GEANT UNITS
      INCLUDE 'D0$INC:GCONST.INC/LIST'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
C
C--   INTERNAL VARIABLES
      INTEGER LJETS
      REAL EX,EY,EZ,E,MAGN
      REAL ET,THETA,PHI,ETA
      REAL SIG2EX,SIG2EY
C----------------------------------------------------------------------
C
C--   CHECK LJET0
      IF(LJET0.LE.0)THEN
        WRITE(LOUT,*)'ENJETS--> PROBLEM WITH LJET0'
        GOTO 999
      END IF
C
C--   BEGIN LOOP OVER JETS BANKS
      LJETS=LJET0
C
C--   CHECK LINK
   10 IF(IQ(LJETS-4).NE.4hJETS)THEN
        WRITE(LOUT,*)'ENJETS--> PROBLEM WITH LJETS'
        GOTO 999
      END IF
C
C--   GET QUANTITIES OF INTEREST
      EX=Q(LJETS+2)
      EY=Q(LJETS+3)
      EZ=Q(LJETS+4)
      E=Q(LJETS+5)
      MAGN=SQRT(EX**2+EY**2+EZ**2)
C
C--   PROTECTION
      IF(MAGN.EQ.0.)THEN
        ET=0.
        THETA=0.
        PHI=0.
        ETA=0.
        SIG2EX=0.
        SIG2EY=0.
        GO TO 20
      END IF
C
C--   CALCULATE QUANTITIES
      ET=SQRT(EX**2+EY**2)
      THETA=ATAN2(ET,EZ)
      PHI=ATAN2(EY,EX)
      IF(PHI.LT.0.)PHI=PHI+TWOPI
      ETA=-LOG(TAN(THETA/2.))
      SIG2EX=0.
      SIG2EY=0.
C
C--   FILL REMAINDER OF BANK
   20 CONTINUE
      Q(LJETS+6)=ET
      Q(LJETS+7)=THETA
      Q(LJETS+8)=PHI
      Q(LJETS+9)=ETA
      Q(LJETS+10)=SIG2EX
      Q(LJETS+11)=SIG2EY
C
C--   READY FOR NEXT BANK
      LJETS=LQ(LJETS)
      IF(LJETS.GT.0)GO TO 10
C
  999 RETURN
      END
