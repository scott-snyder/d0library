      SUBROUTINE CDNCTR(LAYER,SECTOR,NTRSEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : FIND # TRACKS IN CELL
C-
C-   Inputs  : LAYER,SECTOR
C-   Outputs : NTRSEC (# TRACKS IN CELL)
C-   Created  29 SEPT 1987 ROD ENGELMANN
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CDPARA.INC'

      INTEGER KPDTSG, NDTSG, LDTSG

      INTEGER LABEL,NTRSEC
      INTEGER LAYER, SECTOR, WIRE, NUMHIT,I,J,IP,JP

C----------------------------------------------------------------------

      NTRSEC=0
      CALL ZGDTSG( LAYER, NDTSG, LDTSG, KPDTSG )
      IF( NDTSG .EQ. 0) GO TO 999
      DO 10 I = 1, NDTSG
        IP = KPDTSG + (I-1) * LDTSG
C
C ****  Find # tracks in this sector
C
        DO 11 J=1,NBSENS
          JP    = IP + 8 + J
          LABEL = IQ(JP)
          IF( LABEL  .EQ. 0) GO TO 11
          IF( SECTOR .NE. IBITS(LABEL,11,5)) GO TO 10
          NTRSEC = NTRSEC+1
          GO TO 10
   11   CONTINUE
   10 CONTINUE
  999 RETURN
      END
