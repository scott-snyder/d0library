      SUBROUTINE CL1RING(IETAL1,IPHIL1,NRING,NTOWER,LIST)
C----------------------------------------------------------------------
C-
C-   Purposes and Methods : Return list of trigger towers in ring of
C-                           size -NRING- around IETAL1,IPHIL1
C-
C-   Inputs  : NRING = ring radius; ILETAL1,IPHIL1 = Level1 indics
C-   Outputs : NTOWERS = # of towers LIST holds  info
C-             For each of the NTOWER towers, LIST hold 4 pieces of info:
C-              LIST(1,I) = Lv1 Ieta indice (-NETAL1:NETAL1 excluding 0)
C-              LIST(2,I) = lv1 Iphi indice (1,NPHIL1)
C-              LIST(3,I) = relative ieta from this tower to IETAL1
C-              LIST(4,I) = relative iphi from this tower to IPHIL1
C-   Controls:
C-
C-   Created   2-MAR-1990   Dale A. Ross (MSU)
C-   Revised  11-JAN-1991   Richard Astur (MSU)
C-   Modified 10-JUN-1991 RA: Protect against returning out of eta bounds
C-                             cells.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$INC:L2J_UTIL.INC'
      INTEGER NRING,NTOWER,LIST(4,100)
      INTEGER IETAL1,IPHIL1,IE,IP,ISTEP
      INTEGER IETALL,IPHILL
      INTEGER IEMAX,IEMIN
      LOGICAL FIRST,L2J_UTIL_INIT,OK
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C---We depend on using the ieta,iphi lookup arrays in L2J_UTIL common.
C---Make sure it has been initialized.
C
      IF (FIRST) THEN
        OK = L2J_UTIL_INIT()
        FIRST = .FALSE.
      END IF
C
C---We translate ieta to the odd integers for convenience and so that we
C---can use the L2J_XXX arrays.
C
      IPHILL = IPHIL1
      IETALL = 2*IETAL1 - SIGN(1,IETAL1)
      IEMIN  = IETALL - 2*NRING
      IEMAX  = IETALL + 2*NRING
C
C---Cycle around the ring of size NRING
C
      NTOWER = 0
      DO IE = IEMIN,IEMAX,2
        ISTEP = 2*NRING
        IF (IE .EQ. IEMAX .OR. IE .EQ. IEMIN) ISTEP = 1
        DO IP = IPHIL1 - NRING, IPHIL1 + NRING,ISTEP
          IF (L1J_ETA(IE) .NE. 0) THEN
            NTOWER = NTOWER + 1
            LIST(1,NTOWER) = L1J_ETA(IE)          ! IETA indice
            LIST(2,NTOWER) = L1J_PHI(IP)          ! IPHI indice
            LIST(3,NTOWER) = (IE - IETALL)/2
            LIST(4,NTOWER) =  IP - IPHILL
          END IF
        END DO
      END DO

  999 RETURN
      END
