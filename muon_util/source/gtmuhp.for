      SUBROUTINE GTMUHP(IHIT,NCEL,NLAT,LEVE,LODD,NEXT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Gets hit information from bank MUHP for hit IHIT
C-
C-   Inputs  :  IHIT - Module loop variable for which one would like hit
C-                     information
C-
C-   Outputs :  NCEL - Cell number (-1 for missing bank)
C-              NLAT - Latch bits
C-              LEVE - Location in MUD1 of even data
C-              LODD - Location in MUD1 of odd data
C-              NEXT - Location in MUHP of next hit for this module
C-
C-   Created :  26-AUG-93  M. Fortner
C-
C-   Modified:  1/95 M. Fortner - add missing bank protection
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER IHIT,NCEL,NLAT,LEVE,LODD,NEXT
      INTEGER LMUHP,GZMUHP,IMUHP,IMAX,N_MUHP
      PARAMETER (N_MUHP=5)
C
      NCEL = -1
      NEXT = 0
      LMUHP = GZMUHP(0)
      IF (LMUHP.EQ.0.OR.IHIT.LE.0) RETURN
      IMAX = IQ(LMUHP-1)/N_MUHP
      IF (IHIT.GT.IMAX) RETURN
      IMUHP = LMUHP + 5*(IHIT-1)
      NCEL = IQ(IMUHP+1)
      NLAT = IQ(IMUHP+2)
      LEVE = IQ(IMUHP+3)
      LODD = IQ(IMUHP+4)
      NEXT = IQ(IMUHP+5)
      RETURN
      END
