      SUBROUTINE BLVWAL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Builds the bank VWAL hanging from VGEH. This banks
C-                         contains the design values for all the passive
C-                         material and is mainly used in D0GEANT. 
C-
C-   Inputs  :  none
C-   Outputs :  none
C-
C-   Created  17-SEP-1988   Ghita Rahal-Callot
C-   Modified 21-JUN-1989   Tom Trippe - new geometry, see D0 note #808
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER NUMVOL, NPARVL, I, LVWAL, NFORMA
      PARAMETER (NUMVOL=11, NPARVL = 7 )
      INTEGER IOFS(NPARVL-2, NUMVOL)
      CHARACTER*4 VOLNAM(NUMVOL), VOLMOT(NUMVOL)
      DATA VOLNAM / 'VTX ', 'VWL0', 'VWL1', 'VWL2', 'VWL3', 
     +                      'VGV0', 'VGV1', 'VGV2',
     +                      'VEC0', 'VEC1', 'VEC2'/
      DATA VOLMOT / '    ', 10*'VTX '/
      DATA IOFS / 9, 10,  0, 23, 69, 
     +            9, 11,  0, 17, 60,
     +           12, 13,  0, 17, 61,
     +           14, 15,  0, 19, 62,
     +           16, 10,  0, 21, 63,
     +           11, 12,  0, 17, 67,
     +           13, 14,  0, 19, 67,
     +           15, 16,  0, 21, 67,
     +            9, 13, 17, 18, 64,
     +           13, 15, 19, 20, 65,
     +           15, 10, 21, 22, 66/
C----------------------------------------------------------------------
C
C  Book VWAL
      CALL BKVWAL(NUMVOL,LVWAL)
C
C  Fill for each volume
      LVWAL = LVWAL + 2
      DO 100 I = 1, NUMVOL 
      CALL UCTOH(VOLNAM(I), C(LVWAL+1), 4, 4 )
      CALL UCOPY(IOFS(1,I), IC ( LVWAL + 2 ) , 5 )
      CALL UCTOH(VOLMOT(I), C(LVWAL+7), 4, 4 )
      LVWAL = LVWAL + NPARVL
  100 CONTINUE
C
  999 RETURN
      END
