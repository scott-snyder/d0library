      SUBROUTINE BLDWAL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Build the bank DWAL hanging from DGEH. This bank
C-                         contains the design values for all the passive
C-                         material and is mainly used in D0GEANT. 
C-
C-   Inputs  : 
C-   Outputs : 
C-
C-   Created  17-FEB-1988   Ghita Rahal-Callot
C-   Updated  16-DEC-1988   Ghita Rahal-Callot  : added 2 rings 
C-   Updated  26-MAR-1992   Qizhong Li-Demarteau  added ECAN, ECBL and ECDC 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZDWAL.LINK'
C
      INTEGER NUMVOL, NPARVL, I, LDWAL, NFORMA, K
      PARAMETER (NUMVOL=10, NPARVL = 7 )
      INTEGER IOFS(NPARVL-2, NUMVOL)
      CHARACTER*4 VOLNAM(NUMVOL), VOLMOT(NUMVOL)
      DATA VOLNAM / 'CDC ', 'DCWI', 'DCWO', 'DCCE', 'DRFT', 'DCDE',
     &  'DRNG','ECAN','ECBL','ECDC'/
      DATA VOLMOT / 'MCEN', 'CDC ', 'CDC ', 'CDC ', 'CDC ', 'DRFT',
     &  'CDC ','MCEN','MCEN','MCEN'/
      DATA IOFS / 9, 13,  0, 17, 80, 
     &            9, 10,  0, 16, 81,
     &           12, 13,  0, 16, 85,
     &            9, 13, 16, 17, 85,
     &           10, 11,  0, 15, 82,
     &           10, 11, 14, 15, 83,
     &           18, 12, 19, 16, 85,
     &           18, 13, 17, 20, 85,
     &           22, 18, 17, 20, 86, 
     &            9, 22, 17, 21, 87 / 
C----------------------------------------------------------------------
C
      CALL MZFORM ( 'DWAL', '2I / 1H 5I 1H', NFORMA)
      CALL MZBOOK ( IXSTP, LDWAL, LDGEH, -IZDWAL, 'DWAL', 0, 0,
     &              2 + NUMVOL*NPARVL, NFORMA, 0 )
C
C     Design values for CDC walls, i.e. all passive material.
C
      IC ( LDWAL + 1 ) = NUMVOL
      IC ( LDWAL + 2 ) = NPARVL
      K = LDWAL + 2
      DO 100 I = 1, NUMVOL 
      CALL UCTOH(VOLNAM(I), C(K+1), 4, 4)
      CALL UCOPY(IOFS(1,I), IC(K+2), 5)
      CALL UCTOH(VOLMOT(I), C(K+7), 4, 4)
      K = K + NPARVL
  100 CONTINUE
C
  999 RETURN
      END
