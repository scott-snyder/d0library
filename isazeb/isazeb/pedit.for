      FUNCTION PEDIT(ID,P)
C======================================================================
C
C   Purpose and Methods :
C   select particles to be included in Zebra output
C   called for every particle. If PEDIT=.FALSE. particle will
C   be left out
C
C   Inputs  : 
C   ID  = particle id
C   P(4)= particle momenta, px,py,pz and E
C
C   Outputs : NONE
C
C   Created  23-MAY-1987   Serban D. Protopopescu
C
C======================================================================
      IMPLICIT NONE
      LOGICAL PEDIT
      INTEGER ID
      REAL P(4)
C======================================================================
      PEDIT=.TRUE.
      IF(P(4).LE.0.0) PEDIT=.FALSE.
  999 RETURN
      END
