      LOGICAL FUNCTION BPART(IP,IQ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : RETURNS TRUE IF ISAJET PARTICLE CONTAINS B QUARK
C-
C-   Inputs  :IP = ISAJET PARTICLE ID
C-   IQ = QUARK ID.
C-   Outputs :
C-   Controls:
C-
C-   Created  21-DEC-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IP,IPA,IP1,IPR,IQ,IQA
C----------------------------------------------------------------------
      BPART = .FALSE.
      IF ( IP*IQ.LT.0 ) THEN
        RETURN
C QUARK AND PARTICLE OF OPPOSITE SIGNS
      ENDIF
C
      IPA = IABS(IP)
      IQA = IABS(IQ)
C
      DO WHILE (IPA.GT.0)
        IP1 = IPA/10
        IPR = IPA -10*IP1
C
        IF ( IPR.EQ.IQA) THEN
          BPART =.TRUE.
        ENDIF
        IPA = IP1
      ENDDO
C
  999 RETURN
      END
