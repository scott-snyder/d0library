      LOGICAL FUNCTION COMPUTE_MU_QUALITY()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : get muon quality and
C-                         update PMUO banks.
C-
C-   Inputs  : none
C-   Outputs : none (sets bits in PMUO status word)
C-   Controls: none
C-
C-   Created  28-SEP-1993   Darien Wood
C-   Modified 12-jan-1994   DW fix bug in cleanmu arguements
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER STATUS,LPMUO,IVER_PMUO
      INTEGER GZPMUO
      LOGICAL OK
      EXTERNAL GZPMUO
C----------------------------------------------------------------------
      COMPUTE_MU_QUALITY = .TRUE.
C
C ****  get muon quality
C
      LPMUO = GZPMUO(0)
      DO WHILE (LPMUO.NE.0)
        CALL CLEANMU(LPMUO,STATUS,OK)
        IVER_PMUO=IQ(LPMUO+1)
        IF(IVER_PMUO.GE.3) THEN
C update status word in PMUO (IQ(LPMUO+42) V10, IQ(LPMUO+45) V11)
          IQ(LPMUO+45) = STATUS
        ELSE
          IQ(LPMUO+42) = STATUS
        ENDIF  
        LPMUO = LQ(LPMUO)
      END DO
C
  999 RETURN
      END
