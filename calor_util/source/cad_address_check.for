      SUBROUTINE CAD_ADDRESS_CHECK(IWORD,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CHECKS CAD BANK DATA WORD FOR ERRORS
C-                 
C-
C-   Inputs  : IWORD    [I]   CAD BANK DATA WORD TO CHECK
C-   Outputs : IER      [I].  0 = OK
C-                           -1 = ERROR but CONTINUE with DATA
C-                        <= -2 = ERROR - SKIP THIS EVENT 
C-                           -3 = ADDR > 6144
C-                           -4 = DEPTH > 11
C-                           -5 = ADC OVERFLOW 
C-   Controls: NONE
C-
C-   Created  30-JAN-1991   Chip Stewart
C-   Updated  17-Mar-1882   Herbert
C-      Got rid of machine blocks
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IWORD,IER,ADDR,ENERGY,SCALE,DEPTH
      CHARACTER*40 MSG
C----------------------------------------------------------------------
      IER = 0
      ADDR  = IBITS(IWORD,18,14) 
      SCALE = IBITS(IWORD,17,1) 
      ENERGY= IAND(IWORD,65535)      
      IF (ADDR.GT. 6144) THEN
        WRITE(MSG,1002) IWORD
        CALL ERRMSG('BAD CAD ADDR','CAD_ADDRESS_CHECK',MSG,'W')
        IER = -3
        GOTO  999
      END IF
      DEPTH = IAND(ADDR, 15)
      IF ( DEPTH.GT.11) THEN
        WRITE(MSG,1002) IWORD 
        CALL ERRMSG('BAD CAD ADDR','CAD_ADDRESS_CHECK',MSG,'W')
        IER = -4
        GOTO 999
      END IF
      IF ( (ENERGY.EQ.32760 .AND. SCALE.EQ.1 )    !32760= 2FF8 HEX
     &  .OR. (ENERGY.EQ.4095 .AND. SCALE.EQ.0 ) ) THEN   !4095 = FFF HEX
        WRITE(MSG,1002)IWORD
        CALL ERRMSG('CAD-OVERFLOW','CAD_ADDRESS_CHECK',MSG,'W')
        IER = -5
        GOTO 999
      END IF
  999 RETURN
 1002 FORMAT('  CAD WORD  =',Z9.8)
      END
