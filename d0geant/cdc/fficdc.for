      LOGICAL FUNCTION FFICDC()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Central Drift Chamber FFREAD cards defined here
C-
C-   Inputs  : NONE
C-   Outputs : NONE
C-
C-   Created   8-JUL-1987   A.M.Jonckheere
C-   Updated   2-MAR-1988   Ghita Rahal-Callot  : Add default values for
C-                                                SCDC. 
C-   Updated  23-JUN-1989   Qizhong Li-Demarteau  add default values for
C-                                                SCDC(4) and SCDC(5)
C-   Updated  17-JUL-1989   Harrison B. Prosper  
C-   Made into pbd interface function. CDC code moved from FFICEN. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C
      INTEGER I
      LOGICAL PRT_FFICDC
C----------------------------------------------------------------------
      FFICDC = .TRUE.
C
      DCDC = DCEN
      PCDC = 1
      IF ( DCDC .LE. 0 ) GOTO 999
C
      CALL FFKEY('DCDC',DCDC,1,'INTEGER')
      CALL FFKEY('PCDC',PCDC,1,'INTEGER')
C
      DO I = 1, 10
        SCDC(I) = -1.
      ENDDO
C
      CALL FFKEY('SCDC',SCDC,10,'REAL')
C
C ****  Initialize the working flags    SCDC(1) = Digitization ( CDD2 )
C ****                                  SCDC(2) = Hits         ( DSEC )
C ****                                  SCDC(3) = Data         ( DCDA )
C ****                                  SCDC(4) = Bilinear Conversion
C ****                                  SCDC(5) = Zero_suppression
C
C
      IF ( SCDC(1) .EQ. -1. .AND. SCDC(2) .EQ. -1.
     &                      .AND. SCDC(3) .EQ. -1.) THEN
C
C ****  Default = Digitization done
C ****            Hits         not done
C ****            Data         not done
C
        SCDC(1) = 1.
        SCDC(2) = 0.
        SCDC(3) = 0.
      ENDIF
C
C    Default = Bilinear conversion done
C              Zero-suppression    done
C
      IF ( SCDC(4) .EQ. -1) SCDC(4) = 1.
      IF ( SCDC(5) .EQ. -1) SCDC(5) = 0.
C
      ENTRY PRT_FFICDC
C
      PRT_FFICDC = .TRUE.
      WRITE (LOUT,9000) DCDC,PCDC,SCDC
 9000 FORMAT(
     & ' FFICDC ** DCDC ',I3,' PCDC ',I3,/
     & '           SCDC ',10F6.1)
C
  999 RETURN
      END
