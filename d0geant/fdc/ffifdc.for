      LOGICAL FUNCTION FFIFDC()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Forward Drift Chamber FFREAD cards defined here
C-
C-   Inputs  : NONE
C-   Outputs : NONE
C-
C-   Created   8-JUL-1987   A.M.Jonckheere
C-   Updated   5-OCT-1988   Jeffrey Bantly  for use with the FDC 
C-   Updated  17-JUL-1989   Harrison B. Prosper  
C-   Made into pbd interface function; FDC code moved from FFICEN. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C
      INTEGER I
      LOGICAL PRT_FFIFDC
C----------------------------------------------------------------------
      FFIFDC = .TRUE.
C                 
      DFDC = DCEN
      PFDC = 1
      IF ( DFDC .LE. 0 ) GOTO 999
C
      CALL FFKEY_i1('DFDC',DFDC,1,'INTEGER')
      CALL FFKEY_i1('PFDC',PFDC,1,'INTEGER')
C
      DO I = 1, 10
        SFDC(I) = -1.
      ENDDO
C
      CALL FFKEY('SFDC',SFDC,10,'REAL')
C
C ****  Initialize the working flags    SFDC(1) = Digitization ( CDD3 )
C ****                                  SFDC(2) = Hits         ( FTSC,FPSC )
C ****                                  SFDC(3) = Data         ( FTDA,FPDA )
C ****                                  SFDC(4) = Bilinear resp (yes if = 1.)
C ****                                  SFDC(5) = Zero-suppress (no if >= 1.)
C
C
      IF ( SFDC(1) .EQ. -1. .AND. SFDC(2) .EQ. -1.
     &                      .AND. SFDC(3) .EQ. -1.) THEN
C
C ****  Default = Digitization  done
C ****            Hits          not done
C ****            Data          not done
C ****            Bilinear resp done
C ****            Zero-suppres  done
C
        SFDC(1) = 1.
        SFDC(2) = 0.
        SFDC(3) = 0.
        SFDC(4) = 1.
        SFDC(5) = 0.
      ENDIF
C
      ENTRY PRT_FFIFDC
C
      PRT_FFIFDC = .TRUE.
      WRITE (LOUT,9000) DFDC,PFDC,SFDC
 9000 FORMAT(
     & ' FFIFDC ** DFDC ',I3,' PFDC ',I3/
     & '           SFDC ',10F6.1)
C
  999 RETURN
      END
