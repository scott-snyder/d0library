      LOGICAL FUNCTION FFIVTX()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Vertex detector FFREAD cards defined here
C-
C-   Inputs  : NONE
C-   Outputs : SVTX
C-
C-   Created   8-JUL-1987   A.M.Jonckkhere
C-   Updated  22-JUN-1989   P.Grudberg - bilin resp and zero supp switches
C-   Updated  17-JUL-1989   Harrison B. Prosper   
C-   Made into a pbd interface function. VTX code moved from FFICEN to
C-   here.
C-   Updated   09-SEP-1992   Alexandre Zinchenko - add default SVTX(6)=1
C-                           for "realistic" pulse shapes
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C
      INTEGER I
      LOGICAL PRT_FFIVTX
C----------------------------------------------------------------------      
      FFIVTX = .TRUE.
C
      DVTX = DCEN
      PVTX = 1
      IF ( DVTX .LE. 0 ) GOTO 999
C
      CALL FFKEY_i1('DVTX',DVTX,1,'INTEGER')
      CALL FFKEY_i1('PVTX',PVTX,1,'INTEGER')
C
      DO I = 1, 10
        SVTX(I) = -1.
      ENDDO
C
      CALL FFKEY('SVTX',SVTX,10,'REAL')
C
C ****  Initialize the working flags    SVTX(1) = Digitization ( CDD1 )
C ****                                  SVTX(2) = Hits         ( VTXH )
C ****                                  SVTX(3) = Data         ( VWDA )
C ****                                  SVTX(4) = Bilinear response switch
C ****                                  SVTX(5) = Zero suppression switch
C ****                                  SVTX(6) = Realistic pulse shapes
C
      IF ( SVTX(1) .EQ. -1. .AND. SVTX(2) .EQ. -1.
     &     .AND. SVTX(3) .EQ. -1. .AND. SVTX(4) .EQ. -1.
     &     .AND. SVTX(5) .EQ. -1. .AND. SVTX(6) .EQ. -1) THEN
C
C ****  Default = Digitization done
C ****            Hits         not done
C ****            Data         not done
C ****            Bilinear response done
C ****            Zero suppression done
C ****            Realistic pulse shape on
C
        SVTX(1) = 1.
        SVTX(2) = 0.
        SVTX(3) = 0.
        SVTX(4) = 1.
        SVTX(5) = 0.
        SVTX(6) = 1.
      ENDIF
C
      ENTRY PRT_FFIVTX
C
      PRT_FFIVTX = .TRUE.
      WRITE (LOUT,9000) DVTX,PVTX,SVTX
 9000 FORMAT(
     & ' FFIVTX ** DVTX ',I3,' PVTX ',I3/
     & '           SVTX ',10F6.1)
C
  999 RETURN
      END
