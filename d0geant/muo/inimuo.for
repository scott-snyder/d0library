      LOGICAL FUNCTION INIMUO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initializations for the MUON detector
C-
C-   Inputs  :
C-   Outputs :
C-
C-   Created  xx-AUG-1986   S.Kunori
C-   Updated  23-MAY-1988   Ghita Rahal-Callot : Read the geometry banks
C-                          on MUO_STPFILE as done in the Central detector
C-   Updated   6-OCT-1988   A.M.Jonckheere  Add DMUO switch
C-   Updated   2-JUN-1989   Harrison B. Prosper
C-   Made into a program-builder interface function
C-   Updated   2-MAY-1991   A.KL ADD FIELD MAP INITIALIZATION   
C-   Updated   8-MAY-1991   S.Igarashi  Check the version number(SMUO(2))
C-   Updated   5-OCT-1992   S.Igarashi  Set flag for geometry routines.
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCBANK.INC/LIST'
      INCLUDE 'D0$INC:GCLINK.INC/LIST'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      LOGICAL OK
      INTEGER IERR
      INTEGER LGSWT,GZGSWT
      REAL    VERNUM_GSAVE,VERNUM_CURR
      LOGICAL MUGEANT_FLG,FLAG
C----------------------------------------------------------------------
      INIMUO = .TRUE.
      IF ( DMUO .LE. 0 ) GOTO 999
C
C ****  Read data for MUON Geometry on MUO_STPFILE and copy it in /ZEBSTP/
C
      IF ( DMUO.GE.1 ) THEN
        CALL MUISTP ( 'MUO_STPFILE', IERR )
        IF (IERR .NE. 0 ) THEN
          WRITE(LOUT,*)' ****INIMUO: Error initalization geometry'
          CALL EXIT(1)
        ENDIF
C       IF(DMUO.GE.3 .AND. SMUO(3).NE.0) 
C    &   CALL MRZCON('MMAP','MUON_BFIELD',0,OK) !field map
      ENDIF
C
C ****  Check version number (SMUO(2)) in GSAVE
C
      LGSWT=GZGSWT()
      IF(LGSWT.NE.0) THEN
        VERNUM_GSAVE=Q(LGSWT+97+2)
        VERNUM_CURR =SMUO(2)
        IF(VERNUM_GSAVE.NE.VERNUM_CURR) THEN
          CALL ERRMSG('Inconsistent version number in GSAVE',
     &         'INIMUO',' ','F')
        ENDIF
      ENDIF
C
C ****  Set flag for geometry routines of MUON_UTIL
C
      FLAG=MUGEANT_FLG(1)
C
  999 RETURN
      END
