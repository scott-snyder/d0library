      FUNCTION CAJETS_DROP_DST()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Drops banks from CAJETS package from DST
C-
C-   Returned value  : .TRUE.
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  26-FEB-1992   Norman A. Graf  written with CACL in mind
C-   Modified 28-MAR-1995   R. Astur : Add call to compress JPTS and
C-                                     then drop JPTS explicitly.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCAPH.LINK'
      INCLUDE 'D0$LINKS:IZCACL.LINK'
      INCLUDE 'D0$LINKS:IZJPTS.LINK'
      INCLUDE 'D0$LINKS:IZJETS.LINK'
      INTEGER GZPROC,LCAPH,LCACL, LPROC
      INTEGER LJETS, LJPTS, HNAME
      LOGICAL CAJETS_DROP_DST
C----------------------------------------------------------------------
      CAJETS_DROP_DST = .TRUE.
C
C: Add a compressed version of JTPS in case JTPS is dropped (as is normal)
C
      CALL CRUNCH_JPTS
C
C: Now drop JTPS bank along with CACL
C
      LPROC = GZPROC()
      LCAPH = 0
      IF ( LPROC .GT. 0 ) LCAPH = LQ( LPROC- IZCAPH )
      DO WHILE(LCAPH .GT. 0)
C
C: Drop CACL
C
        IF(IQ(LCAPH+4) .NE. 1) THEN          ! Not electrons
          LCACL = LQ(LCAPH-IZCACL)
          CALL MZDROP(IXCOM,LCACL,'L')    ! Drop ALL CACL banks here
        ENDIF
C
C: Drop JPTS
C
        LJETS = LQ( LCAPH - IZJETS )
        DO WHILE ( LJETS .GT. 0 )
          LJPTS = LQ(LJETS-IZJPTS)
          DO WHILE ( LJPTS .GT. 0 )
            CALL UCTOH('JPTS',HNAME,4,4)
            IF ( IQ(LJPTS-4) .EQ. HNAME) THEN
              CALL MZDROP(IXCOM,LJPTS,'L')
              LJPTS = 0
            ELSE
              LJPTS = LQ(LJPTS)
            ENDIF
          ENDDO
          LJETS = LQ(LJETS)
        ENDDO
        IF(LCAPH .NE. 0) LCAPH = LQ(LCAPH)
      ENDDO
  999 RETURN
      END
