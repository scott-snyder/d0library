      FUNCTION CL2HITS()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Call subroutines to generate level 2 hit banks CAEP, PNUT and CAEH
C-              results appear under FILT path
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : CAEP PNUT and CAEH on filter path
C-   Controls: CAHITS.RCP       (lightly)
C-
C-   Created  11-MAY-1991   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL CL2HITS
      INTEGER IER
C      INTEGER GZPNUT,GZPARH
      CHARACTER*4 OLD_PATH
      LOGICAL OK,CAEP_ONLY
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      CALL PATHGT(OLD_PATH)
      IF (FIRST) THEN
        CALL EZPICK('CAHITS_RCP')
        CALL EZERR(IER)
        CAEP_ONLY = .FALSE.
        IF ( IER.EQ.0) THEN
          CALL EZGET('CAEP_ONLY',CAEP_ONLY,IER)
          CALL EZRSET
        END IF
        FIRST = .FALSE.
      ENDIF
      CL2HITS = .FALSE.
      CALL CL2_VZERO_PTCAEP2
      CALL PATHST('FILT')
      CALL MKPATH
      CALL CL2_CAEPFL(OK)               ! does CAEP and PNUT1
C
C...at present does only PTCAEP2 and CAEP, PNUT (partial)
      IF( .NOT. CAEP_ONLY) THEN
C        CALL CAEHFL        ! fill CAEH
C        CALL CPTCAF        ! fill PTCAEP pointer array
C        IF(GZPARH().EQ.0) CALL PARHFL  ! fill PARH only if needed
C        CALL C1PMET    ! do missing Et (first pass)
C        CALL C2PMET    ! do EC/CC corrections and missing Et 2nd pass
C        CALL CPTCTZ        ! zero PTCATE pointer array
C        CALL CATEFL        ! fill CATE
C        CALL CPTCTF        ! fill PTCATE pointer array
      ENDIF
      CL2HITS = OK
C----------------------------------------------------------------------
  999 CONTINUE
      CALL PATHST(OLD_PATH)
      RETURN
      END
