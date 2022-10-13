      SUBROUTINE MU_SET_QUAL_MASK(ICONT,IM_ARRAY,IMASK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : set up muon quality mask for CLEANMU
C-
C-   Inputs  : ICONT (I) : control word   0 = build IMASK from IM_ARRAY
C-                                        1 = build IMASK from CLEANMU_RCP
C-              IM_ARRAY (A) : 32 word array for user-supplied mask bits
C-                             =1 for cut, =0 for don't cut 
C-   Outputs : IMASK (I) : mask for use  with CLEANMU and CHECK_MU_QUALITY
C-   Controls: 
C-
C-   Created  29-SEP-1993   Darien R. Wood
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ICONT,IM_ARRAY(32),IMASK
      INTEGER I,N,IRCP_ARRAY(32),IER
      LOGICAL FIRST_RCP
      DATA FIRST_RCP/.TRUE./
C----------------------------------------------------------------------
C build mask from user array
      IF(ICONT.EQ.0) THEN
        IMASK=0
        DO I = 1,32
          IF(IM_ARRAY(I).EQ.1) THEN
            IMASK=IBSET(IMASK,I-1)
          ENDIF
        ENDDO
      ENDIF
      IF(ICONT.EQ.1) THEN
C on first RCP called, but mask array from CLEANMU_RCP
        IF(FIRST_RCP) THEN
          FIRST_RCP = .FALSE.
          IER = 0
          CALL INRCP('CLEANMU_RCP',IER)
          IF(IER.NE.0)  CALL ERRMSG('CLEANMU_RCP not found',
     &      'MU_SET_QUAL_MASK',' ','W')
          CALL EZPICK('CLEANMU_RCP')
          CALL ERRMSG('Reading CLEANMU_RCP',
     &      'MU_SET_QUAL_MASK',' ','W')
          CALL EZGETA_i('MUON_MASK',0,0,0,N,IER)
          IF(IER.EQ.0) CALL EZGETA('MUON_MASK',1,N,1,IRCP_ARRAY,IER)
          IF(N.NE.32) CALL ERRMSG('Error reading Muon Mask Array',
     &    'MU_SET_QUAL_MASK',' ','F')
          CALL EZRSET
        ENDIF  
C compute mask
        IMASK=0
        DO I = 1,32
          IF(IRCP_ARRAY(I).EQ.1) THEN
            IMASK=IBSET(IMASK,I-1)
          ENDIF
        ENDDO
      ENDIF  
      
  999 RETURN
      END
