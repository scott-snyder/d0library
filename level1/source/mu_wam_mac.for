      SUBROUTINE MU_WAM_MAC(MODNO,MODID,MACHIT,MCRS,MFINE,NFINE)
C-------------------------------------------------------------------
C  Create fine and coarse centroids for one wide angle module
C  Created 6-90  M. Fortner
C  zero out Centroid arrays at initialization     9-27-91, K. Bazizi
C  add fine centroid lists 11-91  M. Fortner
C  change coarse centroid output granularity      5-12-92, K. Bazizi
C  fix PAL boundry problem for A layer centroids  5-18-92, K. Bazizi
C  Add fine centroid truncation                   9-10-92, K. Bazizi
C  Add RCP parameter WAM_MAC_CENT_CUT            10-18-92, K. Bazizi
C  Add different logic for EF A-layer centroids  11-19-92, G. Lima
C_ Add RCP switch for trigger version number 2-2-93, K. Bazizi
C
C  MODNO is Phil Martin number of module
C  MODID is the ID value for the fine centroid address
C  MACHIT is array of pad latch hits in chamber using columns 2-25
C  MCRS is the coarse centroids array after CCT or by four
C  MFINE is the list of fine centroids, and NFINE is the count
C
C-------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER MODNO,MODID,MACHIT(26,4),MFINE(32),NFINE,MCRS(16)
      INTEGER I,J,IB,MCENT(48),ICENT(2),IDFINE
      INTEGER WAM_CENT_CUT,IER,IVERS
      INTEGER SUMI,SUMJ,SUMK,SUML
      CHARACTER*72 STRING
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C<<
      DATA IVERS/2/
C
C-- get fine centroid cut for MAC
      IF (FIRST) THEN
        FIRST=.FALSE.
C<<
        CALL EZPICK('MUSIM_RCP')
        CALL EZERR(IER)     ! Check if error
        IF(IER.NE.0) THEN
          CALL EZGET_ERROR_TEXT(IER,STRING)
          CALL ERRMSG(' EZPICK ERR','MU_WAM_MAC',STRING,'F')
          GOTO 999
        ENDIF
C
        CALL EZGET('WAM_MAC_CENT_CUT',WAM_CENT_CUT,IER)
        IF(IER.NE.0) THEN
          CALL EZGET_ERROR_TEXT(IER,STRING)
          CALL ERRMSG(' WAM_MAC_CENT_CUT','MU_WAM_MAC',STRING,'F')
          GOTO 999
        ENDIF
C
        CALL EZGET('WAM_MAC_VERS',IVERS,IER)
        IF(IER.NE.0) THEN
          CALL EZGET_ERROR_TEXT(IER,STRING)
          CALL ERRMSG(' WAM_MAC_VERS','MU_WAM_MAC',STRING,'F')
          GOTO 999
        ENDIF
C
        CALL EZRSET()
      ENDIF
C
C              Initialize Fine and Coarse Centroid arrays
      DO I=1,48
        MCENT(I)=0
      ENDDO
      DO I=1,16
        MCRS(I)=0
      ENDDO
      NFINE=0
C<<
C.. Get raw centroid positions
      DO I=1,24
        J=I*2-1
        IF( IVERS.EQ.1 ) THEN
          IF (MODNO.LT.100) CALL MU_WAM_CEN_CFA(MACHIT,I,ICENT)
        ENDIF
        IF( IVERS.EQ.2 ) THEN
          IF (MODNO.LE.37) CALL MU_WAM_CEN_CFA(MACHIT,I,ICENT)
          IF (MODNO.GE.61.AND.MODNO.LE.97) CALL MU_WAM_CEN_EFA(MACHIT,
     &      I,ICENT)
        ENDIF
        IF (MODNO.GE.100.AND.MODNO.LT.200) CALL MU_WAM_CEN_B(MACHIT,I,
     &      ICENT)
        IF (MODNO.GE.200.AND.MODNO.LT.400) CALL MU_WAM_CEN_C(MACHIT,I,
     &      ICENT)
        MCENT(J)=ICENT(1)
        MCENT(J+1)=ICENT(2)
      ENDDO
C
C               Correct centroids for adjacent hits
      IF (MCENT(2).NE.0) MCENT(1)=0
      DO I=2,24
        J=I*2-1
        IB=MOD(J,6)-1
        IF(MODNO.LT.100.AND.IB.EQ.0) THEN
          IF (MCENT(J+1).NE.0) MCENT(J)=0
        ELSE
          IF (MCENT(J-1).NE.0.OR.MCENT(J+1).NE.0) MCENT(J)=0
        ENDIF
      ENDDO
C
C               Get coarse centroids and fill fine list
      IDFINE=ABS(MODID)*2048
      DO I=1,16
        DO J=I*3-2,I*3
          IF (MCENT(J).NE.0) THEN
            IF (MODID.GE.0) THEN
              MCRS(I)=1
            ELSE
              MCRS(17-I)=1
            END IF
            IF (NFINE.LT.WAM_CENT_CUT) THEN
              NFINE=NFINE+1
              MFINE(NFINE)=J+IDFINE
            END IF
          END IF
        ENDDO
      ENDDO
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
