      SUBROUTINE ZTRTMP(ZVTX,PHIMIN,PHIMAX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : build ZTMP banks for TRD to analyze electrons
C-
C-   Inputs  : 
C      ZVTX     = vertex position in Z
C-     PHIMIN   = minimum phi
C-     PHIMAX   = maximum phi
C-   Outputs : ZTMP and ZFIT banks are built
C-
C-   Created  19-NOV-1990   Qizhong Li-Demarteau
C-   Updated  10-JUL-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-   Updated   5-SEP-1991   Qizhong Li-Demarteau   use bits definition from
C-                                                params file instead of RCP 
C-   Updated  01-FEB-1992   Qizhong Li-Demarteau  added part of FDC tracks 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INCLUDE 'D0$INC:ZELCLK.INC/LIST'                             
      INCLUDE 'D0$LINKS:IZZTMP.LINK'                             
C
      INTEGER LVERH, LVERT, GZVERH, GZZTRH, LZTRH
      INTEGER NZ1, NZ2, NZ, IZ, IER
      REAL    PHIMIN, PHIMAX, ZVTX
      REAL    FITVTX(3), ERRVTX(3)
      LOGICAL MKZFIT, CDONLY, ZTMPVX
      LOGICAL EZERROR
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('ZTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','ZTRTMP',
     &    'Unable to find bank ZTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('MKZFIT',MKZFIT,IER)
        CALL EZGET('ZTMPVX',ZTMPVX,IER)
        CALL EZRSET
      END IF
C
      LZTRH = GZZTRH()
      LZTMP(1) = LQ(LZTRH - IZZTMP)
      IF (LZTMP(1) .NE. 0) CALL MZDROP(IXCOM,LZTMP(1),'L')
C
      CALL ZTMPCV(PHIMIN,PHIMAX,NZ1)
      CALL ZTMPFV(PHIMIN,PHIMAX,NZ1,NZ2)
C
C   make global fitting for each ZTMP 
C      
  101 IF (MKZFIT) THEN
        CALL ZTRKVT(ZVTX,FITVTX,ERRVTX)
        IF (.NOT.ZTMPVX) THEN
          ERRVTX(1) = 9999.0
        ENDIF
        NZ = NZ1 + NZ2
        DO 201 IZ = 1, NZ
          CALL ZTRKFT(LZTMP(IZ),FITVTX,ERRVTX)
  201   CONTINUE
      ENDIF
C        
  999 RETURN
      END
