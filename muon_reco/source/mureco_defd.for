      FUNCTION MURECO_DEFD()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      define dumps interactively (called by Define Dump option)
C-   Returned value  : true
C-
C-   Created   6-NOV-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL MURECO_DEFD
      INTEGER NDFLMX
      PARAMETER (NDFLMX=20)
      CHARACTER*4 DUMPS(NDFLMX)
      INTEGER I,NDFL,BANKS(NDFLMX),IER
      CHARACTER*16 DFLGNM
      CHARACTER*1 TYPAR(NDFLMX)
      INTEGER LIMITS(2,NDFLMX)
      INTEGER NCHECK
      PARAMETER (NCHECK=2)
      CHARACTER*4 CHECKS(NCHECK)
      LOGICAL DFLAGS(NDFLMX)
      LOGICAL FLGVAL,FLGCHK,FIRST,EZERR
      DATA CHECKS/'MUOH','MUOT'/
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
        FIRST=.FALSE.
        CALL EZPICK('MURECO_RCP')
C
        IF(EZERR(IER)) THEN       ! no file provided
          NDFL=NCHECK
          DO 9 I=1,NDFL
    9     DUMPS(I)=CHECKS(I)
C
        ELSE
          CALL EZGETA('DUMP_BANKS',0,0,0,NDFL,IER)   ! get number of banks
          CALL EZGET('DUMP_BANKS',BANKS,IER) ! get list of banks
C
          IF(NDFL.GT.NDFLMX) THEN
            CALL ERRMSG(' Cannot redefine dumps','MURECO_DEFD',
     &    ' Too many banks for internal dimension','W')   
            GOTO 999
          ENDIF
C
          DO 10 I=1,NDFL
   10     CALL DHTOC(4,BANKS(I),DUMPS(I))
        ENDIF
        CALL EZRSET
C
        DO 11 I=1,NDFL
          DFLAGS(I)=.TRUE.
C
C          Tell the DUMP facility the bank names and flag values
          CALL DMPBNK(DUMPS(I),DFLAGS(I))   
C          set arrays for GETDIS
          TYPAR(I)='L'
          LIMITS(1,I)=0
          LIMITS(2,I)=0
   11   CONTINUE
      ENDIF
C
      IF(NDFL.GT.NDFLMX.OR.NDFL.EQ.0) GOTO 999
C
C           give user option to change which banks to dump
      CALL GETARR(NDFL,DUMPS,TYPAR,LIMITS,DFLAGS)
      DO 12 I=1,NDFL
C            Tell the DUMP facility the new flag values
        CALL DMPBNK(DUMPS(I),DFLAGS(I))  
   12 CONTINUE
      MURECO_DEFD=.TRUE.
  999 RETURN
      END
