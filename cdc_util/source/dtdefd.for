      FUNCTION DTDEFD()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CDC Interface to Define Dump
C-
C-   Returned value  : .TRUE.
C-   Inputs  : 
C-   Outputs : 
C-
C-   Created  28-JUN-1989   Qizhong Li-Demarteau
C-   Updated  29-MAY-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL DTDEFD
      INTEGER NDFLMX
      PARAMETER (NDFLMX=20)
      CHARACTER*4 DUMPS(NDFLMX)
      INTEGER I,NDFL,BANKS(NDFLMX),IER
      CHARACTER*16 DFLGNM
      CHARACTER*1 TYPAR(NDFLMX)
      INTEGER LIMITS(2,NDFLMX)
      LOGICAL DFLAGS(NDFLMX)
      LOGICAL FLGVAL,FLGCHK,FIRST
      LOGICAL EZERROR
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
        FIRST=.FALSE.
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('DTRAKS','DTDEFD',
     &    'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET_i('NUM_DUMPS',NDFL,IER)   ! get number of banks
        CALL EZGET_iarr('DUMP_BANKS',BANKS,IER) ! get list of banks
        CALL EZRSET
C
        IF(NDFL.GT.NDFLMX) 
     &  CALL ERRMSG(' Cannot redefine dumps','DTDEFD',
     &  ' Too many banks for internal dimension','W')   
C
        DO 11 I=1,NDFL
          CALL DHTOC(4,BANKS(I),DUMPS(I))
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
C           give user option to change which banks to dump
      CALL GETDIS(NDFL,DUMPS,TYPAR,LIMITS,
     &  DFLAGS(1),DFLAGS(2),DFLAGS(3),DFLAGS(4),DFLAGS(5),DFLAGS(6),
     &  DFLAGS(7),DFLAGS(8))
      DO 12 I=1,NDFL
C            Tell the DUMP facility the new flag values
        CALL DMPBNK(DUMPS(I),DFLAGS(I))  
   12 CONTINUE
      DTDEFD=.TRUE.
C
  999 RETURN
      END
