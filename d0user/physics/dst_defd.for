      FUNCTION DST_DEFD()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     define dump for DST
C-
C-   Returned value  : true
C-
C-   Created  30-JUL-1990   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL DST_DEFD
      INTEGER NDFL
      PARAMETER (NDFL=7)
      CHARACTER*4 DUMPS(NDFL)
      INTEGER I
      CHARACTER*1 TYPAR(NDFL)
      INTEGER LIMITS(2,NDFL)
      LOGICAL DFLAGS(NDFL)
      DATA DUMPS/'PARH','JETS','PELC','PPHO','PMUO','PNUT','VERT'/
C----------------------------------------------------------------------
C
      DST_DEFD=.TRUE.
      CALL ISA_DEFD
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
   11 CONTINUE
C
C           give user option to change which banks to dump
      CALL GETARR(NDFL,DUMPS,TYPAR,LIMITS,DFLAGS)
      DO 12 I=1,NDFL
C            Tell the DUMP facility the new flag values
        CALL DMPBNK(DUMPS(I),DFLAGS(I))  
   12 CONTINUE
  999 RETURN
      END
