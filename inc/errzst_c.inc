C----------------------------------------------------------------------
C-  Character part of error message buffering; see ERRZST.INC
C-   Created  16-APR-1992   James T. Linnemann
C-
C----------------------------------------------------------------------
      INTEGER MXCHR_ERMG,MXWDS_ERMG
      PARAMETER( MXCHR_ERMG = 200 )
      PARAMETER( MXWDS_ERMG = MXCHR_ERMG/4 )
      CHARACTER*(MXCHR_ERMG) IDWAIT(WAIT_SIZ)
      COMMON /ERRZST_C/ IDWAIT

