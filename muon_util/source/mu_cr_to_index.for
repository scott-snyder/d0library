      SUBROUTINE MU_CR_TO_INDEX(MU_CRATE,I_CRATE,NMOD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : give the muon module number and crate number
C-                         given the crate and module index

C-
C-   Inputs  : MU_CRATE = muon crate number (e.g. 162)
C-   Outputs : I_CRATE = crate index (1=062,2=072,...)
C-   Controls: 
C-
C-   Created   8-APR-1992   Darien Wood
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I_CRATE,MU_CRATE,I_C,NMOD  
      INTEGER ICRATE_ARRAY(18),NMOD_ARRAY(18)
      DATA ICRATE_ARRAY/062,072,082,152,162,172,022,052,112,142,
     &                  092,102,122,132,002,012,032,042/
      DATA NMOD_ARRAY/    9, 11,  9,  9, 11,  9,  8, 10,  8, 10,
     &                   11,  9, 10,  5, 11,  9, 10,  5/
C
      I_CRATE = 0
      NMOD = 0
      DO I_C=1,18
        IF(ICRATE_ARRAY(I_C).EQ.MU_CRATE) THEN 
          I_CRATE = I_C
          NMOD = NMOD_ARRAY(I_C)
        ENDIF  
      ENDDO  
C
C----------------------------------------------------------------------
  999 RETURN
      END
