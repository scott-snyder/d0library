C----------------------------------------------------------------------
C-
C-   Created  10-OCT-1989   Andrew P. White
C-
C----------------------------------------------------------------------
C
C    Pointers to corrections bank CAEC
C
      COMMON /PTCAEC/ PTCZFL,PTCAEC1,PTCAEC2
C
      LOGICAL PTCZFL 
                     
C
C Parameters MNETAMG,MXETAMG,NPHIL,MNLYMG,MXLYMG are in CAL_OFFLINE.PARAMS
C
      INTEGER PTCAEC1(-MXETAMG:-MNETAMG,NPHIL,MNLYMG:MXLYMG)
      INTEGER PTCAEC2( MNETAMG: MXETAMG,NPHIL,MNLYMG:MXLYMG)
C
