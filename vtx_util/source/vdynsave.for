      SUBROUTINE VDYNSAVE(DEVICE,VALUE,DBL3TIME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Interface routine for passing DYNAMIC ADJUSTMENT
C-               quantities between low level routines called by VTX_DYNADJ
C-               and others (e.g. SVTX_TO_VCAL)
C-
C-   Inputs  : DEVICE = 'LUM',       'PABS',                'TDEGC'
C-             VALUE  =  luminosity, absolute pressure abs,  temp
C-             DBL3TIME = packed time of reading
C-   Outputs : 
C-   Controls: 
C-
C-   Created  15-FEB-1994   Ed Oltman
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) DEVICE
      INTEGER  DBL3TIME,LUMTIME,ENVTIME
      REAL     VALUE,LUM,PABS,TDEGC
      REAL     LUM_SAVE,PABS_SAVE,TDEGC_SAVE
      INTEGER  LUMT_SAVE,ENVT_SAVE
      DATA     LUM_SAVE,PABS_SAVE,TDEGC_SAVE/9999.,9999.,99999./
      DATA     LUMT_SAVE,ENVT_SAVE/0,0/
C----------------------------------------------------------------------
      IF (DEVICE(1:3) .EQ. 'LUM') THEN
        LUM_SAVE = VALUE
        LUMT_SAVE = DBL3TIME
      ELSEIF(DEVICE(1:4) .EQ. 'PABS') THEN
        PABS_SAVE = VALUE
        ENVT_SAVE = DBL3TIME
      ELSEIF(DEVICE(1:5) .EQ. 'TDEGC') THEN
        TDEGC_SAVE = VALUE
        ENVT_SAVE = DBL3TIME
      ENDIF
      GO TO 999
      ENTRY VDYNGET(LUM,PABS,TDEGC,LUMTIME,ENVTIME)
      LUM   = LUM_SAVE
      PABS  = PABS_SAVE
      TDEGC = TDEGC_SAVE
      LUMTIME = LUMT_SAVE
      ENVTIME = ENVT_SAVE
  999 RETURN
      END
