C----------------------------------------------------------------------
C-
C-   Created  16-MAY-1988   D0DAQ
C-
C----------------------------------------------------------------------
C
C     Common block containing info for ZBDUMP, host side
C
      CHARACTER CURNOD*6,OUTRAD*1
      LOGICAL DUMP_ON,GOFLAG,INTERRUPT_FLG,DISPLAY_IT
      INTEGER MICHAN,WSTART,WEND
      CHARACTER*4 BANKNAM
      COMMON/ZBDUMP_COM/CURNOD,OUTRAD,DUMP_ON,GOFLAG,MICHAN,
     &  BANKNAM,WSTART,WEND,INTERRUPT_FLG,DISPLAY_IT
