      SUBROUTINE GET_REVISION_DATE (file_name, date)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-    
C-   Return revision date of specified file. This is just the FILE_DATE
C-   function in the SOURCES utility.
C-
C-   Inputs  : file_name        [C*]
C-   Outputs : date             [C*23]
C-   Controls: None
C-
C-   Created  22-JUN-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER file_name(2)
      CHARACTER*(*) date

      INCLUDE '($fabdef)'
      INCLUDE '($rabdef)'
      INCLUDE '($xabdef)'
      INCLUDE '($xabdatdef)'
      RECORD /fabdef/ fab
      RECORD /rabdef/ rab
      RECORD /xabdef/ xab

      INTEGER length
      INTEGER status
      INTEGER sys$open
      INTEGER sys$close
      INTEGER sys$asctim
C----------------------------------------------------------------------
      date      = ' '

      fab.fab$b_bid = fab$c_bid
      fab.fab$b_bln = fab$c_bln

      fab.fab$l_fna = file_name(2)
      fab.fab$b_fns = file_name(1) .AND. 255

      xab.xab$b_cod = xab$c_dat
      xab.xab$b_bln = xab$c_datlen
      fab.fab$l_xab = %loc (xab)

      status = sys$open (fab)
      
      IF ( .not. status ) THEN
        call lib$signal (%VAL(status))
      ELSE
        CALL sys$asctim (length, date, xab.xab$q_rdt,)
        status = sys$close (fab)
        IF ( .not. status ) call lib$signal (%VAL(status))
      ENDIF
      RETURN
      END
