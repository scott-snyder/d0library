      SUBROUTINE NTUPLE_END
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : End the program. Write out the last buffer of
C-                          all remaining Ntuples, and Close all
C-                         remaining Direct Access files.
C-
C-   Inputs  :None
C-   Outputs :None
C-   Controls: none
C-
C-   Created  19-MAY-1991   B.S.Acharya
C-   Updated   7-NOV-1991   Harrison B. Prosper
C-    Add close; remove hdelet(0)
C-   Updated   3-APR-1992   Harrison B. Prosper  
C-      Remove check on counter 
C-   Updated  17-JUN-1992   Harrison B. Prosper  
C-      Use NTUPLE_CLOSE 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER STATUS
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:NT_HOUSEKEEP.INC'
C----------------------------------------------------------------------
C
C- Fish out all directories associated with files which are not yet closed
C
      DO WHILE ( NFILES .GT. 0 )
        CALL NTUPLE_CLOSE(TOPDIR_LIST(1),STATUS)
      ENDDO
C
  999 RETURN
      END
