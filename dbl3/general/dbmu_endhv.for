C----------------------------------------------------------------------
      SUBROUTINE DBMU_ENDHV (LOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To close HV dbl3 file or to hide file list ID
C-
C-   Inputs  : 
C-   Outputs : LOK     .true.   ok
C-                     .false.  something went wrong
C-   Controls: 
C-
C-   Created  18-OCT-1992   Lars Rasmussen
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL DBMU_FILE_CLOSE
C
      LOGICAL LOK
C----------------------------------------------------------------------
      LOK = DBMU_FILE_CLOSE ('HVM',' ',0,' ')
  999 RETURN
      END
