      PROGRAM MEGA_AUTOCOMPARE_HISTOGRAM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Automatically compares all histograms stated in
C-   RCP file with reference specified in RCP.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: MEGA_AUTOCOMPARE_HISTOGRAM_RCP, ! global parameters for
C-                                             ! experts
C-                                      
C-             MEGA_AUTOCOMPARE_INSTRUCTIONS_RCP ! User written.
C-
C-   Created  16-APR-1993   R. J. Genik II
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER Iok
C----------------------------------------------------------------------
C
C ****  Setup Zebra, SRCP, and Hbook
C
C
C ****  Read in global common block
C
      Call MAC_Global_Common_Read('MEGA_AUTOCOMPARE_RCP')
C
C ****  Init current common block.
C
      Call INRCP('MAC_INSTRUCTIONS_RCP', Iok)
      If (Iok.ne.0) then
        STOP ' I Can''t open MEGA_AUTOCOMPARE_INSTRUCTIONS_RCP'
      Endif
C
C ****  Do the Comparisons
C
      Call EZPICK('MAC_INSTRUCTIONS_RCP')
      Call MAC_Do_What_You_Are_Told
      Call EZRSET
      END
