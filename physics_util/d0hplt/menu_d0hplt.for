      SUBROUTINE MENU_D0HPLT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Add menu items for a menu D0HPLT
C-
C-   Inputs :  None
C-   Outputs : None
C-
C-   Created   27-APR-90   by MENU_MAKER
C-   Updated  21-MAY-1990   Harrison B. Prosper
C-      Add ZONE command
C-   Updated  21-JUN-1992   Harrison B. Prosper  
C-      Update help 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*64 MENNAM
C----------------------------------------------------------------------
      CALL MENNEW('D0HPLT')
      CALL MENADD('D0HPLT',.FALSE.,
     &   'Report Status',
     &   'STATUS',
     &   '     Provide a status report on EXAMINE.          '//
     &   ' '   )
      CALL MENADD('D0HPLT',.FALSE.,
     &   'START Updating Plot',
     &   'UPLOT',
     &   '      Start updating a histogram on your ter'//
     &   'minal screen for N events, where N is entered by t'//
     &   'he user. Use STOP Updating Plot to stop upd'//
     &   'ating a plot. Enter 0 to update "forever".'//
     &   ' '   )
      CALL MENADD('D0HPLT',.FALSE.,
     &   'Plot a Histogram',
     &   'PLOT',
     &   '      Plot a histogram on your termi'//
     &   'nal screen.                                       '//
     &   ' '   )
      CALL MENADD('D0HPLT',.FALSE.,
     &   'STOP Updating Plot',
     &   'STOP_UPLOT',
     &   '      Cancel the display of updating histogram.   '//
     &   ' '   )
      CALL MENADD('D0HPLT',.FALSE.,
     &   'Index of histograms',
     &   'INDEX',
     &   '      Display an index of all the histograms that have'//
     &   ' been booked within the current HBOOk4 directory. '//
     &   'Note that with this command you can use the LEFT mouse'//
     &   ' button to SELECT and the RIGHT button to go BACK.'//
     &   ' '   )
      CALL MENADD('D0HPLT',.FALSE.,
     &   'Change Directory',
     &   'CHANGE DIRECTORY',
     &   '      Change the HBOOK4 directory. The top level direc'//
     &   'tory is called //PAWC. '//
     &   'Note that with this command you can use the LEFT mouse'//
     &   ' button to SELECT and the RIGHT button to go BACK.'//
     &   ' '   )
      CALL MENADD('D0HPLT',.FALSE.,
     &   'Laser Plot',
     &   'LPLOT',
     &   '      This command will cause the specified plot t'//
     &   'o be plotted on the Laser Printer.  The default is'//
     &   ' to plot the current histogram.                   '//
     &   ' '   )
      CALL MENADD('D0HPLT',.FALSE.,
     &   'Clear/Reset',
     &   'CLEAR',
     &   '      Clear ALL histograms.'//
     &   ' '   )
      CALL MENADD('D0HPLT',.FALSE.,
     &   'LEGO Plot',
     &   'LEGO',
     &   '      Make a LEGO plot of a specified  2-D hist'//
     &   ' '   )
      CALL MENADD('D0HPLT',.FALSE.,
     &   'List PAWC DIR',
     &   'LIST',
     &   '      List the current directory and all the '//
     &   'directories in PAWC'//
     &   ' '   )
      CALL MENADD('D0HPLT',.FALSE.,
     &   'Show Histogram Title',
     &   'SHOW',
     &   '      Display the title of the '//
     &   'specified histogram.'//
     &   ' '   )
      CALL MENADD('D0HPLT',.FALSE.,
     &   'Header',
     &   'HEADER',
     &   '      Input the header for all '//
     &   'subsequent histograms.'//
     &   ' '   )
      CALL MENADD('D0HPLT',.FALSE.,
     &   'Type Histogram',
     &   'TYPE',
     &   '      Type specified histogram.'//
     &   ' '   )
      CALL MENADD('D0HPLT',.FALSE.,
     &   'Print Histogram',
     &   'PRINT',
     &   '      Print specified histogram '//
     &   ' '   )
      CALL MENADD('D0HPLT',.FALSE.,
     &   'Store Histogram',
     &   'STORE',
     &   '      Store the specified histogram in the file'//
     &   ' HSTSTORE.DAT.                  '//
     &   ' '   )
      CALL MENADD('D0HPLT',.FALSE.,
     &   'Preceding Plot',
     &   'BPLOT',
     &   '      Plot the preceding plot.      '//
     &   ' '   )
      CALL MENADD('D0HPLT',.FALSE.,
     &   'Same Plot',
     &   'SPLOT',
     &   '      Replot the currently selected histogram.'//
     &   ' '   )
      CALL MENADD('D0HPLT',.FALSE.,
     &   'Next Plot',
     &   'NPLOT',
     &   '      Plot the next histogram.       '//
     &   ' '   )
      CALL MENADD('D0HPLT',.FALSE.,
     &   'Zone plots',
     &   'NZONE',
     &   '      More than one histogram can be displayed per screen.'//
     &   ' To do so enter the number of zones in the z and Y '//
     &   'directions for future plots. Enter 1 and 1 to go back to '//
     &   '1 plot per screen. Z is horizontal and Y is vertical.'//
     &   ' '   )
      RETURN
      END
