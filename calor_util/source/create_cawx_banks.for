      LOGICAL FUNCTION CREATE_CAWX_BANKS()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Saves EM 3x3, 5x5, and 7x7 windows in CAW3,
C-   CAW5, and CAW7, for each PELC. Error Checking done at lower level.
C-
C-   Returned value  : .True. False if  GTPELC_TOTAL returns error.
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  24-JUL-1995   R. J. Genik II
C-   Updated  23-AUG-1995   R. J. Genik II  Added CAWC booking and filling.
C-
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      Include 'D0$INC:ZEBCOM.INC'
      Include 'd0$links:izcash.link'
      Integer LPELC,Window, find_lpelc, pelc_index, num_pelc,
     +  ier, LCASH, LCACL, LCAWX
C----------------------------------------------------------------------
      CREATE_CAWX_BANKS= .TRUE.
      Call MERGE_PELC_PPHO_CHAINS(LPELC)
C
      If (lpelc.ne.0) then
        Call GTPELC_TOTAL(NUM_PELC,IER)
        If (ier.ne.0) Then
          Call Errmsg('Problems with GTPELC_TOTAL','EM_WINDOW_ENERGY',
     +      ' ', 'W')
          CREATE_CAWX_BANKS= .FALSE.
          Goto 999
        Endif
        Do 10 pelc_index = 1, num_pelc
          LPELC = find_lpelc(pelc_index)
          Window = 7
          Call Save_Em_Window(LPELC,Window)
          LPELC = find_lpelc(pelc_index)
          Window = 5
          Call Save_Em_Window(LPELC,Window)
          LPELC = find_lpelc(pelc_index)
          Window = 3
          Call Save_Em_Window(LPELC,Window)
          LPELC = find_lpelc(pelc_index)
          Call Save_Em3_Window(LPELC)
          LPELC = find_lpelc(pelc_index)
          LCACL = LQ(LPELC - 2)
          LCASH = LQ(LCACL - IZCASH)
          Call FILL_CAWX(LCASH,LCAWX)
   10   Continue
      Endif
      Call UNMERGE_PELC_PPHO_CHAINS
  999 RETURN
      END
