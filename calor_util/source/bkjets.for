      SUBROUTINE BKJETS (LJETS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :   Book the next bank in the JETS linear structure, at
C-   the end of the current structure.
C-
C-   Inputs  : none
C-   Outputs : LJETS    [I]  Link to the JETS bank booked.
C-   Controls: none
C-
C-
C-   ENTRY BKJETS_GET_CURRENT ( IVER, IDATA, ILINKS ): get the current version
C-   and size of the JETS bank.
C-
C-   Created   6-OCT-1988   Z. Wolf
C-   Updated  24-MAR-1989   Z. Wolf
C-   Modified 24-Apr-1989   S. Protopopescu
C-   MODIFIED 27-JUN-1989   N.J.HADLEY MAKE FIRST JET BANK BANK ZERO
C-   Updated  19-NOV-1991   Nick Hadley, Boaz Klima
C-       Version 2 - moving stuff from JTSH and adding reference links
C-   Updated  21-NOV-1991   Boaz Klima
C-       Replace JTSH structural link by JNEP
C-   Updated  17-MAY-1993   Harrison B. Prosper   Add full error matrix -
C-                                                Version 4
C-   Updated  23-OCT-1993   R. Astur Add 1 link for VCOR and 1 word for jet
C-                                 track-vertex word - Version 5
C-   Updated  25-OCT-1993   Marc Paterno   Add entry BKJETS_GET_CURRENT
C-   Updated  13-DEC-1993   R. Astur Add 7 data word for energy correction
C-                                   Version 6
C-   Updated   2-FEB-1995   Bob Hirosky   Add words for: Seed Et, Preclus Et,
C-                                        ICR Underlying event Et
C-                                        VERSION 7
C-   Updated  18-SEP-1995   Bob Hirosky/Brad Abbott Add # of cells in
C-                          EM,FH,ICD and CH layers, add 2nd ICR undrlyevt wrd
C-                                        VERSION 8
C-   Updated  30-OCT-1995   Dhiman Chakraborty   
C-                          Added 6 words for CAFIX: Ch fraction, ICD fraction,
C-                          errors: stat high, stat low, syst high, syst low
C-                                        VERSION 9
C-                          Also added ENTRY BKJETS_UPDATE to bring an old bank
C-                          to current size and update the version
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C--   I/O
      INTEGER LJETS, IVER, IDATA, ILINKS
C
C--   ZEBRA BANKS
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZJETS.LINK/LIST'
C
C--   INTERNAL VARIABLES
      INTEGER IFORM, LCAPH, GZCAPH, LJETSP, IDN, VERSION
      INTEGER NDATA, NLINKS
      PARAMETER (VERSION =  9)              ! Current bank version
      PARAMETER (NDATA   = 51)
      PARAMETER (NLINKS  =  7)
      INTEGER ND_OLD,NL_OLD,DND,DNL
      INTEGER HJETS,IERR
      LOGICAL FIRST
      DATA  FIRST  /.TRUE./
      SAVE  FIRST
C----------------------------------------------------------------------
C
C--   DO PRELIMINARIES ON FIRST CALL
      IF (FIRST) THEN
        CALL MZFORM('JETS', '1I 13F 2I 3F 2I 4F 2I 14F 4I 6F', IFORM)
        FIRST=.FALSE.
      END IF
      LCAPH = GZCAPH()
      IF (LCAPH .LE. 0) CALL BKCAPH(LCAPH)
      LJETSP = LCAPH-IZJETS
      IDN = 0
      IF (LQ(LJETSP) .EQ. 0) THEN
        CALL MZBOOK(IXMAIN, LJETS, LCAPH, -IZJETS, 'JETS', NLINKS, 2,
     &    NDATA , IFORM, 0)
        IQ(LJETS-5)  =  0
        IQ(LJETS+1)  =  VERSION
      ELSE
   10   CONTINUE
        IF (LQ(LJETSP) .NE. 0) THEN
          LJETSP = LQ(LJETSP)
          IDN = IQ(LJETSP-5)
          GOTO 10
        END IF
C
C--         BOOK THE JETS BANKS, SET THE ID NUMBER
        CALL MZBOOK(IXMAIN, LJETS, LJETSP, 0, 'JETS', NLINKS, 2, NDATA,
     &    IFORM, 0)
        IQ(LJETS-5) = IDN+1
        IQ(LJETS+1) = VERSION
      ENDIF
      RETURN
C#######################################################################
      ENTRY BKJETS_GET_CURRENT ( IVER, IDATA, ILINKS )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get the version number of the most current JETS bank.
C-
C-   Inputs  : none
C-   Outputs : IVER   [I]  The version number of the current bank
C-   Controls: none
C-
C-   Created  25-OCT-1993   Marc Paterno
C-
C----------------------------------------------------------------------
      IVER = VERSION
      IDATA = NDATA
      ILINKS = NLINKS
      RETURN
C#######################################################################
      ENTRY BKJETS_UPDATE(LJETS,IVER,DNL,DND,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : update bank size
C-
C-   Inputs  : LJETS [I] = pointer to the JETS bank
C-   Outputs : IVER  [I] = The version number of the current bank
C-             DNL   [I] = number of LINK words added
C-             DND   [I] = number of DATA words added
C-             IERR  [I] = error status  0 --- OK
C-                                      -1 --- No match for given LJETS
C-   Controls: none
C-
C-   Created  30-OCT-1995 Dhiman Chakraborty
C-   Updated  Oct-31-1995 Bob Kehoe  -- fix JETS bank check bug
C----------------------------------------------------------------------
      IERR = 0
      IVER = VERSION
      DNL = -999
      DND = -999
      IF(LJETS.LE.0) THEN
        IERR = -1
        CALL ERRMSG('No match for given LJETS',
     &                    'BKJETS_UPDATE','LJETS .LE. 0','W')
        GOTO 999
      ENDIF
      CALL DCTOH(4,'JETS',HJETS)
      IF(IQ(LJETS-4).NE.HJETS) THEN
        IERR = -1
        CALL ERRMSG('No match for given LJETS',
     &                    'BKJETS_UPDATE','Check LJETS','W')
        GOTO 999
      ENDIF
      NL_OLD = IQ(LJETS-3)
      ND_OLD = IQ(LJETS-1)
      DNL = NLINKS - NL_OLD
      DND = NDATA - ND_OLD
      IF((DND.NE.0).OR.(DNL.NE.0)) THEN
        CALL MZPUSH(IXCOM,LJETS,DNL,DND,' ')
      ENDIF
      IQ(LJETS+1) = VERSION
  999 RETURN
      END
