      PROGRAM DBMAIN
*     ==============
*
************************************************************************
*                                                                      *
*        PROG. DBMAIN                                                  *
*                                                                      *
*   Main Program of the Interactive DBL3 based on the KUIP Package     *
*                                                                      *
************************************************************************
*
      PARAMETER       (L3CORQ=200000, L3CORH=200000, L3CORK=70000)
*
      COMMON /PAWC/   PAW(L3CORH)
      PARAMETER       (L3WKST=1, NARGL3=100)
      COMMON /L3FLAG/ IFTML3, IGYFL3, IHFIL3, IHISL3, INTXL3, IPRTL3
     +              , IPR2L3, IWKTL3, IXERL3, MOP3L3, ARGSL3(NARGL3)
      INTEGER         IARGL3(NARGL3)
      INTEGER         IFTML3, IGYFL3, IHFIL3, IHISL3, INTXL3, IPRTL3
     +              , IPR2L3, IWKTL3, IXERL3, MOP3L3
      REAL            ARGSL3
      EQUIVALENCE     (IARGL3(1), ARGSL3(1))
      PARAMETER       (MAXJDX=20, MAXVDX= 20, LUKYDX=88, LUDADX=89)
      COMMON /DXLINK/ ISTODX, L3PRDX, LURZDX, LFRSDX, LJOIDX,
     +                LKJNDX(MAXJDX), LDJNDX(MAXJDX), LVIWDX,
     +                LKVWDX(MAXVDX), LDVWDX(MAXVDX),
     +                LASTDX
*
      EXTERNAL        DBTERM
      DATA            LUNER /19/, LUNMF /10/
*
*     ------------------------------------------------------------------
*
* *** Initialize ZEBRA
*
      CALL MZEBRA (-1)
*
*  ** Initialize DB-Store, User-Division, User-Links
*
      CALL DBXINI
*
*  ** Initialize PAW and KUIP
*
      CALL HLIMIT (-L3CORH)
      CALL KUINIT (L3CORK)
      INTXL3 = 1
      NWORD  = 0
      CALL IGINIT (NWORD)
      CALL KUOPEN (LUNMF, 'SYS$LOGIN:DBASE.METAFILE', 'UNKNOWN', ISTAT)
      CALL KUOPEN (LUNER, 'SYS$LOGIN:GKSERROR.LOG', 'UNKNOWN', ISTAT)
      CALL HERMES (LUNER)
      CALL IGWKTY (IWKTL3)
      CALL HPLINT (IWKTL3)
      CALL IGSA   (L3WKST)
      CALL INPAW
*
*  ** Define the Exit Routine
*
      CALL KUEXIT (DBTERM)
      CALL TIMEST (99999.)
*
*  *  Create the Interactive Command structure
*
      CALL HERMES (L3PRDX)
      CALL VECDEF
      CALL INDBIN
      CALL INZEIN
      CALL INPAIN
*
*  *  Set prompt
*
      CALL KUEXEC ('SET/PROMPT ''DB >''')
*
*  *  Execute LOGON Macro
*
      CALL KUEXEC ('EXEC DBLOGN')
*
      CALL KUSPY ('NEVER')
*
      CALL KUWHAT
*
      CALL DBEND
      STOP
*                                                             END DBMAIN
      END
      SUBROUTINE DBTERM
*     =================
*
************************************************************************
*                                                                      *
*        SUBR. DBTERM                                                  *
*                                                                      *
*   Exit routine for Interactive DB version                            *
*                                                                      *
*   Called by DBMAIN                                                   *
*                                                                      *
************************************************************************
*
      CALL DBEND
      CALL HPLEND
*                                                             END DBTERM
      END
      SUBROUTINE ZABEND
*     =================
*
************************************************************************
*                                                                      *
*        SUBR. ZABEND                                                  *
*                                                                      *
*   Called by ZEBRA system                                             *
*                                                                      *
************************************************************************
*
*
      CALL DBEND
      CALL FZENDO (0, 'T')
*
      CALL ZPOSTM ('TCWM.')
*
      CALL MZEND
      STOP
*                                                             END ZABEND
      END
