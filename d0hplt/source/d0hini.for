      SUBROUTINE D0HINI
C----------------------------------------------------------------------
DC-
C-   Purpose and Methods :
C
C  Initializes D0HPLT for the HBOOK version one is using.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C
C  Author:
C  ==========
C  Tami Kramer
C
C  Revision History
C  =================
C  Original Creation - May 22, 1989
C-   Updated  20-FEB-1991   Harrison B. Prosper, Sharon Hagopian
C-   Updated 7-MAY-1992 S. Hagopian
C- Updatead 24-AUG-1992 add USER HEADER flag
C
C----------------------------------------------------------------------
C
      IMPLICIT NONE
      INTEGER NFLG
      PARAMETER( NFLG = 18 )
      CHARACTER*6 FLGARR(NFLG)
      CHARACTER*6 FLG_USERHEAD
      DATA FLGARR/'D0HPID','D0HSHW','D0HINX','D0HCHD','D0HTYP',
     X            'D0HCLR','D0HPRT','D0HSTR','D0HBAK','D0HSAM',
     X            'D0HNEX','D0HLAS','D0HUPH','D0HAUT','D0HZON',
     X            'D0HLGO','D0HLIS','D0HEAD'/
C----------------------------------------------------------------------
C
      CALL FLGBK(FLGARR,NFLG)
      CALL FLGSET('D0HPID',.FALSE.)
      CALL FLGSET('D0HUPH',.FALSE.)
      CALL FLGSET('D0HAUT',.FALSE.)
      CALL FLGSET('D0HSHW',.FALSE.)
      CALL FLGSET('D0HINX',.FALSE.)
      CALL FLGSET('D0HCHD',.FALSE.)
      CALL FLGSET('D0HTYP',.FALSE.)
      CALL FLGSET('D0HCLR',.FALSE.)
      CALL FLGSET('D0HPRT',.FALSE.)
      CALL FLGSET('D0HSTR',.FALSE.)
      CALL FLGSET('D0HBAK',.FALSE.)
      CALL FLGSET('D0HSAM',.FALSE.)
      CALL FLGSET('D0HNEX',.FALSE.)
      CALL FLGSET('D0HLAS',.FALSE.)
      CALL FLGSET('D0HLGO',.FALSE.)
      CALL FLGSET('D0HLIS',.FALSE.)
      CALL FLGSET('D0HEAD',.FALSE.)
      CALL FLGBK(FLG_USERHEAD,1)
      CALL FLGSET('FLG_USERHEAD',.FALSE.)
      CALL D0HSDN('//PAWC')          ! set default directory to top
      RETURN
      END
