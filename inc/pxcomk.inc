C----------------------------------------------------------------------
C-
C-   Created  19-JUN-1990   Lupe Howell
C-   Updated   7-SEP-1990   Harrison B. Prosper
C-      Added EVTCNT and Command stack
C-   Updated  25-SEP-1992   Nobuaki Oshima
C-      Added RUNCNT 
C-   Updated  15-FEB-1993   Vipin Bhatnagar
C-      Added STREAM1 and PICK_FILE logical
C-
C----------------------------------------------------------------------
      INTEGER MAXPAK
      PARAMETER( MAXPAK = 50 )
      CHARACTER*40 PARNAME, SCRENAME , PACKAGE(MAXPAK)
C
      COMMON /PXCOMK_CH/ PARNAME,SCRENAME,PACKAGE
C
      INTEGER NUM_VIEWPORT,NSCREEN,IDEV,EVTCNT,NPACKAGE,RUNCNT
      LOGICAL VIEW3D(4)
      COMMON /PXCOMK_IN/ NUM_VIEWPORT,IDEV,EVTCNT,NPACKAGE,RUNCNT
C
      REAL    XMINPRT(4), XMAXPRT(4),YMINPRT(4), YMAXPRT(4),
     &        XMINWID(4), XMAXWID(4), YMINWID(4), YMAXWID(4),
     &        VIEWREFX(4), VIEWREFY(4), VIEWREFZ(4),
     &        UPVEC(3,4),
     &        CAMX(4), CAMY(4), CAMZ(4)
C
      COMMON /PXCOMK_RE/ XMINPRT, XMAXPRT,YMINPRT, YMAXPRT,
     &                   XMINWID, XMAXWID,YMINWID, YMAXWID,
     &                   VIEWREFX, VIEWREFY, VIEWREFZ,
     &                   UPVEC,
     &                   CAMX, CAMY, CAMZ
C
C--FOR SINGLE FILE CASE
C-
      COMMON/STYPE/STREAM1,PICK_FILE
C
      CHARACTER*24 STREAM1
      LOGICAL PICK_FILE
