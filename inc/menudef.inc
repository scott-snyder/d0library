C
C     Include file for using with COMPACK system
C
      INTEGER MAXPOS,MAXCOM,MAXLEV,COMUNI
      PARAMETER (MAXPOS=32)
      PARAMETER (MAXCOM=10)
      PARAMETER (MAXLEV=5)
      PARAMETER (COMUNI=26)
C
C     Define TOPSET for output in BIG letters
C
      CHARACTER ESC*1,TOPSET*3
      PARAMETER (ESC=CHAR(27),TOPSET=ESC//'#6')
      CHARACTER*40 TOPLIN(0:MAXPOS,MAXLEV)
      CHARACTER*22 MENLIN(MAXPOS,MAXLEV)
      CHARACTER*80 COMLIN(MAXPOS,MAXLEV),COMPRT(0:MAXCOM)
      CHARACTER*32 NAMLEV(MAXLEV)
      INTEGER POS,PF,MAXLIN(MAXLEV),NUMCOL,COMNUM,CURLEV,TCHAN
      LOGICAL FULSCR,SETUP
      CHARACTER*40 HLPTIT(MAXPOS,MAXLEV)
      CHARACTER*2048 HLPINF(MAXPOS,MAXLEV)
      COMMON /COMNUM/POS,PF,MAXLIN,CURLEV,NUMCOL,FULSCR,SETUP,COMNUM,
     *       TCHAN
      COMMON /COMCHR/TOPLIN,MENLIN,COMLIN,COMPRT,NAMLEV,HLPTIT,HLPINF