      SUBROUTINE MUBOOK_TZERO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Books Histograms for TZero adjustment and
C-                         for number of hits per module.
C-
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   1-OCT-1991   Cecilia E Gerber
C-   Updated  22-JAN-1992   C.G. Add histos for number of hits per module
C   DH 4/92 ADD 3 MISS STUFF
C   RM 5/94 Change histogram titles, and binning for T0 histograms
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CHARACTER*10 CMODU
      CHARACTER*40 DTTIT
      INTEGER MODNUM, MUNMOD3, NMODU, ID, I,  DUM
C
      NMODU = MUNMOD3(0,DUM)                  ! number of modules
      DO  I = 1,NMODU                        ! loop over modules
        MODNUM = MUNMOD3(1,I)
        ID = 20000 + MODNUM
        WRITE(CMODU,101) MODNUM
C
        DTTIT = ' S*DEV T1 '//CMODU
        CALL HBOOK1(ID,DTTIT,50,-1.,1.,0.)
        DTTIT = ' S*DEV T2 '//CMODU
        CALL HBOOK1(ID+1000,DTTIT,50,-1.,1.,0.)
        DTTIT = 'DRIFT DIST '//CMODU
        CALL HBOOK1(ID+2000,DTTIT,40,-1.,7.,0.)
c        DTTIT = 'DRIFT TIME '//CMODU
c        CALL HBOOK1(ID+3000,DTTIT,40,-200.,1400.,0.)
        DTTIT = 'Raw Times, Hits on Tracks '//CMODU
        CALL HBOOK1(ID+3000,DTTIT,41,0.,4100.,0.)
        DTTIT = 'DRIFT RAWT '//CMODU
        CALL HBOOK1(ID+4000,DTTIT,41,0.,4100.,0.)
C
        DTTIT = 'DRIFT 3-MISS '//CMODU
        CALL HBOOK1(ID+5000,DTTIT,40,-1.,1.,0.)
C
        DTTIT = 'Delta T0, Module '//CMODU
        CALL HBOOK1(ID+6000,DTTIT,40,-200.,200.,0.)
      ENDDO
C
  101 FORMAT(I5)
  999 RETURN
      END
