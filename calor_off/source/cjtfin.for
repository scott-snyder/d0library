      FUNCTION CJTFIN()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : FINISH UP CAJETS
C-
C-   Returned value  : TRUE if all OK
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  25-APR-1989   Rajendran Raja
C-   Updated  14-SEP-1990   Harrison B. Prosper
C-      Remove printout of histograms; done by frame
C-   Updated  20-NOV-1991   Nick Hadley, Boaz Klima
C-      Add SUMMARY information
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER II,LUN,IER,SSUNIT,NUMBER_CAPH
      INTEGER NOENT1(4),NOENT2(4),NOENT3(4)
      REAL    NJT_MEAN(4),NJT_RMS(4),ET_MEAN(4),ET_RMS(4),HSTATI
      REAL    MJJ_MEAN(4),MJJ_RMS(4)
      LOGICAL CJTFIN,FLGVAL,VERIFY
C----------------------------------------------------------------------
      CJTFIN = .TRUE.
      CALL DHDIR('CAJETS_RCP','HBOOK_DIRECTORY',IER,' ')
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('CALORIMETER','CJTFIN',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
C
      IF ( FLGVAL('VERIFY') ) THEN
        NUMBER_CAPH = 4
      ELSE
        NUMBER_CAPH = 1
      ENDIF
C
      DO II = 1, NUMBER_CAPH
        CALL HNOENT(100+II,NOENT1(II))
        CALL HNOENT(200+II,NOENT2(II))
        CALL HNOENT(400+II,NOENT3(II))
        NJT_MEAN(II) = HSTATI(100+II,1,' ',0)
        ET_MEAN(II) = HSTATI(200+II,1,' ',0)
        MJJ_MEAN(II) = HSTATI(400+II,1,' ',0)
        NJT_RMS(II) = HSTATI(100+II,2,' ',0)
        ET_RMS(II) = HSTATI(200+II,2,' ',0)
        MJJ_RMS(II) = HSTATI(400+II,2,' ',0)
      ENDDO
C
C ****  Write out to SUMMARY
C
      LUN = SSUNIT()
      WRITE (LUN,100)
      DO II = 1, NUMBER_CAPH
        WRITE (LUN,200) II
        WRITE (LUN,300) NOENT1(II),NJT_MEAN(II),NJT_RMS(II),
     &                  NOENT2(II),ET_MEAN(II),ET_RMS(II),
     &                  NOENT3(II),MJJ_MEAN(II),MJJ_RMS(II)
      ENDDO
C
  100 FORMAT (//,' Summary of CAJETS',/)
  200 FORMAT (' Jet Finding Algorithm no.',I3,/)
  300 FORMAT (' Jet Multiplicity: No. of entries =',I6,' ; Mean =',F7.2,
     &' ; RMS =',F7.2,/,
     &' Jet Et          : No. of Entries =',I6,' ; Mean =',F7.2,
     &' ; RMS =',F7.2,/,
     &' Jet-Jet Mass    : No. of Entries =',I6,' ; Mean =',F7.2,
     &' ; RMS =',F7.2,/)
  999 RETURN
      END
