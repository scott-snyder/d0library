      SUBROUTINE L1DMP_TOWER_UNPACK(LUN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Do a debugging dump of the energy assigned to each
C-      trigger tower.
C-
C-   Inputs  : LUN      The unit number to write the information to.
C-      Common block variable TT_ENERGY
C-   Outputs : none
C-   Controls: none
C-
C-   Created   9-JUL-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-   Updated  23-JAN-1992   Philippe Laurens, Steven Klocek   
C-                      Print out 16 per line.
C-                      Added note mentioning the possibility of noise being
C-                        added.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$INC:L1C_EVENT_RAW.INC'
      INTEGER  TRULEN
      EXTERNAL TRULEN
C
      INTEGER LUN, ISTAT
      INTEGER ETA,SIGN_ETA,PHI
      INTEGER ENTRY
      CHARACTER*132 LINE
      INTEGER SLEN
C
      INTEGER PER_LINE
      PARAMETER (PER_LINE = 16)
C
      WRITE (LUN,*)
      WRITE (LUN,*)
      WRITE (LUN,*) 
     &  'Dump Of Trigger Tower Energy From Calorimeter Banks (In GeV)'
      WRITE (LUN,*) 
     &  '============================================================'
C
      WRITE (LUN,*) 
      WRITE (LUN,*) 'Note: These values will include noise ' 
     &           // 'if that option has been selected from L1SIM.'
      DO ETA = ETA_MAX, ETA_MIN, -1
        WRITE (LUN,*)
        DO PHI = PHI_MIN, PHI_MAX, PER_LINE
          LINE = 'ETA=xxx   HD'
          WRITE (LINE(5:7),900, IOSTAT=ISTAT) -ETA
          WRITE (LUN,1000, IOSTAT = ISTAT) LINE(1:12), 
     &      (TT_ENERGY(NEG_ETA, ETA, PHI+ENTRY-1, HD_TOWER), 
     &        ENTRY= 1,PER_LINE)
          CALL STRINT('PHI=',PHI,LINE,SLEN)
          CALL ADDSTR(LINE,':',LINE,SLEN)
          CALL STRINT(LINE(1:SLEN),PHI+PER_LINE-1,LINE,SLEN)
          LINE(11:12) = 'EM'
          WRITE (LUN,1000, IOSTAT=ISTAT) LINE(1:12), 
     &      (TT_ENERGY(NEG_ETA, ETA, PHI+ENTRY-1, EM_TOWER),
     &        ENTRY= 1,PER_LINE)
        END DO
      END DO
C
C       Do positive ETA
C
      DO ETA = ETA_MIN, ETA_MAX
        WRITE (LUN,*)
        DO PHI = PHI_MIN, PHI_MAX, PER_LINE
          LINE = 'ETA=xxx   HD'
          WRITE (LINE(5:7),900, IOSTAT=ISTAT) ETA
          WRITE (LUN,1000, IOSTAT=ISTAT) LINE(1:12), 
     &      (TT_ENERGY(POS_ETA, ETA, PHI+ENTRY-1, HD_TOWER), 
     &        ENTRY= 1,PER_LINE)
          CALL STRINT('PHI=',PHI,LINE,SLEN)
          CALL ADDSTR(LINE,':',LINE,SLEN)
          CALL STRINT(LINE(1:SLEN),PHI+PER_LINE-1,LINE,SLEN)
          LINE(11:12) = 'EM'
          WRITE (LUN,1000, IOSTAT=ISTAT) LINE(1:12), 
     &      (TT_ENERGY(POS_ETA, ETA, PHI+ENTRY-1, EM_TOWER),
     &        ENTRY= 1,PER_LINE)
        END DO
      END DO
C
  900 FORMAT( SP, I3 )
 1000 FORMAT( ' ', A, 16(' ', F6.2 ))
C----------------------------------------------------------------------
  999 RETURN
      END
