      SUBROUTINE VCLEAN(LSTTRK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Loop over all VTXTs with IQ(LVTXT-5)>LSTTRK (NEW)
C-           and compare with those with IQ(LVTXT-5)<=LSTTRK (OLD).  If such a
C-           pair is found that share more the ALLOW hits then
C-           
C-           Delete the NEW VTXT if the old one is 3D matched
C-           Delete the OLD VTXT if the old one is NOT matched
C-
C-   Inputs  : LSTTRK -- Track ID of last track found prior to most recent call
C-                       to VTROAD
C-   Outputs : 
C-   Controls: 
C-
C-   Created  19-MAY-1993   Ed Oltman
C-   Updated  30-Sep-1993   Ed Oltman employ more efficient algorithm. Do not
C-                          reverse order of VTXT IDs
C-   Updated  16-DEC-1993   Ed Oltman  Replace BTEST on IUSED bit with JBIT 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:ZTRAKS.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$LINKS:IZVTXT.LINK'

c I/O:
      INTEGER LSTTRK
c Locals:
      INTEGER MAXTRAK
      PARAMETER (MAXTRAK = 1000)
      INTEGER LTRK(MAXTRAK)
      LOGICAL FIRST
      REAL    CLOSE,PHI1,PHI2,DEL
      INTEGER LVTRH,LVTXT,LVTXT1,LVTXT2,LVTTH1,LVTTH2,DROP
      INTEGER ALLOW,IER,TOTAL,NEWTRK,OLDTRK,HIT1,HIT2,SAME,TRK
      INTEGER IH1,IH2,IAD1,IAD2,WIRE1,WIRE2
c Externals:
      INTEGER GZVTRH,GZVTXT,JBIT
c Date:
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET('CLOSE',CLOSE,IER)
        CALL EZGET('ALLOW',ALLOW,IER)
        CALL EZRSET
      ENDIF
      IF (LSTTRK .LE. 0) GO TO 999
      LVTRH = GZVTRH()
      LVTXT = LQ(LVTRH-IZVTXT)
      TOTAL = 0
      DO WHILE (LVTXT .GT. 0)
        TOTAL = TOTAL + 1
        LTRK(TOTAL) = LVTXT
        LVTXT = LQ(LVTXT)
      ENDDO
C
C ****  
C
      DROP = 0
      DO NEWTRK = 1,TOTAL-LSTTRK
        LVTXT1 = LTRK(NEWTRK)
        IF (LVTXT1 .EQ. 0) GO TO 30         ! this track already dropped
        HIT1 =IQ(LVTXT1+2)
        PHI1 = Q(LVTXT1+6)
        LVTTH1 = LQ(LVTXT1-1)
        DO OLDTRK = TOTAL-LSTTRK+1,TOTAL
          LVTXT2 = LTRK(OLDTRK)
          IF (LVTXT2 .EQ. 0) GO TO 20       ! this track already dropped
          PHI2 = Q(LVTXT2+6)
          DEL = ABS(PHI1-PHI2)
          IF (DEL .GT. 3.) DEL = ABS(DEL-TWOPI)
          IF (DEL .LT. CLOSE) THEN
            HIT2 =IQ(LVTXT2+2)
            LVTTH2 = LQ(LVTXT2-1)
            SAME = 0
            IH2 = 0
            DO IH1 = 0,HIT1-1                    ! Loop over NEW track hit
              IAD1 = IBCLR(IQ(LVTTH1+6+4*IH1),0)
              WIRE1= IBITS(IAD1,1,3) + 8*IBITS(IAD1,9,2)
              DO WHILE (IH2 .LE. HIT2-1)         ! Loop over OLD track hit
                IAD2 = IBCLR(IQ(LVTTH2+6+4*IH2),0)
                WIRE2= IBITS(IAD2,1,3) + 8*IBITS(IAD2,9,2)
                IF (IAD1 .EQ. IAD2) THEN
                  IF (IQ(LVTTH1+7+4*IH1) .EQ. 
     &                IQ(LVTTH2+7+4*IH2)      ) SAME = SAME + 1
                  IF (SAME .GT. ALLOW) THEN
                    DROP = DROP + 1
                    IQ(LVTRH+2) = IQ(LVTRH+2) - 1
                    IQ(LVTRH+5) = IQ(LVTRH+5) - 1
                    IF (JBIT(IQ(LVTXT2),IUSED) .NE. 0) THEN
                      CALL MZDROP(IXCOM,LVTXT1,' ')
                      LTRK(NEWTRK) =  0
                      GO TO 30                   ! Skip to next NEW track
                    ELSE
                      CALL MZDROP(IXCOM,LVTXT2,' ')
                      LTRK(OLDTRK) = 0
                      GO TO 20                   ! Skip to next OLD track
                    ENDIF
                  ENDIF
                ENDIF
                IH2 = IH2 + 1
                IF (WIRE2 .GE. WIRE1) GO TO 10   ! Skip to next NEW track HIT
              ENDDO                              ! end of OLD track HIT loop
   10         CONTINUE
            ENDDO
          ENDIF
   20     CONTINUE
        ENDDO
   30   CONTINUE
      ENDDO
      IF (DROP .GT. 0) THEN
C
C ****  If this call causes tracks to be dropped, renumber tracks..
C
        LVTXT = GZVTXT(0)
        TRK = TOTAL - DROP
        DO WHILE (LVTXT .GT. 0)
          IQ(LVTXT-5) = TRK
          TRK = TRK - 1
          LVTXT = LQ(LVTXT)
        ENDDO
      ENDIF
  999 RETURN
      END
