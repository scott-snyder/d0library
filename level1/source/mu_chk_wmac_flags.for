      SUBROUTINE MU_CHK_WMAC_FLAGS(CENT_TRUNC,TOO_MANY_HITS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Loops over all WAMUS MAC's checking their flags.
C-
C-   Inputs  : None
C-   Outputs : CENT_TRUNC - .TRUE. if at least one MAC had found more than
C-                                 11 fine centroids;
C-             TOO_MANY_HITS - .TRUE. if at least one MAC received more
C-                                 than 32 hits ==> No hits read out.
C-   Controls: 
C-
C-   Created   9-APR-1993   Guilherme Lima, Gilvan Alves
C-   Updated  22-MAR-2004   sss - compile with g77
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      LOGICAL CENT_TRUNC,TOO_MANY_HITS
      INTEGER LMUD1,GZMUD1
      INTEGER JMUD1,JHEADW,IFLAG,MODNO
      INTEGER ICRATE,IMAC,NH,ND
      CHARACTER*80 MSG,CENTMSG,MANYMSG
      DATA CENTMSG /'MUSIM: Centroid truncation'/
      DATA MANYMSG /'MUSIM: Too many hits, module not read out'/
C--------------------------------------------
      CENT_TRUNC=.FALSE.
      TOO_MANY_HITS=.FALSE.

C.. Gets MUD1 address and points to first data crate
      LMUD1=GZMUD1(0)
      ICRATE=LMUD1

C.. Check if crate exists or not
 1000 IF(IQ(LMUD1-1).LE.(ICRATE-LMUD1)+16) GOTO 999
      NH=IQ(ICRATE+1)+1
      ND=IQ(ICRATE+NH)

C.. Loop over MAC's for this crate
      DO IMAC=1,12
        JMUD1=ICRATE+IMAC+6
        JHEADW=IQ(JMUD1)
        MODNO=ISHFT(JHEADW,-16)
        IF(MODNO.NE.0) THEN
          IF(MODNO.GT.310) GOTO 200    ! skip SAMUS crates
          IFLAG=IAND(ISHFT(JHEADW,-12),15)
          IF(IAND(IFLAG,8).NE.0) THEN
            CENT_TRUNC=.TRUE.
            WRITE(MSG,123) MODNO
  123       FORMAT('CENT_TRUNC_',I3)
            CALL ERRMSG(MSG,'WAMUS_FLAG',CENTMSG,'I')
          ENDIF
          IF(IAND(IFLAG,2).NE.0) THEN
            TOO_MANY_HITS=.TRUE.
	    WRITE(MSG,125) MODNO,IFLAG
  125       FORMAT(' Too many hits flag set in module ',I3,
     &              ', (flag=',I2,')')
            CALL INTMSG(MSG)
            WRITE(MSG,124) MODNO
  124       FORMAT('TOO_MANY_HITS_',I3)
            CALL ERRMSG(MSG,'WAMUS_FLAG',MANYMSG,'I')
          ENDIF
        ENDIF
      ENDDO

C.. Go to next crate
  200 ICRATE=ICRATE+NH+ND+5
      GOTO 1000

  999 RETURN
      END
