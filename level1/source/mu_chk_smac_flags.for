      SUBROUTINE MU_CHK_SMAC_FLAGS(CENT_TRUNC,TOO_MANY_HITS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Loops over all SAMUS MAC's checking their flags.
C-
C-   Inputs  : None
C-   Outputs : CENT_TRUNC - .TRUE. if at least one MAC had found more than
C-                                 11 fine centroids;
C-             TOO_MANY_HITS - .TRUE. if at least one MAC received more
C-                                 than 32 hits ==> No hits read out.
C-   Controls: 
C-
C-   Created   5-MAR-1993   Guilherme Lima
C-   Updated  22-MAR-2004   sss - compile with g77
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      LOGICAL CENT_TRUNC,TOO_MANY_HITS
      INTEGER LSAHH,GZSAHH,LMUD1,GZMUD1
      INTEGER I,ISTA,JMUD1,JHEADW,IFLAG,MODNO
      CHARACTER*80 MSG,CENTMSG,MANYMSG
      DATA CENTMSG /'MUSIM: Centroid truncation'/
      DATA MANYMSG /'MUSIM: Too many hits, module not read out'/
C--------------------------------------------
      CENT_TRUNC=.FALSE.
      TOO_MANY_HITS=.FALSE.

      LMUD1=GZMUD1(0)
      LSAHH=GZSAHH()
      IF(LSAHH.EQ.0) THEN
        CALL ERRMSG(' NO SAHH BANK','MU_CHK_SMAC_FLAGS',
     &      ' No SAHH bank found.','W')
        GOTO 999
      ENDIF

C.. Loop over Stations
      DO ISTA=0,5
        JMUD1 = IQ(LSAHH+19+ISTA)         ! Pointer to first data in crate
        
C.. Loop over modules in this crate
        DO I = -7,-2
          JHEADW = IQ(LMUD1+JMUD1+I)
          MODNO=ISHFT(JHEADW,-16)
          IFLAG=IAND(ISHFT(JHEADW,-12),15)
          IF(IAND(IFLAG,8).NE.0) THEN
            CENT_TRUNC=.TRUE.
            WRITE(MSG,123) MODNO
  123       FORMAT('CENT_TRUNC_',I3)
            CALL ERRMSG(MSG,'SAMUS_FLAG',CENTMSG,'I')
          ENDIF
          IF(IAND(IFLAG,2).NE.0) THEN
            TOO_MANY_HITS=.TRUE.
 	    WRITE(MSG,125) MODNO,IFLAG
  125       FORMAT(' Too many hits flag set in module ',I3,
     &              ', (flag=',I2,')')
            CALL INTMSG(MSG)
           WRITE(MSG,124) MODNO
  124       FORMAT('TOO_MANY_HITS_',I3)
            CALL ERRMSG(MSG,'SAMUS_FLAG',MANYMSG,'I')
          ENDIF
        ENDDO

      ENDDO

  999 RETURN
      END
