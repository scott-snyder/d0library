      SUBROUTINE PDRPHI
C======================================================================
C
C  Description:
C  ============
C  Draws the Cosmic Ray Detector and current event
C
C-   Created  16-JAN-1987   Tami Kramer
C-   Updated  17-DEC-1987   Olivier Callot
C-   Updated  27-NOV-1990   Lupe Howell  Implementing PIXIE_RCP
C
C=====================================================================
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER OLAYER,NLAYER,IER,TYP
      INTEGER GZCDCH,LCDCH,NCDCHIT,MAXCHITS
      CHARACTER*4 REM,CVAL
C----------------------------------------------------------------------
      DATA NLAYER/1/
C----------------------------------------------------------------------
C
      CALL EZPICK('PX_CDCDIS_RCP')
      CALL EZ_GET_ARRAY('PXPARAMS','CDC MAX HITS',1,MAXCHITS,
     & CVAL,TYP,REM,IER)
C Check number of hits in CDC
      LCDCH=GZCDCH(0)
      NCDCHIT=IQ(LCDCH+1)
      IF(NCDCHIT.GT.MAXCHITS)THEN
        CALL INTMSG(' !!!! WARNING - TOO MANY CDC HITS')
        GO TO 900
      ENDIF  
      CALL EZ_GET_ARRAY('PXPARAMS','CDC LAYER',
     &         1,OLAYER,CVAL,TYP,REM,IER)
      CALL EZ_SET_ARRAY('PXPARAMS','CDC LAYER',NLAYER,IER)
      CALL PDSIDE( 0, 31 )
      CALL EZ_SET_ARRAY('PXPARAMS','CDC LAYER',OLAYER,IER)
  900 CALL EZRSET
C
      RETURN
      END
