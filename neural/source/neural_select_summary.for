      SUBROUTINE NEURAL_SELECT_SUMMARY(NFILE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Summary of inputs selected.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  10-MAR-1995   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NFILE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:JETNET.INC'
      INCLUDE 'D0$INC:JNDAT1.INC'
C----------------------------------------------------------------------
      INTEGER I,J,K,STATUS
      CHARACTER*80 TITLE
      INTEGER NBINS,ID1,ID2,ID3
      INTEGER NODE_OFF,SCALE_OFF,FILE_OFF
      REAL    POW,S1,S2,HSUM,HI
C----------------------------------------------------------------------
C
C ****  GET HISTOGRAM ID OFFSETS
C
      CALL EZGET('INPUT_NODE_OFFSET',NODE_OFF,STATUS)
      IF ( NODE_OFF .LE. 0 ) NODE_OFF=1
      CALL EZGET('FILE_OFFSET',FILE_OFF,STATUS)
      IF ( FILE_OFF .LE. 0 ) FILE_OFF=100
      CALL EZGET('SCALE_OFFSET',SCALE_OFF,STATUS)
      IF ( SCALE_OFF .LE. 0 ) SCALE_OFF=1000
C
C ****  GET BINS
C
      CALL EZGET('HISTO_BINS',NBINS,STATUS)
      IF ( STATUS .NE. 0 ) THEN
        CALL ERRMSG('NO_HISTO_BINS','NEURAL_SELECT',
     &        'HISTO_BINS set to 50','I')
        NBINS = 50
      END IF
C
      IF(NTAG.EQ.0)
     &    CALL ERRMSG('NEURAL','NEURAL_SELECT','NTUPLE EMPTY','F')
      CALL INTMSG(' ')
      WRITE(TITLE,21)'  NT','NAME    ','  USED ','  P ','TAG   '
      CALL INTMSG(TITLE)
      K = 0
      DO I = 1, NLABI
        WRITE(TITLE,20)I,LABI(I),'        ',TAGS(I)
        IF(LABI(I)(1:1).NE.'*') THEN
          K = K + 1
          IF(NFILE.GT.1) THEN
            ID1 = FILE_OFF*1+SCALE_OFF*2+NODE_OFF*K
            ID2 = FILE_OFF*2+SCALE_OFF*2+NODE_OFF*K
            ID3 = FILE_OFF*9+SCALE_OFF*2+NODE_OFF*K
            S1= 0.5/HSUM(ID1)
            S2= 0.5/HSUM(ID2)
            CALL HOPERA(ID1,'-',ID2,ID3,S1,S2)
            CALL HIDOPT(ID3,'INTE')
            POW = 0
            DO J = 1, NBINS
              POW =  POW + ABS(HI(ID3,J))
            END DO
            WRITE(TITLE,22)I,LABI(I),' INPUT  ',POW,TAGS(I)
          ELSE
            WRITE(TITLE,20)I,LABI(I),' INPUT  ',TAGS(I)
          ENDIF
        ELSE IF( I.LE.NLABO ) THEN
          WRITE(TITLE,20)I,LABI(I),' OUTPUT ',TAGS(I)
        END IF
        CALL INTMSG(TITLE)
   20   FORMAT(1X,I5,1X,A16,A8,5X,10(1X,A7))
   21   FORMAT(1X,A5,1X,A16,A8,A5,10(1X,A7))
   22   FORMAT(1X,I5,1X,A16,A8,F5.3,10(1X,A7))
      END DO
      CALL INTMSG(' ')
  999 RETURN
      END
