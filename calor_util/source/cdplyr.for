      SUBROUTINE CDPLYR(DEPTH,IETAC,ILYRC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Converts the hardware DEPTH index into ILYRC,
C-                         using a lookup table based on IETAC and DEPTH.
C-                         The lookup table contains the values for 
C-                         positive IETAC; those for negative IETAC are
C-                         the same as for positive  except for IETAC = 12
C-                         or 14, which have the differences from the table
C-                         overwritten in the code.  The values in the 
C-                         table are deduced from D0 Note 774, pp. 13-15.
C-                         (The conversion is independent of phi.)
C-
C-   Inputs  : DEPTH     depth index in the ADC system [0,11]
C-             IETAC     physics eta index [-37,-1],[1,37]
C-
C-   Outputs : ILYRC     physics radial index  [1,17] (if = -1, flags
C-                             invalid input combination)
C-
C-   Created  19-MAY-1988   A.P.White
C-   Updated  13-JAN-1989   K. Wyatt Merritt
C-   Updated   7-DEC-1990   Joan Guida, Chip Stewart - address fix 
C-----------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:CUNFLG.INC'
      INTEGER DEPTH,IETAC,ILYRC
      INTEGER IE,IER
      INTEGER IDL(0:11,37)    ! Conversion  array for DEPTH --> ILYRC:
C                     ! 1st index=DEPTH [0,11]; 2nd=ABS(IETAC) [1,37];
C                     ! value = ILYRC
      LOGICAL FIRST
      INTEGER IDLV001(0:11) ! Correction array for IETA =12 
      INTEGER IDLV002(2:5)  ! Correction array for MC SFTVSN=1,2 IETA 
      INTEGER IDLV003(3:6)  ! Correction array for MC SFTVSN=1,2
      INTEGER IDLV004(9:11) ! Correction array for MC SFTVSN=1,2 IETA = 18
      INTEGER IDLV005(9:11) ! Correction array for MC SFTVSN=1,2 IETA = 19
      INTEGER IDLEM_M(2:5)  ! Correction array for MC SFTVSN=1,2
C 
C-----------------------------------------------------------------
      DATA FIRST/.TRUE./
C------------------------------------------------------------------------------
C LOOK-UP TABLES for converting ELECTRONICS ADDRESSES to PHYSICS ADDRESSES
C               (DEPTH,IETA)--> ILYR
C------------------------------------------------------------------------------
      DATA IDL /
     & 1, 2, 4, 3, 6, 5, 7,11,12,13,15,-1, ! IETA  = 1      CC Crates
     & 1, 2, 4, 3, 6, 5, 7,11,12,13,15,-1, !       = 2          |
     & 1, 2, 4, 3, 6, 5, 7,11,12,13,15,-1, !       = 3         \ /
     & 1, 2, 4, 3, 6, 5, 7,11,12,13,15,-1, !       = 4          |
     & 1, 2, 4, 3, 6, 5, 7,11,12,13,15,-1, !       = 5
     & 1, 2, 4, 3, 6, 5, 7,11,12,13,15,-1, !       = 6
     & 1, 2, 4, 3, 6, 5, 7,11,12,13,-1,-1, !       = 7
     & 1, 2, 4, 3, 6, 5, 7,11,12,13,15,-1, !       = 8; includes 1 EC sig
     & 1, 2, 4, 3, 6, 5, 7,11,12,15,16,-1, !       = 9; includes 2 EC sig
     & 1, 2, 4, 3, 6, 5, 7,11,15,16,-1,-1, !       =10; includes 2 EC sig
     & 1, 2, 4, 3, 6, 5, 7,11,15,16,17,-1, !       =11; includes 3 EC sig
     & 1, 2, 4, 3,11,12,15,16,17,-1,-1,-1, !       =12; includes 3 EC sig
     &-1,-1,-1,-1,-1,-1,11,12,13,14,16,17, !       =13      EC Crates
     &-1,-1,-1, 6, 5, 7,11,12,13,14,15,17, !       =14          |       ! ?????
     & 1, 2, 4, 3, 6, 5, 7,11,12,13,14,15, !       =15          |
     & 1, 2, 4, 3, 6, 5, 7,11,12,13,14,15, !       =16         \ /
     & 1, 2, 4, 3, 6, 5, 7,11,12,13,14,15, !       =17          |
     & 1, 2, 4, 3, 6, 5, 7,11,12,-1,14,15, !       =18
     & 1, 2, 4, 3, 6, 5, 7,11,12,13,-1,15, !       =19
     & 1, 2, 4, 3, 6, 5, 7,11,12,13,14,15, !       =20
     & 1, 2, 4, 3, 6, 5, 7,11,12,13,14,15, !       =21
     & 1, 2, 4, 3, 6, 5, 7,11,12,13,14,15, !       =22
     & 1, 2, 4, 3, 6, 5, 7,11,12,13,14,15, !       =23
     & 1, 2, 4, 3, 6, 5, 7,11,12,13,14,15, !       =24
     & 1, 2, 4, 3, 6, 5, 7,11,12,13,14,15, !       =25
     & 1, 2, 4, 3, 6, 5, 7,11,12,13,14,15, !       =26
     & 1, 2, 3,-1,-1,-1, 7,11,12,13,14,15, !       =27
     & 1, 2, 3,-1,-1,-1, 7,11,12,13,14,15, !       =28
     & 1, 2, 3,-1,-1,-1, 7,11,12,13,14,15, !       =29
     & 1, 2, 3,-1,-1,-1, 7,11,12,13,14,15, !       =30
     & 1, 2, 3,-1,-1,-1, 7,11,12,13,14,15, !       =31
     & 1, 2, 3,-1,-1,-1, 7,11,12,13,14,15, !       =32
     & 1, 2, 3, 7,-1,-1,-1,11,12,13,14,15, !       =33
     & 1, 2, 3, 7,-1,-1,-1,11,12,13,14,15, !       =34
     & 1, 2, 3, 7,-1,-1,-1,11,12,13,14,15, !       =35
     &-1,-1,-1,-1,-1,-1,-1,11,12,13,14,15, !       =36
     &-1,-1,-1,-1,-1,-1,-1,-1,-1,13,14,15/ !       =37
      DATA IDLV001/              !CORRECTION FOR ETA 12 SOFTWARE VERSION 1 CAD
     & 1, 2, 3, 4,-1,-1,-1,11,12,15,16,17/
      DATA IDLV002/ 3, 4, 5, 6/  ! D=2-5 CORRECTION FOR EM 3 MC SFTVSN 1,2
      DATA IDLV003/-1,-1,-1, 7/  ! D=3-6 ETA=33-35  FOR EM 4 MC SFTVSN 1,2
      DATA IDLEM_M/ 5, 6, 3, 4/  ! D=2-5 CORRECTION FOR MINUS IETAC 
      DATA IDLV004/14,15,-1 /    ! D=9-11 CORRECTION  FOR IETA=18 MC SFTVSN 1,2
      DATA IDLV005/13,15,-1 /    ! D=9-11 CORRECTION  FOR IETA=19 MC SFTVSN 1,2
C-----------------------------------------------------------------------
C
      IF(FIRST) THEN
        FIRST = .FALSE.
        IF(SFTVSN.EQ.1 .AND. BTEST(D0VSN,5) ) THEN
          CALL UCOPY ( IDLV001(0),IDL(0,12), 12)        ! IE = 12
        END IF
        IF(SFTVSN.LE.2 .AND. BTEST(D0VSN,5) ) THEN
          DO IE = 1, 11
            CALL UCOPY ( IDLV002(2),IDL(2,IE), 4)       ! IE = 1-11, D= 2-5
            CALL UCOPY ( IDLV002(2),IDL(2,IE+14), 4)    ! IE =15-25, D= 2-5
          END DO
          CALL UCOPY ( IDLV002(4),IDL(3,14),  2)        ! IE =14, D=2,3
          CALL UCOPY ( IDLV002(2),IDL(2,12) , 2)        ! IE =12, D=2,3
          CALL UCOPY ( IDLV002(2),IDL(2,26) , 4)        ! IE =26, D= 2-5
          CALL UCOPY ( IDLV002(2),IDLEM_M(2), 4)        ! IETAC<0, D= 2-5
          DO IE = 33,35
            CALL UCOPY ( IDLV003(3),IDL(3,IE), 4)       ! IE =33-35, D= 3-6
          END DO
          CALL UCOPY ( IDLV004(9),IDL(9,18), 3)         ! IE = 18, D= 9-11
          CALL UCOPY ( IDLV005(9),IDL(9,19), 3)         ! IE = 19, D= 9-11
        END IF
      END IF
      IE = ABS(IETAC)                   ! Table is indexed on ABS(IETAC)
C
      IF (IE.EQ.0 .OR. IE.GT.37 .OR. DEPTH.LT.0 .OR. DEPTH.GT.11)
     &   THEN ! Check for invalid input variables;
        ILYRC = -2      ! set variable to invalid value
        GO TO 999                       ! return
      ENDIF
C
      ILYRC = IDL(DEPTH,IE)             ! Look up ILYRC in the table
C
      IF ((IETAC.LT.0).AND.(IETAC.NE.-14).AND.(IETAC.GT.-27)
     &  .AND.(DEPTH.GT.1).AND.(DEPTH.LT.6)) THEN    
C
C ****  North -eta,-z EM floor 3 fix
C
        IF(ILYRC.GE.3.AND.ILYRC.LE.6) ILYRC = IDLEM_M(DEPTH)
      END IF
C
      IF (IETAC .EQ. -12) THEN    ! Overwrite the values in the table
        IF (ILYRC .EQ. 3) ILYRC = 5 ! to allow for asymmetry between
        IF (ILYRC .EQ. 4) ILYRC = 6 ! IETAC = + 12 and -12
C
      ELSE IF (IETAC .EQ. -14) THEN   ! Overwrite the values in the table
        IF (ILYRC .EQ. 5) ILYRC = 3 ! to allow for asymmetry between
        IF (ILYRC .EQ. 6) ILYRC = 4 ! IETAC = + 14 and -14 
      END IF
C
C
  999 RETURN
      END
