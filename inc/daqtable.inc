C	==================================================
C	Global Common Block Definitions for the DAQ System
C	==================================================
C
C	Modulename:-
C	============
C	DAQ$CMS:DAQTABLE.INC
C
C	Description:-
C	=============
C	This Include File contains the DAQ System Control Table
C	Common Block Definitions. The size of this Table is 
C	deteremined by PArameters defined in the DAQCONFIG.INC
C	Include File.
C
C	Creation Date :  8 Mar 1983
C	===========================
C
      Parameter         TABLE_SIZE =   DAQ_HEAD_LENG                 +
     &                                 DAQ_MAX_PROCESSES*CON_CT_LENG +
     &                 DAQ_MAX_REQUIRE*DAQ_MAX_PROCESSES*REQ_CT_LENG +
     &                                 DAQ_MAX_EVENTS*EVT_CT_LENG
      Parameter         TABLE_PAGE_SIZE = 128*((TABLE_SIZE+127)/128)
      Common/DAQ_TABLES/DAQ_TABLES(0:TABLE_PAGE_SIZE-1)
      Integer*4         DAQ_TABLES
C
