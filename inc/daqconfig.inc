C	===================================================================
C	Configuration Parameter Definitions for the Data Acquisition System
C	===================================================================
C
C	Modulename:-
C	============
C	DAQ$CMS:DAQCONFIG.INC
C
C	Description:-
C	=============
C	This Include File contains all the Installation-specific
C	Parameter Definitions (hopefully).
C
C	Creation Date : 11 Dec 1984
C	===========================
C
C	Modified      :  8 Dec 1986
C	===========================
C	Set the D0 parameters -- J. Featherly
C
C-----------------------------------------------------------------------
C
C	DAQ_EVENT_BUFFER_SIZE determines the size of the Global Event Buffer
C
      Parameter DAQ_EVENT_BUFFER_SIZE	= 262144
C
C-----------------------------------------------------------------------
C
C	DAQ_MAX_RESERVE_SIZE determines how much space may be reserved
C	by a single Producer Process.
C
      Parameter DAQ_MAX_RESERVE_SIZE	= DAQ_EVENT_BUFFER_SIZE
C
C-----------------------------------------------------------------------
C
C	DAQ_MAX_PROCESSES determines the Number of allowed Consumer (or
C	Producer) Processes on this Computer. Note that one of these
C	slots is always reserved for Consumers to Include themselves,
C	and that the Buffer Multiplexor occupies another slot. Thus the
C	effective number of Consumers is 2 less than DAQ_MAX_PROCESSES.
C
      Parameter DAQ_MAX_PROCESSES	= 12
C
C-----------------------------------------------------------------------
C
C	DAQ_MAX_REQUIRE determines the Number of allowed Requirements
C	per Consumer on this Computer. This should not exceed 32 unless
C	the code for DAQ_STATISTICS is also changed (two dimension statements).
C
      Parameter DAQ_MAX_REQUIRE		= 8
C
C-----------------------------------------------------------------------
C
C	DAQ_MAX_EVENTS determines the maximum Number of Events allowed in
C	the Global Event Buffer. While the System will operate if more
C	than this number is attempted, it will operate inefficiently.
C
      Parameter DAQ_MAX_EVENTS		= 256
C
C-----------------------------------------------------------------------
C
      Parameter DAQ_LOG_NAME		= 'Buffer_Multiplx'
C
C-----------------------------------------------------------------------
C
C	DAQ_VAX_NAME is the Destination Name of this VAX Computer in a
C	Buffer Manager and Multi-VAX Configuration. It determines whether
C	this VAX is an explicit destination of an Event.
C
      Parameter DAQ_VAX_NAME		= 'CUR:'
C
C-----------------------------------------------------------------------
C
C	DAQ_RID_NAME is the Rights Identifier Name associated with the
C	DAQ Package. It is the Token (Granted to a VAX Account by the
C	AUTHORIZE Utility) that, depending on the DAQ_BIT_RID Mode Bit
C	defined later, allows a Consumer Process to request inclusion
C	as a VIP Consumer.
C
      Parameter DAQ_RID_NAME		= 'D0$DAQ_VIP'
C
C-----------------------------------------------------------------------
C
C	DAQ_BASE_PRIO is the Base Priority to which DAQ Processes will
C	be raised upon Inclusion into the DAQ System. Their original
C	Base Priority will be restored when they Exclude themselves,
C	or by the Exit Handler if they terminate abnormally (e.g. Ctrl-Y).
C	Note that, if a Processes Base Priority already exceeds 
C	DAQ_BASE_PRIO, then it will be unchanged.
C
      Parameter DAQ_BASE_PRIO		= 4
C
C-----------------------------------------------------------------------
C
C	DAQ_WAIT_TIMEOUT is the length of time that a process waiting for
C	space will hibernate before checking other processes in the DAQ
C	system to ensure that no lockup has occurred because of a process
C	falling off. It is in the standard VAX/VMS delta-time format.
C
      Parameter DAQ_WAIT_TIMEOUT	= '0 00:00:10.00'
C
C-----------------------------------------------------------------------
C
C	DAQ_HOLE_QUANTUM is the smallest hole that is allowed in 
C	non-Page-Aligned mode of operation. It prevents too much
C	fragmentation of the Global Event Buffer.
C
      Parameter DAQ_HOLE_QUANTUM	= 32	
C
C-----------------------------------------------------------------------
C
C	DAQ_HOLE_HEADER is the size of the Header attached to each Event.
C	This has a minimum allowed value of 1, being the header required
C	to contain the Event Length Word. Additional header words may
C	preceed this Length Word (e.g. to form a physical record header).
C	Note that the Event Length Word always is at the end of this header,
C	immediately preceeding the Event Data.
C
      Parameter DAQ_HOLE_HEADER		= 6	
C
C-----------------------------------------------------------------------
C
C	DAQ_DEFAULT_EVENT_TYPE determines the Event Type corresponding
C	to an Implicit Requirement.
C
      Parameter DAQ_DEFAULT_EVENT_TYPE	= 0	
C
C-----------------------------------------------------------------------
C
C	DAQ_TOP_EVENT_TYPE determines the highest Event Type that
C	will be considered an Event, with associated Trigger Mask etc.
C	All Event Types greater than this are considered non-Events and
C	the Trigger Mask only specifies a range of Event Types for
C	acceptance of a Requirment.
C
      Parameter DAQ_TOP_EVENT_TYPE	= 1	
C
C-----------------------------------------------------------------------
C
C	DAQ Mode Bits
C	=============
C
C	The TRACE Bit determines whether a detailed traceback buffer
C	is kept, describing the last "n" DAQ transactions. This is
C	not currently implemented.
C
      Parameter DAQ_BIT_TRACE		= 0	
C
C-----------------------------------------------------------------------
C
C	The LOCAL Bit determines whether the DAQ System operates
C	totally locally, or whether a Buffer Manager is also
C	present. If LOCAL is set, no communication with the
C	Buffer Manager occurs, and statistics etc. are accumulated
C	locally.
C
      Parameter DAQ_BIT_LOCAL		= 1	
C
C-----------------------------------------------------------------------
C
C	The BUFMAN Bit determines whether the Buffer Manager is
C	implemented in Software or Hardware. If BUFMAN is set
C	a Hardware Buffer Manager is implemented on FASTBUS and
C	communication to and from it takes the form of FASTBUS
C	interrupt messages. Otherwise the Buffer Manager is 
C	implemented in software and communication to and from it
C	takes the form of message queues in a Global Section.
C	Note that BUFMAN is only relevant when LOCAL is reset.
C
      Parameter DAQ_BIT_BUFMAN		= 0	
C
C-----------------------------------------------------------------------
C
C	The ALIGN Bit determines whether Events within the Global
C	Event Buffer are Page-Aligned or not. If ALIGN is set 
C	Events will be Page-Aligned, otherwise not. In the latter
C	case, DAQ_HOLE_QUANTUM is used to determine the smallest
C	Hole size in order to prevent too much space fragmentation.
C
      Parameter DAQ_BIT_ALIGN		= 1	
C
C-----------------------------------------------------------------------
C
C	The DELAY Bit determines whether unwanted Events are immediately
C	dropped or are kept around until the space is required. If DELAY
C	is set, unwanted Events are kept until the occupied space is
C	required by another Producer.
C
      Parameter DAQ_BIT_DELAY		= 1	
C
C-----------------------------------------------------------------------
C
C	The DEFREQ Bit determines whether an Implicit Requirement is
C	allowed, or whether all Consumers must explicitly state
C	requirements via DAQ_ADD_REQ or DAQ_ADD_VIP. If DEFREQ is
C	set then an implicit requirement is allowed and corresponds
C	to Events with Event Type DAQ_DEF_TYPE.
C
      Parameter DAQ_BIT_DEFREQ		= 1	
C
C-----------------------------------------------------------------------
C
C	The 3264 and 64128 Bits determine the initial default for Trigger
C	Masks. The settings of these bits determine how many Trigger Mask
C	Bits (32,64 or 128) are significant when making event matches
C	unless the Consumer explicitly modifies this default by calling
C	DAQ_MASK_MODE. The following bit combinations are allowed:-
C
C	3264 Reset, 64128 Reset ==>  32 Trigger Mask Bits
C	3264   Set, 64128 Reset ==>  64 Trigger Mask Bits
C	3264   Set, 64128   Set ==> 128 Trigger Mask Bits
C
      Parameter DAQ_BIT_3264		= 0	
      Parameter DAQ_BIT_64128		= 0	
C
C-----------------------------------------------------------------------
C
C	The VIP, RID & UIC Bits together determine the characteristics that
C	a Consumer Process must possess before it will be allowed to make
C	VIP Requests (DAQ_ADD_VIP, DAQ_RESERVE_EVENT etc.). The various
C	settings of these bits are as follows:-
C
C	DAQ_BIT_VIP Set		Process MUST call DAQ_INCLUDE_VIP or
C				DAQ_CONNECT_VIP rather than DAQ_INCLUDE
C				or DAQ_CONNECT.
C
C	DAQ_BIT_RID Set		Process must have access to DAQ Package
C				Rights Identifier (as defined by 
C				DAQ_RID_NAME).
C
C	DAQ_BIT_UIC Set		Process must have same UIC as Buffer
C				Multiplexer.
C
C	Any combination of these Bits may be setup for a particular
C	installation.
C
      Parameter DAQ_BIT_VIP		= 0	
      Parameter DAQ_BIT_RID		= 1	
      Parameter DAQ_BIT_UIC		= 0	
C
C-----------------------------------------------------------------------
C
C	The TOPVIP Bit determines whether a non-VIP Process may make
C	VIP Requirement Requests for Events with Event Type > TOPTYP.
C	This allows these Consumers to request to see all Run Startup
C	Records etc.
C	If TOPVIP is set then Non-VIP Processes can issue VIP Requests
C	for Event Types > TOPTYP.
C
      Parameter DAQ_BIT_TOPVIP          = 1
C
C-----------------------------------------------------------------------
C
C	The ASWAKE Bit determines whether a Process making an Asynchronous
C	Request (either for an EVENT or for SPACE) will be woken by the
C	availability of the appropriate Event or Space, or whether it will
C	have to use DAQ_TEST_xxxx to determine whether its request has
C	been satisfied. This allows a Process to perform a DAQ_GET_EVENT,
C	perform some other duties, call SYS$HIBER and be activated when an
C	Event is available.
C	If ASWAKE is set then the process will be woken.
C
      Parameter DAQ_BIT_ASWAKE          = 1     
C
C-----------------------------------------------------------------------
C
