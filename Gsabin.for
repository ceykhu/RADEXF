      SUBROUTINE GSABIN(INDATA,LUSTIN,LUSTOT,LU,LUSAVE,NCGCX,NCGCY,
     1                  NCGCZ,CGX,CGY,CGZ,VOLUME,GX,GY,GZ,ZONEV,NVMAX,
     2                  NZMAX,NATMAX,FILE1,FILE2,NATM,GASK,FINGSK)
C
C  Routine to control the input of the gas absorptivity data
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**22/02/91
C
C
      INTEGER INDATA,LUSTIN,LUSTOT,LU,LUSAVE,NATMAX,NATM,NVMAX,NZMAX,
     1        NCGCX,NCGCY,NCGCZ,CGX(NVMAX),CGY(NVMAX),CGZ(NZMAX)
      REAL GASK(NATMAX,NVMAX,NVMAX,NZMAX),ZONEV(2,NVMAX,NVMAX,NZMAX),
     1     VOLUME(NVMAX,NVMAX,NZMAX),FINGSK(NATMAX,NVMAX,NVMAX,NZMAX)
      CHARACTER*30 FILE1,FILE2
      CHARACTER*1 YORN
      LOGICAL FRMFIL,SAVE
      INTEGER I,J,K,INOPT
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  CGX     Array of fine grid line numbers of x coarse grid lines
C  CGY     Array of fine grid line numbers of y coarse grid lines
C  CGZ     Array of fine grid line numbers of z coarse grid lines
C  FINGSK  Array to hold the fine grid gas absorptivities
C  GASK    Array to hold the coarse grid gas absorptivities
C  GX      Array of fine grid x co-ordinates
C  GY      Array of fine grid y co-ordinates
C  GZ      Array of fine grid z co-ordinates
C  INDATA  Flag indicating mode of data entry 
C          1-interactive, 2-quasi-interactive, 3-non-interactive
C  LU      Logical unit number for data file
C  LUSAVE  Logical unit number for file created from input data
C  LUSTIN  Logical unit number for standard input
C  LUSTOT  Logical unit number for standard output
C  NATMAX  Maximum number of atmospheres permitted
C  NCGCX   Number of coarse grid x cells
C  NCGCY   Number of coarse grid y cells
C  NCGCZ   Number of coarse grid z cells
C  NVMAX   Maximum number of vertices allowed
C  NZMAX   Maximum number of z slices allowed
C  VOLUME  Array of coarse grid cell volumes
C  ZONEV   Array to hold data entered on a zone by zone basis
C
C**** Arguments - output
C  FILE1   Name of file read from
C  FILE2   Name of file written to
C  GASK    Array of gas absorptivities for the different atmospheres
C  NATM    Number of atmospheres defined
C
C**** Local variables
C  FRMFIL  Logical flag indicating if data is from a file
C  I       Loop counter on x coarse grid cells
C  INOPT   Input option when data entry mode is quasi-interactive
C  J       Loop counter on y coarse grid cells
C  K       Loop counter on z coarse grid cells
C  SAVE    Logical flag indicating if data should be saved when INDATA=2
C  YORN    Character variable wilth value Y or N (yes or no)
C
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  CLEAR   Clear the screen
C  CRFNGK  Transfer coarse grid absorptivity data to the fine grid
C  GSABFL  Read gas absorptivity data from a file
C  GSABTM  Read gas absorptivity data from the terminal
C  OPENFL  Open a file on a particular logical unit number
C  PRSRTC  Display message 'Press return to continue'
C
C******************************************************************************

C**** Give introductory message
      CALL CLEAR(LUSTOT)
      WRITE (LUSTOT,10)
10    FORMAT (' GAS ABSOPTIVITY DEFINITION PHASE'/
     1        ' ================================'/)

C**** Initialise the file names
      FILE1='NOT USED'
      FILE2='NOT USED'

C**** Determine where the data is coming from and if it should be saved
      IF (INDATA.EQ.1) THEN
C****    Interactive data entry (ie from terminal)
         FRMFIL=.FALSE.
         SAVE=.TRUE.
      ELSE IF (INDATA.EQ.2) THEN
C****    Quasi-interactive data entry (ie from terminal and/or file)
20       WRITE (LUSTOT,30)
30       FORMAT (' Gas emissivity data may be entered from:'/
     1           '     1.   The terminal only'/
     2           '     2.   A file then more from the terminal'/
     3           '     3.   A file only'/
     4           ' Enter option number (1-3) >> ',$)
         READ (LUSTIN,*) INOPT
         IF ((INOPT.LT.1).OR.(INOPT.GT.3)) THEN
            WRITE (LUSTOT,40)
40          FORMAT (/' INVALID RESPONSE'
     1              /' Answer must be in range 1-3'/)
            GOTO 20
         ELSE IF (INOPT.EQ.1) THEN
            FRMFIL=.FALSE.
            SAVE=.TRUE.
         ELSE IF (INOPT.EQ.2) THEN
            FRMFIL=.TRUE.
            SAVE=.TRUE.
         ELSE
            FRMFIL=.TRUE.
            SAVE=.FALSE.
         END IF
      ELSE
C****    Non-interactive data entry (ie from file)
         FRMFIL=.TRUE.
         SAVE=.FALSE.
      END IF

C**** Set all absorptivites for first atmopshere to zero
      NATM=1
      DO 70 K=1,NCGCZ
         DO 60 J=1,NCGCY
            DO 50 I=1,NCGCX
               GASK(NATM,I,J,K)=0.
50          CONTINUE
60       CONTINUE
70    CONTINUE

C**** If any data is to come from a file open the file
      IF (FRMFIL) THEN
         WRITE (LUSTOT,80)
80       FORMAT (/' Some/all gas absorptivity data is from a file'/)
         CALL OPENFL(LUSTIN,LUSTOT,LU,FILE1,2,0)
C****    If there is data from the terminal as well all the data
C****    is saved together in a new file - open this file
         IF (SAVE) THEN
            WRITE (LUSTOT,90)
90          FORMAT (/' The gas absorptivity data is from a file',
     1               ' and the terminal'/
     2               ' It will all be saved together in a new file'/)
            CALL OPENFL(LUSTIN,LUSTOT,LUSAVE,FILE2,2,1)
         END IF
C****    Get the data from the file
         CALL GSABFL(SAVE,LU,LUSTOT,LUSAVE,NATM,NCGCX,NCGCY,NCGCZ,
     1               CGX,CGY,CGZ,VOLUME,GX,GY,GZ,ZONEV,NVMAX,NZMAX,
     2               NATMAX,GASK)
         CLOSE(LU)
      END IF

C**** There is data to read from the terminal if SAVE is true
      IF (SAVE) THEN
         IF (.NOT.FRMFIL) THEN
C****       Data is only from the terminal so the file in which it
C****       is to be saved has not yet been opened
            WRITE (LUSTOT,100)
100         FORMAT (/' Gas absorptivity data will be saved in a file'
     1              /)
            CALL OPENFL(LUSTIN,LUSTOT,LUSAVE,FILE2,2,1)
         END IF
         CALL GSABTM(FRMFIL,LUSTIN,LUSTOT,LUSAVE,NATM,NCGCX,NCGCY,NCGCZ,
     1               CGX,CGY,CGZ,VOLUME,GX,GY,GZ,ZONEV,NVMAX,NZMAX,
     2               NATMAX,GASK)
         CLOSE(LUSAVE)
      END IF

C**** Transfer the coarse grid absorptivity data to the fine grid
      CALL CRFNGK(GASK,NVMAX,NZMAX,NATMAX,NATM,NCGCX,NCGCY,NCGCZ,CGX,
     1            CGY,CGZ,FINGSK)

C**** Gas absorptivity data has now all been entered - give message
      WRITE (LUSTOT,110)
110   FORMAT (//' End of gas absorptivity definiton phase'//)
      CALL PRSRTC(LUSTIN,LUSTOT)

      RETURN
      END


      SUBROUTINE GSABTM(FRMFIL,LUSTIN,LUSTOT,LUSAVE,NATM,NCGCX,NCGCY,
     1                  NCGCZ,CGX,CGY,CGZ,VOLUME,GX,GY,GZ,ZONEV,NVMAX,
     2                  NZMAX,NATMAX,GASK)
C
C  Routine to read the gas absorptivity data from the terminal
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**16/04/91
C
C
      INTEGER LUSTIN,LUSTOT,LUSAVE,NATM,NCGCX,NCGCY,NCGCZ,
     1        CGX(NVMAX),CGY(NVMAX),CGZ(NZMAX),NVMAX,NZMAX,NATMAX
      REAL VOLUME(NVMAX,NVMAX,NZMAX),GX(NVMAX),GY(NVMAX),GZ(NZMAX),
     1     ZONEV(2,NVMAX,NVMAX,NZMAX),GASK(NATMAX,NVMAX,NVMAX,NZMAX)
      INTEGER NEWATM(6),IOPT
      CHARACTER*1 YORN
      LOGICAL FRMFIL,TRMINL,SAVE
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  CGX     Array of fine grid line numbers of x coarse grid lines
C  CGY     Array of fine grid line numbers of y coarse grid lines
C  CGZ     Array of fine grid line numbers of z coarse grid lines
C  FRMFIL  Logical flag indicating if data has already been read from a file
C  GASK    Array to hold the gas absorptivities
C  GX      Array of fine grid x co-ordinates
C  GY      Array of fine grid y co-ordinates
C  GZ      Array of fine grid z co-ordinates
C  LUSAVE  Logical unit number for file created from input data
C  LUSTIN  Logical unit number for standard input
C  LUSTOT  Logical unit number for standard output
C  NATM    Number of atmospheres already set
C  NATMAX  Maximum number of atmospheres allowed
C  NCGCX   Number of coarse x grid cells
C  NCGCY   Number of coarse y grid cells
C  NCGCZ   Number of coarse z grid cells
C  NVMAX   Maximum number of vertices allowed
C  NZMAX   Maximum number of z slices allowed
C  VOLUME  Array of coarse grid cell volumes
C  ZONEV   Array to hold data entered on a zone by zone basis
C
C**** Arguments - output
C  GASK    Array of gas absorptivities
C  NATM    Number of atmospheres set
C
C**** Local variables
C  IOPT    Gas absorptivity input option number
C  SAVE    Logical flag indicating if data is to be saved
C  TRMINL  Logical flag indiciating if data is from the terminal
C  YORN    Character variable with value Y=YES or N=NO
C
C**** Local Arrays
C  NEWATM  Array of number of new atmospheres set by each gas absorptivity 
C          option
C
C**** Functions and Subroutines
C  ABOPT1  Input gas absorptivities via option 1 (uniform value)
C  ABOPT2  Input gas absorptivities via option 2 (zone by zone values)
C  ABOPT3  Input gas absorptivities via option 3 (sum of grey gas - uniform
C          excess air and no carbon)
C  ABOPT4  Input gas absorptivities via option 4 (sum of grey gas - uniform
C          excess air and uniform carbon)
C  ABOPT5  Input gas absorptivities via option 5 (sum of grey gas - uniform
C          excess air and zone by zone carbon)
C  ABOPT6  Input gas absorptivities via option 6 (sum of grey gas - zone by
C          zone excess air and carbon)
C  CLEAR   Clear the screen
C
C******************************************************************************

      DATA NEWATM/1,1,2,6,6,6/

C**** Data is from the terminal so should be saved - set logical flags
      SAVE=.TRUE.
      TRMINL=.TRUE.

C**** Record the number of coarse grid cells in the created input file
      IF (.NOT.FRMFIL) WRITE (LUSAVE,5) NCGCX,NCGCY,NCGCZ
5     FORMAT (1X,3(I3,2X),24X,'NO. OF COARSE X,Y,Z CELLS')

C**** Clear the screen and give introductory message
      CALL CLEAR(LUSTOT)
      WRITE (LUSTOT,10)
10    FORMAT (' INTERACTIVE GAS ABSORPTIVITY DEFINITION'/
     1        ' ======================================='/
     2        ' RADEXF will compute exchange areas for a number',
     3        ' of different atmospheres'/
     4        ' (ie a number of different gas absorptivity arrays)'/
     5        ' Exchange areas for a clear (non-participating) gas',
     6        ' are always calculated'/
     7        ' Exchange areas for other atmospheres are calculated',
     8        ' as requested'//
     9        ' A number of options are available for defining',$)
      WRITE (LUSTOT,20)
20    FORMAT (' participating atmospheres'/
     1        '      Absorptivity uniform throughout the enclosure'/
     2        '      Absorptivity specified zone by zone'/
     3        '      Absorptivity specified via a sum of grey gases'//
     4        ' When using a sum of grey gases the carbon',
     5        ' concentration must be specified'/
     6        ' If no carbon is present this creates 2 atmospheres',
     7        ' - one for each grey gas'/
     8        ' If carbon is present this creates 6 atmospheres',
     9        ' - two each for the clear gas'/' and the 2 grey gases'/)
30    WRITE (LUSTOT,40) NATMAX
40    FORMAT (' Up to ',I3,' different atmospheres may be defined'/
     1        ' If this is not enough abort this run and change the',
     2        ' value of the parameter'/' NATMAX in the file',
     3        ' comvar.for and recompile the whole program'//
     4        ' DO YOU WISH TO ABORT (Y/N)? >> ',$)
      READ (LUSTIN,50) YORN
50    FORMAT (A1)
      IF ((YORN.EQ.'Y').OR.(YORN.EQ.'y')) THEN
         WRITE (LUSTOT,60)
60       FORMAT (/' Aborting run as requested'/)
         STOP
      ELSE IF ((YORN.EQ.'N').OR.(YORN.EQ.'n')) THEN
         CONTINUE
      ELSE
         WRITE (LUSTOT,70)
70       FORMAT (/' Invalid response - Answer Y or N'/)
         GOTO 30
      END IF

C**** Give message telling number of atmopsheres already set
      IF (NATM.EQ.1) THEN
         WRITE (LUSTOT,80)
80       FORMAT (//' Exchange areas for a clear gas are',
     1           ' automatically produced')
      ELSE
         WRITE (LUSTOT,90) NATM
90       FORMAT (//1X,I3,' atmopsheres have been read from a file')
      END IF

C**** Determine if there is room for any more atmopsheres
100   IF (NATM.EQ.NATMAX) THEN
         WRITE(LUSTOT,110)
110      FORMAT (/' All available atmopsheres have been defined'//
     1           ' Gas absorptivity definition phase is complete'//)
         CALL PRSRTC(LUSTIN,LUSTOT)
         WRITE (LUSAVE,115)
115      FORMAT (1X,'0',38X,'END OF GAS ABSORPTIVITIES')
         RETURN
      END IF

C**** See if further atmospheres are to be defined
      WRITE (LUSTOT,120) NATMAX,NATM,NATMAX-NATM
120   FORMAT (/' Up to ',I3,' different atmospheres may be defined'/
     1        ' So far ',I3,' have been set'/
     2        ' Hence ',I3,' more may be defined'/
     3        ' A sum of grey gases including carbon needs 6'/
     4        ' A sum of grey gases with no carbon needs 2'/
     5        ' Other specifications need just 1 atmosphere'/)
125   WRITE (LUSTOT,130)
130   FORMAT (' Are any more atmospheres required (Y/N)?  >> ',$)
140   READ (LUSTIN,150) YORN
150   FORMAT (A1)
      IF ((YORN.EQ.'Y').OR.(YORN.EQ.'y')) THEN
         CONTINUE
      ELSE IF ((YORN.EQ.'N').OR.(YORN.EQ.'n')) THEN
         WRITE (LUSAVE,165)
165      FORMAT (1x,'0',38X,'END OF GAS ABSORPTIVITIES')
         RETURN
      ELSE
         WRITE (LUSTOT,170)
170      FORMAT (/' Invalid Response - Answer Y or N'/)
         GOTO 125
      END IF

C**** Determine the details of the next atmopshere
      WRITE (LUSTOT,180)
180   FORMAT (//' The options for specifying the gas',
     1        ' absorptivities are: '/
     2        '   1. Specify a single value for the whole enclosure'/
     3        '   2. Specify on a zone-by-zone basis'/
     4        '   3. Sum of grey gases, uniform excess air &',
     5             ' no carbon'/
     6        '   4. Sum of grey gases, uniform excess air &',
     7             ' uniform carbon concentration'/
     8        '   5. Sum of grey gases, uniform excess air &',
     9             ' varying carbon concentration')
      WRITE (LUSTOT,185)
185   FORMAT ('   6. Sum of grey gases, varying excess air &',
     1             ' varying carbon concentration'/)
190   WRITE (LUSTOT,200)
200   FORMAT (' Enter option number (1-6) >> ',$)
      READ (LUSTIN,*) IOPT
      IF ((IOPT.LT.1).OR.(IOPT.GT.6)) THEN
         WRITE (LUSTOT,210)
210      FORMAT (/' Invalid Response - Answer in range 1-6'/)
         GOTO 190
      END IF

C**** Check that there is enough space left in the absorptivity array
      IF ((NATM+NEWATM(IOPT)).GT.NATMAX) THEN
         WRITE (LUSTOT,220) NATMAX-NATM,IOPT,NEWATM(IOPT)
220      FORMAT (' There are only ',I1,' atmospheres left to be set'/
     1           ' Option ',I1,' requires ',I1,' atmospheres and so ',
     2           ' cannot be processed'/)
         GOTO 100
      END IF
      WRITE (LUSAVE,225) IOPT
225   FORMAT (1X,I1,38X,'GAS ABSORPTIVITY OPTION')

C**** Process the selected option
      IF (IOPT.EQ.1) THEN
         CALL ABOPT1(TRMINL,SAVE,LUSTIN,LUSTOT,LUSAVE,NATM,
     1               NCGCX,NCGCY,NCGCZ,NATMAX,NVMAX,NZMAX,GASK)
      ELSE IF (IOPT.EQ.2) THEN
         CALL ABOPT2(TRMINL,SAVE,LUSTIN,LUSTOT,LUSAVE,NATM,
     1               NCGCX,NCGCY,NCGCZ,CGX,CGY,CGZ,VOLUME,GX,GY,
     2               GZ,ZONEV,NVMAX,NZMAX,NATMAX,GASK)
      ELSE IF (IOPT.EQ.3) THEN
         CALL ABOPT3(TRMINL,SAVE,LUSTIN,LUSTOT,LUSAVE,NATM,
     1               NCGCX,NCGCY,NCGCZ,NATMAX,NVMAX,NZMAX,GASK)
      ELSE IF (IOPT.EQ.4) THEN
         CALL ABOPT4(TRMINL,SAVE,LUSTIN,LUSTOT,LUSAVE,NATM,
     1               NCGCX,NCGCY,NCGCZ,NATMAX,NVMAX,NZMAX,GASK)
      ELSE IF (IOPT.EQ.5) THEN
         CALL ABOPT5(TRMINL,SAVE,LUSTIN,LUSTOT,LUSAVE,NATM,
     1               NCGCX,NCGCY,NCGCZ,CGX,CGY,CGZ,VOLUME,GX,GY,
     2               GZ,ZONEV,NVMAX,NZMAX,NATMAX,GASK)
      ELSE IF (IOPT.EQ.6) THEN
         CALL ABOPT6(TRMINL,SAVE,LUSTIN,LUSTOT,LUSAVE,NATM,
     1               NCGCX,NCGCY,NCGCZ,CGX,CGY,CGZ,VOLUME,GX,GY,
     2               GZ,ZONEV,NVMAX,NZMAX,NATMAX,GASK)
      END IF
C**** Update the number of atmospheres set so far
      NATM=NATM+NEWATM(IOPT)

C**** Go back and see if any further atmopsheres are to be defined
      GOTO 100

      END
 
      SUBROUTINE GSABFL(SAVE,LUIN,LUSTOT,LUSAVE,NATM,NCGCX,NCGCY,
     1                  NCGCZ,CGX,CGY,CGZ,VOLUME,GX,GY,GZ,ZONEV,
     2                  NVMAX,NZMAX,NATMAX,GASK)
C
C  Routine to read the gas absorptivity data from a file
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**16/04/91
C
C
      INTEGER LUIN,LUSTOT,LUSAVE,NATM,NCGCX,NCGCY,NCGCZ,
     1        CGX(NVMAX),CGY(NVMAX),CGZ(NZMAX),NVMAX,NZMAX,NATMAX
      REAL VOLUME(NVMAX,NVMAX,NZMAX),GX(NVMAX),GY(NVMAX),GZ(NZMAX),
     1     ZONEV(2,NVMAX,NVMAX,NZMAX),GASK(NATMAX,NVMAX,NVMAX,NZMAX)
      LOGICAL SAVE
      INTEGER IOPT,NEWATM(6),NCXCFL,NCYCFL,NCZCFL
      LOGICAL TRMINL,RTSIZE
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  CGX     Array of fine grid line numbers of x coarse grid lines
C  CGY     Array of fine grid line numbers of y coarse grid lines
C  CGZ     Array of fine grid line numbers of z coarse grid lines
C  GASK    Array to hold the gas absorptivities
C  GX      Array of fine grid x co-ordinates
C  GY      Array of fine grid y co-ordinates
C  GZ      Array of fine grid z co-ordinates
C  LUIN    Logical unit number for input
C  LUSAVE  Logical unit number for file created from input data
C  LUSTOT  Logical unit number for standard output
C  NATM    Number of atmospheres already set
C  NATMAX  Maximum number of atmopsheres allowed
C  NCGCX   Number of coarse grid x cells
C  NCGCY   Number of coarse grid y cells
C  NCGCZ   Number of coarse grid z cells
C  NVMAX   Maximum number of vertices allowed
C  NZMAX   Maximum number of z slices allowed   
C  SAVE    Logical flag indicating if data read from a file is to be saved 
C  VOLUME  Array of coarse grid cell volumes
C  ZONEV   Array to hold data entered on a zone by zone basis
C  
C**** Arguments - output
C  GASK    Array of gas absorptivities
C  NATM    Number of atmospheres set
C
C**** Local variables
C  IOPT    Gas absorptivity option number
C  NCXCFL  Number of coarse grid x cells for data in input file
C  NCYCFL  Number of coarse grid y cells for data in input file
C  NCZCFL  Number of coarse grid z cells for data in input file
C  RTSIZE  Logical flag indicating if the number of coarse grid cells
C          in the input data file is the same as for the defined geometry
C  TRMINL  Logical flag indicating if data is from the terminal
C
C**** Local Arrays
C  NEWATM  Array of number of new atmospheres set by each gas absorptivity
C          option
C
C**** Functions and Subroutines
C  ABOPT1  Input gas absorptivities via option 1 (uniform value)
C  ABOPT2  Input gas absorptivities via option 2 (zone by zone value)
C  ABOPT3  Input gas absorptivities via option 3 (sum of grey gas - uniform
C          excess air and no carbon)
C  ABOPT4  Input gas absorptivities via option 4 (sum of grey gas - uniform
C          excess air and uniform carbon)
C  ABOPT5  Input gas absorptivities via option 5 (sum of grey gas - uniform
C          excess air and zone by zone carbon)
C  ABOPT6  Input gas absorptivities via option 6 (sum of grey gas - zone by
C          zone excess air and carbon)
C******************************************************************************

      DATA NEWATM /1,1,2,6,6,6/

C**** Data is from a file
      TRMINL=.FALSE.

C**** If an input file is being created record number of coarse grid cells
      IF (SAVE) WRITE (LUSAVE,5) NCGCX,NCGCY,NCGCZ
5     FORMAT (1X,3(I3,2X),24X,'NO. OF COARSE X,Y,Z CELLS')

C**** Read the number of coarse grid cells for data in input file
      READ (LUIN,*) NCXCFL,NCYCFL,NCZCFL

C**** Check that the number of coarse grid cells for the geometry is the
C**** same as for the data in the input file
      RTSIZE=.TRUE.
      IF (NCXCFL.NE.NCGCX) RTSIZE=.FALSE.
      IF (NCYCFL.NE.NCGCY) RTSIZE=.FALSE.
      IF (NCZCFL.NE.NCGCZ) RTSIZE=.FALSE.
      
C**** Determine option number      
10    READ(LUIN,*) IOPT
C**** If a zone by zone option has been selected the number of coarse grid
C**** cells in the input file must be the same as the current geometry
      IF (((IOPT.EQ.2).OR.(IOPT.EQ.5).OR.(IOPT.EQ.6))
     1    .AND.(.NOT.RTSIZE)) THEN
          WRITE (LUSTOT,7) NCGCX,NCGCY,NCGCZ,NCXCFL,NCYCFL,NCZCFL
7	  FORMAT (/' ERROR IN ABSORPTIVITY INPUT FILE'/
     1             ' Mismatch in number of coarse grid cells'/
     2             ' The geometry defined for this run has:'/
     3             5X,I3,' coarse x cells'/
     4             5X,I3,' coarse y cells'/
     5             5X,I3,' coarse z cells'/
     6             ' Input data file has:'/
     7             5X,I3,' coarse x cells'/
     8             5X,I3,' coarse y cells'/
     9             5X,I3,' coarse z cells')
         WRITE (LUSTOT,8) IOPT
8        FORMAT (' Requested absorptivity input option ',I1,
     1           ' requires zone by zone entry of data'/
     2           ' So there is the wrong amount of data in',
     3           ' the input file'/
     4           ' RUN ABORTED'/)
         STOP
      END IF
      IF ((IOPT.LT.0).OR.(IOPT.GT.6)) THEN
C****    Check option number is valid (0-6) are valid
C****    Option 0 means no more atmospheres
         WRITE (LUSTOT,20)
20	 FORMAT (/' INVALID GAS ABSORPTIVITY OPTION'/
     1           ' SHOULD BE IN RANGE 0-6'/
     2           ' RUN ABORTED')
         STOP
      ELSE IF (IOPT.NE.0) THEN
C****    Check enough space in arrays for requested option
         IF ((NATM+NEWATM(IOPT)).GT.NATMAX) THEN
	    WRITE (LUSTOT,30) NATMAX
30	    FORMAT (/' TOO MANY ATMOSPHERES REQUESTED'/
     1              ' MAXIMUM ALLOWED IS ',I3/
     2              ' INCREASE NATMAX IN comvar.for AND RECOMPILE'/
     3              ' RUN ABORTED')
            STOP
	 END IF
C****    When INDATA=2 and gas data is from file & terminal data from
C****    file must be saved in a new file
         IF (SAVE) WRITE (LUSAVE,40) IOPT
40	 FORMAT (1X,I1,38X,'GAS ABSORPTIVITY OPTION')

C****    Process requested option
         IF (IOPT.EQ.1) THEN
            CALL ABOPT1(TRMINL,SAVE,LUIN,LUSTOT,LUSAVE,NATM,
     1                  NCGCX,NCGCY,NCGCZ,NATMAX,NVMAX,NZMAX,GASK)
         ELSE IF (IOPT.EQ.2) THEN
            CALL ABOPT2(TRMINL,SAVE,LUIN,LUSTOT,LUSAVE,NATM,
     1                  NCGCX,NCGCY,NCGCZ,CGX,CGY,CGZ,VOLUME,GX,GY,
     2                  GZ,ZONEV,NVMAX,NZMAX,NATMAX,GASK)
         ELSE IF (IOPT.EQ.3) THEN
            CALL ABOPT3(TRMINL,SAVE,LUIN,LUSTOT,LUSAVE,NATM,
     1                  NCGCX,NCGCY,NCGCZ,NATMAX,NVMAX,NZMAX,GASK)
         ELSE IF (IOPT.EQ.4) THEN
            CALL ABOPT4(TRMINL,SAVE,LUIN,LUSTOT,LUSAVE,NATM,
     1                  NCGCX,NCGCY,NCGCZ,NATMAX,NVMAX,NZMAX,GASK)
         ELSE IF (IOPT.EQ.5) THEN
            CALL ABOPT5(TRMINL,SAVE,LUIN,LUSTOT,LUSAVE,NATM,
     1                  NCGCX,NCGCY,NCGCZ,CGX,CGY,CGZ,VOLUME,GX,GY,
     2                  GZ,ZONEV,NVMAX,NZMAX,NATMAX,GASK)
         ELSE IF (IOPT.EQ.6) THEN
            CALL ABOPT6(TRMINL,SAVE,LUIN,LUSTOT,LUSAVE,NATM,
     1                  NCGCX,NCGCY,NCGCZ,CGX,CGY,CGZ,VOLUME,GX,GY,
     2                  GZ,ZONEV,NVMAX,NZMAX,NATMAX,GASK)
         END IF

C****    Update number of atmopsheres
	 NATM=NATM+NEWATM(IOPT)
	 GOTO 10
      END IF

      RETURN
      END

      SUBROUTINE ABOPT1(TRMINL,SAVE,LUIN,LUSTOT,LUSAVE,NATM,NCGCX,
     1                  NCGCY,NCGCZ,NATMAX,NVMAX,NZMAX,GASK)
C
C  Routine to determine gas absorptivities via option 1 
C  ie gas absorptivity uniform throughout the enclosure
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**16/04/91
C
C
      INTEGER LUIN,LUSTOT,LUSAVE,NATM,NCGCX,NCGCY,NCGCZ,NATMAX,NVMAX,
     1        NZMAX
      REAL GASK(NATMAX,NVMAX,NVMAX,NZMAX)
      LOGICAL TRMINL,SAVE
      INTEGER I,J,K
      REAL KGASU
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  GASK    Array to hold the gas absorptivities
C  LUIN    Logical unit number for input
C  LUSAVE  Logical unit number for file created from input data
C  LUSTOT  Logical unit number for standard output
C  NATM    Number of atmospheres set to date
C  NATMAX  Maximum number of atmospheres allowed
C  NCGCX   Number of coarse grid x cells
C  NCGCY   Number of coarse grid y cells
C  NCGCZ   Number of coarse grid z cells
C  NVMAX   Maximum number of vertices allowed
C  NZMAX   Maximum number of z slices allowed
C  SAVE    Logical flag indicating if input data is to be saved
C  TRMINL  Logical flag indicating if input data is from the terminal
C  
C**** Arguments - output
C  GASK    Array of gas absorptivites
C
C**** Local variables
C  I       Loop counter on the coarse grid x cells
C  J       Loop counter on the coarse grid y cells
C  K       Loop counter on the coarse grid z cells
C  KGASU   VAlue of the gas absorptivity throughout enclosure
C
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  NONE
C
C******************************************************************************

C**** Read the absorptivity value giving a prompt if appropriate
      IF (TRMINL) WRITE (LUSTOT,10)
10    FORMAT (/' Enter the gas absorption coefficient >> ',$)
      READ (LUIN,*) KGASU
C**** Write this in the data file being created
      IF (SAVE) WRITE (LUSAVE,20) KGASU
20    FORMAT (1X,G13.6,26X,'UNIFORM ABSORP. COEFF')

C**** Set absorptivity array
      DO 50 K=1,NCGCZ
         DO 40 J=1,NCGCY
            DO 30 I=1,NCGCX
               GASK(NATM+1,I,J,K)=KGASU
30          CONTINUE
40       CONTINUE
50    CONTINUE

      RETURN
      END

      SUBROUTINE ABOPT2(TRMINL,SAVE,LUIN,LUSTOT,LUSAVE,NATM,NCGCX,
     1                  NCGCY,NCGCZ,CGX,CGY,CGZ,VOLUME,GX,GY,GZ,ZONEV,
     2                  NVMAX,NZMAX,NATMAX,GASK)
C
C  Routine to set gas absorptivities via option 2
C  ie Actual absorptivites entered on a zone by zone basis
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**16/04/91
C
C
      INTEGER LUIN,LUSTOT,LUSAVE,NATM,NCGCX,NCGCY,NCGCZ,CGX(NVMAX),
     1        CGY(NVMAX),CGZ(NZMAX),NVMAX,NZMAX,NATMAX
      REAL VOLUME(NVMAX,NVMAX,NZMAX),GX(NVMAX),GY(NVMAX),GZ(NZMAX),
     1     ZONEV(2,NVMAX,NVMAX,NZMAX),GASK(NATMAX,NVMAX,NVMAX,NZMAX)
      LOGICAL TRMINL,SAVE
      INTEGER I,J,K,NTHING
      CHARACTER*30 WHAT(2)
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  CGX     Array of fine grid line numbers of coarse x grid lines
C  CGY     Array of fine grid line numbers of coarse y grid lines
C  CGZ     Array of fine grid line numbers of coarse z grid lines
C  GASK    Array to hold the gas absorptivities
C  GX      Array of fine grid line x co-ordinates
C  GY      Array of fine grid line y co-ordinates
C  GZ      Array of fine grid line z co-ordinates
C  LUIN    Logical unit number for input
C  LUSAVE  Logical unit number for data file created from input
C  LUSTOT  Logical unit number for standard output
C  NATM    Number of atmopsheres set to date
C  NATMAX  Maximum number of atmospheres allowed
C  NCGCX   Number of coarse grid x cells
C  NCGCY   Number of coarse grid y cells
C  NCGCZ   Number of coarse grid z cells
C  NVMAX   Maximum number of vertices allowed
C  NZMAX   Maximum number of z slices allowed
C  SAVE    Logical flag indicating if a file is to be created from input data
C  TRMINL  Logical flag indicating if input is from the terminal
C  VOLUME  Array of coarse grid cell volumes
C  ZONEV   Array to hold the zone by zone values entered in EACHZN
C
C**** Arguments - output
C  GASK    Array of gas absorptivites
C
C**** Local variables
C  I       Loop counter over coarse grid x cells
C  J       Loop counter over coarse grid y cells
C  K       Loop counter over coarse grid z cells
C  NTHING  Number of things to be entered on a zone by zone basis
C
C**** Local Arrays
C  WHAT    Titles of those things to be entered on a zone by zone basis
C
C**** Functions and Subroutines
C  EACHZN  Enter up to 1 quantities on a zone by zone basis
C
C******************************************************************************

C**** Determine zone by zone absorptivities
      NTHING=1
      WHAT(1)='ABSORPTION COEFFICIENT'
      CALL EACHZN(TRMINL,SAVE,LUIN,LUSTOT,LUSAVE,
     1            NCGCX,NCGCY,NCGCZ,CGX,CGY,CGZ,VOLUME,NVMAX,
     2            NZMAX,NTHING,WHAT,GX,GY,GZ,ZONEV)

C**** Set absorptivity array
      DO 30 K=1,NCGCZ
         DO 20 J=1,NCGCY
            DO 10 I=1,NCGCX
               GASK(NATM+1,I,J,K)=ZONEV(1,I,J,K)
10          CONTINUE
20       CONTINUE
30    CONTINUE

      RETURN
      END

 
      SUBROUTINE EACHZN(TRMINL,SAVE,LUIN,LUSTOT,LUSAVE,NCGCX,NCGCY,
     1                  NCGCZ,CGX,CGY,CGZ,VOLUME,NVMAX,NZMAX,NTHING,
     2                  WHAT,GX,GY,GZ,ZONEV)
C
C  Routine to read in the values of NTHING qunatities which differ from
C  zone to zone
C  
C*****VERSION 1.2********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**16/04/91
C
C
      LOGICAL TRMINL,SAVE
      INTEGER LUIN,LUSTOT,LUSAVE,NCGCX,NCGCY,NCGCZ,NTHING
      INTEGER CGX(NVMAX),CGY(NVMAX),CGZ(NZMAX)
      REAL GX(NVMAX),GY(NVMAX),GZ(NZMAX),VOLUME(NVMAX,NVMAX,NZMAX),
     1     ZONEV(2,NVMAX,NVMAX,NZMAX)
      CHARACTER*30 WHAT(2)
      INTEGER I,J,K,L
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  CGX     Array of coarse x grid line fine grid line numbers
C  CGY     Array of coarse y grid line fine grid line numbers
C  CGZ     Array of coarse z grid line fine grid line numbers
C  GX      Array of fine grid line x co-ordinates
C  GY      Array of fine grid line y co-ordinates
C  GZ      Array of fine grid line z co-ordinates
C  LUIN    Logical unit for input
C  LUSAVE  Logical unit for saving the input data in a file
C  LUSTOT  Logical unit for standard output
C  NCGCX   Number of x grid lines on the coarse grid
C  NCGCY   Number of y grid lines on the coarse grid
C  NCGCZ   Number of z grid lines on the coarse grid
C  NTHING  Number of things to be input in each zone
C  NVMAX    Dimension of some arrays passed as arguments
C  NZMAX    Dimension of some arrays passed as arguments
C  SAVE    Logical flag indicating if an input file is to be created
C  TRMINL  Logical flag indicating if input is from the terminal
C  VOLUME  Array giving the volume of each zone
C  WHAT    Array of character variables - names of the quantities to be input
C  ZONEV   Array which will hold all the qunatities input
C
C**** Arguments - output
C  ZONEV   Array holding all the quantities input
C
C**** Local variables
C  I       Loop counter on coarse grid x cells
C  J       Loop counter on coarse grid y cells
C  K       Loop counter on coarse grid z cells
C  L       Loop counter on number of things to be input
C
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  NONE
C
C******************************************************************************

C     Check that NTHING is not too big for the dimensions of the arrays
C     WHAT and ZONEV - If an error occurs here it is a programming fault
C     not a data fault - Consult D.A.Lawson if the remedy is not obvious
      IF (NTHING.GT.2) THEN
         WRITE (LUSTOT,10) NTHING
10       FORMAT (' EACHZN IS CALLED WITH NTHING=',I2/
     1           ' THIS VALUE IS TOO LARGE'/
     2           ' THE DIMENSIONS OF ARRAYS IN EACHZN AND PASSED TO',
     3           ' EACHZN NEED CHANGING',/
     4           ' RUN ABORTED')
      END IF

C**** Give preliminary prompts if input is from the terminal
      IF (TRMINL) THEN
         WRITE (LUSTOT,20)
20       FORMAT (/' Values of the following are to be input for each'
     1           ' zone :-')
         DO 40 L=1,NTHING
            WRITE (LUSTOT,30) WHAT(L)
30          FORMAT (10X,A30)
40       CONTINUE
      END IF

C**** Read in the appropriate quantities on a zone by zone basis
C**** giving explanatory prompts if input is from the terminal
      DO 140 K=1,NCGCZ
         IF (TRMINL) WRITE (LUSTOT,50) K,GZ(CGZ(K)),GZ(CGZ(K+1))
50       FORMAT (' z-slice ',I3,' from z=',G11.4,' to z=',G11.4)
         DO 130 J=1,NCGCY
            IF (TRMINL) WRITE (LUSTOT,60) J,GY(CGY(J)),GY(CGY(J+1))
60          FORMAT (5X,' y-row ',I3,' from y=',G11.4,' to y=',G11.4)
            DO 120 I=1,NCGCX
               IF (TRMINL) WRITE (LUSTOT,70) I,GX(CGX(I)),GX(CGX(I+1))
70             FORMAT (10X,' x-column ',I3,' from x=',G11.4,' to x=',
     1                 G11.4)
               IF (VOLUME(I,J,K).GT.0.0) THEN
                  DO 100 L=1,NTHING
                     IF (TRMINL) WRITE (LUSTOT,80) I,J,K,WHAT(L)
80                   FORMAT (15X,' Cell ',I3,',',I3,',',I3/
     1                       15X,' Enter ',A30,' >> ',$)
                     READ (LUIN,*) ZONEV(L,I,J,K)
                     IF (SAVE) WRITE (LUSAVE,90) ZONEV(L,I,J,K),
     1                                           WHAT(L),I,J,K
90                   FORMAT (1X,G13.6,26X,A18,1X,I3,',',I3,',',I3)
100               CONTINUE
               ELSE
                  IF (TRMINL) WRITE (LUSTOT,110) I,J,K
110               FORMAT (15X,' Cell ',I3,',',I3,',',I3,
     1                    ' has zero volume'/
     2                    15X,' No values required')
               END IF
120         CONTINUE
130      CONTINUE
140   CONTINUE

      RETURN
      END

      SUBROUTINE ABOPT3(TRMINL,SAVE,LUIN,LUSTOT,LUSAVE,NATM,NCGCX,
     1                  NCGCY,NCGCZ,NATMAX,NVMAX,NZMAX,GASK)
C
C  Routine to determine gas absorptivites vai option 3
C  ie sum of grey gases with uniform excess air and no carbon
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**16/04/91
C
C
      INTEGER LUIN,LUSTOT,LUSAVE,NATM,NCGCX,NCGCY,NCGCZ,NATMAX,
     1        NVMAX,NZMAX
      REAL GASK(NATMAX,NVMAX,NVMAX,NZMAX)
      LOGICAL TRMINL,SAVE
      INTEGER IDFUEL,I,J,K
      REAL XSAIRU,PPRAD,AKGN(6),AKSN(6)
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  GASK    Array to hold gas absorptivites
C  LUIN    Logical unit number for input
C  LUSAVE  Logical unit number for data file created from input data
C  LUSTOT  Logical unit number for standard output
C  NATM    Number of atmospheres set to date
C  NATMAX  Maximum number of atmospheres allowed
C  NCGCX   Number of coarse grid x cells
C  NCGCY   Number of coarse grid y cells
C  NCGCZ   Number of coarse grid z cells
C  NVMAX   Maximum number of vertices allowed
C  NZMAX   Maximum number of z slices allowed
C  SAVE    Logical flag indicating if input data is to be saved
C  TRMINL  Logical flag indicating if input data is from the terminal
C
C**** Arguments - output
C  GASK    Array of gas absorptivities
C
C**** Local variables
C  I       Loop counter on coarse grid x cells
C  IDFUEL  Fuel identity flag
C  J       Loop counter on coarse grid y cells
C  K       Loop counter on coarse grid z cells
C  PPRAD   Partial pressure of CO2 and H2O in combustion products
C  XSAIRU  Uniform excess air level (%)
C
C**** Local Arrays
C  AKGN    Array of grey gas component absorption coefficient (no soot)
C  AKSN    Array of grey gas component absorption coefficient (with soot)
C
C**** Functions and Subroutines
C  GREYGS  Determine grey gas characteristics of specified gas
C
C******************************************************************************

C**** Determine the fuel type
10    IF (TRMINL) WRITE (LUSTOT,20)
20    FORMAT (/' Specify fuel type:-'/
     1         '     1. Natural Gas'/
     2         '     2. Gas Oil'/
     3         '     3. Heavy Fuel Oil'/
     4         ' Enter fuel type >> ',$)
      READ (LUIN,*) IDFUEL
C**** Make sure requested fuel type is legal
      IF ((IDFUEL.LT.1).OR.(IDFUEL.GT.3)) THEN
         IF (TRMINL) THEN
            WRITE (LUSTOT,30)
30          FORMAT (/' INVALID RESPONSE - Answer in range 1-3'/)
            GOTO 10
         ELSE
            WRITE (LUSTOT,40)
40          FORMAT (/' INVALID FUEL TYPE REQUESTED'/
     1              /' RUN ABORTED'/)
            STOP
         END IF
      END IF
C**** Save the fuel type in the input file being created
      IF (SAVE) WRITE (LUSAVE,50) IDFUEL
50    FORMAT (1X,I1,38X,'FUEL TYPE:1-NAT GAS,2-GAS OIL')

C**** Determine the uniform excess air level and save it when appropriate
      IF (TRMINL) WRITE (LUSTOT,60) 
60    FORMAT (/' Enter the excess air level (%) >> ',$)
      READ (LUIN,*) XSAIRU
      IF (SAVE) WRITE (LUSAVE,70) XSAIRU
70    FORMAT (1X,G13.6,26X,'EXCESS AIR LEVEL %')

C**** Use the sum of grey gas model to determine the absorptivities
      CALL GREYGS(IDFUEL,XSAIRU,PPRAD,AKGN,AKSN)

C**** Load the absorptivities into the gas absorptivity array
      DO 100 K=1,NCGCZ
         DO 90 J=1,NCGCY
            DO 80 I=1,NCGCX
               GASK(NATM+1,I,J,K)=AKGN(3)*PPRAD
               GASK(NATM+2,I,J,K)=AKGN(5)*PPRAD
80          CONTINUE
90       CONTINUE
100   CONTINUE

      RETURN
      END
      SUBROUTINE ABOPT4(TRMINL,SAVE,LUIN,LUSTOT,LUSAVE,NATM,NCGCX,
     1                  NCGCY,NCGCZ,NATMAX,NVMAX,NZMAX,GASK)
C
C  Routine to determine gas absorptivites via option 4
C  ie sum of grey gases with uniform excess air and uniform carbon
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**16/04/91
C
C
      INTEGER LUIN,LUSTOT,LUSAVE,NATM,NCGCX,NCGCY,NCGCZ,NATMAX,
     1        NVMAX,NZMAX
      REAL GASK(NATMAX,NVMAX,NVMAX,NZMAX)
      LOGICAL TRMINL,SAVE
      INTEGER IDFUEL,I,J,K,L
      REAL XSAIRU,PPRAD,AKGN(6),AKSN(6),CARBON
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  GASK    Array to hold gas absorptivites
C  LUIN    Logical unit number for input
C  LUSAVE  Logical unit number for data file created from input data
C  LUSTOT  Logical unit number for standard output
C  NATM    Number of atmospheres set to date
C  NATMAX  Maximum number of atmospheres allowed
C  NCGCX   Number of coarse grid x cells
C  NCGCY   Number of coarse grid y cells
C  NCGCZ   Number of coarse grid z cells
C  NVMAX   Maximum number of vertices allowed
C  NZMAX   Maximum number of z slices allowed
C  SAVE    Logical flag indicating if input data is to be saved
C  TRMINL  Logical flag indicating if input data is from the terminal
C
C**** Arguments - output
C  GASK    Array of gas absorptivities
C
C**** Local variables
C  CARBON  Value of the uniform carbon concentration (kg/m**3)
C  I       Loop counter on coarse grid x cells
C  IDFUEL  Fuel identity flag
C  J       Loop counter on coarse grid y cells
C  K       Loop counter on coarse grid z cells
C  L       Loop counter on number of new atmospheres
C  PPRAD   Partial pressure of CO2 and H2O in combustion products
C  XSAIRU  Uniform excess air level (%)
C
C**** Local Arrays
C  AKGN    Array of grey gas component absorption coefficient (no soot)
C  AKSN    Array of grey gas component absorption coefficient (with soot)
C
C**** Functions and Subroutines
C  GREYGS  Determine grey gas characteristics of specified gas
C
C******************************************************************************

C**** Determine the fuel type
10    IF (TRMINL) WRITE (LUSTOT,20)
20    FORMAT (/' Specify fuel type:-'/
     1         '     1. Natural Gas'/
     2         '     2. Gas Oil'/
     3         '     3. Heavy Fuel Oil'/
     4         ' Enter fuel type >> ',$)
      READ (LUIN,*) IDFUEL
C**** Make sure requested fuel type is legal
      IF ((IDFUEL.LT.1).OR.(IDFUEL.GT.3)) THEN
         IF (TRMINL) THEN
            WRITE (LUSTOT,30)
30          FORMAT (/' INVALID RESPONSE - Answer in range 1-3'/)
            GOTO 10
         ELSE
            WRITE (LUSTOT,40)
40          FORMAT (/' INVALID FUEL TYPE REQUESTED'/
     1              /' RUN ABORTED'/)
            STOP
         END IF
      END IF
C**** Save the fuel type in the input file being created
      IF (SAVE) WRITE (LUSAVE,50) IDFUEL
50    FORMAT (1X,I1,38X,'FUEL TYPE:1-NAT GAS,2-GAS OIL')

C**** Determine the uniform excess air level and save it when appropriate
      IF (TRMINL) WRITE (LUSTOT,60) 
60    FORMAT (/' Enter the excess air level (%) >> ',$)
      READ (LUIN,*) XSAIRU
      IF (SAVE) WRITE (LUSAVE,70) XSAIRU
70    FORMAT (1X,G13.6,26X,'EXCESS AIR LEVEL %')

C**** Determine the uniform carbon concentration and save it when appropriate
      IF (TRMINL) WRITE (LUSTOT,73)
73    FORMAT (/' Enter the carbon concentration (kg/m**3) >> ',$)
      READ (LUIN,*) CARBON
      IF (SAVE) WRITE (LUSAVE,76) CARBON
76    FORMAT (1X,G13.6,26X,'CARBON CONCENTRATION kg/m3')

C**** Use the sum of grey gas model to determine the absorptivities
      CALL GREYGS(IDFUEL,XSAIRU,PPRAD,AKGN,AKSN)

C**** Load the absorptivities into the gas absorptivity array
      DO 110 K=1,NCGCZ
         DO 100 J=1,NCGCY
            DO 90 I=1,NCGCX
               DO 80 L=1,6
                  GASK(NATM+L,I,J,K)=AKGN(L)*PPRAD+AKSN(L)*CARBON
80             CONTINUE
90          CONTINUE
100      CONTINUE
110   CONTINUE

      RETURN
      END
      SUBROUTINE ABOPT5(TRMINL,SAVE,LUIN,LUSTOT,LUSAVE,NATM,NCGCX,
     1                  NCGCY,NCGCZ,CGX,CGY,CGZ,VOLUME,GX,GY,GZ,ZONEV,
     2                  NVMAX,NZMAX,NATMAX,GASK)
C
C  Routine to set gas absorptivities via option 5
C  ie Absorptivites set using a sum of grey gas model with uniform
C  excess air but with the carbon concentration varying from zone to zone
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**16/04/91
C
C
      INTEGER LUIN,LUSTOT,LUSAVE,NATM,NCGCX,NCGCY,NCGCZ,CGX(NVMAX),
     1        CGY(NVMAX),CGZ(NZMAX),NVMAX,NZMAX,NATMAX
      REAL VOLUME(NVMAX,NVMAX,NZMAX),GX(NVMAX),GY(NVMAX),GZ(NZMAX),
     1     ZONEV(2,NVMAX,NVMAX,NZMAX),GASK(NATMAX,NVMAX,NVMAX,NZMAX)
      LOGICAL TRMINL,SAVE
      INTEGER I,J,K,L,NTHING,IDFUEL
      REAL XSAIRU,PPRAD,AKGN(6),AKSN(6)
      CHARACTER*30 WHAT(2)
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  CGX     Array of fine grid line numbers of coarse x grid lines
C  CGY     Array of fine grid line numbers of coarse y grid lines
C  CGZ     Array of fine grid line numbers of coarse z grid lines
C  GASK    Array to hold the gas absorptivities
C  GX      Array of fine grid line x co-ordinates
C  GY      Array of fine grid line y co-ordinates
C  GZ      Array of fine grid line z co-ordinates
C  LUIN    Logical unit number for input
C  LUSAVE  Logical unit number for data file created from input
C  LUSTOT  Logical unit number for standard output
C  NATM    Number of atmopsheres set to date
C  NATMAX  Maximum number of atmospheres allowed
C  NCGCX   Number of coarse grid x cells
C  NCGCY   Number of coarse grid y cells
C  NCGCZ   Number of coarse grid z cells
C  NVMAX   Maximum number of vertices allowed
C  NZMAX   Maximum number of z slices allowed
C  SAVE    Logical flag indicating if a file is to be created from input data
C  TRMINL  Logical flag indicating if input is from the terminal
C  VOLUME  Array of coarse grid cell volumes
C  ZONEV   Array to hold the zone by zone values entered in EACHZN
C
C**** Arguments - output
C  GASK    Array of gas absorptivites
C
C**** Local variables
C  I       Loop counter over coarse grid x cells
C  IDFUEL  Fuel identity flag
C  J       Loop counter over coarse grid y cells
C  K       Loop counter over coarse grid z cells
C  L       Loop counter on number of new atmospheres
C  NTHING  Number of things to be entered on a zone by zone basis
C  PPRAD   Partial pressure of CO2 and H2O in combustion products
C  XSAIRU  Uniform excess air level (%)
C
C**** Local Arrays
C  AKGN    Array of grey gas component absorption coefficient (no soot)
C  AKSN    Array of grey gas component absorption coefficient (with soot)
C  WHAT    Titles of those things to be entered on a zone by zone basis
C
C**** Functions and Subroutines
C  EACHZN  Enter up to 1 quantities on a zone by zone basis
C  GREYGS  Determine grey gas characteristics of specified gas
C
C******************************************************************************

C**** Determine the fuel type
10    IF (TRMINL) WRITE (LUSTOT,20)
20    FORMAT (/' Specify fuel type:-'/
     1         '     1. Natural Gas'/
     2         '     2. Gas Oil'/
     3         '     3. Heavy Fuel Oil'/
     4         ' Enter fuel type >> ',$)
      READ (LUIN,*) IDFUEL
C**** Make sure requested fuel type is legal
      IF ((IDFUEL.LT.1).OR.(IDFUEL.GT.3)) THEN
         IF (TRMINL) THEN
            WRITE (LUSTOT,30)
30          FORMAT (/' INVALID RESPONSE - Answer in range 1-3'/)
            GOTO 10
         ELSE
            WRITE (LUSTOT,40)
40          FORMAT (/' INVALID FUEL TYPE REQUESTED'/
     1              /' RUN ABORTED'/)
            STOP
         END IF
      END IF
C**** Save the fuel type in the input file being created
      IF (SAVE) WRITE (LUSAVE,50) IDFUEL
50    FORMAT (1X,I1,38X,'FUEL TYPE:1-NAT GAS,2-GAS OIL')

C**** Determine the uniform excess air level and save it when appropriate
      IF (TRMINL) WRITE (LUSTOT,60) 
60    FORMAT (/' Enter the excess air level (%) >> ',$)
      READ (LUIN,*) XSAIRU
      IF (SAVE) WRITE (LUSAVE,70) XSAIRU
70    FORMAT (1X,G13.6,26X,'EXCESS AIR LEVEL %')

C**** Determine the zone by zone carbon concentration
      NTHING=1
      WHAT(1)='CARBON CONCENTRATION (kg/m^3)'
      CALL EACHZN(TRMINL,SAVE,LUIN,LUSTOT,LUSAVE,
     1            NCGCX,NCGCY,NCGCZ,CGX,CGY,CGZ,VOLUME,NVMAX,
     2            NZMAX,NTHING,WHAT,GX,GY,GZ,ZONEV)

C**** Use the sum of grey gas model to determine the absorptivities
      CALL GREYGS(IDFUEL,XSAIRU,PPRAD,AKGN,AKSN)

C**** Load the absorptivities into the gas absorptivity array
      DO 110 K=1,NCGCZ
         DO 100 J=1,NCGCY
            DO 90 I=1,NCGCX
               DO 80 L=1,6
                  GASK(NATM+L,I,J,K)=AKGN(L)*PPRAD
     1                               +AKSN(L)*ZONEV(1,I,J,K)
80             CONTINUE
90          CONTINUE
100      CONTINUE
110   CONTINUE

      RETURN
      END

 
 
      SUBROUTINE ABOPT6(TRMINL,SAVE,LUIN,LUSTOT,LUSAVE,NATM,NCGCX,
     1                  NCGCY,NCGCZ,CGX,CGY,CGZ,VOLUME,GX,GY,GZ,ZONEV,
     2                  NVMAX,NZMAX,NATMAX,GASK)
C
C  Routine to set gas absorptivities via option 6
C  ie Absorptivites set using a sum of grey gas model with both
C  excess air and the carbon concentration varying from zone to zone
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**16/04/91
C
C
      INTEGER LUIN,LUSTOT,LUSAVE,NATM,NCGCX,NCGCY,NCGCZ,CGX(NVMAX),
     1        CGY(NVMAX),CGZ(NZMAX),NVMAX,NZMAX,NATMAX
      REAL VOLUME(NVMAX,NVMAX,NZMAX),GX(NVMAX),GY(NVMAX),GZ(NZMAX),
     1     ZONEV(2,NVMAX,NVMAX,NZMAX),GASK(NATMAX,NVMAX,NVMAX,NZMAX)
      LOGICAL TRMINL,SAVE
      INTEGER I,J,K,L,NTHING,IDFUEL
      REAL PPRAD,AKGN(6),AKSN(6)
      CHARACTER*30 WHAT(2)
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  CGX     Array of fine grid line numbers of coarse x grid lines
C  CGY     Array of fine grid line numbers of coarse y grid lines
C  CGZ     Array of fine grid line numbers of coarse z grid lines
C  GASK    Array to hold the gas absorptivities
C  GX      Array of fine grid line x co-ordinates
C  GY      Array of fine grid line y co-ordinates
C  GZ      Array of fine grid line z co-ordinates
C  LUIN    Logical unit number for input
C  LUSAVE  Logical unit number for data file created from input
C  LUSTOT  Logical unit number for standard output
C  NATM    Number of atmopsheres set to date
C  NATMAX  Maximum number of atmospheres allowed
C  NCGCX   Number of coarse grid x cells
C  NCGCY   Number of coarse grid y cells
C  NCGCZ   Number of coarse grid z cells
C  NVMAX   Maximum number of vertices allowed
C  NZMAX   Maximum number of z slices allowed
C  SAVE    Logical flag indicating if a file is to be created from input data
C  TRMINL  Logical flag indicating if input is from the terminal
C  VOLUME  Array of coarse grid cell volumes
C  ZONEV   Array to hold the zone by zone values entered in EACHZN
C
C**** Arguments - output
C  GASK    Array of gas absorptivites
C
C**** Local variables
C  I       Loop counter over coarse grid x cells
C  IDFUEL  Fuel identity flag
C  J       Loop counter over coarse grid y cells
C  K       Loop counter over coarse grid z cells
C  L       Loop counter on number of new atmospheres
C  NTHING  Number of things to be entered on a zone by zone basis
C  PPRAD   Partial pressure of CO2 and H2O in combustion products
C
C**** Local Arrays
C  AKGN    Array of grey gas component absorption coefficient (no soot)
C  AKSN    Array of grey gas component absorption coefficient (with soot)
C  WHAT    Titles of those things to be entered on a zone by zone basis
C
C**** Functions and Subroutines
C  EACHZN  Enter up to 1 quantities on a zone by zone basis
C  GREYGS  Determine grey gas characteristics of specified gas
C
C******************************************************************************

C**** Determine the fuel type
10    IF (TRMINL) WRITE (LUSTOT,20)
20    FORMAT (/' Specify fuel type:-'/
     1         '     1. Natural Gas'/
     2         '     2. Gas Oil'/
     3         '     3. Heavy Fuel Oil'/
     4         ' Enter fuel type >> ',$)
      READ (LUIN,*) IDFUEL
C**** Make sure requested fuel type is legal
      IF ((IDFUEL.LT.1).OR.(IDFUEL.GT.3)) THEN
         IF (TRMINL) THEN
            WRITE (LUSTOT,30)
30          FORMAT (/' INVALID RESPONSE - Answer in range 1-3'/)
            GOTO 10
         ELSE
            WRITE (LUSTOT,40)
40          FORMAT (/' INVALID FUEL TYPE REQUESTED'/
     1              /' RUN ABORTED'/)
            STOP
         END IF
      END IF
C**** Save the fuel type in the input file being created
      IF (SAVE) WRITE (LUSAVE,50) IDFUEL
50    FORMAT (1X,I1,38X,'FUEL TYPE:1-NAT GAS,2-GAS OIL')

C**** Determine the zone by zone excess air and carbon concentration
      NTHING=2
      WHAT(1)='EXCESS AIR LEVEL (%)'
      WHAT(2)='CARBON CONCENTRATION (kg/m^3)'
      CALL EACHZN(TRMINL,SAVE,LUIN,LUSTOT,LUSAVE,
     1            NCGCX,NCGCY,NCGCZ,CGX,CGY,CGZ,VOLUME,NVMAX,
     2            NZMAX,NTHING,WHAT,GX,GY,GZ,ZONEV)

C**** The sum of grey gas model must be called on a zone by zone basis
C**** and then the absorptivity of each zone may be calculated
      DO 110 K=1,NCGCZ
         DO 100 J=1,NCGCY
            DO 90 I=1,NCGCX
               CALL GREYGS(IDFUEL,ZONEV(1,I,J,K),PPRAD,AKGN,AKSN)
               DO 80 L=1,6
                  GASK(NATM+L,I,J,K)=AKGN(L)*PPRAD
     1                               +AKSN(L)*ZONEV(2,I,J,K)
80             CONTINUE
90          CONTINUE
100      CONTINUE
110   CONTINUE

      RETURN
      END

 
 
      SUBROUTINE GREYGS(IDFUEL,XSAIR,PPRAD,AKGN,AKSN)
C
C  Routine to set the arrays AKGN and AKSN of absorption coefficients for
C  each grey gas component and also to calculate the (combined) partial
C  pressure (PPRAD) of CO2 and H2O
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**08/10/90
C
C
C
      REAL XSAIR,PPRAD,AKGN(6),AKSN(6)
      REAL RATIO(3),PPCO2(3),PPH2O(3),GDNSTY(3),SOOT(6,3),GAS(6,3)
      REAL VOLSCP,VOLAIR
      INTEGER IDFUEL,I
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  IDFUEL  Fuel type; 1-natural gas, 2-gas oil, 3-heavy fuel oil
C  XSAIR   Excess air level (%)
C
C**** Arguments - output
C  AKGN    Array of absorption coefficients for each grey gas component
C  AKSN    Array of absorption coefficients for each grey gas component
C          with soot
C  PPRAD   Combined partial pressure of CO2 and H2O in combustion products
C
C**** Local variables
C  I       Loop counter over grey gas components
C  VOLAIR  Air volume
C  VOLSCP  Stoichiometric combustion products volume
C
C**** Local Arrays
C  GAS     Array of absorption coefficients for each grey has component 
C          for each fuel type
C  GDNSTY  Combustion products density
C  PPCO2   Partial pressure of CO2 in combustion products
C  PPH2O   Partial pressure of H2O in combustion products
C  RATIO   Stoichiometric air/fuel mass ratio (kg/kg)
C  SOOT    Array of absorption coefficients for each grey gas component
C          with soot for each fuel type
C
C**** Functions and Subroutines
C  NONE
C
C******************************************************************************

      DATA RATIO /16.65529, 14.44, 13.84/
      DATA PPCO2 /0.09627,  0.135, 0.1438/
      DATA PPH2O /0.18752,  0.123, 0.11516/
      DATA GDNSTY/1.1755,   1.257, 1.2416/
      DATA GAS /0.0, 0.0, 1.88, 1.88, 68.8,  68.8,
     1          0.0, 0.0, 2.5,  2.5,  109.0, 109.0,
     2          0.0, 0.0, 2.5,  2.5,  109.0, 109.0/
      DATA SOOT/350., 1780., 350., 1780., 350., 1780.,
     1          350., 1780., 350., 1780., 350., 1780.,
     2          350., 1780., 350., 1780., 350., 1780./

C     Calculate the (combined) partial pressure of CO2 and H2O (PPRAD) in 
C     the furnace gases from the volume ratio of stoichiometric combustion
C     products (VOLSCP) to mixture
      VOLSCP=(1.+RATIO(IDFUEL))/GDNSTY(IDFUEL)
      VOLAIR=RATIO(IDFUEL)*XSAIR*0.01/1.2255
      PPRAD=(PPCO2(IDFUEL)+PPH2O(IDFUEL))*VOLSCP/(VOLAIR+VOLSCP)

C     Set gas and soot absorption coefficients for each of the grey gas 
C     components - appropriate to this fuel type
      DO 10 I=1,6
         AKGN(I)=GAS(I,IDFUEL)
         AKSN(I)=SOOT(I,IDFUEL)
10    CONTINUE

      RETURN
      END

      SUBROUTINE CRFNGK(CRSGSK,NVMAX,NZMAX,NATMAX,NATM,NCGCX,NCGCY,
     1                  NCGCZ,CGX,CGY,CGZ,FINGSK)
C
C  Routine to set the array of fine grid gas absorptivities FINGSK from
C  the coarse grid array CRSGSK
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**19/10/90
C
C
C
      REAL CRSGSK(NATMAX,NVMAX,NVMAX,NZMAX),
     1     FINGSK(NATMAX,NVMAX,NVMAX,NZMAX)
      INTEGER NVMAX,NZMAX,NATMAX,NCGCX,NCGCY,NCGCZ,CGX(NVMAX),
     1        CGY(NVMAX),CGZ(NZMAX),NATM,IC,JC,KC,L,IF,JF,KF
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  CGX     Array of fine x grid line numbers of coarse x grid lines
C  CGY     Array of fine y grid line numbers of coarse y grid lines
C  CGZ     Array of fine z grid line numbers of coarse z grid lines
C  CRSGSK  Array of coarse grid cell gas absorptivities
C  FINGSK  Array to hold fine grid cell gas absorptivities
C  NATMAX  Maximum number of atmospheres allowed (dimension of some arrays)
C  NCGCX   Number of coarse grid x cells
C  NCGCY   Number of coarse grid y cells
C  NCGCZ   Number of coarse grid z cells
C  NVMAX   Maximum number of vertices allowed (dimension of some arrays)
C  NZMAX   Maximum number of z slices allowed (dimesnion of some arrays)
C  
C**** Arguments - output
C  FINGSK  Array of fine grid cell gas absorptivities
C
C**** Local variables
C  IC      Loop counter over x coarse grid cells
C  IF      Loop counter over x fine grid cells
C  JC      Loop counter over y coarse grid cells
C  JF      Loop counter over y fine grid cells
C  KC      Loop counter over z coarse grid cells
C  KF      Loop counter over x fine grid cells
C  L       Loop counter of different atmospheres
C
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  NONE
C
C******************************************************************************

      DO 70 KC=1,NCGCZ
        DO 60 JC=1,NCGCY
          DO 50 IC=1,NCGCX
            DO 40 L=1,NATM
              DO 30 KF=CGZ(KC),CGZ(KC+1)
                DO 20 JF=CGY(JC),CGY(JC+1)
                  DO 10 IF=CGX(IC),CGX(IC+1)
                    FINGSK(L,IF,JF,KF)=CRSGSK(L,IC,JC,KC)
10                CONTINUE
20              CONTINUE
30            CONTINUE
40          CONTINUE
50        CONTINUE
60      CONTINUE
70    CONTINUE

      RETURN
      END

