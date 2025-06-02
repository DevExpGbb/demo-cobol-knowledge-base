      ******************************************************************
      * PROGRAM: CANADA-DAY-CHECK                                     *
      * PURPOSE: Determine if a given date is Canada Day              *
      * AUTHOR:  Enterprise COBOL Development Team                    *
      * DATE:    2024                                                 *
      * VERSION: 1.0                                                  *
      ******************************************************************
      * DESCRIPTION:                                                  *
      * This program determines whether a given date falls on Canada  *
      * Day (July 1st) and calculates the observed holiday date when *
      * July 1st falls on a weekend.                                 *
      *                                                               *
      * INPUT: Date in YYYYMMDD format, observance flag              *
      * OUTPUT: Canada Day indicator, observed date, return code     *
      ******************************************************************
       
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CANADA-DAY-CHECK.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-Z.
       OBJECT-COMPUTER. IBM-Z.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      * Input/Output Parameters
       01  WS-INPUT-PARAMETERS.
           05  WS-INPUT-DATE               PIC 9(8).
           05  WS-OBSERVANCE-FLAG          PIC X(1).
       
       01  WS-OUTPUT-PARAMETERS.
           05  WS-CANADA-DAY-FLAG          PIC X(1).
           05  WS-OBSERVED-DATE            PIC 9(8).
           05  WS-RETURN-CODE              PIC 9(2).
           05  WS-ERROR-MESSAGE            PIC X(50).
       
      * Work fields for date processing
       01  WS-DATE-WORK-FIELDS.
           05  WS-INPUT-YEAR               PIC 9(4).
           05  WS-INPUT-MONTH              PIC 9(2).
           05  WS-INPUT-DAY                PIC 9(2).
           05  WS-JULY-FIRST               PIC 9(8).
           05  WS-DAY-OF-WEEK              PIC 9(1).
       
      * Constants
       01  WS-CONSTANTS.
           05  WS-CANADA-DAY-MONTH         PIC 9(2) VALUE 07.
           05  WS-CANADA-DAY-DAY           PIC 9(2) VALUE 01.
           05  WS-CONFEDERATION-YEAR       PIC 9(4) VALUE 1867.
           05  WS-MAX-YEAR                 PIC 9(4) VALUE 9999.
           05  WS-SATURDAY                 PIC 9(1) VALUE 6.
           05  WS-SUNDAY                   PIC 9(1) VALUE 0.
       
      * Return codes
       01  WS-RETURN-CODES.
           05  WS-RC-SUCCESS               PIC 9(2) VALUE 00.
           05  WS-RC-INVALID-FORMAT        PIC 9(2) VALUE 01.
           05  WS-RC-INVALID-YEAR          PIC 9(2) VALUE 02.
           05  WS-RC-INVALID-MONTH         PIC 9(2) VALUE 03.
           05  WS-RC-INVALID-DAY           PIC 9(2) VALUE 04.
           05  WS-RC-FUTURE-DATE           PIC 9(2) VALUE 05.
       
      * Error messages
       01  WS-ERROR-MESSAGES.
           05  WS-MSG-INVALID-FORMAT       PIC X(50) 
               VALUE 'Invalid date format - use YYYYMMDD'.
           05  WS-MSG-INVALID-YEAR         PIC X(50)
               VALUE 'Invalid year - must be 1867 or later'.
           05  WS-MSG-INVALID-MONTH        PIC X(50)
               VALUE 'Invalid month - must be 01-12'.
           05  WS-MSG-INVALID-DAY          PIC X(50)
               VALUE 'Invalid day for given month and year'.
           05  WS-MSG-FUTURE-DATE          PIC X(50)
               VALUE 'Date exceeds system maximum'.
       
       LINKAGE SECTION.
       01  L-INPUT-DATE                    PIC 9(8).
       01  L-OBSERVANCE-FLAG               PIC X(1).
       01  L-CANADA-DAY-FLAG               PIC X(1).
       01  L-OBSERVED-DATE                 PIC 9(8).
       01  L-RETURN-CODE                   PIC 9(2).
       01  L-ERROR-MESSAGE                 PIC X(50).
       
       PROCEDURE DIVISION USING L-INPUT-DATE
                               L-OBSERVANCE-FLAG
                               L-CANADA-DAY-FLAG
                               L-OBSERVED-DATE
                               L-RETURN-CODE
                               L-ERROR-MESSAGE.
       
      ******************************************************************
      * MAIN PROCESSING ROUTINE                                       *
      ******************************************************************
       0000-MAIN-PROCESSING.
           PERFORM 1000-INITIALIZE-PROGRAM
           PERFORM 2000-VALIDATE-INPUT-DATE
           IF WS-RETURN-CODE = WS-RC-SUCCESS
               PERFORM 3000-CHECK-CANADA-DAY
               PERFORM 4000-CALCULATE-OBSERVED-DATE
           END-IF
           PERFORM 5000-SET-OUTPUT-PARAMETERS
           PERFORM 9999-PROGRAM-EXIT.
       
      ******************************************************************
      * INITIALIZE PROGRAM VARIABLES                                  *
      ******************************************************************
       1000-INITIALIZE-PROGRAM.
           INITIALIZE WS-OUTPUT-PARAMETERS
           MOVE L-INPUT-DATE TO WS-INPUT-DATE
           MOVE L-OBSERVANCE-FLAG TO WS-OBSERVANCE-FLAG
           MOVE WS-RC-SUCCESS TO WS-RETURN-CODE
           MOVE SPACES TO WS-ERROR-MESSAGE.
       
      ******************************************************************
      * VALIDATE INPUT DATE FORMAT AND VALUES                        *
      ******************************************************************
       2000-VALIDATE-INPUT-DATE.
           PERFORM 2100-VALIDATE-DATE-FORMAT
           IF WS-RETURN-CODE = WS-RC-SUCCESS
               PERFORM 2200-EXTRACT-DATE-COMPONENTS
               PERFORM 2300-VALIDATE-DATE-COMPONENTS
           END-IF.
       
      ******************************************************************
      * VALIDATE DATE IS NUMERIC                                     *
      ******************************************************************
       2100-VALIDATE-DATE-FORMAT.
           IF WS-INPUT-DATE IS NOT NUMERIC
               MOVE WS-RC-INVALID-FORMAT TO WS-RETURN-CODE
               MOVE WS-MSG-INVALID-FORMAT TO WS-ERROR-MESSAGE
           END-IF.
       
      ******************************************************************
      * EXTRACT YEAR, MONTH, DAY FROM INPUT DATE                     *
      ******************************************************************
       2200-EXTRACT-DATE-COMPONENTS.
           MOVE WS-INPUT-DATE(1:4) TO WS-INPUT-YEAR
           MOVE WS-INPUT-DATE(5:2) TO WS-INPUT-MONTH
           MOVE WS-INPUT-DATE(7:2) TO WS-INPUT-DAY.
       
      ******************************************************************
      * VALIDATE DATE COMPONENT VALUES                               *
      ******************************************************************
       2300-VALIDATE-DATE-COMPONENTS.
           PERFORM 2310-VALIDATE-YEAR
           IF WS-RETURN-CODE = WS-RC-SUCCESS
               PERFORM 2320-VALIDATE-MONTH
           END-IF
           IF WS-RETURN-CODE = WS-RC-SUCCESS
               PERFORM 2330-VALIDATE-DAY
           END-IF.
       
      ******************************************************************
      * VALIDATE YEAR IS IN ACCEPTABLE RANGE                         *
      ******************************************************************
       2310-VALIDATE-YEAR.
           IF WS-INPUT-YEAR < WS-CONFEDERATION-YEAR
               MOVE WS-RC-INVALID-YEAR TO WS-RETURN-CODE
               MOVE WS-MSG-INVALID-YEAR TO WS-ERROR-MESSAGE
           ELSE
               IF WS-INPUT-YEAR > WS-MAX-YEAR
                   MOVE WS-RC-FUTURE-DATE TO WS-RETURN-CODE
                   MOVE WS-MSG-FUTURE-DATE TO WS-ERROR-MESSAGE
               END-IF
           END-IF.
       
      ******************************************************************
      * VALIDATE MONTH IS BETWEEN 01 AND 12                          *
      ******************************************************************
       2320-VALIDATE-MONTH.
           IF WS-INPUT-MONTH < 1 OR WS-INPUT-MONTH > 12
               MOVE WS-RC-INVALID-MONTH TO WS-RETURN-CODE
               MOVE WS-MSG-INVALID-MONTH TO WS-ERROR-MESSAGE
           END-IF.
       
      ******************************************************************
      * VALIDATE DAY IS VALID FOR GIVEN MONTH AND YEAR               *
      ******************************************************************
       2330-VALIDATE-DAY.
           EVALUATE WS-INPUT-MONTH
               WHEN 01 WHEN 03 WHEN 05 WHEN 07 WHEN 08 WHEN 10 WHEN 12
                   IF WS-INPUT-DAY < 1 OR WS-INPUT-DAY > 31
                       PERFORM 2340-SET-INVALID-DAY-ERROR
                   END-IF
               WHEN 04 WHEN 06 WHEN 09 WHEN 11
                   IF WS-INPUT-DAY < 1 OR WS-INPUT-DAY > 30
                       PERFORM 2340-SET-INVALID-DAY-ERROR
                   END-IF
               WHEN 02
                   PERFORM 2350-VALIDATE-FEBRUARY-DAY
               WHEN OTHER
                   PERFORM 2340-SET-INVALID-DAY-ERROR
           END-EVALUATE.
       
      ******************************************************************
      * SET INVALID DAY ERROR                                        *
      ******************************************************************
       2340-SET-INVALID-DAY-ERROR.
           MOVE WS-RC-INVALID-DAY TO WS-RETURN-CODE
           MOVE WS-MSG-INVALID-DAY TO WS-ERROR-MESSAGE.
       
      ******************************************************************
      * VALIDATE FEBRUARY DAY (HANDLE LEAP YEARS)                    *
      ******************************************************************
       2350-VALIDATE-FEBRUARY-DAY.
           IF WS-INPUT-DAY < 1 OR WS-INPUT-DAY > 29
               PERFORM 2340-SET-INVALID-DAY-ERROR
           ELSE
               IF WS-INPUT-DAY = 29
                   PERFORM 2360-CHECK-LEAP-YEAR
                   IF WS-RETURN-CODE NOT = WS-RC-SUCCESS
                       PERFORM 2340-SET-INVALID-DAY-ERROR
                   END-IF
               END-IF
           END-IF.
       
      ******************************************************************
      * CHECK IF YEAR IS A LEAP YEAR                                 *
      ******************************************************************
       2360-CHECK-LEAP-YEAR.
           IF FUNCTION MOD(WS-INPUT-YEAR, 4) = 0
               IF FUNCTION MOD(WS-INPUT-YEAR, 100) = 0
                   IF FUNCTION MOD(WS-INPUT-YEAR, 400) = 0
                       CONTINUE
                   ELSE
                       MOVE WS-RC-INVALID-DAY TO WS-RETURN-CODE
                   END-IF
               END-IF
           ELSE
               MOVE WS-RC-INVALID-DAY TO WS-RETURN-CODE
           END-IF.
       
      ******************************************************************
      * CHECK IF INPUT DATE IS CANADA DAY                            *
      ******************************************************************
       3000-CHECK-CANADA-DAY.
           IF WS-INPUT-MONTH = WS-CANADA-DAY-MONTH AND
              WS-INPUT-DAY = WS-CANADA-DAY-DAY
               MOVE 'Y' TO WS-CANADA-DAY-FLAG
           ELSE
               MOVE 'N' TO WS-CANADA-DAY-FLAG
           END-IF.
       
      ******************************************************************
      * CALCULATE OBSERVED HOLIDAY DATE                               *
      ******************************************************************
       4000-CALCULATE-OBSERVED-DATE.
           IF WS-CANADA-DAY-FLAG = 'Y' AND WS-OBSERVANCE-FLAG = 'Y'
               PERFORM 4100-BUILD-JULY-FIRST-DATE
               PERFORM 4200-GET-DAY-OF-WEEK
               PERFORM 4300-CALCULATE-OBSERVED-DATE-LOGIC
           ELSE
               MOVE WS-INPUT-DATE TO WS-OBSERVED-DATE
           END-IF.
       
      ******************************************************************
      * BUILD JULY 1ST DATE FOR GIVEN YEAR                           *
      ******************************************************************
       4100-BUILD-JULY-FIRST-DATE.
           STRING WS-INPUT-YEAR
                  WS-CANADA-DAY-MONTH
                  WS-CANADA-DAY-DAY
                  DELIMITED BY SIZE
                  INTO WS-JULY-FIRST.
       
      ******************************************************************
      * GET DAY OF WEEK FOR JULY 1ST                                 *
      ******************************************************************
       4200-GET-DAY-OF-WEEK.
           COMPUTE WS-DAY-OF-WEEK = 
               FUNCTION MOD(FUNCTION INTEGER-OF-DATE(WS-JULY-FIRST), 7).
       
      ******************************************************************
      * APPLY WEEKEND OBSERVANCE RULES                               *
      ******************************************************************
       4300-CALCULATE-OBSERVED-DATE-LOGIC.
           EVALUATE WS-DAY-OF-WEEK
               WHEN WS-SATURDAY
      *            If Saturday, observe on Friday (June 30)
                   PERFORM 4310-CALCULATE-PREVIOUS-DAY
               WHEN WS-SUNDAY
      *            If Sunday, observe on Monday (July 2)
                   PERFORM 4320-CALCULATE-NEXT-DAY
               WHEN OTHER
      *            Weekday - observe on actual date
                   MOVE WS-JULY-FIRST TO WS-OBSERVED-DATE
           END-EVALUATE.
       
      ******************************************************************
      * CALCULATE PREVIOUS DAY (JUNE 30)                             *
      ******************************************************************
       4310-CALCULATE-PREVIOUS-DAY.
           COMPUTE WS-OBSERVED-DATE = 
               FUNCTION DATE-OF-INTEGER(
                   FUNCTION INTEGER-OF-DATE(WS-JULY-FIRST) - 1).
       
      ******************************************************************
      * CALCULATE NEXT DAY (JULY 2)                                  *
      ******************************************************************
       4320-CALCULATE-NEXT-DAY.
           COMPUTE WS-OBSERVED-DATE = 
               FUNCTION DATE-OF-INTEGER(
                   FUNCTION INTEGER-OF-DATE(WS-JULY-FIRST) + 1).
       
      ******************************************************************
      * SET OUTPUT PARAMETERS                                        *
      ******************************************************************
       5000-SET-OUTPUT-PARAMETERS.
           MOVE WS-CANADA-DAY-FLAG TO L-CANADA-DAY-FLAG
           MOVE WS-OBSERVED-DATE TO L-OBSERVED-DATE
           MOVE WS-RETURN-CODE TO L-RETURN-CODE
           MOVE WS-ERROR-MESSAGE TO L-ERROR-MESSAGE.
       
      ******************************************************************
      * PROGRAM EXIT                                                 *
      ******************************************************************
       9999-PROGRAM-EXIT.
           EXIT PROGRAM.