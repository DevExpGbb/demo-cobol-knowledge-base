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

      * Copy standard Canada Day data structures
       COPY 'canada-day-copybook.cpy'.
       
      * Work fields for date processing
       01  WS-DATE-WORK-FIELDS.
           05  WS-INPUT-YEAR               PIC 9(4).
           05  WS-INPUT-MONTH              PIC 9(2).
           05  WS-INPUT-DAY                PIC 9(2).
           05  WS-JULY-FIRST               PIC 9(8).
           05  WS-DAY-OF-WEEK              PIC 9(1).
       
       LINKAGE SECTION.
       01  L-INPUT-DATE                    PIC 9(8).
       01  L-OBSERVANCE-FLAG               PIC X(1).
       01  L-CANADA-DAY-FLAG               PIC X(1).
       01  L-OBSERVED-DATE                 PIC 9(8).
       01  L-RETURN-CODE                   PIC 9(2).
       01  L-ERROR-MESSAGE                 PIC X(40).
       
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
           IF CDO-SUCCESS
               PERFORM 3000-CHECK-CANADA-DAY
               PERFORM 4000-CALCULATE-OBSERVED-DATE
           END-IF
           PERFORM 5000-SET-OUTPUT-PARAMETERS
           PERFORM 9999-PROGRAM-EXIT.
       
      ******************************************************************
      * INITIALIZE PROGRAM VARIABLES                                  *
      ******************************************************************
       1000-INITIALIZE-PROGRAM.
           INITIALIZE CANADA-DAY-OUTPUT
           MOVE L-INPUT-DATE TO CDI-INPUT-DATE
           MOVE L-OBSERVANCE-FLAG TO CDI-OBSERVANCE-FLAG
           MOVE 00 TO CDO-RETURN-CODE
           MOVE SPACES TO CDO-ERROR-MESSAGE.
       
      ******************************************************************
      * VALIDATE INPUT DATE FORMAT AND VALUES                        *
      ******************************************************************
       2000-VALIDATE-INPUT-DATE.
           PERFORM 2100-VALIDATE-DATE-FORMAT
           IF CDO-SUCCESS
               PERFORM 2200-EXTRACT-DATE-COMPONENTS
               PERFORM 2300-VALIDATE-DATE-COMPONENTS
           END-IF.
       
      ******************************************************************
      * VALIDATE DATE IS NUMERIC                                     *
      ******************************************************************
       2100-VALIDATE-DATE-FORMAT.
           IF CDI-INPUT-DATE IS NOT NUMERIC
               MOVE 01 TO CDO-RETURN-CODE
               MOVE CDE-INVALID-FORMAT TO CDO-ERROR-MESSAGE
           END-IF.
       
      ******************************************************************
      * EXTRACT YEAR, MONTH, DAY FROM INPUT DATE                     *
      ******************************************************************
       2200-EXTRACT-DATE-COMPONENTS.
           MOVE CDI-INPUT-DATE(1:4) TO WS-INPUT-YEAR
           MOVE CDI-INPUT-DATE(5:2) TO WS-INPUT-MONTH
           MOVE CDI-INPUT-DATE(7:2) TO WS-INPUT-DAY.
       
      ******************************************************************
      * VALIDATE DATE COMPONENT VALUES                               *
      ******************************************************************
       2300-VALIDATE-DATE-COMPONENTS.
           PERFORM 2310-VALIDATE-YEAR
           IF CDO-SUCCESS
               PERFORM 2320-VALIDATE-MONTH
           END-IF
           IF CDO-SUCCESS
               PERFORM 2330-VALIDATE-DAY
           END-IF.
       
      ******************************************************************
      * VALIDATE YEAR IS IN ACCEPTABLE RANGE                         *
      ******************************************************************
       2310-VALIDATE-YEAR.
           IF WS-INPUT-YEAR < CDC-CONFEDERATION-YEAR
               MOVE 02 TO CDO-RETURN-CODE
               MOVE CDE-INVALID-YEAR TO CDO-ERROR-MESSAGE
           ELSE
               IF WS-INPUT-YEAR > 9999
                   MOVE 05 TO CDO-RETURN-CODE
                   MOVE CDE-FUTURE-DATE TO CDO-ERROR-MESSAGE
               END-IF
           END-IF.
       
      ******************************************************************
      * VALIDATE MONTH IS BETWEEN 01 AND 12                          *
      ******************************************************************
       2320-VALIDATE-MONTH.
           IF WS-INPUT-MONTH < 1 OR WS-INPUT-MONTH > 12
               MOVE 03 TO CDO-RETURN-CODE  
               MOVE CDE-INVALID-MONTH TO CDO-ERROR-MESSAGE
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
           MOVE 04 TO CDO-RETURN-CODE
           MOVE CDE-INVALID-DAY TO CDO-ERROR-MESSAGE.
       
      ******************************************************************
      * VALIDATE FEBRUARY DAY (HANDLE LEAP YEARS)                    *
      ******************************************************************
       2350-VALIDATE-FEBRUARY-DAY.
           IF WS-INPUT-DAY < 1 OR WS-INPUT-DAY > 29
               PERFORM 2340-SET-INVALID-DAY-ERROR
           ELSE
               IF WS-INPUT-DAY = 29
                   PERFORM 2360-CHECK-LEAP-YEAR
                   IF NOT CDO-SUCCESS
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
                       MOVE 04 TO CDO-RETURN-CODE
                   END-IF
               END-IF
           ELSE
               MOVE 04 TO CDO-RETURN-CODE
           END-IF.
       
      ******************************************************************
      * CHECK IF INPUT DATE IS CANADA DAY                            *
      ******************************************************************
       3000-CHECK-CANADA-DAY.
           IF WS-INPUT-MONTH = CDC-CANADA-DAY-MONTH AND
              WS-INPUT-DAY = CDC-CANADA-DAY-DAY
               MOVE 'Y' TO CDO-CANADA-DAY-FLAG
           ELSE
               MOVE 'N' TO CDO-CANADA-DAY-FLAG
           END-IF.
       
      ******************************************************************
      * CALCULATE OBSERVED HOLIDAY DATE                               *
      ******************************************************************
       4000-CALCULATE-OBSERVED-DATE.
           IF CDO-IS-CANADA-DAY AND CDI-CHECK-OBSERVED
               PERFORM 4100-BUILD-JULY-FIRST-DATE
               PERFORM 4200-GET-DAY-OF-WEEK
               PERFORM 4300-CALCULATE-OBSERVED-DATE-LOGIC
           ELSE
               MOVE CDI-INPUT-DATE TO CDO-OBSERVED-DATE
           END-IF.
       
      ******************************************************************
      * BUILD JULY 1ST DATE FOR GIVEN YEAR                           *
      ******************************************************************
       4100-BUILD-JULY-FIRST-DATE.
           STRING WS-INPUT-YEAR
                  CDC-CANADA-DAY-MONTH
                  CDC-CANADA-DAY-DAY
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
               WHEN CDC-SATURDAY
      *            If Saturday, observe on Friday (June 30)
                   PERFORM 4310-CALCULATE-PREVIOUS-DAY
               WHEN CDC-SUNDAY
      *            If Sunday, observe on Monday (July 2)
                   PERFORM 4320-CALCULATE-NEXT-DAY
               WHEN OTHER
      *            Weekday - observe on actual date
                   MOVE WS-JULY-FIRST TO CDO-OBSERVED-DATE
           END-EVALUATE.
       
      ******************************************************************
      * CALCULATE PREVIOUS DAY (JUNE 30)                             *
      ******************************************************************
       4310-CALCULATE-PREVIOUS-DAY.
           COMPUTE CDO-OBSERVED-DATE = 
               FUNCTION DATE-OF-INTEGER(
                   FUNCTION INTEGER-OF-DATE(WS-JULY-FIRST) - 1).
       
      ******************************************************************
      * CALCULATE NEXT DAY (JULY 2)                                  *
      ******************************************************************
       4320-CALCULATE-NEXT-DAY.
           COMPUTE CDO-OBSERVED-DATE = 
               FUNCTION DATE-OF-INTEGER(
                   FUNCTION INTEGER-OF-DATE(WS-JULY-FIRST) + 1).
       
      ******************************************************************
      * SET OUTPUT PARAMETERS                                        *
      ******************************************************************
       5000-SET-OUTPUT-PARAMETERS.
           MOVE CDO-CANADA-DAY-FLAG TO L-CANADA-DAY-FLAG
           MOVE CDO-OBSERVED-DATE TO L-OBSERVED-DATE
           MOVE CDO-RETURN-CODE TO L-RETURN-CODE
           MOVE CDO-ERROR-MESSAGE TO L-ERROR-MESSAGE.
       
      ******************************************************************
      * PROGRAM EXIT                                                 *
      ******************************************************************
       9999-PROGRAM-EXIT.
           EXIT PROGRAM.
