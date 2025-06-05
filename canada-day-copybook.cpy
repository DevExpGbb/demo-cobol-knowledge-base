      ******************************************************************
      * COPYBOOK: CANADA-DAY-COPYBOOK                                 *
      * PURPOSE:  Data structures for Canada Day processing           *
      * AUTHOR:   Enterprise COBOL Development Team                   *
      * DATE:     2024                                                *
      * VERSION:  1.0                                                 *
      ******************************************************************
      * DESCRIPTION:                                                  *
      * This copybook defines the standard data structures used       *
      * for Canada Day holiday determination routines across          *
      * enterprise applications.                                      *
      ******************************************************************
       
      * Canada Day processing input parameters
       01  CANADA-DAY-INPUT.
           05  CDI-INPUT-DATE              PIC 9(8).
               88  CDI-VALID-DATE-FORMAT   VALUE 10000101 THRU 99991231.
           05  CDI-OBSERVANCE-FLAG         PIC X(1).
               88  CDI-CHECK-OBSERVED      VALUE 'Y'.
               88  CDI-CHECK-ACTUAL        VALUE 'N'.
       
      * Canada Day processing output parameters
       01  CANADA-DAY-OUTPUT.
           05  CDO-CANADA-DAY-FLAG         PIC X(1).
               88  CDO-IS-CANADA-DAY       VALUE 'Y'.
               88  CDO-NOT-CANADA-DAY      VALUE 'N'.
           05  CDO-OBSERVED-DATE           PIC 9(8).
           05  CDO-RETURN-CODE             PIC 9(2).
               88  CDO-SUCCESS             VALUE 00.
               88  CDO-INVALID-FORMAT      VALUE 01.
               88  CDO-INVALID-YEAR        VALUE 02.
               88  CDO-INVALID-MONTH       VALUE 03.
               88  CDO-INVALID-DAY         VALUE 04.
               88  CDO-FUTURE-DATE         VALUE 05.
           05  CDO-ERROR-MESSAGE           PIC X(40).
       
      * Canada Day constants
       01  CANADA-DAY-CONSTANTS.
           05  CDC-CANADA-DAY-MONTH        PIC 9(2) VALUE 07.
           05  CDC-CANADA-DAY-DAY          PIC 9(2) VALUE 01.
           05  CDC-CONFEDERATION-YEAR      PIC 9(4) VALUE 1867.
           05  CDC-SATURDAY                PIC 9(1) VALUE 6.
           05  CDC-SUNDAY                  PIC 9(1) VALUE 0.
       
      * Error message constants - memory optimized table
       01  CANADA-DAY-ERROR-MESSAGES.
           05  CDE-ERROR-TABLE.
               10  CDE-MSG-01              PIC X(40)
                   VALUE 'Invalid date format - use YYYYMMDD'.
               10  CDE-MSG-02              PIC X(40)
                   VALUE 'Invalid year - must be 1867 or later'.
               10  CDE-MSG-03              PIC X(40)
                   VALUE 'Invalid month - must be 01-12'.
               10  CDE-MSG-04              PIC X(40)
                   VALUE 'Invalid day for given month and year'.
               10  CDE-MSG-05              PIC X(40)
                   VALUE 'Date exceeds system maximum'.
           05  CDE-ERROR-ARRAY REDEFINES CDE-ERROR-TABLE.
               10  CDE-ERROR-MSG           PIC X(40) OCCURS 5 TIMES.
           05  CDE-INVALID-FORMAT          PIC X(40) 
               VALUE 'Invalid date format - use YYYYMMDD'.
           05  CDE-INVALID-YEAR            PIC X(40)
               VALUE 'Invalid year - must be 1867 or later'.
           05  CDE-INVALID-MONTH           PIC X(40)
               VALUE 'Invalid month - must be 01-12'.
           05  CDE-INVALID-DAY             PIC X(40)
               VALUE 'Invalid day for given month and year'.
           05  CDE-FUTURE-DATE             PIC X(40)
               VALUE 'Date exceeds system maximum'.
