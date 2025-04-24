I need high-quality Markdown documentation for COBOL code. Here's an example of the documentation quality I'm looking for:

EXAMPLE DOCUMENTATION:
# Customer Credit Limit Validation Routine

## Purpose
This routine validates customer credit limit increases against business rules including customer history, account standing, and regulatory requirements.

## Inputs
- `CUSTOMER-RECORD`: Complete customer information including:
  - `CUST-ID`: 10-digit unique identifier
  - `CUST-HISTORY`: 5-character credit history code
  - `CURRENT-LIMIT`: Current approved credit limit

## Process Flow
1. **Initial Validation**: Checks customer record format and required fields
2. **Credit History Analysis**: Evaluates the customer's past payment behavior
   ```cobol
   IF CUST-HISTORY-LEN < 5
      MOVE 'INCOMPLETE HISTORY' TO REASON-CODE
      PERFORM REJECTION-ROUTINE
   END-IF
   ```
3. **Limit Calculation**: Applies business rules to determine appropriate limit
[Additional explanation and code snippets...]

[Rest of the exemplary documentation...]

Now, using the same style and level of detail, document the following COBOL routine:

[PASTE TARGET CODE]