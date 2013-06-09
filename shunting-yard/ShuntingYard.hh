/******************************************************************************
 * File: ShuntingYard.hh
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * An implementation of Dijkstra's shunting-yard algorithm for parsing infix
 * expressions.  The shunting-yard algorithm takes as input an expression in
 * infix form (where each operator has a precedence), parses the expression,
 * then outputs it in reverse-Polish notation.  Since RPN has no operator
 * precedence (and hence no ambiguity), the resulting expression can easily be
 * evaluated.
 *
 * The shunting-yard algorithm is a stack-based algorithm that makes use of
 * an intermediary operator stack to shuffle around the tokens in the input
 * sequence into the correct order.  The basic intuition behind the algorithm
 * is to continuously shift operands out of the infix expression to the output
 * RPN expression, temporarily holding back the operators.  An operator is
 * shifted into the resulting expression only when the operator that would
 * follow it has a lower priority (or when the input is consumed).
 *
 * As an example, we can trace the execution of the algorithm on the input
 * 3 + 4 * 1 + (2 - 3).  We begin in this state:
 *
 * Result:
 * Operators:
 * Input:     3 + 4 * 1 + (2 - 3)
 *
 * We begin by shifting the 3 to the result stack, since it's an operand:
 *
 * Result:    3
 * Operators:
 * Input:     + 4 * 1 + (2 - 3)
 *
 * Next, we shift the + onto the operator stack:
 *
 * Result:    3
 * Operators: +
 * Input:     4 * 1 + (2 - 3)
 *
 * Next, we shift the four onto the result stack:
 *
 * Result:    3 4
 * Operators: +
 * Input:     * 1 + (2 - 3)
 *
 * Now, we come to the next operator, *.  Since * has higher precedence than +,
 * it will bind to the 4 and the upcoming 1, and so we don't want to do the
 * addition of the 3 and 4 yet.  Consequently, we shift the * onto the operator
 * stack:
 *
 * Result:    3 4
 * Operators: + *
 * Input:     1 + (2 - 3)
 *
 * We then shift the next operand:
 *
 * Result:    3 4 1
 * Operators: + *
 * Input:     + (2 - 3)
 *
 * The next operator we hit is this +.  This has lower precedence than the *
 * operator atop the stack, and so we continuously pop operators off the stack
 * until we find an operator with strictly lower precedence than this +.  This
 * means we pop both the * and the + off the operator stack, as shown here:
 *
 * Result:    3 4 1 * +
 * Operators:
 * Input:     + (2 - 3)
 *
 * Now that the stack is empty, the + has higher priority than all other
 * operators, and so we can shift it:
 *
 * Result:    3 4 1 * +
 * Operators: +
 * Input:     (2 - 3)
 *
 * When we encounter parentheses, the logic becomes a bit trickier.  We shift
 * on the opening parenthesis as a marker that all precedence rules from before
 * this point no longer apply to the upcoming tokens:
 *
 * Result:    3 4 1 * +
 * Operators: + (
 * Input:     2 - 3)
 *
 * We now continue shifting numbers and operators as before, until we arrive at
 * this point:
 *
 * Result:    3 4 1 * + 2 3
 * Operators: + ( -
 * Input:     )
 *
 * Now that we have found a matching parenthesis, we pop operators off the
 * stack until we match the original parenthesis, which we similarly remove:
 *
 * Result:    3 4 1 * + 2 3 -
 * Operators: +
 * Input:     
 *
 * And finally, now that we've run out of input, we pop all the remaining
 * operators off the stack and shift them into the output sequence:
 *
 * Result:    3 4 1 * + 2 3 - +
 * Operators:
 * Input:
 *
 * This results in a correct RPN expression for the original infix expression.
 *
 * More generally, the algorithm is as follows:
 *
 * Begin with an empty operator stack and output sequence.
 * While more tokens remain in the input sequence:
 *   If the token is a number, shift it to the result.
 *   If the token is an operator:
 *      While the topmost element of the operator stack does not have lower
 *         priority, pop that operator off the stack and shift it onto the end
 *         of the output sequence.
 *      Push the operator on top of the operator stack.
 *   If the token is an open parenthesis, shift it atop the operator stack.
 *   If the token is a close parenthesis, pop all operators off the operator
 *      stack and shift them onto the end of the output sequence until an open
 *      parenthesis is found.
 * Finally, pop all the remaining operators off the stack and shift them onto
 *   the output sequence.
 */

#ifndef ShuntingYard_Included
#define ShuntingYard_Included

#include <string>
#include <vector>

/**
 * Function: std::vector<std::string> InfixLex(const std::string& expr);
 * Usage: std::vector<std::string> tokens = InfixLex("1 + 3");
 * ----------------------------------------------------------------------------
 * Given a string representation of an indix expression, returns a vector of
 * strings containing a parsed representation of that expression.  The 
 * validty of this expression is not validated; after all, this is a lexing
 * step, rather than a parsing step.  If the input cannot be lexed, a
 * runtime_error exception is thrown.
 */
std::vector<std::string> InfixLex(const std::string& expr);

/**
 * Function: ShuntingYardToRPN(const std::vector<std::string>& expr);
 * Usage: RPNEvaluate(ShuntingYardToRPN(InfixLex("1 + 2 - 3 * 4")));
 * ----------------------------------------------------------------------------
 * Given a sequence of tokens corresponding to an infix expression, applies the
 * shunting-yard algorithm to convert it to an RPN expression.  If the input is
 * malformed, throws a runtime_error exception.
 */
std::vector<std::string> ShuntingYardToRPN(const std::vector<std::string>& expr);

#endif
