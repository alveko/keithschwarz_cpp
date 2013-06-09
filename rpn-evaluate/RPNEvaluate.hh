/*****************************************************************************
 * File: RPNEvaluate.hh
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * An interface for parsing and evaluating expressions in reverse Polish
 * notation (RPN).  This simple RPN evaluator supports addition, subtraction,
 * multiplication, division, and modulus over integers.
 */

#ifndef RPNEvaluate_Included
#define RPNEvaluate_Included

#include <string>
#include <vector>

/**
 * Function: std::vector<std::string> RPNLex(const std::string& expr);
 * Usage: std::vector<std::string> tokens = RPNLex("1 3 -2 * +");
 * ---------------------------------------------------------------------------
 * Given a string representation of an RPN expression, returns a vector of
 * strings containing a parsed representation of that expression.  The 
 * validty of this expression is not validated; after all, this is a lexing
 * step, rather than a parsing step.  If the input cannot be lexed, a
 * runtime_error exception is thrown.
 */
std::vector<std::string> RPNLex(const std::string& expr);

/**
 * Function: int RPNEvaluate(const std::vector<std::string>& expr);
 * Usage: cout << RPNEvaluate(RPNLex("1 3 -2 * +")) << endl; // Prints -5
 * ---------------------------------------------------------------------------
 * Given a vector of strings corresponding to an RPN expression, evaluates
 * that RPN expression and returns the result.  If the input is malformed,
 * or if evaluating the exception would cause an arithmetic expression, an
 * runtime_error exception is thrown.
 */
int RPNEvaluate(const std::vector<std::string>& expr);

#endif
