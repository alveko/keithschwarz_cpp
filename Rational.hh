/*************************************************************************
 * File: Rational.hh
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * Implementation of a rational number class.  This object tracks a
 * numerator and denominator of arbitrary integral type, and allows for
 * operations that manipulate the rational quantity.  Each addition,
 * subtraction, multiplication, or division runs in time O(op + lg n),
 * where op is the time it takes to perform the operation.  The lg n term
 * comes in from the application of Euclid's algorithm to normalize the
 * numerator and denominator by their gcd.
 */
#ifndef Rational_Included
#define Rational_Included

#include <ostream>   // For std::ostream
#include <stdexcept> // For std::domain_error
#include <algorithm> // For std::swap
#include <sstream>   // For std::basic_stringstream

/**
 * Class: Rational<T>
 * -----------------------------------------------------------------------
 * A generalized rational number class.  The template parameter must be
 * an integral type supporting +, -, *, /, and the compound assignment
 * versions of these operators.  The template parameter should be signed.
 */
template <typename SignedInteger>
class Rational {
public:
  /**
   * Rational(const SignedInteger& num = 0, 
   *          const SignedInteger& denom = 1);
   * Usage: Rational<int> piApprox(355, 113);
   * ---------------------------------------------------------------------
   * Constructs a new Rational with the specified numerator and
   * denominator.  If the denominator is zero, a domain_error is thrown.
   */
  Rational(const SignedInteger& numerator = SignedInteger(0),
           const SignedInteger& denominator = SignedInteger(1));

  /**
   * SignedInteger numerator() const;
   * SignedInteger denominator() const;
   * ---------------------------------------------------------------------
   * Retrieves the numerator or denominator of the rational number.
   */
  SignedInteger numerator() const;
  SignedInteger denominator() const;

  /**
   * Rational& operator+= (const Rational<SignedInteger>& rhs);
   * Rational& operator-= (const Rational<SignedInteger>& rhs);
   * Rational& operator*= (const Rational<SignedInteger>& rhs);
   * Rational& operator/= (const Rational<SignedInteger>& rhs);
   * ---------------------------------------------------------------------
   * Adds, subtracts, multiplies, or divides this rational number by some
   * other rational number.
   */
  Rational& operator+= (const Rational<SignedInteger>& rhs);
  Rational& operator-= (const Rational<SignedInteger>& rhs);
  Rational& operator*= (const Rational<SignedInteger>& rhs);
  Rational& operator/= (const Rational<SignedInteger>& rhs);

  /**
   * Rational& operator*= (const SignedInteger& scaleFactor);
   * Rational& operator/= (const SignedInteger& scaleFactor);
   * ---------------------------------------------------------------------
   * Scales the rational number by the specified value.  Dividing by zero
   * will produce a domain_error.
   */
  Rational& operator*= (const SignedInteger& scaleFactor);
  Rational& operator/= (const SignedInteger& scaleFactor);

private:
  /* The numerator and denominator. */
  SignedInteger mNumerator, mDenominator;

  /* Yields the GCD of two numbers. */
  static SignedInteger gcd(const SignedInteger& one, 
                           const SignedInteger& two);

  /* Simplifies the fraction by dividing by the GCD of the numerator
   * and denominator.
   */
  void simplify();
};

/*** Utility functions ***/

/**
 * double AsReal(const Rational<T>&) const;
 * Usage: cout << AsReal(piApprox) << endl;
 * ----------------------------------------------------------------------
 * Returns an approximation of the rational number as a real number.
 * This operation is only supported if SignedInteger can be converted to
 * a double.
 */
template <typename SignedInteger>
double AsReal(const Rational<SignedInteger>& ratio);

/**
 * const Rational<T> Reciprocal(const Rational<T>&);
 * Usage: Rational<int> oneOverPi = Reciprocal(piApprox);
 * ---------------------------------------------------------------------
 * Returns the reciprocal of the specified fraction, throwing a
 * domain_error if the argument is zero.
 */
template <typename SignedInteger>
const Rational<SignedInteger> Reciprocal(const Rational<SignedInteger>& ratio);

/* Binary arithmetic operators */
template <typename SignedInteger>
const Rational<SignedInteger> operator+ (const Rational<SignedInteger>& lhs,
                                         const Rational<SignedInteger>& rhs);
template <typename SignedInteger>
const Rational<SignedInteger> operator- (const Rational<SignedInteger>& lhs,
                                         const Rational<SignedInteger>& rhs);
template <typename SignedInteger>
const Rational<SignedInteger> operator* (const Rational<SignedInteger>& lhs,
                                         const Rational<SignedInteger>& rhs);
template <typename SignedInteger>
const Rational<SignedInteger> operator/ (const Rational<SignedInteger>& lhs,
                                         const Rational<SignedInteger>& rhs);

/* Comparison operators. */
template <typename SignedInteger>
bool operator<  (const Rational<SignedInteger>& lhs,
                 const Rational<SignedInteger>& rhs);
template <typename SignedInteger>
bool operator<= (const Rational<SignedInteger>& lhs,
                 const Rational<SignedInteger>& rhs);
template <typename SignedInteger>
bool operator== (const Rational<SignedInteger>& lhs,
                 const Rational<SignedInteger>& rhs);
template <typename SignedInteger>
bool operator!= (const Rational<SignedInteger>& lhs,
                 const Rational<SignedInteger>& rhs);
template <typename SignedInteger>
bool operator>= (const Rational<SignedInteger>& lhs,
                 const Rational<SignedInteger>& rhs);
template <typename SignedInteger>
bool operator>  (const Rational<SignedInteger>& lhs,
                 const Rational<SignedInteger>& rhs);
  
/* Unary arithmetic operators */
template <typename SignedInteger>
const Rational<SignedInteger> operator- (const Rational<SignedInteger>& r);
template <typename SignedInteger>
const Rational<SignedInteger> operator+ (const Rational<SignedInteger>& r);

/* Stream insertion operator. */
template <typename SignedInteger, typename charT, typename traits>
std::basic_ostream<charT, traits>&
operator<< (std::basic_ostream<charT, traits>&,
            const Rational<SignedInteger>& r);

/* * * * * Implementation Below This Point * * * * */

/* Constructor sets the numerator and denominator, then normalizes them. */
template <typename SignedInteger>
Rational<SignedInteger>::Rational(const SignedInteger& numerator,
                                  const SignedInteger& denominator) 
  : mNumerator(numerator), mDenominator(denominator) {
  /* Report an error if the denominator is zero. */
  if (denominator == SignedInteger(0))
    throw std::domain_error("Denominator cannot be zero.");

  /* Simplify the fraction. */
  simplify();
}

/* Getting the numerator or denominator is straightforward. */
template <typename SignedInteger>
SignedInteger Rational<SignedInteger>::numerator() const {
  return mNumerator;
}
template <typename SignedInteger>
SignedInteger Rational<SignedInteger>::denominator() const {
  return mDenominator;
}

/* Scaling by a constant works by multiplying the numerator by the
 * specified value, then simplifying the fraction.
 */
template <typename SignedInteger>
Rational<SignedInteger>& Rational<SignedInteger>::operator*= (const SignedInteger& scale) {
  mNumerator *= scale;
  simplify();

  return *this;
}

/* Dividing by a constant checks whether that constant is zero, then
 * scales the denominator appropriately.
 */
template <typename SignedInteger>
Rational<SignedInteger>& Rational<SignedInteger>::operator/= (const SignedInteger& scale) {
  /* Check for division-by-zero. */
  if (scale == SignedInteger(0))
    throw std::domain_error("Attempted to divide by zero.");

  mDenominator *= scale;
  simplify();

  return *this;
}

/* Addition works by scaling each side by the denominator of the other,
 * adding the two sides, then simplifying.
 */
template <typename SignedInteger>
Rational<SignedInteger>& Rational<SignedInteger>::operator+= (const Rational<SignedInteger>& rhs) {
  /* a/b + c/d = ad + bc / bd */
  mNumerator = numerator() * rhs.denominator() +
               rhs.numerator() * denominator();
  mDenominator *= rhs.denominator();

  /* Simplify everything. */
  simplify();

  return *this;
}

/* Subtraction is similar to addition, but we add the inverse of the other
 * side instead of directly adding the other side.
 */
template <typename SignedInteger>
Rational<SignedInteger>& Rational<SignedInteger>::operator-= (const Rational<SignedInteger>& rhs) {
  return *this += -rhs;
}

/* Multiplication works by scaling the numerator and denominator by the
 * numerator and denominator of the other fraction, then normalizing.
 */
template <typename SignedInteger>
Rational<SignedInteger>& Rational<SignedInteger>::operator*= (const Rational<SignedInteger>& rhs) {
  mNumerator *= rhs.numerator();
  mDenominator *= rhs.denominator();
  simplify();

  return *this;
}

/* Division is just multiplication by the reciprocal. */
template <typename SignedInteger>
Rational<SignedInteger>& Rational<SignedInteger>::operator/= (const Rational<SignedInteger>& rhs) {
  return *this *= Reciprocal(rhs);
}

/* Simplification works by getting the GCD of the numerator and denominator,
 * then dividing each by it.
 */
template <typename SignedInteger>
void Rational<SignedInteger>::simplify() {
  /* Compute the GCD. */
  const SignedInteger GCD = gcd(numerator(), denominator());

  /* Normalize by it. */
  mNumerator /= GCD;
  mDenominator /= GCD;

  /* If the denominator is negative, then flip its sign and make the
   * numerator negative.
   */
  if (mDenominator < SignedInteger(0)) {
    mDenominator = -mDenominator;
    mNumerator = -mNumerator;
  }
}

/* Getting the GCD uses Euclid's algorithm.  In particular:
 *               GCD(b, a)     if a < b
 * GCD(a, b) = { a             if b == 0
 *               GCD(b, a % b) otherwise
 * However, we have to account for the possibility that one or more
 * of the numbers is negative.  In that case, we'll use the GCD
 * algorithm on the positive version of the numbers, then will
 * correct the sign of the GCD appropriately upon return.  In
 * particular, we'll only say that the GCD is negative if both
 * values are negative.
 *
 * Note that this will behave weirdly if either a or b are the
 * minimum possible integer in a two's-complement, fixed-size
 * representation because -INT_MIN == INT_MIN.  We won't handle
 * this case here.
 */
template <typename SignedInteger>
SignedInteger Rational<SignedInteger>::gcd(const SignedInteger& one,
                                           const SignedInteger& two) {
  /* Copy the values here so that we can mangle them in the process.*/
  SignedInteger a = one, b = two;
  
  /* Correct for sign errors. */
  const bool bothNegative = a < SignedInteger(0) && b < SignedInteger(0);

  /* Make both values positive. */
  if (a < SignedInteger(0))
    a = -a;
  if (b < SignedInteger(0))
    b = -b;

  /* Loop until convergence. */
  while (true) {
    /* Make sure a >= b. */
    if (a < b)
      std::swap(a, b);

    /* Base case: If b == 0, the answer is a. */
    if (b == 0) break;

    /* Otherwise, set a = b, b = a % b. */
    const SignedInteger tmp = b;
    b = a % b;
    a = tmp;
  }

  /* Return a, after possibly multiplying by -1. */
  return bothNegative? -a : a;
}

/* AsReal works by casting the numerator and denominator to doubles and
 * performing the division.
 */
template <typename SignedInteger>
double AsReal(const Rational<SignedInteger>& rational) {
  return double(rational.numerator()) / double(rational.denominator());
}

/* Reciprocal just creates a new Rational with the numerator and
 * denominator swapped.  The Rational constructor handles the error-checking.
 */
template <typename SignedInteger>
const Rational<SignedInteger> Reciprocal(const Rational<SignedInteger>& rational) {
  return Rational<SignedInteger>(rational.denominator(), rational.numerator());
}

/* Binary arithmetic operators implemented in terms of compound assignment
 * operators.
 */
template <typename SignedInteger>
const Rational<SignedInteger> operator+ (const Rational<SignedInteger>& lhs,
                                         const Rational<SignedInteger>& rhs) {
  return Rational<SignedInteger>(lhs) += rhs;
}
template <typename SignedInteger>
const Rational<SignedInteger> operator- (const Rational<SignedInteger>& lhs,
                                         const Rational<SignedInteger>& rhs) {
  return Rational<SignedInteger>(lhs) -= rhs;
}
template <typename SignedInteger>
const Rational<SignedInteger> operator* (const Rational<SignedInteger>& lhs,
                                         const Rational<SignedInteger>& rhs) {
  return Rational<SignedInteger>(lhs) *= rhs;
}
template <typename SignedInteger>
const Rational<SignedInteger> operator/ (const Rational<SignedInteger>& lhs,
                                         const Rational<SignedInteger>& rhs) {
  return Rational<SignedInteger>(lhs) /= rhs;
}

/* Binary equality works by testing if the two objects have identical
 * representations.  This works because we always simplify the
 * representation to a canonical form.
 */
template <typename SignedInteger>
bool operator== (const Rational<SignedInteger>& lhs,
                 const Rational<SignedInteger>& rhs) {
  return lhs.numerator() == rhs.numerator() && lhs.denominator() == rhs.denominator();
}

/* Less-than works by scaling both rationals up to have the same denominator,
 * then checking the numerators.
 */
template <typename SignedInteger>
bool operator<  (const Rational<SignedInteger>& lhs,
                 const Rational<SignedInteger>& rhs) {
  /* a/b < c/d   iff   ad/bd < bc/bd */
  return lhs.numerator() * rhs.denominator() < rhs.numerator() * lhs.denominator();
}

/* Remaining relational operators implemented in terms of the existing
 * operators.
 */
template <typename SignedInteger>
bool operator <= (const Rational<SignedInteger>& lhs,
                  const Rational<SignedInteger>& rhs) {
  return !(rhs < lhs);
}
template <typename SignedInteger>
bool operator != (const Rational<SignedInteger>& lhs,
                  const Rational<SignedInteger>& rhs) {
  return !(rhs == lhs);
}
template <typename SignedInteger>
bool operator >= (const Rational<SignedInteger>& lhs,
                  const Rational<SignedInteger>& rhs) {
  return !(lhs < rhs);
}
template <typename SignedInteger>
bool operator >  (const Rational<SignedInteger>& lhs,
                  const Rational<SignedInteger>& rhs) {
  return rhs < lhs;
}

/* Unary arithmetic operators implemented in terms of constructor and
 * identity.
 */
template <typename SignedInteger>
const Rational<SignedInteger> operator+ (const Rational<SignedInteger>& op) {
  return op;
}
template <typename SignedInteger>
const Rational<SignedInteger> operator- (const Rational<SignedInteger>& op) {
  return Rational<SignedInteger>(-op.numerator(), op.denominator());
}

/* Stream insertion operator works by printing out the numerator and
 * denominator separated by a / sign.
 */
template <typename SignedInteger, typename charT, typename traits>
std::basic_ostream<charT, traits>& 
operator<< (std::basic_ostream<charT, traits>& out,
            const Rational<SignedInteger>& rational) {
  /* Buffer all operations into a stringstream. */
  std::basic_stringstream<charT, traits> buffer;
 
  /* Copy formatting info, but not width. */
  buffer.imbue(out.getloc());
  buffer.flags(out.flags());
  buffer.precision(out.precision());

  /* Get a copy of the code conversion facet so we can print out appropriate
   * representations of spaces and slashes.
   */
  const std::ctype<charT>& ccvt = std::use_facet< std::ctype<charT> >(buffer.getloc());

  /* Print out the rational number. */
  buffer << rational.numerator() << ccvt.widen('/') << rational.denominator();

  /* Flush buffer into the stream. */
  return out << buffer.str();
}

#endif
