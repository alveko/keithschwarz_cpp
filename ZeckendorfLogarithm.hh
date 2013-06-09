/*****************************************************************************
 * File: ZeckendorfLogarithm.hh
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * An implementation of a function for computing logarithms based on the
 * Zeckendorf decomposition.  Zeckendorf's theorem states that any number can
 * be written as the sum of unique Fibonacci numbers such that no two
 * consecutive Fibonacci numbers are used.  For example:
 *
 *   15 = 13 + 2
 *   95 = 65 + 21 + 8 + 1
 *
 * The proof of Zeckendorf's theorem is actually quite simple and also gives
 * an algorithm for computing such a decomposition.  The proof is by
 * induction.  As base cases, both 0 and 1 are Fibonacci numbers.  For the
 * inductive step, assume that for all n' < n the claim holds and consider n.
 * If n is a Fibonacci number then the claim is trivially true.  Otherwise,
 * let F(k) be the largest Fibonacci number smaller than n and consider the
 * number n - F(k).  By the inductive hypothesis, n - F(k) can be written as
 * the sum of unique Fibonacci numbers with no two consecutive numbers used.
 * Consider the sum of those unique numbers, plus F(k).  If we can show that
 * F(k) doesn't appear twice, we have that each number is unique, and if we
 * can show that F(k-1) doesn't appear anywhere in the sum, then we have that
 * no two consecutive Fibonacci numbers are used.
 *
 * To see that F(k) doesn't appear twice, assume for the sake of contradiction
 * that it does.  This means that F(k) must be in the decomposition of 
 * n - F(k), and so we have that n - F(k) <= F(k), or that n <= 2F(k).  But
 * F(k) = F(k-1) + F(k-2) (assuming that F(k) >= 2; if it's not, then n = 0 or
 * n = 1 and we already know that the claim holds).  This means that
 * n <= F(k) + F(k-1) + F(k-2) = F(k+1) + F(k-2), contradicting the fact that
 * F(k) is the largest Fibonacci number smaller than n.
 *
 * To see that F(k-1) doesn't appear in the decomposition of n - F(k) we use
 * a similar proof by contradiction.  Assume that F(k-1) does appear in the
 * sum; then we have that n - F(k) <= F(k-1), so n <= F(k) + F(k-1) = F(k+1),
 * again contradicting that F(k) is the largest Fibonacci number less than n.
 *
 * Notice that this proof gives us a very simple algorithm for computing the
 * Zeckendorf decomposition of a number - just continuously subtract out the
 * largest Fibonacci number less than n and add it to the result.  It is
 * possible to compute this in O(lg n) time and O(1) space using the following
 * observation:
 *
 *   Given two consecutive Fibonacci numbers F(k) and F(k+1), we can compute
 *   the pairs (F(k-1), F(k)) and (F(k+1), F(k+2)) in constant time.
 *
 * This is true because
 *
 *   F(k-1) = F(k+1) - F(k)
 *
 * and
 *
 *   F(k+2) = F(k+1) + F(k)
 *
 * In other words, we can think of a pair of adjacent Fibonacci numbers as a
 * sort of "bidirectional iterator" that can advance forwards and backwards
 * one step in the Fibonacci sequence.
 *
 * Given such an iterator, we can compute the Zeckendorf decomposition in
 * O(lg n) time, O(1) space using the following trick:
 *
 *   1. Begin a "Fibonacci iterator" at (0, 1).
 *   2. Until the second term of the iterator exceeds n, advance the iterator.
 *   3. While n is not zero:
 *      1. If the first term of the iterator is less than n, add the first 
 *         term of the iterator to the resulting decomposition, then subtract
 *         the first term from n.
 *      2. Back the iterator up a step.
 *
 * This uses only O(1) memory (plus any memory used to store the resulting
 * sequence).  To see that it runs in O(lg n) time, recall that
 *
 *    F(k) = Theta(Phi^k)
 *
 * where Phi = (1 + sqrt(5)) / 2 is the Golden Ratio.
 *
 * The power of using this decomposition of a number is that the Fibonacci
 * sequence can be generalized to arbitrary groups, not just the natural
 * numbers under addition.  In fact, given any group G with generator g, we
 * can define the generalized Fibonacci sequence on G as
 *
 *    F_G (0)   = g^0 = 1
 *    F_G (1)   = g^1 = g
 *    F_G (n+2) = F_G(n)F_G(n+1) = g^(F(n) + F(n+1))
 *
 * (That last equality can be verified by a quick inductive argument)
 *
 * This provides us an extremely elegant algorithm for computing logarithms
 * using only O(lg lg n) arithmetic operations and O(1) space.  The idea is as
 * follows.  Suppose that we are given n = a^b and want to compute log_a(n).
 * Then we can compute b by computing its Zeckendorf decomposition in the
 * generalized Fibonacci with generator a.  More specifically:
 *
 *   1. Begin a "generalized Fibonacci iterator" at (a^0, a^1).  Record the
 *      exponents of both terms in the iterator at all times.
 *   2. Until the second term of the iterator exceeds n, advance the iterator.
 *   3. While n is not zero:
 *      1. If the first term of the iterator is less than n, add the
 *         exponent of the first term to the result, then divide n by the
 *         first term in the iterator.
 *      2. Back the iterator up a step.
 *
 * Assuming that b is integral, then this solution will yield an exact answer
 * after doing O(lg b) arithmetic operations.  Since b = O(lg n), this
 * algorithm runs using only O(lg lg n) arithmetic operations!  Moreover, it
 * uses memory for only a constant number of integers.
 */
         
#ifndef ZeckendorfLogarithm_Included
#define ZeckendorfLogarithm_Included

#include <utility>   // For pair
#include <stdexcept> // For std::domain_error

/**
 * Function: Integer ZeckendorfLogarithm(T n, const T& a);
 * Usage: cout << ZeckendorfLogarithm(256, 2) << endl; // Prints 8
 * ---------------------------------------------------------------------------
 * Given two values n and a, returns log_a n.  It is assumed that n is
 * an integral power of a; if not, the function rounds down.  If the base or
 * exponent is zero, a domain_error exception is thrown.
 *
 * The parameters must be of a type that supports multiplication and division.
 * It also must allow for the numeric values 0 and 1 to be cast to an element
 * of its type.  The elements of type T must be totally ordered and 
 * comparable.
 */
template <typename Integer, typename T>
Integer ZeckendorfLogarithm(T n, const T& a) {
  /* Check that the input is valid. */
  if (n == T(0))
    throw std::domain_error("Cannot take the log of zero.");
  if (a == T(0) || a == T(1))
    throw std::domain_error("Cannot take log base zero or one.");

  /* Construct our "Fibonacci iterator" to keep track of where we are in the
   * Fibonacci sequence, including both the base and the logarithm.
   */
  std::pair<T, T>             fibExponentIterator(T(1), a);
  std::pair<Integer, Integer> fibBaseIterator(0u, 1u);

  /* Keep marching the iterator forward until we arrive at n. */
  while (fibExponentIterator.second < n) {
    /* Compute the next value in the generalized Fibonacci sequence as
     *
     *   (F(k), F(k+1)) -> (F(k+1), F(k+2))
     */
    T nextExp = fibExponentIterator.first * fibExponentIterator.second;
    fibExponentIterator.first = fibExponentIterator.second;
    fibExponentIterator.second = nextExp;

    Integer nextBase = fibBaseIterator.first + fibBaseIterator.second;
    fibBaseIterator.first = fibBaseIterator.second;
    fibBaseIterator.second = nextBase;
  }

  Integer result = Integer(0);

  /* Keep dividing out terms from the Zeckendorf representation of the number
   * until it reaches one.
   */
  while (fibBaseIterator.first != Integer(0)) {
    /* If the first of the Fibonacci numbers is less than the current number,
     * divide it out and add the corresponding Fibonacci number to the
     * total.
     */
    if (fibExponentIterator.first <= n) {
      n /= fibExponentIterator.first;
      result += fibBaseIterator.first;
    }

    /* Walk both iterators backward a step:
     *
     *   (F(k), F(k+1)) -> (F(k-1), F(k))
     */
    T prevExp = fibExponentIterator.second / fibExponentIterator.first;
    fibExponentIterator.second = fibExponentIterator.first;
    fibExponentIterator.first  = prevExp;

    Integer prevBase = fibBaseIterator.second - fibBaseIterator.first;
    fibBaseIterator.second = fibBaseIterator.first;
    fibBaseIterator.first = prevBase;
  }

  return result;
}

#endif
