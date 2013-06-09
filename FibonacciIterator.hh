/******************************************************************************
 * File: FibonacciIterator.hh
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * An STL-style iterator for iterating across the Fibonacci numbers.  This
 * iterator stores two Fibonacci numbers, F(n) and F(n+1), and can move around
 * the entries of the Fibonacci sequence one at a time by using the two
 * values to produce either the next value in the sequence or the preivous
 * value in the sequence.  For example, given F(n) and F(n+1) we can produce
 * an iterator for F(n+1), F(n+2) by using the fact that
 *
 *    (F(n+1), F(n+2)) = (F(n+1), F(n) + F(n+1))
 *
 * Similarly, we can get an iterator to (F(n-1), F(n)) by noting that
 *
 *    (F(n-1), F(n)) = (F(n+1) - F(n), F(n))
 *
 * At each point, the iterator acts as though it is iterating over the first
 * value in the tuple, so the iterator (F(3), F(4)) looks like an iterator over
 * the value F(3) = 2.
 *
 * The implementation of a Fibonacci iterator provided in this header file
 * allows the type of the numbers used by the iterator to be customized along
 * with the binary addition and subtraction operators used.  This means that
 * the implementation can be customized so that it computes Fibonacci numbers
 * with a custom integer type, or to allow for computation of Fibonacci numbers
 * in a group other than the integers.  For example, if the addition and
 * subtraction operators are implemented as multiplication and division, then
 * the Fibonacci iterator will visit Fibonacci powers of a number, which can be
 * used to take logarithms efficiently.
 */
#ifndef FibonacciIterator_Included
#define FibonacciIterator_Included

#include <functional> // For std::plus, std::minus
#include <iterator>   // For std::bidirectional_iterator_tag, std::iterator

/**
 * An iterator class capable of navigating across the Fibonacci sequence using
 * a user-specified integer type.
 */
template <typename Integer, typename Plus = std::plus<Integer>,
          typename Minus = std::minus<Integer> >
class FibonacciIterator: public std::iterator<std::bidirectional_iterator_tag,
                                              const Integer> {
public:
  /**
   * Constructor: FibonacciIterator(Integer zero = Integer(0),
   *                                Integer one = Integer(1),
   *                                Plus p = Plus(), Minus m = Minus())
   * Usage: FibonacciIterator<int> itr;
   * --------------------------------------------------------------------------
   * Constructs a new Fibonacci iterator traversing the Fibonacci sequence
   * whose first two terms are zero and one and that uses the specified plus
   * and minus function objects to navigate the sequence.
   */
  explicit FibonacciIterator(Integer zero = Integer(0),
                             Integer one  = Integer(1),
                             Plus p = Plus(), Minus m = Minus());

  /**
   * operator*  () const;
   * operator-> () const;
   * Usage: cout << *itr << endl;
   * --------------------------------------------------------------------------
   * Dereferences and returns the current integer in the sequence.  You should
   * not modify the values returned as they are not guaranteed to be valid
   * after the iterator advances.  Moreover, you should not hold pointers or
   * references to these values, as the memory will be recycled after the
   * iterator is incremented or decremented.
   */
  const Integer& operator*  () const;
  const Integer* operator-> () const;

  /**
   * operator++ ();
   * operator++ (int);
   * operator-- ();
   * operator-- (int);
   * Usage: ++itr; --itr; itr++; itr--;
   * --------------------------------------------------------------------------
   * Moves the iterator one step forward or backward in the Fibonacci sequence.
   * If integer overflow occurs, the results depend on the type of the integer
   * being used as a counter.  If the iterator is backed up while at 0, the
   * results are mathematically well-defined but depend on the underlying type
   * of the integer for correctness.
   */
  FibonacciIterator&      operator++ ();
  const FibonacciIterator operator++ (int);

  FibonacciIterator&      operator-- ();
  const FibonacciIterator operator-- (int);

private:
  /* The current and next Fibonacci values in the sequence. */
  Integer curr, next;

  /* The plus and minus operators. */
  Plus plus;
  Minus minus;
};

/* Comparison functions for FibonacciIterator. */
template <typename Integer, typename Plus, typename Minus>
bool operator== (const FibonacciIterator<Integer, Plus, Minus>& lhs,
                 const FibonacciIterator<Integer, Plus, Minus>& rhs);
template <typename Integer, typename Plus, typename Minus>
bool operator!= (const FibonacciIterator<Integer, Plus, Minus>& lhs,
                 const FibonacciIterator<Integer, Plus, Minus>& rhs);

/* * * * * Implementation Below This Point * * * * */

/* Constructor sets up the internal fields based on the parameters. */
template <typename Integer, typename Plus, typename Minus>
FibonacciIterator<Integer, Plus, Minus>::FibonacciIterator(Integer zero,
                                                           Integer one,
                                                           Plus plus,
                                                           Minus minus)
  : curr(zero), next(one), plus(plus), minus(minus) {
  // Handled in initializer list.
}

/* Dereferencing to a value just returns the current value in the sequence. */
template <typename Integer, typename Plus, typename Minus>
const Integer& FibonacciIterator<Integer, Plus, Minus>::operator*  () const {
  return curr;
}
template <typename Integer, typename Plus, typename Minus>
const Integer* FibonacciIterator<Integer, Plus, Minus>::operator-> () const {
  return &**this;
}

/* Incrementing the Fibonacci iterator walks forward one step in the Fibonacci
 * series.
 */
template <typename Integer, typename Plus, typename Minus>
FibonacciIterator<Integer, Plus, Minus>&
FibonacciIterator<Integer, Plus, Minus>::operator++ () {
  Integer newNext = plus(curr, next);
  curr = next;
  next = newNext;
 
  return *this;
}
template <typename Integer, typename Plus, typename Minus>
const FibonacciIterator<Integer, Plus, Minus>
FibonacciIterator<Integer, Plus, Minus>::operator++ (int) {
  FibonacciIterator result = *this;
  ++ *this;
  return result;
}

/* Decrementing the Fibonacci iterator backs it up one step in the sequence. */
template <typename Integer, typename Plus, typename Minus>
FibonacciIterator<Integer, Plus, Minus>&
FibonacciIterator<Integer, Plus, Minus>::operator-- () {
  Integer prev = minus(next, curr);
  next = curr;
  curr = prev;

  return *this;
}
template <typename Integer, typename Plus, typename Minus>
const FibonacciIterator<Integer, Plus, Minus>
FibonacciIterator<Integer, Plus, Minus>::operator-- (int) {
  FibonacciIterator result = *this;
  -- *this;
  return result;
}

/* Equality comparisons just check if the two values are equal. */
template <typename Integer, typename Plus, typename Minus>
bool operator== (const FibonacciIterator<Integer, Plus, Minus>& lhs,
                 const FibonacciIterator<Integer, Plus, Minus>& rhs) {
  return lhs.curr == rhs.curr && lhs.next == rhs.next;
}

/* Disequality implemented in terms of equality. */
template <typename Integer, typename Plus, typename Minus>
bool operator!= (const FibonacciIterator<Integer, Plus, Minus>& lhs,
                 const FibonacciIterator<Integer, Plus, Minus>& rhs) {
  return !(lhs == rhs);
}

#endif
