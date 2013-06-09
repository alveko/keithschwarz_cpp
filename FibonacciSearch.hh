/******************************************************************************
 * File: FibonacciSearch.hh
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * An implementation of the Fibonacci search algorithm, an algorithm for
 * locating the position of a particular key in a sorted sequence of elements
 * in logarithmic time.  It is similar to binary search, though it uses a
 * different sequence of probes.
 *
 * The idea behind the algorithm is to do a modified binary search where
 * instead of probing the middle of the range at each step, we probe indices
 * based on the Fibonacci numbers.  In particular, at each point our probe is
 * at the index of the largest Fibonacci number smaller than the size of the
 * entire array.
 *
 * To understand how the search works, it's best to see it graphically.  If we
 * begin with a range of elements, we can split that range into two pieces,
 * one of which has a size equal to the largest Fibonacci number smaller than
 * the array size, and one of which has size equal to the rest of what remains.
 * This is shown here:
 *
 *     +------------------------------------+------------------------+
 *     |            F(k) elements           |   n - F(k) elements    |
 *     +------------------------------------+------------------------+
 *
 * When we probe the F(k)th element and decide which half we should search on,
 * we have two options.  If the element is in the block containing F(k)
 * elements, then we split those F(k) elements into two groups of size F(k-1)
 * and F(k-2), then recursively probe the split point, which occurs at
 * position F(k-1).  If the element is in the block containing n - F(k)
 * elements, then we begin by noting that n - F(k) < F(k-1), since if this
 * weren't true, then we'd have that n < F(k-1) + F(k) = F(k+1), and it would
 * no longer be true that F(k) is the largest Fibonacci number smaller than the
 * size of the input.
 *
 * One subtlety we have to be careful about is that when we split the elements
 * into two groups, one of these groups will not have a size that's a Fibonacci
 * number, and so one of the probes we make may be grossly out of range.  For
 * example, suppose that we're trying to search in an array of size 14.  This
 * would be split into two groups of size 13 and 1.  If the element we were
 * searching for were equal to that last element, the series of probes we would
 * be making would start at position 13 (one-indexed), realize that the element
 * in question is larger than this value, and thus continue at 13 + 5 = 18
 * (one-indexed), which is out of range.  We therefore pretend that the array
 * is padded on the right with infinitely many copies of an infinitely large
 * value, so that any search we make that ends up out of range will always
 * make it look like the element we're searching for is smaller than the value
 * we find.
 *
 * The reason that this algorithm works is due to Zeckendorf's theorem, which
 * states that any number can be written uniquely as the sum of unique
 * Fibonacci numbers in such a way that no two adjacent Fibonacci numbers are
 * used.  This gives rise to the "Fibonacci number system," a way of 
 * representing numbers in a mixed-radix binary system where the kth bit 
 * corresponds to the kth Fibonacci number.  For example, here are the first 
 * few numbers, written out in the Fibonacci number system:
 *
 *     100 = 89 + 8 + 2 + 1   = F(11) + F(5) + F(3) + F(1)
 *                            = 100000101010_F
 *      10 = 8 + 2            = F(5) + F(3)
 *                            = 101000_F
 *     137 = 89 + 34 + 13 + 1 = F(11) + F(9) + F(6) + F(1)
 *                            = 101001000010_F
 *
 * When executing a Fibonacci search, we are trying to recover each of these
 * Fibonacci bits one at a time.  The first probe sees whether the first digit
 * is a one or a zero.  If it's a one, then we know that the next digit must be
 * a zero (because no two consecutive Fibonacci numbers are used), while if
 * it's a zero we check whether the next digit is a one or zero with another
 * probe because we can't immediately tell its value.
 *
 * Fibonacci search is rarely used in practice, because a standard binary
 * search can be much more effective, but it is useful in that it does not do
 * any divisions - all the probes are computed from additions and subtractions.
 * This can be useful in some applications.
 *
 * This code relies on the existence of a FibonacciIterator class, also
 * available from the Archive.  You can find it online at
 *
 *    http://www.keithschwarz.com/interesting/code/?dir=fibonacci-iterator
 */
#ifndef FibonacciSearch_Included
#define FibonacciSearch_Included

/**
 * bool FibonacciSearch(RandomIterator begin, RandomIterator end, 
 *                      const Value& value);
 * Usage: if (FibonacciSearch(v.begin(), v.end(), value)) ...
 * ----------------------------------------------------------------------------
 * Uses the Fibonacci search technique to scan the range [begin, end) for the
 * given value, returning whether or not it was found.
 */
template <typename RandomIterator, typename Value>
bool FibonacciSearch(RandomIterator begin, RandomIterator end, 
                     const Value& value);

/**
 * bool FibonacciSearch(RandomIterator begin, RandomIterator end, 
 *                      const Value& value, Comparator comp);
 * Usage: if (FibonacciSearch(v.begin(), v.end(), value, myComparator)) ...
 * ----------------------------------------------------------------------------
 * Uses the Fibonacci search technique to scan the range [begin, end) for the
 * given value, returning whether or not it was found.  The elements are
 * assumed to be sorted in ascending order according to comp.
 */
template <typename RandomIterator, typename Value, typename Comparator>
bool FibonacciSearch(RandomIterator begin, RandomIterator end, 
                     const Value& value, Comparator comp);

/* * * * * Implementation Below This Point * * * * */

#include <iterator>   // For std::distance, std::iterator_traits
#include <functional> // For std::less
#include "FibonacciIterator.hh"

/* Main implementation of Fibonacci search uses a Fibonacci iterator to access
 * the consecutive Fibonacci numbers as efficiently as possible.
 */
template <typename RandomIterator, typename Value, typename Comparator>
bool FibonacciSearch(RandomIterator begin, RandomIterator end, 
                     const Value& value, Comparator comp) {
  /* See what type we use to keep track of iterator distances, then use that
   * to build our Fibonacci iterator.
   */
  typedef typename std::iterator_traits<RandomIterator>::difference_type Integer;

  /* Create a Fibonacci iterator so that we can quickly access Fibonacci values
   * in sequence.
   */
  FibonacciIterator<Integer> itr;

  /* Find the smallest Fibonacci number greater than the number of elements in
   * the range, then back it up one step.  This loop always executes at least
   * once, since when the iterator starts off it has value zero, which can't be
   * bigger than the number of elements.  For this reason, we write this as a
   * do ... while loop to make it clearer what's going on.
   */
  do
    ++ itr;
  while (*itr <= end - begin);

  /* Back up a step, which is well-defined because we took at least one
   * step.
   */
  -- itr;

  /* Until the iterator has reached zero (meaning that there's nothing left to
   * check), apply the Fibonacci search to the range.
   */
  while (*itr != Integer(0)) {
    /* See if this element is in range.  If it isn't, then we need to back the
     * iterator up a step.  Note that we allow *itr to be equal to end - begin
     * because we always read right before the given index (see below).
     */
    if (*itr > end - begin) {
      -- itr;
    }
    /* Otherwise, compare the element at the given index to the value we're
     * looking for.  If the current element is larger, then we look in the
     * first half of the array.
     *
     * Notice that we compare the value at begin[*itr - 1] rather than at
     * begin[*itr].  This is because the array is zero-indexed, but *itr is
     * giving us back one-indexed locations.
     */
    else if (comp(value, begin[*itr - Integer(1)])) {
      -- itr;
    }
    /* If the value we're probing is larger than the element in question, then
     * we continue the search on the right.  We also drop the Fibonacci number
     * twice in order to avoid a useless comparison.
     */
    else if (comp(begin[*itr - Integer(1)], value)) {
      begin += *itr;
      -- itr; -- itr;
    }
    /* Otherwise, we know that the values match, and so we're done. */
    else
      return true;
  }

  /* If we made it here, we didn't find the value in question. */
  return false;
}

/* Non-comparator version implemented in terms of the comparator version. */
template <typename RandomIterator, typename Value>
bool FibonacciSearch(RandomIterator begin, RandomIterator end,
                     const Value& value) {
  return 
    FibonacciSearch(begin, end, value,
                    std::less<typename std::iterator_traits<RandomIterator>::value_type>());
}

#endif
